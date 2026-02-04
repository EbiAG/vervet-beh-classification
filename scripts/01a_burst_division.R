library(data.table)
library(tidyverse)
library(arrow)

# Schema for output parquet files ----
acc_raw_schema <- arrow::schema(
    Ind_ID = arrow::string(),
    burst_id = arrow::int64(),
    new_burst = arrow::int64(),
    timestamp = arrow::timestamp(unit = "ms", timezone = "Africa/Johannesburg"),
    X = arrow::float32(),
    Y = arrow::float32(),
    Z = arrow::float32()
)

# Load and merge all acceleration data files ----
load_and_merge_all_files <- function(
    file_paths, drop_cols = c(1:5, 8, 10:13, 15),
    tz = "Africa/Johannesburg", tz_offset = -19,
    verbose = TRUE
) {
    if (verbose) cat("Loading and merging all acceleration data files...\n")
    start_time <- Sys.time()

    # Validate input files
    missing <- file_paths[!file.exists(file_paths)]
    if (length(missing) > 0) {
        stop(paste("Files not found:", paste(missing, collapse = ", ")))
    }

    # Load all files
    data_list <- lapply(file_paths, function(fp) {
        if (verbose) cat(sprintf("  Loading: %s\n", basename(fp)))
        data.table::fread(fp, fill = TRUE, drop = drop_cols)
    })

    # Merge all files
    acc_data <- data.table::rbindlist(data_list)

    if (verbose) {
        cat(sprintf("  Total rows loaded: %d\n", nrow(acc_data)))
    }

    # Convert timestamp from UTC to target timezone with offset
    acc_data[, timestamp := as.POSIXct(
        as.POSIXlt(timestamp, tz = "UTC"),
        tz = tz
    ) + tz_offset]

    # Rename column for clarity
    setnames(acc_data, "individual-local-identifier", "Ind_ID")

    # Sort by individual and timestamp for efficient subsetting
    setorder(acc_data, Ind_ID, timestamp)

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(sprintf(
            "All files loaded and merged in %.2f minutes\n",
            as.numeric(elapsed)
        ))
    }

    return(acc_data)
}

# Parse raw acceleration for individual's data ----
parse_raw_acceleration <- function(dt, verbose = FALSE) {
    if (verbose) cat("  Parsing raw acceleration...\n")

    # Check if raw column exists
    if (!("eobs:accelerations-raw" %in% names(dt))) {
        stop("Column 'eobs:accelerations-raw' not found")
    }

    # Determine sampling structure from first non-NA value
    first_raw <- dt[
        !is.na(`eobs:accelerations-raw`), `eobs:accelerations-raw`
    ][1]
    acc_sample_count <- length(unlist(strsplit(first_raw, " ")))
    sampling_nb <- acc_sample_count / 3

    # Create axis identifiers
    axes_names <- rep(c("X", "Y", "Z"), sampling_nb)
    sampling_id <- rep(1:sampling_nb, each = 3)
    axes_id <- paste(axes_names, sampling_id, sep = ".")

    # Separate raw acceleration
    dt <- tidyr::separate(
        dt,
        col = `eobs:accelerations-raw`,
        into = axes_id, sep = " ", remove = TRUE
    )

    # Convert to numeric
    dt[, (axes_id) := lapply(.SD, as.numeric), .SDcols = axes_id]

    return(list(dt = dt, sampling_nb = sampling_nb, axes_id = axes_id))
}

# Reshape individual's data to long format ----
reshape_to_long_format <- function(
    dt, sampling_nb, axes_id, verbose = FALSE
) {
    if (verbose) cat("  Reshaping to long format...\n")

    # Create burst ID based on row number
    dt[, burst_id := seq_len(.N)]

    # Keep only ID, burst_id, timestamp and acceleration columns
    dt <- dt[, c("Ind_ID", "burst_id", "timestamp", axes_id), with = FALSE]

    # Melt to long format
    dt <- melt(
        dt,
        id.vars = c("Ind_ID", "burst_id", "timestamp"),
        variable.name = "axis_sample",
        value.name = "value"
    )

    # Extract axis and sample number
    dt[, c("axis", "sample") := tstrsplit(axis_sample, "\\.", fixed = TRUE)]
    dt[, sample := as.integer(sample)]
    dt[, axis_sample := NULL]

    # Cast back to wide format (X, Y, Z columns)
    dt <- dcast(
        dt,
        Ind_ID + burst_id + timestamp + sample ~ axis,
        value.var = "value"
    )

    # Remove incomplete samples, if any
    dt <- dt[!is.na(X) & !is.na(Y) & !is.na(Z)]

    setorder(dt, burst_id, sample)

    return(dt)
}

# Generate timestamps for individual's acceleration samples ----
generate_timestamps <- function(
    dt, sampling_frequency = 10, verbose = FALSE
) {
    if (verbose) cat("  Generating sample timestamps...\n")

    # Calculate sample interval in milliseconds
    sample_interval <- 1000 / sampling_frequency

    # Get burst start time (first sample per burst)
    burst_starts <- dt[, .(burst_start = timestamp[1]), by = burst_id]
    dt <- merge(dt, burst_starts, by = "burst_id")

    # Calculate timestamp for each sample
    dt[, timestamp := burst_start + (sample - 1) * sample_interval * 1000]
    dt[, timestamp := as.POSIXct(timestamp / 1000,
        origin = "1970-01-01",
        tz = "Africa/Johannesburg"
    )]
    dt[, burst_start := NULL]

    return(dt)
}

# Create burst divisions for individual data ----
create_burst_divisions <- function(
    dt, burst_divider = 1, verbose = FALSE
) {
    if (burst_divider == 1) {
        dt[, new_burst := burst_id]
        return(dt)
    }

    # Get sample count per burst
    burst_sample_counts <- dt[, .N, by = burst_id]
    min_samples <- burst_sample_counts[, min(N)]

    # Calculate samples to keep
    samples_to_keep <- min_samples - (min_samples %% burst_divider)

    # Keep only valid samples and create new burst IDs
    dt <- dt[sample <= samples_to_keep]

    dt[, new_burst := as.integer(
        burst_id * burst_divider + ceiling(sample / (samples_to_keep / burst_divider)) - 1
    )]

    return(dt)
}

# Save individual data to parquet ----
save_individual_parquet <- function(
    dt, individual, output_dir, burst_divider = 1,
    schema = acc_raw_schema,
    compression = "zstd", compression_level = 9,
    verbose = FALSE
) {
    if (nrow(dt) == 0) {
        return(invisible(NULL))
    }

    # Ensure correct types
    dt[, Ind_ID := as.character(Ind_ID)]
    dt[, burst_id := as.integer(burst_id)]
    dt[, new_burst := as.integer(new_burst)]
    dt[, X := as.numeric(X)]
    dt[, Y := as.numeric(Y)]
    dt[, Z := as.numeric(Z)]

    # Select columns in schema order
    dt <- dt[, names(schema$names), with = FALSE]

    # Create arrow table
    ar_table <- arrow::Table$create(dt, schema = schema)

    # Generate filename with burst divider value
    output_file <- file.path(
        output_dir,
        sprintf(
            "ACC_data_burst_%d_individual_%s_uncorrected.parquet",
            burst_divider, individual
        )
    )

    # Write parquet
    arrow::write_parquet(
        ar_table, output_file,
        compression = compression,
        compression_level = compression_level
    )

    if (verbose) {
        cat(sprintf(
            "    Saved: %s (%d rows)\n", basename(output_file), nrow(dt)
        ))
    }

    return(invisible(NULL))
}

# Main processing function ----
process_raw_acc_data <- function(
    input_files, output_dir,
    timezone = "Africa/Johannesburg", tz_offset = -19,
    sampling_frequency = 10, burst_dividers = c(1, 2, 3, 4),
    compression = "zstd", compression_level = 9,
    verbose = TRUE) {
    total_start <- Sys.time()

    if (verbose) {
        cat("====== Processing Raw Accelerometer Data ======\n")
        cat(sprintf("Timestamp: %s\n", format(Sys.time(), "%d/%m/%Y %H:%M:%S")))
        cat(sprintf("Input files: %d\n", length(input_files)))
        cat(sprintf("Output directory: %s\n", output_dir))
        cat(sprintf("Timezone: %s | Offset: %d seconds\n", timezone, tz_offset))
        cat(sprintf("Sampling frequency: %.1f Hz\n", sampling_frequency))
        cat(sprintf(
            "Burst dividers: %s\n", paste(burst_dividers, collapse = ", ")
        ))
        cat("===============================================\n\n")
    }

    # Create output directories
    for (divider in burst_dividers) {
        div_dir <- file.path(output_dir, paste0("burst_divided_by_", divider))
        if (!dir.exists(div_dir)) {
            dir.create(div_dir, recursive = TRUE)
        }
    }

    # 1. Load and merge all data files
    acc_data <- load_and_merge_all_files(
        input_files,
        tz = timezone, tz_offset = tz_offset,
        verbose = verbose
    )

    if (verbose) cat("\n")

    # 2. Get individual IDs
    individuals <- sort(unique(acc_data$Ind_ID))

    if (verbose) {
        cat(sprintf("Found %d individuals\n", length(individuals)))
        cat("===============================================\n\n")
    }

    # 3. Process each individual
    for (i in seq_along(individuals)) {
        ind <- individuals[i]

        if (verbose) {
            cat(sprintf(
                "[%d/%d] Processing individual: %s\n",
                i, length(individuals), ind
            ))
        }

        ind_start <- Sys.time()

        # Filter data for this individual
        ind_data <- acc_data[Ind_ID == ind]

        if (nrow(ind_data) == 0) {
            if (verbose) cat("  No data found, skipping\n\n")
            next
        }

        if (verbose) {
            cat(sprintf("  Rows: %d\n", nrow(ind_data)))
        }

        # Parse raw acceleration for this individual
        parse_result <- parse_raw_acceleration(
            ind_data,
            verbose = verbose
        )
        ind_data <- parse_result$dt
        sampling_nb <- parse_result$sampling_nb
        axes_id <- parse_result$axes_id

        # Reshape to long format for this individual
        ind_data <- reshape_to_long_format(
            ind_data, sampling_nb, axes_id,
            verbose = verbose
        )

        # Generate timestamps for this individual
        ind_data <- generate_timestamps(
            ind_data,
            sampling_frequency = sampling_frequency,
            verbose = verbose
        )

        if (verbose) {
            cat(sprintf(
                "  Bursts: %d | Samples: %d\n",
                uniqueN(ind_data$burst_id), nrow(ind_data)
            ))
        }

        # Process each burst divider
        for (divider in burst_dividers) {
            # Create burst divisions for this individual
            dt_divided <- create_burst_divisions(
                copy(ind_data),
                burst_divider = divider, verbose = FALSE
            )

            # Save to parquet
            div_dir <- file.path(output_dir, paste0("Burst_", divider))
            save_individual_parquet(
                dt_divided,
                individual = ind, output_dir = div_dir,
                burst_divider = divider,
                schema = acc_raw_schema,
                compression = compression,
                compression_level = compression_level,
                verbose = FALSE
            )

            # Clean up
            rm(dt_divided)
        }

        # Clean up individual data
        rm(ind_data)
        gc()

        if (verbose) {
            elapsed <- difftime(Sys.time(), ind_start, units = "secs")
            cat(sprintf("  Completed in %.1f seconds\n\n", as.numeric(elapsed)))
        }
    }

    # Clean up full dataset
    rm(acc_data)
    gc()

    # Report total time
    total_elapsed <- difftime(Sys.time(), total_start, units = "mins")
    if (verbose) {
        cat("===============================================\n")
        cat(sprintf(
            "Processing complete: %s\n",
            format(Sys.time(), "%d/%m/%Y %H:%M:%S")
        ))
        cat(sprintf("Total time: %.2f minutes\n", as.numeric(total_elapsed)))
        cat("===============================================\n")
    }

    return(invisible(NULL))
}

# Usage example ----
# process_raw_acc_data(
#     input_files = c(
#         "../../Movebank_ACC_Data/1_Acceleration_2023_02_01.csv",
#         "../../Movebank_ACC_Data/2_Acceleration_2023_02-02_to_2023-12.csv"
#     ),
#     output_dir = "./data/raw/acc/",
#     timezone = "Africa/Johannesburg",
#     tz_offset = -19,
#     sampling_frequency = 10,
#     burst_dividers = c(1, 2, 3, 4),
#     compression = "zstd",
#     compression_level = 9,
#     verbose = TRUE
# )