library(data.table)
library(zoo)
library(tidyverse)
library(arrow)

# This script is a template for the orientation correction of the raw accelerometer data based on a rotation matrix.
# It is meant to be run on the full 1.5 year dataset only.
# This will not work for shortlisted annotated data, since the shortlisted data does not have sufficient data from each individual for daily and basal rotation matrix calculations.

# Functions for raw data corrections ----
## Schema for parquet files ----
acc_schema <- arrow::schema(
    # Ind_ID as dictionary with minimal int size since each file has one ID
    Ind_ID = arrow::string(),

    # Burst identifiers
    burst_id = arrow::int64(),
    new_burst = arrow::int64(),

    # Standard approach - single timestamp column
    # timestamp = arrow::timestamp(unit = "ms"),
    # Delta encoding approach
    burst_start_time = arrow::timestamp(
        unit = "ms", timezone = "Africa/Johannesburg"
    ),
    sample_number = arrow::int16(),

    # Acceleration values as 32-bit floats
    X = arrow::float32(),
    Y = arrow::float32(),
    Z = arrow::float32()
)

## Load parquet file as data.table ----
load_parquet <- function(file_path, select_cols = NULL, verbose = TRUE) {
    # Check if file exists
    if (!file.exists(file_path)) {
        stop(paste("File not found:", file_path))
    }

    # Start timing
    start_time <- Sys.time()

    if (verbose) cat("Loading parquet file:", file_path, "\n")

    # Read the parquet file
    if (is.null(select_cols)) {
        df <- arrow::read_parquet(file_path)
    } else {
        df <- arrow::read_parquet(file_path, col_select = select_cols)
    }

    # Convert to data.table
    dt <- data.table::as.data.table(df)

    # Report information
    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(sprintf(
            "Loaded %d rows x %d columns in %.2f minutes\n",
            nrow(dt), ncol(dt), as.numeric(elapsed)
        ))
        cat(sprintf(
            "Memory usage: %.2f MB\n",
            object.size(dt) / 1024^2
        ))
    }

    return(dt)
}

## Convert raw acceleration counts to g ----
convert_to_g <- function(dt) {
    # Constants for conversion
    SLOPE <- 1 / 512
    OFFSET <- 2048

    # Standard data.table in-place modification (very fast)
    dt[, `:=`(
        X = (X - OFFSET) * SLOPE,
        Y = (Y - OFFSET) * SLOPE,
        Z = (Z - OFFSET) * SLOPE
    )]

    return(dt)
}

## Vector normalisation ----
normalize_vec <- function(v) {
    v <- as.numeric(v)
    n <- sqrt(sum(v^2))
    if (n == 0 || is.na(n)) {
        return(rep(NA_real_, length(v)))
    }
    v / n
}

## 3D cross product ----
cross3d <- function(a, b) {
    c(
        a[2] * b[3] - a[3] * b[2],
        a[3] * b[1] - a[1] * b[3],
        a[1] * b[2] - a[2] * b[1]
    )
}

## Compute static components grouped by burst ----
compute_static_components <- function(
    dt, static_window_size = 6, verbose = TRUE) {
    # Start timing
    start_time <- Sys.time()
    # Apply rollmean to each axis within each burst group
    dt[, `:=`(
        X_static = nafill(nafill(frollmean(
            X,
            n = static_window_size, align = "center"
        ), "nocb"), "locf"),
        Y_static = nafill(nafill(frollmean(
            Y,
            n = static_window_size, align = "center"
        ), "nocb"), "locf"),
        Z_static = nafill(nafill(frollmean(
            Z,
            n = static_window_size, align = "center"
        ), "nocb"), "locf")
    ), by = new_burst]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(sprintf(
            "Static components calculated in %.2f minutes\n",
            as.numeric(elapsed)
        ))
    }

    return(dt)
}

## Compute dynamic body acceleration ----
compute_dba <- function(dt, verbose = TRUE) {
    start_time <- Sys.time()

    dt[, `:=`(
        DBA_X = X - X_static,
        DBA_Y = Y - Y_static,
        DBA_Z = Z - Z_static
    )]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cat(sprintf("DBA computed in %.2f seconds\n", as.numeric(elapsed)))
    }

    return(dt)
}

## Compute VeDBAs (smoothed VeDBA) for each burst ----
compute_burst_vedbas <- function(dt, static_window_size = 6, verbose = FALSE) {
    # Start timing if verbose mode is on
    start_time <- Sys.time()

    # Verify DBA columns exist
    if (!all(c("DBA_X", "DBA_Y", "DBA_Z") %in% names(dt))) {
        stop("DBA columns (DBA_X, DBA_Y, DBA_Z) must exist in data.table. Run compute_dba() first.")
    }

    # get unique bursts
    total_bursts <- uniqueN(dt$new_burst)

    # Process each burst separately
    result_dt <- dt[,
        {
            # 1. Calculate VeDBA (vectorial dynamic body acceleration)
            vedba <- sqrt(DBA_X^2 + DBA_Y^2 + DBA_Z^2)

            # 2. Smooth VeDBA using rollmean
            vedbas <- nafill(nafill(frollmean(
                vedba,
                n = static_window_size, align = "center"
            ), "nocb"), "locf")

            # Calculate mean vedba
            mean_vedba_val <- mean(vedba, na.rm = TRUE)

            # 3. Calculate mean VeDBAs for this burst
            mean_vedbas_val <- mean(vedbas, na.rm = TRUE)

            # Return a single-row data.table with the summary value
            list(mean_vedbas = mean_vedbas_val, mean_vedba = mean_vedba_val)
        },
        by = new_burst
    ]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(
            "\nVeDBAs computation completed in",
            sprintf("%.2f minutes", as.numeric(elapsed)), "\n"
        )
    }

    # Return just the burst_id and mean_vedbas columns
    return(result_dt[, .(new_burst, mean_vedbas, mean_vedba)])
}

## Identify walking bursts based on VeDBAs thresholds ----
identify_walking_bursts <- function(
    burst_vedbas, vedbas_threshold = c(0.17, 0.3), verbose = FALSE) {
    min_vedbas <- vedbas_threshold[1]
    max_vedbas <- vedbas_threshold[2]

    # Identify walking bursts based on vedbas threshold
    walking_bursts <- burst_vedbas[
        mean_vedbas > min_vedbas &
            mean_vedbas < max_vedbas,
        new_burst
    ]

    if (verbose) {
        cat(sprintf(
            "Found %d walking bursts (%.1f%% of total)\n",
            length(walking_bursts),
            100 * length(walking_bursts) / uniqueN(burst_vedbas$new_burst)
        ))
    }

    return(walking_bursts)
}

## Compute rotation matrix from gravity + DBA structure ----
compute_rotation_matrices <- function(
    dt, walking_burst_ids, verbose = FALSE
) {
    start_time <- Sys.time()

    # Filter to walking bursts only
    walking_data <- dt[new_burst %in% walking_burst_ids]

    # Compute rotation matrix for each walking burst
    rotation_matrices <- walking_data[,
        {
            # Need minimum data points for reliable rotation matrix
            if (.N < 20) {
                list(
                    R11 = NA_real_, R12 = NA_real_, R13 = NA_real_,
                    R21 = NA_real_, R22 = NA_real_, R23 = NA_real_,
                    R31 = NA_real_, R32 = NA_real_, R33 = NA_real_
                )
            } else {
                # 1) Gravity direction from mean static acceleration
                g_vec <- c(mean(X_static), mean(Y_static), mean(Z_static))
                g_hat <- normalize_vec(g_vec)
                z_body <- -g_hat # Up direction opposite to gravity

                # 2) Forward direction from DBA (PCA)
                dba_mat <- cbind(DBA_X, DBA_Y, DBA_Z)

                pc <- tryCatch(
                    prcomp(dba_mat, center = TRUE, scale. = FALSE),
                    error = function(e) NULL
                )

                if (is.null(pc)) {
                    list(
                        R11 = NA_real_, R12 = NA_real_, R13 = NA_real_,
                        R21 = NA_real_, R22 = NA_real_, R23 = NA_real_,
                        R31 = NA_real_, R32 = NA_real_, R33 = NA_real_
                    )
                } else {
                    # PC1 = main forward/back axis
                    x_temp <- normalize_vec(pc$rotation[, 1])

                    # Make orthogonal to vertical
                    x_body <- x_temp - sum(x_temp * z_body) * z_body
                    x_body <- normalize_vec(x_body)

                    if (any(is.na(x_body))) {
                        list(
                            R11 = NA_real_, R12 = NA_real_, R13 = NA_real_,
                            R21 = NA_real_, R22 = NA_real_, R23 = NA_real_,
                            R31 = NA_real_, R32 = NA_real_, R33 = NA_real_
                        )
                    } else {
                        # 3) Sideways axis (cross product)
                        y_body <- cross3d(z_body, x_body)
                        y_body <- normalize_vec(y_body)

                        # Flip if mean projection is negative to keep +x forward
                        mean_proj <- mean(dba_mat %*% x_body)
                        if (!is.na(mean_proj) && mean_proj < 0) {
                            x_body <- -x_body
                            y_body <- -y_body
                        }

                        # Build rotation matrix
                        B <- cbind(x_body, y_body, z_body)
                        R <- t(B)

                        list(
                            R11 = R[1, 1], R12 = R[1, 2], R13 = R[1, 3],
                            R21 = R[2, 1], R22 = R[2, 2], R23 = R[2, 3],
                            R31 = R[3, 1], R32 = R[3, 2], R33 = R[3, 3]
                        )
                    }
                }
            }
        },
        by = .(Ind_ID, new_burst)
    ]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cat(sprintf("Rotation matrices computed in %.2f seconds\n", as.numeric(elapsed)))
    }

    return(rotation_matrices)
}

## Compute daily mean rotation matrices ----
compute_daily_rotation_matrices <- function(
    rotation_matrices, dt, verbose = FALSE
) {
    # Ensure date column exists
    dt[, date := as.Date(timestamp)]

    # Merge dates with rotation matrices
    rm_with_dates <- merge(rotation_matrices,
        unique(dt[, .(Ind_ID, new_burst, date)]),
        by = c("Ind_ID", "new_burst")
    )

    # Average rotation matrix elements by date
    daily_rm <- rm_with_dates[, lapply(.SD, mean, na.rm = TRUE),
        by = .(Ind_ID, date),
        .SDcols = c(
            "R11", "R12", "R13",
            "R21", "R22", "R23",
            "R31", "R32", "R33"
        )
    ]

    # Map to all bursts
    burst_dates <- unique(dt[, .(Ind_ID, new_burst, date)])
    daily_rm_all <- merge(burst_dates, daily_rm, by = c("Ind_ID", "date"))

    return(daily_rm_all[
        , .(
            Ind_ID, new_burst, R11, R12, R13,
            R21, R22, R23, R31, R32, R33
        )
    ])
}

## Compute basal rotation matrices ----
compute_basal_rotation_matrices <- function(
    rotation_matrices, dt, basal_window_size, verbose = FALSE
) {
    # Create full burst list
    all_bursts <- unique(dt[, .(Ind_ID, new_burst)])

    # Merge and fill
    rm_all <- merge(all_bursts, rotation_matrices,
        by = c("Ind_ID", "new_burst"), all.x = TRUE
    )

    setkey(rm_all, Ind_ID, new_burst)

    # Fill each matrix element
    matrix_cols <- c(
        "R11", "R12", "R13", "R21", "R22", "R23",
        "R31", "R32", "R33"
    )
    for (col in matrix_cols) {
        rm_all[, (col) := nafill(
            nafill(get(col), type = "nocb"),
            type = "locf"
        ), by = Ind_ID]
    }

    # Apply rolling mean
    for (col in matrix_cols) {
        basal_col <- paste0("basal_", col)
        rm_all[, (basal_col) := nafill(nafill(
            frollmean(get(col), n = basal_window_size, align = "center"),
            "nocb"
        ), "locf"), by = Ind_ID]
    }

    # Return only basal columns
    return(rm_all[, .(
        Ind_ID, new_burst,
        basal_R11, basal_R12, basal_R13,
        basal_R21, basal_R22, basal_R23,
        basal_R31, basal_R32, basal_R33
    )])
}

## Apply rotation matrix correction ----
apply_matrix_correction <- function(
    dt, matrix_data, correction_type = "daily", verbose = FALSE
) {
    start_time <- Sys.time()

    # Determine column prefix
    prefix <- if (correction_type == "daily") "" else "basal_"

    # Merge matrices with data
    dt_merged <- merge(dt, matrix_data, by = c("Ind_ID", "new_burst"))

    # Apply rotation: [X', Y', Z'] = R * [X, Y, Z]
    dt_merged[, `:=`(
        X_corrected = get(paste0(prefix, "R11")) * X +
            get(paste0(prefix, "R12")) * Y +
            get(paste0(prefix, "R13")) * Z,
        Y_corrected = get(paste0(prefix, "R21")) * X +
            get(paste0(prefix, "R22")) * Y +
            get(paste0(prefix, "R23")) * Z,
        Z_corrected = get(paste0(prefix, "R31")) * X +
            get(paste0(prefix, "R32")) * Y +
            get(paste0(prefix, "R33")) * Z
    )]

    # Update original columns
    dt_merged[, `:=`(
        X = round(X_corrected, 4),
        Y = round(Y_corrected, 4),
        Z = round(Z_corrected, 4)
    )]

    # Clean up
    dt_merged[, c(
        "X_corrected", "Y_corrected", "Z_corrected",
        paste0(prefix, c(
            "R11", "R12", "R13", "R21", "R22", "R23",
            "R31", "R32", "R33"
        ))
    ) := NULL]

    if (correction_type == "daily") dt_merged[, date := NULL]

    keep_cols <- c(
        "Ind_ID", "burst_id", "new_burst", "timestamp", "X", "Y", "Z"
    )
    dt_merged <- dt_merged[, ..keep_cols]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cat(sprintf("Matrix correction applied in %.2f seconds\n", as.numeric(elapsed)))
    }

    return(dt_merged)
}

## Function to create delta encoding columns ----
apply_delta_encoding <- function(dt, verbose = TRUE) {
    start_time <- Sys.time()
    # Calculate sample intervals within each burst and overall
    intervals_by_burst <- dt[order(timestamp), .(
        interval = median(diff(as.numeric(timestamp)))
    ), by = new_burst]

    # Get the overall sample interval (typically constant)
    sample_interval <- intervals_by_burst[, median(interval)]

    if (verbose) {
        cat(sprintf(
            "Detected sample interval: %.3f milliseconds\n",
            sample_interval
        ))
    }

    # Check if sampling is regular enough for delta encoding
    interval_variation <- intervals_by_burst[, sd(interval) / mean(interval)]
    if (interval_variation > 0.01) { # More than 1% variation
        warning(
            "Sample intervals vary by more than 1%.
            Delta encoding may lose precision."
        )
        if (verbose) {
            cat(
                "WARNING: Sample interval variation detected.
                Proceeding with median value.\n"
            )
        }
    }

    # Create burst_start_time column
    dt[, burst_start_time := min(timestamp), by = new_burst]

    # Create sample_number column (1-based counter within each burst)
    dt[order(timestamp), sample_number := seq_len(.N), by = new_burst]

    # Drop original timestamp column
    dt[, timestamp := NULL]

    # Order columns for consistency
    dt <- dt[, .(
        Ind_ID, burst_id, new_burst, burst_start_time, sample_number, X, Y, Z
    )]

    # Store attributes
    data.table::setattr(dt, "sample_interval", sample_interval)
    data.table::setattr(dt, "delta_encoded", TRUE)
    data.table::setattr(dt, "individual", unique(dt$Ind_ID))

    # Get timezone and set it
    tz <- attr(dt$burst_start_time, "tzone")[1]
    if (is.null(tz)) tz <- "Africa/Johannesburg" # Default if missing
    data.table::setattr(dt, "timezone", tz)

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cat(sprintf(
            "Encoding completed in %.2f seconds\n",
            as.numeric(elapsed)
        ))
    }

    return(dt)
}

## Function to convert to arrow table ----
convert_to_arrow_table <- function(dt, schema = acc_schema) {
    # First change new_burst to integer
    dt[, new_burst := as.integer(new_burst)]

    # Convert to arrow table using schema
    ar_tb <- arrow::Table$create(
        dt,
        schema = schema
    )

    # Add metadata
    ar_tb$metadata$individual <- attr(dt, "individual")
    ar_tb$metadata$sample_interval <- attr(dt, "sample_interval")
    ar_tb$metadata$delta_encoded <- attr(dt, "delta_encoded")
    ar_tb$metadata$timezone <- attr(dt, "timezone")

    return(ar_tb)
}

## Apply all the functions sequentially based on correction type ----
process_acc_data <- function(
    input_path, output_dir, schema = acc_schema,
    correction_type = "daily", roll_only = TRUE,
    static_window_size = 6, basal_window_size = NULL,
    vedbas_threshold = c(0.17, 0.3),
    compression = "zstd", compression_level = 9,
    verbose = TRUE, return_data = FALSE) {
    # Check if correction type is either basal or daily
    if (!correction_type %in% c("basal", "daily")) {
        stop("Correction type must be either 'basal' or 'daily'")
    }

    # Start timing
    total_start <- Sys.time()
    if (verbose) {
        cat("==== Processing Accelerometer Data ====\n")
        if (!is.null(input_path)) {
            cat("Input file:", input_path, "\n")
        }
        cat("Correction type:", correction_type, "\n")
        cat("Static window size:", static_window_size, "\n")
        if (correction_type == "basal") {
            cat("Basal window size:", basal_window_size, "\n")
        }
        cat(
            "Correction mode:",
            ifelse(roll_only, "Roll only", "Roll and Pitch"), "\n"
        )
        cat("VeDBAs threshold:", paste(vedbas_threshold, collapse = "-"), "\n")
        cat("======================================\n\n")
    }

    # 1. Load data
    dt <- load_parquet(input_path, verbose = verbose)

    # 2. Convert raw counts to g units
    dt <- convert_to_g(dt)

    # 3. Compute static components
    dt <- compute_static_components(dt, static_window_size, verbose = verbose)

    # 4. Compute DBA
    dt <- compute_dba(dt, verbose = verbose)

    # 5. Compute burst-level VeDBAs
    burst_vedbas <- compute_burst_vedbas(
        dt, static_window_size,
        verbose = verbose
    )

    # 6. Identify walking bursts
    walking_bursts <- identify_walking_bursts(
        burst_vedbas, vedbas_threshold,
        verbose = verbose
    )

    # 7. Ensure date column exists
    dt[, date := as.Date(timestamp)]

    # 8. Compute rotation matrices for walking bursts
    rotation_matrices <- compute_rotation_matrices(
        dt, walking_bursts,
        verbose = verbose
    )

    # 9. Compute rotation corrections
    if (correction_type == "daily") {
        daily_rm <- compute_daily_rotation_matrices(
            rotation_matrices, dt,
            verbose = verbose
        )
        dt <- apply_matrix_correction(
            dt, daily_rm,
            correction_type = "daily", verbose = verbose
        )
    } else if (correction_type == "basal") {
        basal_rm <- compute_basal_rotation_matrices(
            rotation_matrices, dt, basal_window_size,
            verbose = verbose
        )
        dt <- apply_matrix_correction(
            dt, basal_rm,
            correction_type = "basal", verbose = verbose
        )
    } else {
        # Just to be explicit
        stop("Invalid correction_type. Must be either 'daily' or 'basal'")
    }

    # 10. Apply delta encoding
    dt <- apply_delta_encoding(dt, verbose = verbose)

    # 11. Convert to arrow table
    ar_tb <- convert_to_arrow_table(dt, schema = schema)

    # 12. Save output
    if (!is.null(output_dir)) {

        # Generate filename with correction type and thresholds
        input_filename <- basename(input_path)
        output_filename <- gsub(
            "\\.parquet$",
            paste0(
                "_rotation_", correction_type, ".parquet"
            ),
            input_filename
        )

        # Create full output path
        output_path <- file.path(output_dir, output_filename)

        # Write to parquet file
        arrow::write_parquet(
            ar_tb, output_path,
            compression = compression,
            compression_level = compression_level
        )
        if (verbose) cat("Data saved to:", output_path, "\n")
    }

    # Report total time
    total_elapsed <- as.numeric(
        difftime(Sys.time(), total_start, units = "mins")
    )
    if (verbose) {
        cat(sprintf(
            "Processing complete at %s\n",
            format(Sys.time(), "%d/%m/%Y %H:%M:%S")
        ))
        cat(sprintf("Total processing time: %.2f minutes\n", total_elapsed))
        cat("============================\n")
    }

    if (return_data) {
        return(dt)
    } else {
        return(invisible(NULL))
    }
}

##  Process multiple accelerometer files with all correction types ----
process_acc_data_folder <- function(
    input_dir, output_dir, schema = acc_schema,
    static_window_size = 6, basal_window_size = NULL,
    vedbas_threshold = c(0.17, 0.3), compression = "zstd",
    compression_level = 9, verbose = TRUE) {
    # Start timing
    batch_start <- Sys.time()

    # Create output directories if they don't exist
    subfolder_names <- c(
        "uncorrected", "rotation_daily", "rotation_basal"
    )
    output_dirs <- lapply(subfolder_names, function(folder) {
        dir_path <- file.path(output_dir, folder)
        if (!dir.exists(dir_path)) {
            dir.create(dir_path, recursive = TRUE)
        }
        return(dir_path)
    })
    names(output_dirs) <- subfolder_names

    # Get list of parquet files
    parquet_files <- list.files(
        input_dir,
        pattern = "\\.parquet$", full.names = TRUE
    )

    if (length(parquet_files) == 0) {
        warning("No parquet files found in input directory")
        return(invisible(NULL))
    }

    # Process each file
    for (file_path in parquet_files) {
        # Extract burst value and individual name from filename
        file_name <- basename(file_path)
        burst_match <- regexpr("burst_(\\d+)", file_name)
        burst_value <- regmatches(file_name, burst_match)
        burst_value <- gsub("burst_", "", burst_value)

        individual_match <- regexpr("individual_([A-Za-z0-9_]+)", file_name)
        individual_name <- regmatches(file_name, individual_match)
        individual_name <- gsub("individual_", "", individual_name)

        # Print burst and individual information
        cat("============================\n")
        cat(sprintf(
            "Processing - Burst: %s, Individual: %s\n",
            burst_value, individual_name
        ))

        # Common preprocessing steps (done only once per file)
        # 1. Load data
        dt <- load_parquet(file_path, verbose = verbose)

        # 2. Convert raw counts to g units
        dt <- convert_to_g(dt)

        # 4. Compute static components
        dt <- compute_static_components(
            dt, static_window_size,
            verbose = verbose
        )

        # 5. Compute DBA
        dt <- compute_dba(dt, verbose = FALSE)

        # 6. Compute burst-level VeDBAs
        burst_vedbas <- compute_burst_vedbas(
            dt, static_window_size,
            verbose = FALSE
        )

        # 7. Identify walking bursts
        walking_bursts <- identify_walking_bursts(
            burst_vedbas, vedbas_threshold,
            verbose = verbose
        )

        # 8. Ensure date column exists
        dt[, date := as.Date(timestamp)]

        # 9. Compute rotation matrices for walking bursts
        rotation_matrices <- compute_rotation_matrices(
            dt, walking_bursts,
            verbose = FALSE
        )

        # 10. Compute rotation corrections
        # Daily correction
        daily_rm <- compute_daily_rotation_matrices(
            rotation_matrices, dt,
            verbose = verbose
        )
        daily_dt <- apply_matrix_correction(
            dt, daily_rm,
            correction_type = "daily", verbose = verbose
        )
        # Generate output and save
        daily_dt <- apply_delta_encoding(daily_dt, verbose = FALSE)
        daily_dt <- convert_to_arrow_table(daily_dt, schema = schema)
        daily_filename <- gsub(
            "\\.parquet$",
            paste0("_daily.parquet"),
            file_name
        )
        # print(daily_filename)
        arrow::write_parquet(
            daily_dt, file.path(output_dirs$rotation_daily, daily_filename),
            compression = compression,
            compression_level = compression_level
        )
        rm(daily_rm, daily_dt) # Free memory
        output_time_1 <- Sys.time()
        cat(sprintf(
            "Processed daily correction %s in %.2f minutes\n",
            file_name,
            as.numeric(
                difftime(output_time_1, batch_start, units = "mins")
            )
        ))

        # Basal correction
        basal_rm <- compute_basal_rotation_matrices(
            rotation_matrices, dt, basal_window_size,
            verbose = FALSE
        )
        basal_dt <- apply_matrix_correction(
            dt, basal_rm,
            correction_type = "basal", verbose = FALSE
        )
        # Generate output and save
        basal_dt <- apply_delta_encoding(basal_dt, verbose = FALSE)
        basal_dt <- convert_to_arrow_table(basal_dt, schema = schema)
        basal_filename <- gsub(
            "\\.parquet$",
            paste0("_basal.parquet"),
            file_name
        )
        arrow::write_parquet(
            basal_dt, file.path(output_dirs$rotation_basal, basal_filename),
            compression = compression,
            compression_level = compression_level
        )
        rm(basal_rm, basal_dt) # Free memory
        output_time_2 <- Sys.time()
        cat(sprintf(
            "Processed basal correction %s in %.2f minutes\n",
            file_name,
            as.numeric(
                difftime(output_time_2, output_time_1, units = "mins")
            )
        ))

        # Clean up memory
        rm(dt, rotation_matrices, burst_vedbas, walking_bursts)
        gc()

        # Output total time taken
        total_time <- difftime(
            Sys.time(), batch_start,
            units = "mins"
        )
        cat(sprintf(
            "Processing complete at %s\n",
            format(Sys.time(), "%d/%m/%Y %H:%M:%S")
        ))
        cat(sprintf(
            "Total processing time: %.2f mins\n",
            as.numeric(total_time)
        ))
        cat("============================\n")
    }

    # Report total batch time in hours
    batch_elapsed <- as.numeric(
        difftime(Sys.time(), batch_start, units = "hours")
    )
    cat(sprintf("All files processing time: %.2f hours\n", batch_elapsed))

    return(invisible(NULL))
}

# Trial each step sequentially ----
dt <- load_parquet(
    "./data/raw_parquet/Burst_4/ACC_data_burst_4_individual_Heelal.parquet",
    verbose = TRUE
)
head(dt)

# Convert to g
dt <- convert_to_g(dt)

# Compute static components
dt <- compute_static_components(dt, static_window_size = 6, verbose = TRUE)

# Calculate roll and pitch angles
dt <- compute_dba(dt, verbose = TRUE)

# Compute burst-level VeDBAs
burst_vedbas <- compute_burst_vedbas(dt, static_window_size = 6, verbose = TRUE)
head(burst_vedbas)

# Identify walking bursts
walking_bursts <- identify_walking_bursts(
    burst_vedbas,
    vedbas_threshold = c(0.17, 0.3), verbose = TRUE
)

# Ensure date column exists
dt[, date := as.Date(timestamp)]

# Compute rotation matrices
rotation_matrices <- compute_rotation_matrices(
    dt, walking_bursts,
    verbose = TRUE
)

# Compute daily rotation matrices
daily_rm <- compute_daily_rotation_matrices(
    rotation_matrices, dt,
    verbose = TRUE
)

# Apply daily rotation correction
daily_dt <- apply_matrix_correction(
    dt, daily_rm,
    correction_type = "daily",
    verbose = TRUE
)
head(dt)
head(daily_dt)

# Apply delta encoding
daily_dt_enc <- apply_delta_encoding(daily_dt, verbose = TRUE)

# Convert to arrow table
ar_daily_dt <- convert_to_arrow_table(daily_dt_enc, schema = acc_schema)
head(ar_daily_dt)

# Save to parquet file
write_parquet(
    ar_daily_dt,
    "./data/output/ACC_data_burst_4_individual_Heelal_arrow_daily.parquet",
    compression = "zstd", compression_level = 9
)

# Basal rotation matrices
basal_rm <- compute_basal_rotation_matrices(
    rotation_matrices, dt,
    basal_window_size = 20,
    verbose = TRUE
)

# Apply basal rotation correction
basal_dt <- apply_matrix_correction(
    dt, basal_rm,
    correction_type = "basal",
    verbose = TRUE
)
head(basal_dt)
head(daily_dt)
head(dt)

# Apply delta encoding
basal_dt_enc <- apply_delta_encoding(basal_dt, verbose = TRUE)

# Convert to arrow table
ar_basal_dt <- convert_to_arrow_table(basal_dt_enc, schema = acc_schema)

head(ar_basal_dt)

# Run over all individual files ----
##### THE RAW_PARQUET FOLDER MUST CONTAIN INDIVIDUAL PARQUET FILES #####
# This function should only be run over the entire dataset
process_acc_data_folder(
    input_dir = "./data/raw_parquet/Burst_1/",
    output_dir = "./data/raw/acc/Burst_1/",
    static_window_size = 6,
    basal_window_size = 20,
    vedbas_threshold = c(0.17, 0.3),
    compression = "zstd",
    compression_level = 9,
    verbose = TRUE
)
