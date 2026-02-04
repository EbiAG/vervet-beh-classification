library(data.table)
library(zoo)      # For rollmean function
library(tidyverse)
library(arrow)
library(chron)

# This script is a template for calculating 60 accelerometer features used in this project.
# It is meant to be run on the full 1.5 year dataset after raw data corrections have been applied.
# It can also be run on shortlisted annotated bursts.

# Functions for feature calculations ----
## Function to load corrected parquet files ----
load_corrected_parquet <- function(
    file_path, select_cols = NULL, verbose = TRUE) {
    # Check if file exists
    if (!file.exists(file_path)) {
        stop(paste("File not found:", file_path))
    }

    # Start timing
    start_time <- Sys.time()

    if (verbose) {
        cat(
            "Loading parquet file:", basename(file_path), "\n"
        )
    }

    # Read the parquet file
    if (is.null(select_cols)) {
        pq_table <- arrow::read_parquet(file_path, as_data_frame = FALSE)
    } else {
        pq_table <- arrow::read_parquet(file_path,
            col_select = select_cols,
            as_data_frame = FALSE
        )
    }

    # Extract metadata
    metadata <- pq_table$metadata

    # Convert to data frame first, then properly initialize as data.table
    dt <- as.data.frame(pq_table)
    data.table::setDT(dt) # This is the key change

    # Add Ind_ID from metadata if available
    if (!"Ind_ID" %in% names(dt) && !is.null(metadata$individual)) {
        # Add Ind_ID column ONLY if it doesn't already exist
        dt[, Ind_ID := metadata$individual]
    }

    # Ensure Ind_ID is the first column if it exists
    if ("Ind_ID" %in% names(dt)) {
        # FIX: Corrected 'df' to 'dt' in setdiff
        data.table::setcolorder(
            dt, c("Ind_ID", setdiff(names(dt), "Ind_ID"))
        )
    }

    # Set all metadata attributes correctly using setattr
    if (length(metadata) > 0) {
        for (key in names(metadata)) {
            data.table::setattr(dt, key, metadata[[key]])
        }
    }

    # Report information
    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "secs")
        cat(sprintf(
            "Loaded %d rows x %d columns in %.2f seconds\n",
            nrow(dt), ncol(dt), as.numeric(elapsed)
        ))
        cat(sprintf(
            "Memory usage: %.2f MB\n",
            object.size(dt) / 1024^2
        ))
        if ("Ind_ID" %in% names(dt) && length(unique(dt$Ind_ID)) > 1) {
            cat(sprintf(
                "Individuals found in data: %d\n", length(unique(dt$Ind_ID))
            ))
        } else {
            cat(sprintf(
                "Individual ID: %s\n",
                ifelse(
                    !is.null(metadata$individual), metadata$individual,
                    "Not available"
                )
            ))
        }
    }

    return(dt)
}

## Compute static components grouped by burst ----
compute_static_components <- function(
    dt, static_window_size = 6, verbose = TRUE) {
    # Start timing
    start_time <- Sys.time()

    # Check input
    required_cols <- c("X", "Y", "Z", "new_burst")
    missing_cols <- setdiff(required_cols, names(dt))
    if (length(missing_cols) > 0) {
        stop(paste(
            "Missing required columns:",
            paste(missing_cols, collapse = ", ")
        ))
    }

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
    ), by = .(Ind_ID, new_burst)]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(sprintf(
            "Static components calculated in %.2f minutes\n",
            as.numeric(elapsed)
        ))
    }

    return(dt)
}

## Calculate basic statistical features for each burst ----
calculate_basic_features <- function(dt, verbose = FALSE) {
    # Start timing
    start_time <- Sys.time()

    # Check for required columns
    required_cols <- c("X", "Y", "Z", "new_burst")
    missing_cols <- setdiff(required_cols, names(dt))
    if (length(missing_cols) > 0) {
        stop(paste(
            "Missing required columns:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    # Calculate features by burst
    features <- dt[, .(
        # Central tendency
        X_mean = mean(X, na.rm = TRUE),
        Y_mean = mean(Y, na.rm = TRUE),
        Z_mean = mean(Z, na.rm = TRUE),

        # Dispersion
        X_sd = sd(X, na.rm = TRUE),
        Y_sd = sd(Y, na.rm = TRUE),
        Z_sd = sd(Z, na.rm = TRUE),

        # Min/Max
        X_min = min(X, na.rm = TRUE),
        Y_min = min(Y, na.rm = TRUE),
        Z_min = min(Z, na.rm = TRUE),
        X_max = max(X, na.rm = TRUE),
        Y_max = max(Y, na.rm = TRUE),
        Z_max = max(Z, na.rm = TRUE),

        # Range
        X_range = max(X, na.rm = TRUE) - min(X, na.rm = TRUE),
        Y_range = max(Y, na.rm = TRUE) - min(Y, na.rm = TRUE),
        Z_range = max(Z, na.rm = TRUE) - min(Z, na.rm = TRUE),

        # Variance
        X_var = var(X, na.rm = TRUE),
        Y_var = var(Y, na.rm = TRUE),
        Z_var = var(Z, na.rm = TRUE),

        # Inverse coefficient of variation (mean/sd)
        X_inv_coef_var = mean(X, na.rm = TRUE) /
            (sd(X, na.rm = TRUE) + .Machine$double.eps),
        Y_inv_coef_var = mean(Y, na.rm = TRUE) /
            (sd(Y, na.rm = TRUE) + .Machine$double.eps),
        Z_inv_coef_var = mean(Z, na.rm = TRUE) /
            (sd(Z, na.rm = TRUE) + .Machine$double.eps),

        # Distribution shape
        X_skewness = e1071::skewness(X, na.rm = TRUE),
        Y_skewness = e1071::skewness(Y, na.rm = TRUE),
        Z_skewness = e1071::skewness(Z, na.rm = TRUE),
        X_kurtosis = e1071::kurtosis(X, na.rm = TRUE),
        Y_kurtosis = e1071::kurtosis(Y, na.rm = TRUE),
        Z_kurtosis = e1071::kurtosis(Z, na.rm = TRUE),

        # Overall magnitude (q metric)
        q = mean(X^2 + Y^2 + Z^2, na.rm = TRUE)
    ), by = .(Ind_ID, new_burst)]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(sprintf(
            "Basic features calculated in %.2f minutes\n",
            as.numeric(elapsed)
        ))
    }

    return(features)
}

## Compute spectral features ----
calculate_spectral_features <- function(dt, freq = 10.0, verbose = FALSE) {
    start_time <- Sys.time()

    # Check input
    required_cols <- c("X", "Y", "Z", "new_burst")
    missing_cols <- setdiff(required_cols, names(dt))
    if (length(missing_cols) > 0) {
        stop(paste(
            "Missing required columns:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    # Calculate spectral features by burst
    spectral_features <- dt[,
        {
            # Helper function for efficient peak detection
            get_peak_info <- function(axis_data) {
                # Remove NAs
                axis_data <- na.omit(axis_data)
                n <- length(axis_data)

                # Perform FFT directly
                # - Remove mean to reduce DC component
                # - Calculate absolute magnitude of complex FFT result
                # - Normalize by dividing by n
                fft_result <- abs(fft(axis_data - mean(axis_data))) / n

                # Only use first half of symmetric FFT result
                half_n <- floor(n / 2)
                amplitudes <- fft_result[2:(half_n + 1)] # Skip DC component
                frequencies <- seq(freq / n, freq / 2, length.out = half_n)

                # Find peaks using efficient sorting approach
                order_indices <- order(amplitudes, decreasing = TRUE)[1:2]

                # Return top two peaks (amplitudes and frequencies)
                c(
                    amplitudes[order_indices[1]], amplitudes[order_indices[2]],
                    frequencies[order_indices[1]], frequencies[order_indices[2]]
                )
            }

            # Apply to each axis
            x_peaks <- get_peak_info(X)
            y_peaks <- get_peak_info(Y)
            z_peaks <- get_peak_info(Z)

            # Return results
            list(
                # X axis peaks
                X_dps_mean = x_peaks[1], # First peak amplitude
                X_dps2_mean = x_peaks[2], # Second peak amplitude
                X_fdps_mean = x_peaks[3], # First peak frequency
                X_fdps2_mean = x_peaks[4], # Second peak frequency

                # Y axis peaks
                Y_dps_mean = y_peaks[1],
                Y_dps2_mean = y_peaks[2],
                Y_fdps_mean = y_peaks[3],
                Y_fdps2_mean = y_peaks[4],

                # Z axis peaks
                Z_dps_mean = z_peaks[1],
                Z_dps2_mean = z_peaks[2],
                Z_fdps_mean = z_peaks[3],
                Z_fdps2_mean = z_peaks[4]
            )
        },
        by = .(Ind_ID, new_burst)
    ]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(sprintf(
            "Spectral features calculated in %.2f minutes\n",
            as.numeric(elapsed)
        ))
    }

    return(spectral_features)
}

## Compute dynamic features ----
calculate_dynamic_features <- function(
    dt, static_window_size = 6, verbose = FALSE) {
    # Start timing
    start_time <- Sys.time()

    # Check for required columns
    required_cols <- c(
        "X", "Y", "Z", "X_static", "Y_static", "Z_static", "new_burst"
    )
    missing_cols <- setdiff(required_cols, names(dt))
    if (length(missing_cols) > 0) {
        stop(paste(
            "Missing required columns:",
            paste(missing_cols, collapse = ", ")
        ))
    }

    # Calculate dynamic features by burst
    dynamic_features <- dt[,
        {
            # 1. Calculate dynamic body acceleration
            DBA_X <- X - X_static
            DBA_Y <- Y - Y_static
            DBA_Z <- Z - Z_static

            # 2. Calculate VeDBA (vectorial dynamic body acceleration)
            vedba <- sqrt(DBA_X^2 + DBA_Y^2 + DBA_Z^2)

            # 3. Calculate ODBA (overall dynamic body acceleration)
            odba <- abs(DBA_X) + abs(DBA_Y) + abs(DBA_Z)

            # 4. Smooth VeDBA using rolling mean
            vedbas <- nafill(nafill(frollmean(
                vedba,
                n = static_window_size,
                align = "center"
            ), "nocb"), "locf")

            # 5. Calculate orientation angles
            pitch_angle <- -atan2(
                X_static,
                sqrt(Y_static^2 + Z_static^2)
            )
            roll_angle <- atan2(Y_static, Z_static)

            # 6. PCA on DBA
            dba_matrix <- cbind(DBA_X, DBA_Y, DBA_Z)
            dba_matrix_clean <- na.omit(dba_matrix)

            pc_list <- list(
                PC1_var = NA_real_,
                PC2_var = NA_real_,
                PC3_var = NA_real_,
                PC1_mean = NA_real_,
                PC2_mean = NA_real_,
                PC3_mean = NA_real_,
                PC1_PC2_ratio = NA_real_,
                PC1_PC3_ratio = NA_real_,
                PC2_PC3_ratio = NA_real_
            )
            # Only compute PCA if we have enough data
            if (nrow(dba_matrix_clean) >= 3) {
                tryCatch(
                    {
                        pca_result <- prcomp(dba_matrix_clean, center = TRUE, scale. = FALSE)
                        pc_scores <- pca_result$x

                        pc_list <- list(
                            PC1_var = pca_result$sdev[1]^2,
                            PC2_var = pca_result$sdev[2]^2,
                            PC3_var = pca_result$sdev[3]^2,
                            PC1_mean = mean(pc_scores[, 1], na.rm = TRUE),
                            PC2_mean = mean(pc_scores[, 2], na.rm = TRUE),
                            PC3_mean = mean(pc_scores[, 3], na.rm = TRUE),
                            PC1_PC2_ratio = pca_result$sdev[1]^2 / (pca_result$sdev[2]^2 + 1e-10),
                            PC1_PC3_ratio = pca_result$sdev[1]^2 / (pca_result$sdev[3]^2 + 1e-10),
                            PC2_PC3_ratio = pca_result$sdev[2]^2 / (pca_result$sdev[3]^2 + 1e-10)
                        )
                    },
                    error = function(e) {
                        pc_list
                    }
                )
            }

            # Return all computed features
            c(
                list(
                    # Mean values
                    mean_vedba = mean(vedba, na.rm = TRUE),
                    mean_vedbas = mean(vedbas, na.rm = TRUE),
                    mean_ODBA = mean(odba, na.rm = TRUE),

                    # Partial DBA means
                    X_PDBA_mean = mean(abs(DBA_X), na.rm = TRUE),
                    Y_PDBA_mean = mean(abs(DBA_Y), na.rm = TRUE),
                    Z_PDBA_mean = mean(abs(DBA_Z), na.rm = TRUE),

                    # Static component means
                    X_static_mean = mean(X_static, na.rm = TRUE),
                    Y_static_mean = mean(Y_static, na.rm = TRUE),
                    Z_static_mean = mean(Z_static, na.rm = TRUE),

                    # Orientation angles
                    pitch_angle = mean(pitch_angle, na.rm = TRUE),
                    roll_angle = mean(roll_angle, na.rm = TRUE)
                ),
                pc_list
            )
        },
        by = .(Ind_ID, new_burst)
    ]

    if (verbose) {
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat(sprintf(
            "Dynamic features calculated in %.2f minutes\n",
            as.numeric(elapsed)
        ))
    }

    return(dynamic_features)
}

## Function to convert to arrow table with schema ----
create_arrow_table_from_dt <- function(dt) {
    # Convert Ind_ID to character
    dt[, Ind_ID := as.character(Ind_ID)]

    # Round out numeric columns to 4 decimal points
    numeric_cols <- setdiff(
        names(dt), c("Ind_ID", "new_burst")
    )
    for (col in numeric_cols) {
        if (is.numeric(dt[[col]])) {
            set(
                dt,
                j = col, value = round(dt[[col]], 4)
            )
        }
    }

    # Define schema fields
    schema_fields <- list()
    schema_fields[["Ind_ID"]] <- arrow::string()
    schema_fields[["burst_id"]] <- arrow::int32()
    schema_fields[["new_burst"]] <- arrow::int32()
    schema_fields[["burst_start_time"]] <- arrow::timestamp(
        unit = "ms", timezone = "Africa/Johannesburg"
    )

    # Schema for numeric columns
    for (col in numeric_cols) {
        if (is.numeric(dt[[col]])) {
            schema_fields[[col]] <- arrow::float32()
        }
    }

    # Create schema
    feature_schema <- arrow::schema(schema_fields)

    # Order columns
    desired_order <- names(schema_fields)
    desired_order <- desired_order[desired_order %in% names(dt)]
    remaining_cols <- setdiff(names(dt), desired_order)
    setcolorder(dt, c(desired_order, remaining_cols))

    # Convert to arrow table
    feature_table <- arrow::Table$create(dt, schema = feature_schema)

    return(feature_table)
}

## Wrapper function to process accelerometer features ----
process_acc_features <- function(
    input_path, output_dir = NULL,
    static_window_size = 6, sample_freq = 10.0,
    compression = "zstd", compression_level = 9,
    return_data = FALSE, verbose = TRUE) {
    # Overall timing
    total_start_time <- Sys.time()

    # 1. Load parquet file
    if (verbose) cat("===========================================\n")
    dt <- load_corrected_parquet(input_path, verbose = verbose)

    # 2. Compute static components (modifies dt in place)
    dt <- compute_static_components(dt,
        static_window_size = static_window_size,
        verbose = verbose
    )

    # 3. Calculate basic features
    basic_features <- calculate_basic_features(dt, verbose = verbose)

    # 4. Calculate spectral features
    spectral_features <- calculate_spectral_features(dt,
        freq = sample_freq,
        verbose = verbose
    )

    # 5. Calculate dynamic features
    dynamic_features <- calculate_dynamic_features(dt,
        static_window_size = static_window_size,
        verbose = verbose
    )

    # 6. Merge all features by new_burst ID
    all_features <- Reduce(
        function(x, y) merge(x, y, by = c("Ind_ID", "new_burst"), all = TRUE),
        list(basic_features, spectral_features, dynamic_features)
    )

    # Add burst start time and burst_id information
    burst_info <- unique(dt[, .(Ind_ID, new_burst, burst_start_time, burst_id)])
    all_features <- merge(
        all_features, burst_info,
        by = c("Ind_ID", "new_burst"),
        all.x = TRUE
    )

    if (verbose) {
        cat(sprintf(
            "Features calculated for %d bursts\n",
            nrow(all_features)
        ))
    }

    # 7. Set column order (Ind_ID and new_burst first)
    if ("Ind_ID" %in% names(all_features)) {
        setcolorder(all_features, c("Ind_ID", "new_burst"))
    } else {
        setcolorder(all_features, "new_burst")
    }

    # 8. Convert to arrow table
    feature_table <- create_arrow_table_from_dt(all_features)

    # 9. Save to output file if specified
    if (!is.null(output_dir)) {
        # Create directory if needed
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

        input_filename <- basename(input_path)
        output_filename <- gsub(
            "_data_", "_features_", input_filename
        )

        output_path <- file.path(output_dir, output_filename)
        # Write to parquet
        arrow::write_parquet(
            feature_table,
            output_path,
            compression = compression,
            compression_level = compression_level
        )
    }

    # Report total time
    if (verbose) {
        total_elapsed <- difftime(Sys.time(), total_start_time, units = "mins")
        cat(sprintf(
            "Feature calculation complete at %s\n",
            format(Sys.time(), "%d/%m/%Y %H:%M:%S")
        ))
        cat(sprintf(
            "Total processing time: %.2f minutes\n",
            as.numeric(total_elapsed)
        ))
        cat("===========================================\n")
    }

    # Return the feature data.table
    if (return_data) {
        return(all_features)
    } else {
        rm(all_features, feature_table, dt)
        gc()
        return(invisible(NULL))
    }
}

## Wrapper function to run process_acc_features over a folder ----
process_acc_features_folder <- function(
    input_dir, output_dir = NULL,
    static_window_size = 6, sample_freq = 10.0,
    compression = "zstd", compression_level = 9,
    return_data = FALSE, verbose = TRUE) {
    # List all files in the input directory
    input_files <- list.files(input_dir, full.names = TRUE)

    # Process each file
    for (input_file in input_files) {
        process_acc_features(
            input_file,
            output_dir = output_dir,
            static_window_size = static_window_size,
            sample_freq = sample_freq,
            compression = compression,
            compression_level = compression_level,
            return_data = return_data, verbose = verbose
        )
    }
}

# Trial each step sequentially ----
acc_pq <- load_corrected_parquet(
    "./data/output/corrected/roll_daily_2/ACC_data_burst_4_individual_Heelal_RD.parquet"
)

# Static components
acc_pq <- compute_static_components(acc_pq, static_window_size = 6)

# Basic features
basic_features <- calculate_basic_features(acc_pq, verbose = TRUE)

# Spectral features
spectral_features <- calculate_spectral_features(
    acc_pq,
    freq = 10.0, verbose = TRUE
)

# Dynamic features
dynamic_features <- calculate_dynamic_features(
    acc_pq,
    static_window_size = 6, verbose = TRUE
)

# Merge dataframe
all_features <- Reduce(
    function(x, y) merge(x, y, by = c("Ind_ID", "new_burst"), all = TRUE),
    list(basic_features, spectral_features, dynamic_features)
)
head(all_features)
colnames(all_features)
str(all_features)

feature_table <- create_arrow_table_from_dt(all_features)

# Run over entire folders ----
##### THIS WILL OVERWRITE FILES IN THE OUTPUT DIRECTORY #####
process_acc_features_folder(
    input_dir = "./data/raw/acc/Burst_1/",
    output_dir = "./data/raw/features/Burst_1/",
    static_window_size = 6, sample_freq = 10.0,
    compression = "zstd", compression_level = 9,
    return_data = FALSE, verbose = TRUE
)