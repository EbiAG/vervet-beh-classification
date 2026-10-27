# We compare the night-time predictions of HydraMultiROCKET on Burst 2 with the
# three orientation versions (uncorrected, basal and daily correction).
# Night-time is 18:00-05:59 local time, the complement of the 06:00-18:00
# daytime window used for the focal-sampling comparison; there are no
# night-time annotations.
#
# Tilt: when the animal is still, the accelerometer mostly measures gravity, so
# the mean of X, Y and Z over a burst gives the direction of gravity in the
# collar's axes. Sleeping, resting and being groomed involve almost no movement,
# so tilt is one of the few signals that separates them. Orientation correction
# changes tilt but not the amount of movement (VeDBA).
#
# 1. Night-time Sleeping per individual and its spread across individuals
# 2. Tilt: angle from each night-time burst to the most similar annotated
#    Sleeping, Resting and Grooming receiver windows, per individual. Resting
#    and Grooming receiver are subsampled to the number of Sleeping windows,
#    so all three classes have the same chance of containing a close window
# 3. Movement: VeDBA of night-time bursts and annotated windows, and how each
#    model labels night-time bursts as stillness changes
# Tables are written to data/output/statistical_analysis and plotting data to
# data/temp/plotting (Fig. S7 in 12e_publication_plots_supplementary).

library(tidyverse)
library(arrow)
library(data.table)

# Settings ----
static_window <- 6
night_hours <- c(18:23, 0:5)
k <- 5 # number of most similar annotated windows used for the angle
n_rep <- 10 # subsamples averaged for classes with more windows than Sleeping
seed <- 42
frames <- c(Unc = "uncorrected", Basal = "rotbasal", Daily = "rotdaily")
pred_files <- c(
    Unc = "final_hr_b2_unc",
    Basal = "final_hr_b2_rotbasal",
    Daily = "final_hr_b2_rotdaily"
)
ref_classes <- c(
    sleep = "Sleeping",
    rest = "Resting",
    groomrec = "Grooming receiver"
)

# Load data ----
## Annotated B2 windows used in the cross-validation ----
lookup_b2 <- read.csv("./data/temp/indices_cv_fold_lookup.csv") %>%
    filter(burst == "burst_2") %>%
    mutate(new_burst = as.character(new_burst)) %>%
    select(Ind_ID, new_burst, behaviour)

# Individuals whose sleep was annotated
sleep_annotated_ids <- lookup_b2 %>%
    filter(behaviour == "Sleeping") %>%
    distinct(Ind_ID) %>%
    pull(Ind_ID)

## Night-time predictions of the three HR B2 models (10b) ----
pred_night <- imap_dfr(pred_files, function(name, correction) {
    read_parquet(
        paste0(
            "./data/output/inference_results/", name,
            "_focal_sampled_random_predictions.parquet"
        ),
        col_select = c("Ind_ID", "burst_start_time", "predicted_behaviour")
    ) %>%
        filter(
            predicted_behaviour != "nan",
            hour(burst_start_time) %in% night_hours
        ) %>%
        mutate(correction = correction)
}) %>%
    mutate(correction = factor(correction, levels = names(frames)))
count(pred_night, correction)

# 1. Night-time Sleeping per individual ----
night_ind <- pred_night %>%
    group_by(correction, Ind_ID) %>%
    summarise(
        n_night = n(),
        prop_sleep = mean(predicted_behaviour == "Sleeping"),
        prop_groom_rec = mean(predicted_behaviour == "Grooming receiver"),
        .groups = "drop"
    ) %>%
    mutate(annotated = if_else(
        Ind_ID %in% sleep_annotated_ids, "Sleeping annotated", "Not annotated"
    ))
cat(
    "Individuals with Sleeping annotations in the focal set:",
    sum(sleep_annotated_ids %in% night_ind$Ind_ID), "of",
    length(sleep_annotated_ids), "\n"
)

## Spread across individuals ----
# If individuals sleep similar amounts at night, a wide spread reflects model
# error rather than behaviour
sleep_spread <- night_ind %>%
    group_by(correction) %>%
    summarise(
        n_ind = n(),
        median_prop_sleep = median(prop_sleep),
        min_prop_sleep = min(prop_sleep),
        max_prop_sleep = max(prop_sleep),
        sd_prop_sleep = sd(prop_sleep),
        .groups = "drop"
    )
sleep_spread

# 2. Tilt ----
## Annotated Sleeping, Resting and Grooming receiver windows ----
# Static means per orientation version
ref_tilt <- imap_dfr(frames, function(suffix, frame) {
    read_parquet(
        paste0(
            "./data/raw/features/Burst_2/annotated_features_burst_2_",
            suffix, ".parquet"
        ),
        col_select = c(
            "Ind_ID", "new_burst", "X_static_mean", "Y_static_mean", "Z_static_mean"
        )
    ) %>%
        mutate(new_burst = as.character(new_burst)) %>%
        inner_join(
            filter(lookup_b2, behaviour %in% ref_classes),
            by = c("Ind_ID", "new_burst")
        ) %>%
        transmute(
            frame, behaviour,
            X = X_static_mean, Y = Y_static_mean, Z = Z_static_mean
        ) %>%
        as_tibble()
})
count(ref_tilt, frame, behaviour) # 53 Sleeping windows in each frame

## Night-time bursts: static means per orientation version ----
# Same running-mean static means as X/Y/Z_static_mean in the focal-sampled
# feature files (a centred running mean over static_window samples within each
# burst, edges filled with the nearest value, then averaged over the burst; they
# match to 4 decimals). Recalculated here because those features exist only for
# uncorrected B2. The raw focal-sampling files are available on request
night_static_means <- function(path) {
    dt <- open_dataset(path) %>%
        select(Ind_ID, burst_id, new_burst, burst_start_time, sample_number, X, Y, Z) %>%
        filter(hour(burst_start_time) %in% night_hours) %>%
        collect() %>%
        as.data.table()
    setorder(dt, Ind_ID, new_burst, sample_number)
    roll_static <- function(v) {
        nafill(nafill(frollmean(v, n = static_window, align = "center"), "nocb"), "locf")
    }
    dt[, `:=`(
        X_static = roll_static(X),
        Y_static = roll_static(Y),
        Z_static = roll_static(Z)
    ), by = .(Ind_ID, new_burst)]
    dt[, .(
        X = mean(X_static), Y = mean(Y_static), Z = mean(Z_static)
    ), by = .(Ind_ID, burst_id, new_burst, burst_start_time)] %>%
        as_tibble()
}

night_tilt <- imap_dfr(frames, function(suffix, frame) {
    night_static_means(paste0(
        "./data/raw/focal_sampling/acc/focal_sampled_acc_burst_2_", suffix, ".parquet"
    )) %>%
        mutate(frame = frame)
})
count(night_tilt, frame)

## Angle from each night-time burst to the k most similar windows of a class ----
# Tilts are scaled to length 1 and compared by the angle between them
unit_rows <- function(m) m / sqrt(rowSums(m^2))
angle_knn <- function(G, R, k, chunk = 20000) {
    out <- numeric(nrow(G))
    for (start in seq(1, nrow(G), by = chunk)) {
        idx <- start:min(start + chunk - 1, nrow(G))
        ang <- acos(pmin(pmax(G[idx, , drop = FALSE] %*% t(R), -1), 1)) * 180 / pi
        # Sum of the k smallest angles per row: take each row's minimum k
        # times, removing it after it is counted
        rows <- seq_len(nrow(ang))
        total <- 0
        for (i in seq_len(k)) {
            ij <- cbind(rows, max.col(-ang, ties.method = "first"))
            total <- total + ang[ij]
            ang[ij] <- Inf
        }
        out[idx] <- total / k
    }
    out
}

# Classes with more windows than Sleeping: subsampled to the same number,
# averaged over n_rep subsamples
angle_knn_matched <- function(G, R, k, n_target, n_rep, seed) {
    if (nrow(R) <= n_target) {
        return(angle_knn(G, R, k))
    }
    set.seed(seed)
    rowMeans(replicate(
        n_rep,
        angle_knn(G, R[sample.int(nrow(R), n_target), , drop = FALSE], k)
    ))
}

night_tilt <- map_dfr(names(frames), function(f) {
    nt <- filter(night_tilt, frame == f)
    G <- unit_rows(cbind(nt$X, nt$Y, nt$Z))
    rf <- filter(ref_tilt, frame == f)
    n_target <- sum(rf$behaviour == "Sleeping")
    for (key in names(ref_classes)) {
        rc <- filter(rf, behaviour == ref_classes[[key]])
        R <- unit_rows(cbind(rc$X, rc$Y, rc$Z))
        nt[[paste0("knn_", key)]] <- angle_knn_matched(G, R, k, n_target, n_rep, seed)
    }
    nt
})

## Per individual: median angles, joined to predicted Sleeping ----
knn_cols <- paste0("knn_", names(ref_classes))
night_sleep_ind <- night_tilt %>%
    group_by(frame, Ind_ID) %>%
    summarise(across(all_of(knn_cols), median), .groups = "drop") %>%
    transmute(
        correction = factor(frame, levels = names(frames)), Ind_ID,
        theta_sleep = knn_sleep,
        across(all_of(knn_cols))
    ) %>%
    right_join(night_ind, by = c("correction", "Ind_ID")) %>%
    select(
        correction, Ind_ID, annotated, n_night, prop_sleep, prop_groom_rec,
        theta_sleep, all_of(knn_cols)
    ) %>%
    arrange(correction, Ind_ID)

## Summaries ----
# Per orientation version: median angle to each class and median predicted
# Sleeping
tilt_summary <- night_sleep_ind %>%
    group_by(correction) %>%
    summarise(
        n_ind = n(),
        across(all_of(knn_cols), median),
        median_prop_sleep = median(prop_sleep),
        .groups = "drop"
    )
tilt_summary

# Individuals whose sleep was annotated vs the others
night_annotated <- night_sleep_ind %>%
    group_by(correction, annotated) %>%
    summarise(
        n_ind = n(),
        median_theta_sleep = median(theta_sleep),
        min_theta_sleep = min(theta_sleep),
        max_theta_sleep = max(theta_sleep),
        median_prop_sleep = median(prop_sleep),
        .groups = "drop"
    )
night_annotated

# 3. Movement ----
## VeDBA of annotated windows and night-time bursts ----
# VeDBA does not depend on collar orientation, so uncorrected features suffice
ref_vedba <- read_parquet(
    "./data/raw/features/Burst_2/annotated_features_burst_2_uncorrected.parquet",
    col_select = c("Ind_ID", "new_burst", "mean_vedba")
) %>%
    mutate(new_burst = as.character(new_burst)) %>%
    inner_join(
        filter(lookup_b2, behaviour %in% ref_classes),
        by = c("Ind_ID", "new_burst")
    ) %>%
    transmute(source = behaviour, mean_vedba)

night_vedba <- read_parquet(
    "./data/raw/focal_sampling/features/focal_sampled_features_burst_2_uncorrected.parquet",
    col_select = c("Ind_ID", "burst_start_time", "mean_vedba")
) %>%
    filter(hour(burst_start_time) %in% night_hours)

# Quantiles per source (night-time bursts and annotated windows, all
# individuals pooled)
vedba_summary <- bind_rows(
    ref_vedba,
    transmute(night_vedba, source = "Night-time bursts", mean_vedba)
) %>%
    group_by(source) %>%
    summarise(
        n = n(),
        q05 = quantile(mean_vedba, 0.05),
        q25 = quantile(mean_vedba, 0.25),
        median = median(mean_vedba),
        q75 = quantile(mean_vedba, 0.75),
        q95 = quantile(mean_vedba, 0.95),
        .groups = "drop"
    )
vedba_summary

## Predicted label by VeDBA ----
pred_vedba <- inner_join(pred_night, night_vedba, by = c("Ind_ID", "burst_start_time"))
count(pred_vedba, correction) # should match count(pred_night, correction)

# Proportion predicted Sleeping, Resting and Grooming receiver in bins of
# log10 VeDBA
vedba_bins <- pred_vedba %>%
    mutate(bin = cut(log10(mean_vedba), breaks = seq(-3, -0.5, by = 0.1))) %>%
    filter(!is.na(bin)) %>%
    group_by(correction, bin) %>%
    summarise(
        vedba = median(mean_vedba),
        n = n(),
        Sleeping = mean(predicted_behaviour == "Sleeping"),
        Resting = mean(predicted_behaviour == "Resting"),
        `Grooming receiver` = mean(predicted_behaviour == "Grooming receiver"),
        .groups = "drop"
    ) %>%
    filter(n >= 200) %>%
    pivot_longer(
        c(Sleeping, Resting, `Grooming receiver`),
        names_to = "label", values_to = "prop"
    )

## Bursts at least as still as the median annotated Sleeping window ----
# In these bursts gravity outweighs movement several hundred-fold, so correction
# changes essentially only their tilt
still_threshold <- vedba_summary$median[vedba_summary$source == "Sleeping"]
still_bursts <- pred_vedba %>%
    group_by(correction) %>%
    summarise(
        vedba_threshold = still_threshold,
        prop_of_night = mean(mean_vedba <= still_threshold),
        n = sum(mean_vedba <= still_threshold),
        sleeping = mean(
            predicted_behaviour[mean_vedba <= still_threshold] == "Sleeping"
        ),
        grooming_receiver = mean(
            predicted_behaviour[mean_vedba <= still_threshold] == "Grooming receiver"
        ),
        .groups = "drop"
    )
still_bursts

# Night-time label shares for all Experiment 4 models ----
night_files <- c(
  "RF" = "final_rf_b2_unc",
  "TabPFN" = "final_tabpfn_b2_unc",
  "HydraMultiROCKET" = "final_hr_b2_unc",
  "HydraMultiROCKET-Basal" = "final_hr_b2_rotbasal",
  "HydraMultiROCKET-Daily" = "final_hr_b2_rotdaily"
)
night_all <- imap_dfr(night_files, function(name, model) {
  read_parquet(
    paste0(
      "./data/output/inference_results/", name,
      "_focal_sampled_random_predictions.parquet"
    ),
    col_select = c("Ind_ID", "burst_start_time", "predicted_behaviour")
  ) %>%
    filter(
      predicted_behaviour != "nan",
      hour(burst_start_time) %in% night_hours
    ) %>%
    mutate(model = model)
})
# Share of night-time bursts per predicted behaviour: over all bursts
# (prop_all) and per individual (median, min, max across the 32 individuals)
night_prediction_shares <- night_all %>%
  count(model, Ind_ID, predicted_behaviour) %>%
  complete(model, Ind_ID, predicted_behaviour, fill = list(n = 0)) %>%
  group_by(model, Ind_ID) %>%
  mutate(prop = n / sum(n)) %>%
  group_by(model, predicted_behaviour) %>%
  summarise(
    n = sum(n),
    median_prop_ind = median(prop),
    min_prop_ind = min(prop),
    max_prop_ind = max(prop),
    .groups = "drop"
  ) %>%
  group_by(model) %>%
  mutate(prop_all = n / sum(n)) %>%
  ungroup()
night_prediction_shares %>% filter(predicted_behaviour %in% c("Sleeping", "Grooming receiver", "Resting"))

# Save ----
## Tables ----
write.csv(sleep_spread,
    "./data/output/statistical_analysis/night_sleep_spread.csv",
    row.names = FALSE
)
write.csv(tilt_summary,
    "./data/output/statistical_analysis/night_tilt_summary.csv",
    row.names = FALSE
)
write.csv(night_annotated,
    "./data/output/statistical_analysis/night_sleep_tilt_annotated_individuals.csv",
    row.names = FALSE
)
write.csv(vedba_summary,
    "./data/output/statistical_analysis/night_vedba_summary.csv",
    row.names = FALSE
)
write.csv(still_bursts,
    "./data/output/statistical_analysis/night_still_bursts_predictions.csv",
    row.names = FALSE
)
write.csv(
  night_prediction_shares,
  "./data/output/statistical_analysis/night_prediction_shares.csv",
  row.names = FALSE
)

## Plotting data (Fig. S7 in 12e) ----
# Panel A: angle to each class vs predicted Sleeping, per individual
write.csv(night_sleep_ind,
    "./data/temp/plotting/night_sleep_tilt_per_individual.csv",
    row.names = FALSE
)
# Panel B: predicted label vs VeDBA
write.csv(vedba_bins,
    "./data/temp/plotting/night_vedba_binned_predictions.csv",
    row.names = FALSE
)
