# Supplementary figures (numbered in order of first citation in the main text)
# S1: annotated windows per individual and behaviour
# S2: proportion of pure windows
# S3: static acceleration of Resting windows by orientation correction
# S4: smoothed VeDBA by behaviour, with the walking range used for orientation correction
# S5: main cross-validation vs leave-one-individual-out macro-F1
# S6: night-time Sleeping of HydraMultiROCKET (Burst 2) by orientation correction: collar tilt and movement
# S7: night-time collar orientation by model, individuals with annotated sleep
# S8: class-weight ablation

library(tidyverse)
library(arrow)
library(cowplot)
library(ggtext)
library(ggrepel)

beh_order <- c(
    "Resting", "Eating",
    "Walking", "Grooming actor", "Grooming receiver",
    "Sleeping", "Self-scratching", "Running"
)
# S1 - Annotated windows per individual and behaviour ----
## Data ----
# S1-S4 use the cross-validation fold lookup, i.e. the windows every model was
# trained and tested on: Remove windows and the Resting outliers above 0.1 g
# VeDBA (03_combine_burst_attributions) are already excluded
ind_annotations <- read.csv(
    "./data/temp/indices_cv_fold_lookup.csv"
) %>%
    rename(attribution_merged = behaviour) %>%
    mutate(burst = str_to_title(burst)) %>%
    filter(burst == "Burst_1") %>%
    group_by(Ind_ID, attribution_merged) %>%
    summarise(
        n_samples = n(),
        .groups = "drop"
    ) %>%
    complete(
        Ind_ID,
        attribution_merged,
        fill = list(n_samples = 0)
    ) %>%
    mutate(
        attribution_merged = factor(
            attribution_merged,
            levels = beh_order
        )
    )
table(ind_annotations$attribution_merged)

## Plot ----
# make heatmap
ind_variation_plot <- ggplot(
    ind_annotations,
    aes(x = attribution_merged, y = Ind_ID, fill = n_samples)
) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(
        name = "Number of\nwindows",
        option = "D"
    ) +
    geom_label(
        aes(label = n_samples),
        colour = "white", fill = "black",
        size = 4
    ) +
    scale_x_discrete(
        labels = c(
            "Resting" = "Resting",
            "Eating" = "Eating",
            "Walking" = "Walking",
            "Grooming actor" = "Grooming\nactor",
            "Grooming receiver" = "Grooming\nreceiver",
            "Sleeping" = "Sleeping",
            "Self-scratching" = "Self-\nscratching",
            "Running" = "Running"
        )
    ) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(
            angle = 45, hjust = 1, size = 12, colour = "black"
        ),
        axis.text.y = element_text(size = 12, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
    )
ind_variation_plot
ggsave(
    "./plots/figs1_individual_validation_data_variation.png",
    ind_variation_plot,
    width = 10, height = 8, dpi = 300
)

# S2 - Proportion of pure windows ----
## Data ----
# Per-sample labels are mapped to the modelled classes as in
# 03_combine_burst_attributions (whose window majority gives the training
# label); behaviours that were not modelled become Remove, so a window with any
# of them counts as mixed
beh_merge <- c(
    "Foraging ground" = "Eating", "Foraging canopy" = "Eating",
    "Canopy movement" = "Walking", "Aggression" = "Running"
)
beh_remove <- c(
    "Out of sight", "Other", "Mating", "Vigilant", "Male inspection",
    "Social", "Aggressive display", "Jumping"
)

# Load burst annotations
burst_annotations <- bind_rows(
    read.csv("./data/raw/annotations/annotations_burst1.csv") %>%
        mutate(burst = "Burst_1"),
    read.csv("./data/raw/annotations/annotations_burst2.csv") %>%
        mutate(burst = "Burst_2"),
    read.csv("./data/raw/annotations/annotations_burst3.csv") %>%
        mutate(burst = "Burst_3"),
    read.csv("./data/raw/annotations/annotations_burst4.csv") %>%
        mutate(burst = "Burst_4")
) %>%
    mutate(Final_behaviour = case_when(
        Final_behaviour %in% beh_remove ~ "Remove",
        Final_behaviour %in% names(beh_merge) ~ beh_merge[Final_behaviour],
        TRUE ~ Final_behaviour
    )) %>%
    group_by(burst, Ind_ID, new_burst) %>%
    summarise(
        n_samples = n(),
        n_behaviours = n_distinct(Final_behaviour),
        list_behaviours = paste0(
            sort(unique(Final_behaviour)),
            collapse = "; "
        ),
        .groups = "drop"
    ) %>%
    mutate(
        annotation_type = case_when(
            n_behaviours == 1 ~ "Pure",
            n_behaviours > 1 ~ "Mixed"
        )
    )

# Load final annotations
final_annotations <- read.csv(
    "./data/temp/indices_cv_fold_lookup.csv"
) %>%
    rename(attribution_merged = behaviour) %>%
    mutate(burst = str_to_title(burst)) %>%
    left_join(
        burst_annotations %>%
            select(
                burst, Ind_ID, new_burst, annotation_type, list_behaviours
            ),
        by = c("burst", "Ind_ID", "new_burst")
    )

# Summarise proportions overall
pure_burst_prop <- final_annotations %>%
    group_by(burst, annotation_type) %>%
    summarise(
        n_bursts = n(),
        .groups = "drop"
    ) %>%
    group_by(burst) %>%
    mutate(
        prop = n_bursts / sum(n_bursts)
    ) %>%
    filter(annotation_type == "Pure")

# Summarise proportions per behaviour
pure_burst_prop_beh <- final_annotations %>%
    group_by(burst, attribution_merged, annotation_type) %>%
    summarise(
        n_bursts = n(),
        .groups = "drop"
    ) %>%
    group_by(burst, attribution_merged) %>%
    mutate(
        prop = n_bursts / sum(n_bursts)
    ) %>%
    filter(annotation_type == "Pure") %>%
    ungroup() %>%
    complete(
        burst,
        attribution_merged = beh_order,
        fill = list(n_bursts = 0, prop = 0)
    ) %>%
    mutate(
        attribution_merged = factor(
            attribution_merged,
            levels = beh_order
        )
    ) %>%
    arrange(burst, attribution_merged)

# Output CSV: proportion of pure windows per burst length, overall and per
# behaviour (values cited in the Methods)
write.csv(
  bind_rows(
    pure_burst_prop %>% mutate(attribution_merged = "All"),
    pure_burst_prop_beh
  ),
  "./data/output/statistical_analysis/annotation_pure_window_proportions.csv",
  row.names = FALSE
)

## Plot for overall pure bursts ----
pure_burst_prop_plot <- ggplot(
    pure_burst_prop,
    aes(x = burst, y = prop, group = 1, shape = burst)
) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 4) +
    scale_y_continuous(
        limits = c(0.55, 0.85), breaks = seq(0.6, 0.8, by = 0.1)
    ) +
    scale_x_discrete(
        labels = c("Burst 1", "Burst 2", "Burst 3", "Burst 4")
    ) +
    scale_shape_manual(
        name = "Burst",
        values = c(16, 17, 15, 18),
        labels = c("Burst 1", "Burst 2", "Burst 3", "Burst 4"),
    ) +
    theme_bw() +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        # The burst legend is shown once, under panel B
        legend.position = "none",
        panel.grid = element_blank(),
    ) +
    ylab("Proportion of pure windows")
pure_burst_prop_plot

## Plot per behaviour ----
pure_burst_beh_prop_plot <- ggplot(
    pure_burst_prop_beh,
    aes(
        x = attribution_merged, y = prop, group = burst, shape = burst
    )
) +
    geom_line(aes(linetype = burst)) +
    geom_point(size = 4) +
    scale_y_continuous(
        limits = c(0, 1), breaks = seq(0, 1, by = 0.5)
    ) +
    scale_x_discrete(
        labels = c(
            "Resting" = "Resting",
            "Eating" = "Eating",
            "Walking" = "Walking",
            "Grooming actor" = "Grooming\nactor",
            "Grooming receiver" = "Grooming\nreceiver",
            "Sleeping" = "Sleeping",
            "Self-scratching" = "Self-\nscratching",
            "Running" = "Running"
        )
    ) +
    scale_linetype_manual(
        name = "Burst",
        values = c("solid", "dashed", "dotted", "dotdash"),
        labels = c("Burst 1", "Burst 2", "Burst 3", "Burst 4"),
    ) +
    scale_shape_manual(
        name = "Burst",
        values = c(16, 17, 15, 18),
        labels = c("Burst 1", "Burst 2", "Burst 3", "Burst 4"),
    ) +
    theme_bw() +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black"),
        legend.position = "bottom",
        panel.grid = element_blank(),
    ) +
    ylab("Proportion of pure windows")
pure_burst_beh_prop_plot

# Combined plot
pure_burst_combined_plot <- plot_grid(
    pure_burst_prop_plot,
    pure_burst_beh_prop_plot,
    labels = c("A", "B"), nrow = 1,
    align = "h", axis = "tb",
    rel_widths = c(1, 2)
)
pure_burst_combined_plot
ggsave(
    "./plots/figs2_pure_burst_proportions.png",
    pure_burst_combined_plot,
    width = 16, height = 8, dpi = 300
)

# Table S1 and Methods: annotated windows at each filtering step ----
# All annotated windows with their majority label (Remove = behaviours not
# modelled or out of sight), and the windows kept for cross-validation; the
# difference is the Resting windows above 0.1 g VeDBA
annotated_window_counts <- read.csv(
  "./data/raw/annotations/annotations_merged_majority.csv"
) %>%
  mutate(burst = str_to_title(burst)) %>%
  group_by(burst) %>%
  summarise(
    n_annotated = n(),
    n_unmodelled = sum(attribution_merged == "Remove"),
    .groups = "drop"
  ) %>%
  left_join(
    read.csv("./data/temp/indices_cv_fold_lookup.csv") %>%
      mutate(burst = str_to_title(burst)) %>%
      count(burst, name = "n_modelled"),
    by = "burst"
  ) %>%
  mutate(
    n_resting_vedba_removed = n_annotated - n_unmodelled - n_modelled,
    prop_unmodelled = n_unmodelled / n_annotated,
    prop_resting_vedba_removed = n_resting_vedba_removed / n_annotated
  )
annotated_window_counts
write.csv(
  annotated_window_counts,
  "./data/output/statistical_analysis/annotation_window_counts.csv",
  row.names = FALSE
)

# S3 - Static acceleration of Resting windows by orientation correction ----
## Data ----
# Load static X, Y, Z values for Burst 1 uncorrected
b1_unc_features <- read_parquet(
    "./data/raw/features/Burst_1/annotated_features_burst_1_uncorrected.parquet",
    col_select = c(
        "Ind_ID", "new_burst",
        "X_static_mean", "Y_static_mean", "Z_static_mean"
    )
) %>%
    mutate(burst = "Burst_1", correction_type = "Uncorrected")

# Load for Burst 1 basal corrected
b1_basal_features <- read_parquet(
    "./data/raw/features/Burst_1/annotated_features_burst_1_rotbasal.parquet",
    col_select = c(
        "Ind_ID", "new_burst",
        "X_static_mean", "Y_static_mean", "Z_static_mean"
    )
) %>%
    mutate(burst = "Burst_1", correction_type = "Basal")

# Burst 1 daily corrected
b1_daily_features <- read_parquet(
    "./data/raw/features/Burst_1/annotated_features_burst_1_rotdaily.parquet",
    col_select = c(
        "Ind_ID", "new_burst",
        "X_static_mean", "Y_static_mean", "Z_static_mean"
    )
) %>%
    mutate(burst = "Burst_1", correction_type = "Daily")

# Combine and add annotation
static_acc <- bind_rows(
    b1_unc_features,
    b1_basal_features,
    b1_daily_features
) %>%
    inner_join(
        read.csv("./data/temp/indices_cv_fold_lookup.csv") %>%
            select(burst, Ind_ID, new_burst, attribution_merged = behaviour) %>%
            mutate(burst = str_to_title(burst)),
        by = c("burst", "Ind_ID", "new_burst")
    ) %>%
    filter(attribution_merged == "Resting") %>%
    pivot_longer(
        cols = c(X_static_mean, Y_static_mean, Z_static_mean),
        names_to = "axis",
        values_to = "static_acc_value"
    ) %>%
    mutate(
        axis = case_when(
            axis == "X_static_mean" ~ "X",
            axis == "Y_static_mean" ~ "Y",
            axis == "Z_static_mean" ~ "Z"
        ),
        axis = factor(
            axis,
            levels = c("X", "Y", "Z")
        ),
        correction_type = factor(
            correction_type,
            levels = c("Uncorrected", "Daily", "Basal")
        )
    )

# Filter individuals with at least 15 Resting windows
ind_shortlist <- static_acc %>%
    filter(correction_type == "Uncorrected", axis == "X") %>%
    group_by(Ind_ID) %>%
    summarise(n_samples = n(), .groups = "drop") %>%
    filter(n_samples >= 15) %>%
    pull(Ind_ID)

static_acc <- static_acc %>%
    filter(Ind_ID %in% ind_shortlist)

## Plot ----
static_acc_plot <- ggplot(
    static_acc,
    aes(
        x = correction_type,
        y = static_acc_value,
        fill = correction_type
    )
) +
    geom_hline(
        data = data.frame(axis = c("X", "Y"), intercept = 0),
        aes(yintercept = intercept),
        linetype = "dashed", linewidth = 1
    ) +
    geom_hline(
        data = data.frame(axis = "Z", intercept = -1),
        aes(yintercept = intercept),
        linetype = "dashed", linewidth = 1
    ) +
    geom_boxplot() +
    facet_grid(
        Ind_ID ~ axis,
        labeller = labeller(
            axis = c(
                X = "Mean Static X",
                Y = "Mean Static Y",
                Z = "Mean Static Z"
            )
        )
    ) + 
    scale_y_continuous(
        name = "Static acceleration (g)",
        limits = c(-1.1, 1.02),
        breaks = seq(-1, 1, by = 1)
    ) +
    scale_x_discrete(
        name = "Orientation correction"
    ) +
    scale_fill_brewer(
        palette = "Set1",
        name = "Orientation\ncorrection"
    ) +
    theme_bw() +
    theme(
        legend.position = "none",
        legend.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    )
static_acc_plot
ggsave(
    "./plots/figs3_orientation_correction_static_acc_resting.png",
    static_acc_plot,
    width = 12, height = 15, dpi = 300
)

# S4 - Smoothed VeDBA by behaviour ----
# Smoothed VeDBA (mean_vedbas) of the Burst 2 windows, the measure and burst length
# of the walking selection (0.17-0.30 g) used for orientation correction in
# 01b_orientation_correction. One burst length, so each
# annotated window is counted once. Resting stops near 0.1 g because Resting
# bursts above 0.1 g mean VeDBA were removed as outliers
## Data ----
vedba <- read_parquet(
    "./data/raw/features/Burst_2/annotated_features_burst_2_uncorrected.parquet",
    col_select = c("Ind_ID", "new_burst", "mean_vedbas")
) %>%
    mutate(burst = "burst_2") %>%
    inner_join(
        read.csv("./data/temp/indices_cv_fold_lookup.csv") %>%
            select(burst, Ind_ID, new_burst, attribution_merged = behaviour),
        by = c("burst", "Ind_ID", "new_burst")
    ) %>%
    mutate(attribution_merged = factor(attribution_merged, levels = beh_order))

## Plot vedba by behaviour ----
vedba_threshold_plot <- ggplot(
    vedba,
    aes(x = attribution_merged, y = mean_vedbas)
) +
    geom_rect(
        aes(
            xmin = -Inf, xmax = Inf,
            ymin = 0.17, ymax = 0.3
        ),
        fill = "grey", alpha = 0.1
    ) +
    geom_hline(
        yintercept = 0.17, linetype = "dashed",
        color = "red", linewidth = 1
    ) +
    geom_hline(
        yintercept = 0.3, linetype = "dashed",
        color = "red", linewidth = 1
    ) +
    scale_x_discrete(
        labels = c(
            "Resting" = "Resting",
            "Eating" = "Eating",
            "Walking" = "Walking",
            "Grooming actor" = "Grooming\nactor",
            "Grooming receiver" = "Grooming\nreceiver",
            "Sleeping" = "Sleeping",
            "Self-scratching" = "Self-\nscratching",
            "Running" = "Running"
        )
    ) +
    scale_y_continuous(
        name = "Mean smoothed VeDBA (g)",
        limits = c(0, 2.2), breaks = c(0, 0.17, 0.3, 0.5, 1, 1.5, 2),
        labels = c(
            "0", "<span style='color:red;'>0.17</span>",
            "<span style='color:red;'>0.30</span>",
            "0.50", "1.00", "1.50", "2.00"
        )
    ) +
    geom_boxplot(size = 1.2) +
    theme_bw() +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(
            angle = 45, hjust = 1, size = 12, colour = "black"
        ),
        axis.text.y = element_markdown(size = 14, colour = "black"),
        panel.grid = element_blank(),
    )
vedba_threshold_plot
ggsave(
    "./plots/figs4_vedba_by_behaviour.png",
    vedba_threshold_plot,
    width = 10, height = 8, dpi = 300
)

# S5 - Main CV vs LOIO macro-F1 ----
# RF, TabPFN and HydraMultiROCKET at Burst 1 and Burst 2 uncorrected, as in the
# class-weight ablation (S8)
loio_cols <- c(
    "RF" = "#FF5722", "TabPFN" = "#AB47BC", "HydraMultiROCKET" = "#26A69A"
)

## Data ----
# Macro-F1 change: Main CV vs LOIO, per model
# Both sources are scored on the same individuals: the main CV out-of-fold
# predictions are restricted to the individuals held out in LOIO, and F1
# is restricted to the 5 well-populated classes

# Running, Self-scratching, Sleeping are dropped: no individual has
# enough samples of them for LOIO test folds to reliably contain any,
# so LOIO metrics restricted to only these 5 are the ones that mean
# the same thing as their Main CV counterparts.
retained_classes <- c(
    "Eating", "Grooming actor", "Grooming receiver", "Resting", "Walking"
)

focal_datasets <- tribble(
    ~burst, ~correction_type, ~dataset_label,
    "Burst_1", "uncorrected", "B1",
    "Burst_2", "uncorrected", "B2"
)

# LOIO predictions: confusion-matrix counts per held-out individual,
# expanded to one row per test window
loio_preds <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_loio_confusion.csv"
    ) %>%
        filter(model == "RF") %>%
        mutate(model_label = "RF"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_loio_confusion.csv"
    ) %>%
        filter(model == "TabPFN") %>%
        mutate(model_label = "TabPFN"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_loio_confusion.csv"
    ) %>%
        filter(model == "HydraMultiRocket") %>%
        mutate(model_label = "HydraMultiROCKET")
) %>%
    uncount(count) %>%
    mutate(source = "LOIO") %>%
    select(
        model_label, burst, correction_type, Ind_ID = held_out_individual,
        actual_label, predicted_label, source
    )

# Main CV out-of-fold predictions, restricted to the individuals held out in
# LOIO for that dataset, so both sources are scored on the same windows
loio_ids <- loio_preds %>% distinct(burst, correction_type, Ind_ID)

main_preds <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_oof_predictions.csv"
    ) %>%
        filter(model == "RF") %>%
        mutate(model_label = "RF"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_oof_predictions.csv"
    ) %>%
        filter(model == "TabPFN") %>%
        mutate(model_label = "TabPFN"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_oof_predictions.csv"
    ) %>%
        filter(model == "HydraMultiRocketPlus") %>%
        mutate(model_label = "HydraMultiROCKET")
) %>%
    semi_join(loio_ids, by = c("burst", "correction_type", "Ind_ID")) %>%
    mutate(source = "Main CV") %>%
    select(
        model_label, burst, correction_type, Ind_ID,
        actual_label, predicted_label, source
    )

all_preds <- bind_rows(main_preds, loio_preds) %>%
    inner_join(focal_datasets, by = c("burst", "correction_type"))

# Macro-F1 restricted to retained_classes
# actual/predicted are per-window labels. A window of a dropped class
# predicted as a retained class doesn't count as a false positive either.
macro_f1_restricted <- function(actual, predicted, classes) {
    keep <- actual %in% classes
    actual <- actual[keep]
    predicted <- predicted[keep]

    f1s <- sapply(classes, function(cls) {
        tp <- sum(actual == cls & predicted == cls)
        fp <- sum(actual != cls & predicted == cls)
        fn <- sum(actual == cls & predicted != cls)
        precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
        recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
        if ((precision + recall) == 0) 0 else 2 * precision * recall / (precision + recall)
    })
    mean(f1s) # macro average = unweighted mean across classes
}

# Pool each (model, dataset, source) into one F1, scoring all windows
# together rather than averaging per-fold/per-individual metrics
f1_by_dataset <- all_preds %>%
    group_by(model_label, dataset_label, source) %>%
    summarise(
        f1_macro = macro_f1_restricted(
            actual_label, predicted_label, retained_classes
        ),
        n_pooled = n(),
        .groups = "drop"
    ) %>%
    mutate(
        source = factor(source, levels = c("Main CV", "LOIO")),
        model_label = factor(
            model_label, levels = c("RF", "TabPFN", "HydraMultiROCKET")
        ),
        dataset_label = factor(
            dataset_label, levels = c("B1", "B2")
        )
    )

# Output CSV: macro-F1 over the five scored behaviours, main CV vs LOIO,
# on the same individuals (values cited in the Supplementary Results)
write.csv(
  f1_by_dataset,
  "./data/output/comparison_behaviour/behaviour_loio_vs_main_cv_f1macro.csv",
  row.names = FALSE
)

## Plot ----
f1_dumbbell_plot <- ggplot(
    f1_by_dataset,
    aes(x = source, y = f1_macro, group = model_label, colour = model_label)
) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 5) +
    geom_label_repel(
        data = f1_by_dataset %>% filter(source == "LOIO"),
        aes(
            label = model_label, fill = model_label,
            segment.colour = model_label,
            colour = I("black")
        ),
        size = 4, direction = "both", nudge_x = 0.02,
        force_pull = 0.5, show.legend = FALSE,
        seed = 1
    ) +
    scale_colour_manual(
        name = "Model",
        values = loio_cols,
        aesthetics = c("colour", "segment.colour")
    ) +
    scale_fill_manual(
        values = loio_cols,
    ) +
    coord_cartesian(ylim = c(0.4, 0.75)) +
    scale_y_continuous(breaks = seq(0.4, 0.7, by = 0.1)) +
    # dataset_label stays B1/B2 in the CSV; panels are titled in full
    facet_wrap(
        ~dataset_label,
        labeller = labeller(dataset_label = c("B1" = "Burst 1", "B2" = "Burst 2"))
    ) +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    ) +
    labs(x = NULL, y = "Macro-F1")
f1_dumbbell_plot
ggsave(
    "./plots/figs5_main_cv_vs_loio_f1macro.png",
    plot = f1_dumbbell_plot,
    width = 10, height = 6, dpi = 300
)

# S6 - Night-time Sleeping: collar tilt and movement ----
# HydraMultiROCKET, HydraMultiROCKET-Basal and HydraMultiROCKET-Daily (Burst 2)
# on the focal-sampled nights
# (18:00-05:59), from 11b_night_prediction_comparisons
# A: angle between each individual's night-time collar tilt and the 5 most
#    similar annotated Sleeping, Resting and Grooming receiver windows,
#    against predicted night-time Sleeping
# B: proportion of night-time bursts predicted Sleeping, Resting or Grooming
#    receiver as movement (VeDBA) changes; grey: distribution of night-time
#    bursts
## Data ----
night_models <- c(
    Unc = "HydraMultiROCKET",
    Basal = "HydraMultiROCKET-Basal",
    Daily = "HydraMultiROCKET-Daily"
)
night_cols <- c(
    "HydraMultiROCKET" = "#26A69A",
    "HydraMultiROCKET-Basal" = "#1565C0",
    "HydraMultiROCKET-Daily" = "#00695C"
)
night_refs <- c(
    knn_sleep = "Sleeping",
    knn_groomrec = "Grooming receiver",
    knn_rest = "Resting"
)

night_ind <- read.csv("./data/temp/plotting/night_sleep_tilt_per_individual.csv") %>%
    filter(correction %in% names(night_models)) %>%
    mutate(Model = factor(night_models[correction], levels = night_models)) %>%
    pivot_longer(
        all_of(names(night_refs)),
        names_to = "reference", values_to = "angle"
    ) %>%
    mutate(reference = factor(night_refs[reference], levels = night_refs)) %>%
    arrange(reference, Ind_ID, Model)

night_vedba_bins <- read.csv("./data/temp/plotting/night_vedba_binned_predictions.csv") %>%
    filter(correction %in% names(night_models), vedba <= 0.1) %>%
    mutate(
        Model = factor(night_models[correction], levels = night_models),
        label = factor(label, levels = night_refs)
    )

# Distribution of night-time bursts over VeDBA (the same bursts in every
# orientation version), scaled to a peak of 0.9
night_dist <- night_vedba_bins %>%
    filter(correction == "Unc", label == "Sleeping") %>%
    transmute(vedba, dens = 0.9 * n / max(n)) %>%
    crossing(label = levels(night_vedba_bins$label)) %>%
    mutate(label = factor(label, levels = levels(night_vedba_bins$label)))

# Median VeDBA of the annotated windows (dashed lines)
night_vedba_summary <- read.csv(
    "./data/output/statistical_analysis/night_vedba_summary.csv"
)
night_ref_vedba <- night_vedba_summary %>%
    filter(source %in% night_refs) %>%
    transmute(source, median)

night_theme <- theme_bw() +
    theme(
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_blank(),
        legend.text = element_text(size = 14, colour = "black"),
        panel.grid = element_blank(),
        plot.margin = margin(5.5, 20, 5.5, 5.5)
    )

## Plot ----
### A: collar tilt vs predicted Sleeping ----
night_tilt_plot <- ggplot(night_ind, aes(x = angle, y = prop_sleep)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_path(aes(group = Ind_ID), colour = "grey80", linewidth = 0.4) +
    geom_point(aes(colour = Model, shape = annotated), size = 2.5) +
    facet_wrap(~reference, nrow = 1) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_colour_manual(values = night_cols, guide = "none") +
    scale_shape_manual(
        name = NULL,
        values = c("Not annotated" = 16, "Sleeping annotated" = 17),
        labels = c(
            "Not annotated" = "Other individuals",
            "Sleeping annotated" = "Individuals with annotated sleep"
        )
    ) +
    night_theme +
    theme(legend.position = "bottom") +
    labs(
        x = "Night-time collar tilt: angle to the 5 most similar annotated windows (°)",
        y = "Proportion of night-time\nwindows predicted Sleeping"
    )

### B: predicted label vs movement ----
night_vedba_plot <- ggplot(
    night_vedba_bins,
    aes(x = vedba, y = prop, colour = Model)
) +
    geom_area(
        data = night_dist, aes(x = vedba, y = dens),
        inherit.aes = FALSE, fill = "grey88", colour = NA
    ) +
    geom_vline(
        data = night_ref_vedba, aes(xintercept = median),
        linetype = "dashed", colour = "grey50"
    ) +
    geom_text(
        data = night_ref_vedba,
        aes(x = median, y = Inf, label = source),
        inherit.aes = FALSE, angle = 90, hjust = 1.03, vjust = -0.4,
        size = 3.8, colour = "grey30"
    ) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~label) +
    scale_x_log10() +
    scale_y_continuous(limits = c(0, 1.02), breaks = c(0, 0.5, 1)) +
    scale_colour_manual(name = NULL, values = night_cols) +
    night_theme +
    theme(legend.position = "bottom") +
    labs(
        x = "Mean VeDBA of night-time burst (g, log scale)",
        y = "Proportion of night-time\nwindows predicted"
    )

### Combined ----
night_combined_plot <- plot_grid(
    night_tilt_plot, night_vedba_plot,
    labels = c("A", "B"), ncol = 1,
    align = "v", axis = "lr"
)
night_combined_plot
ggsave(
    "./plots/figs6_night_time_sleep_tilt_movement.png",
    night_combined_plot,
    width = 16, height = 13, dpi = 300
)

# S7 - Night-time collar orientation by model ----
# Static X, Y, Z (uncorrected collar frame) for the individuals with annotated
# sleep, one row each:
#   Annotated (all): annotated Resting, Grooming receiver and Sleeping windows
#                    of all individuals pooled, i.e. what the models learn from
#   Annotated (own): the same individual's annotated windows (points; few)
#   Night (all):     all of the individual's night-time bursts, any label
#   Models:          the individual's night-time bursts labelled Resting,
#                    Grooming receiver or Sleeping by each model
# Night-time is 18:00-05:59, as in S6. HydraMultiROCKET-Daily's labels are shown on the
# uncorrected static values, so every column is in the same frame
## Data ----
s7_behs <- c("Resting", "Grooming receiver", "Sleeping")
s7_static <- c("X_static_mean", "Y_static_mean", "Z_static_mean")
s7_night_hours <- c(18:23, 0:5)
# Same model order as the other figures
s7_models <- c(
    "RF" = "final_rf_b2_unc",
    "TabPFN" = "final_tabpfn_b2_unc",
    "HydraMultiROCKET" = "final_hr_b2_unc",
    "HydraMultiROCKET-Daily" = "final_hr_b2_rotdaily"
)
s7_groups <- c("Annotated (all)", "Annotated (own)", "Night (all)", names(s7_models))
s7_fill <- c(
    "Resting" = "#D32F2F",
    "Grooming receiver" = "#512DA8",
    "Sleeping" = "#42A5F5",
    "All night-time bursts" = "#9E9E9E"
)

s7_lookup <- read.csv("./data/temp/indices_cv_fold_lookup.csv") %>%
    filter(burst == "burst_2", behaviour %in% s7_behs) %>%
    transmute(Ind_ID = as.character(Ind_ID), new_burst = as.integer(new_burst), behaviour)

s7_inds <- s7_lookup %>%
    filter(behaviour == "Sleeping") %>%
    distinct(Ind_ID) %>%
    pull(Ind_ID) %>%
    sort()

# Annotated windows (Burst 2, uncorrected)
s7_annot <- read_parquet(
    "./data/raw/features/Burst_2/annotated_features_burst_2_uncorrected.parquet",
    col_select = all_of(c("Ind_ID", "new_burst", s7_static))
) %>%
    mutate(Ind_ID = as.character(Ind_ID), new_burst = as.integer(new_burst)) %>%
    inner_join(s7_lookup, by = c("Ind_ID", "new_burst"))

s7_annot_all <- s7_annot %>%
    select(behaviour, all_of(s7_static)) %>%
    crossing(row_ind = s7_inds) %>%
    mutate(group = "Annotated (all)")
s7_annot_own <- s7_annot %>%
    filter(Ind_ID %in% s7_inds) %>%
    transmute(row_ind = Ind_ID, behaviour, across(all_of(s7_static)), group = "Annotated (own)")

# Night-time bursts of these individuals (focal-sampled days, uncorrected)
s7_night <- read_parquet(
    "./data/raw/focal_sampling/features/focal_sampled_features_burst_2_uncorrected.parquet",
    col_select = all_of(c("Ind_ID", "burst_id", "new_burst", "burst_start_time", s7_static))
) %>%
    mutate(
        Ind_ID = as.character(Ind_ID),
        burst_id = as.integer(burst_id),
        new_burst = as.integer(new_burst)
    ) %>%
    filter(Ind_ID %in% s7_inds, hour(burst_start_time) %in% s7_night_hours)

s7_night_all <- s7_night %>%
    transmute(
        row_ind = Ind_ID, behaviour = "All night-time windows",
        across(all_of(s7_static)), group = "Night (all)"
    )

# Night-time bursts labelled Resting, Grooming receiver or Sleeping by each model
s7_pred <- imap_dfr(s7_models, function(file, model) {
    read_parquet(
        paste0("./data/output/inference_results/", file, "_focal_sampled_random_predictions.parquet"),
        col_select = c("Ind_ID", "burst_id", "new_burst", "predicted_behaviour")
    ) %>%
        mutate(
            Ind_ID = as.character(Ind_ID),
            burst_id = as.integer(burst_id),
            new_burst = as.integer(new_burst),
            group = model
        )
}) %>%
    filter(predicted_behaviour %in% s7_behs) %>%
    inner_join(s7_night, by = c("Ind_ID", "burst_id", "new_burst")) %>%
    transmute(
        row_ind = Ind_ID, behaviour = predicted_behaviour,
        across(all_of(s7_static)), group
    )

s7_long <- bind_rows(s7_annot_all, s7_annot_own, s7_night_all, s7_pred) %>%
    pivot_longer(all_of(s7_static), names_to = "axis", values_to = "static_acc_value") %>%
    mutate(
        axis = factor(str_sub(axis, 1, 1), levels = c("X", "Y", "Z")),
        group = factor(group, levels = s7_groups),
        behaviour = factor(behaviour, levels = names(s7_fill))
    )

# Windows or bursts per box
s7_long %>%
    filter(axis == "X") %>%
    count(row_ind, group, behaviour) %>%
    pivot_wider(names_from = behaviour, values_from = n, values_fill = 0) %>%
    print(n = Inf)

## Plot ----
s7_plot <- ggplot(
    s7_long %>% filter(group != "Annotated (own)"),
    aes(x = group, y = static_acc_value, fill = behaviour)
) +
    geom_boxplot(
        position = position_dodge2(preserve = "single"),
        outlier.size = 0.4, outlier.alpha = 0.3
    ) +
    # After the boxplots, so the x scale is already discrete
    geom_vline(xintercept = 3.5, colour = "grey60") +
    geom_point(
        data = s7_long %>% filter(group == "Annotated (own)"),
        aes(fill = behaviour),
        shape = 21, colour = "black", stroke = 0.2, size = 1.6,
        position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75, seed = 42)
    ) +
    facet_grid(
        row_ind ~ axis,
        labeller = labeller(axis = c(
            X = "Mean Static X", Y = "Mean Static Y", Z = "Mean Static Z"
        ))
    ) +
    coord_cartesian(ylim = c(-1.1, 1.1)) +
    scale_x_discrete(
        drop = FALSE,
        labels = function(x) {
            case_when(
                x == "HydraMultiROCKET" ~ "HydraMulti-\nROCKET",
                x == "HydraMultiROCKET-Daily" ~ "HydraMulti-\nROCKET-Daily",
                TRUE ~ str_replace(x, " ", "\n")
            )
        }
    ) +
    scale_y_continuous(name = "Static acceleration (g)", breaks = seq(-1, 1, by = 1)) +
    scale_fill_manual(name = NULL, values = s7_fill) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 11, colour = "black"),
        axis.text.y = element_text(size = 13, colour = "black"),
        strip.text = element_text(size = 15, colour = "black"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    )
s7_plot
ggsave(
    "./plots/figs7_night_orientation_by_model.png",
    s7_plot,
    width = 22, height = 2.6 * length(s7_inds) + 2, dpi = 300
)

# S8 - Class-weight ablation ----
# Per-behaviour precision and recall at Burst 1 and Burst 2 uncorrected, with
# class weighting (the models used throughout) and without, for the same models
# and datasets as the LOIO validation (S5). Pooled out-of-fold predictions,
# as in Fig. 4B
## Data ----
ablation_models <- c("RF", "TabPFN", "HydraMultiROCKET")
ablation_bursts <- c("Burst_1", "Burst_2")

read_oof_ablation <- function(path, weighting) {
    read.csv(path) %>%
        filter(
            burst %in% ablation_bursts,
            correction_type == "uncorrected",
            model %in% c("RF", "TabPFN", "HydraMultiRocketPlus")
        ) %>%
        mutate(
            model = if_else(model == "HydraMultiRocketPlus", "HydraMultiROCKET", model),
            weighting = weighting
        ) %>%
        select(
            weighting, model, burst, Ind_ID, new_burst, burst_id,
            actual_label, pred_label = predicted_label
        )
}

ablation_files <- tribble(
    ~path, ~weighting,
    "behaviour_features_ml_oof_predictions.csv", "Weighted",
    "behaviour_features_dl_tabpfn_oof_predictions.csv", "Weighted",
    "behaviour_acceleration_dl_oof_predictions.csv", "Weighted",
    "behaviour_features_ml_nocw_oof_predictions.csv", "Unweighted",
    "behaviour_features_dl_tabpfn_nocw_oof_predictions.csv", "Unweighted",
    "behaviour_acceleration_dl_nocw_oof_predictions.csv", "Unweighted"
)
ablation_preds <- map2_dfr(
    paste0("./data/output/comparison_behaviour/", ablation_files$path),
    ablation_files$weighting,
    read_oof_ablation
) %>%
    mutate(parent = paste(Ind_ID, burst_id, sep = "_"))
# Every model, burst and weighting should have all windows (Burst 1: 1052, Burst 2: 2147)
count(ablation_preds, burst, model, weighting)

# TP/FP/FN per parent burst, for pooled metrics and parent-burst bootstrap CIs
# (resample Ind_ID + burst_id, 1000 draws, seed 42, as in Fig. 4B)
ablation_counts <- map_dfr(beh_order, function(b) {
    ablation_preds %>%
        group_by(weighting, model, burst, parent) %>%
        summarise(
            tp = sum(actual_label == b & pred_label == b),
            fp = sum(actual_label != b & pred_label == b),
            fn = sum(actual_label == b & pred_label != b),
            .groups = "drop"
        ) %>%
        mutate(behaviour = b)
})

ablation_parents <- unique(ablation_counts$parent)
set.seed(42)
ablation_weights <- replicate(
    1000,
    tabulate(
        sample.int(length(ablation_parents), replace = TRUE),
        nbins = length(ablation_parents)
    )
)

ratio <- function(num, den) if_else(den == 0, NA_real_, num / den)
ablation_pr <- ablation_counts %>%
    group_by(weighting, model, burst, behaviour) %>%
    group_modify(~ {
        w <- ablation_weights[match(.x$parent, ablation_parents), , drop = FALSE]
        tp_b <- colSums(w * .x$tp)
        fp_b <- colSums(w * .x$fp)
        fn_b <- colSums(w * .x$fn)
        tp <- sum(.x$tp)
        fp <- sum(.x$fp)
        fn <- sum(.x$fn)
        prec_b <- ratio(tp_b, tp_b + fp_b)
        rec_b <- ratio(tp_b, tp_b + fn_b)
        f1_b <- ratio(2 * tp_b, 2 * tp_b + fp_b + fn_b)
        tibble(
            n_windows = tp + fn,
            precision = ratio(tp, tp + fp),
            precision_lo = quantile(prec_b, 0.025, na.rm = TRUE),
            precision_hi = quantile(prec_b, 0.975, na.rm = TRUE),
            recall = ratio(tp, tp + fn),
            recall_lo = quantile(rec_b, 0.025, na.rm = TRUE),
            recall_hi = quantile(rec_b, 0.975, na.rm = TRUE),
            f1 = ratio(2 * tp, 2 * tp + fp + fn),
            f1_lo = quantile(f1_b, 0.025, na.rm = TRUE),
            f1_hi = quantile(f1_b, 0.975, na.rm = TRUE)
        )
    }) %>%
    ungroup() %>%
    mutate(
        weighting = factor(weighting, levels = c("Weighted", "Unweighted")),
        model = factor(model, levels = ablation_models),
        burst = factor(burst, levels = ablation_bursts),
        behaviour = factor(behaviour, levels = beh_order)
    ) %>%
    arrange(burst, model, behaviour, weighting)

# Summary table (with CIs) for the text
write.csv(
    ablation_pr,
    "./data/output/comparison_behaviour/behaviour_class_weight_ablation_summary.csv",
    row.names = FALSE
)

## Plot ----
# Behaviour colour palette (as defined in 12a)
ablation_beh_cols <- c(
    "Resting" = "#D32F2F",
    "Eating" = "#BA68C8",
    "Walking" = "#827717",
    "Grooming actor" = "#D81B60",
    "Grooming receiver" = "#512DA8",
    "Sleeping" = "#42A5F5",
    "Self-scratching" = "#64DD17",
    "Running" = "#FF6F00"
)

class_weight_plot <- ggplot(
    ablation_pr,
    aes(x = precision, y = recall, colour = behaviour)
) +
    geom_abline(
        slope = 1, intercept = 0, linetype = "dashed", colour = "grey60"
    ) +
    geom_line(aes(group = behaviour), linewidth = 1) +
    geom_point(
        aes(shape = weighting),
        size = 3.5, stroke = 1.2, fill = "white"
    ) +
    facet_grid(
        burst ~ model,
        labeller = labeller(burst = c(
            "Burst_1" = "Burst 1", "Burst_2" = "Burst 2"
        ))
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
    scale_colour_manual(name = NULL, values = ablation_beh_cols) +
    scale_shape_manual(
        name = NULL,
        values = c("Weighted" = 16, "Unweighted" = 21),
        labels = c(
            "Weighted" = "Class-weighted",
            "Unweighted" = "Unweighted"
        )
    ) +
    theme_bw() +
    theme(
        aspect.ratio = 1,
        legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    ) +
    labs(x = "Precision", y = "Recall")
class_weight_plot
ggsave(
    "./plots/figs8_class_weight_ablation.png",
    class_weight_plot,
    width = 13, height = 10, dpi = 300
)
