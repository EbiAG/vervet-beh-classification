# We will make plots for publication figure 3 which will contain results for experiment 3

library(tidyverse)
library(cowplot)
library(ggrepel)

model_cols <- c(
    "HydraMultiROCKET" = "#26A69A",
    "TSSeq" = "#81D4FA",
    "LSTM" = "#1565C0",
    "TabPFN" = "#AB47BC",
    "GANDALF" = "#E1BEE7",
    "CatEmbed" = "#F48FB1",
    "SVC" = "#FFA000",
    "XGB" = "#FFCA28",
    "RF" = "#FF5722"
)

# Load data ----
## Global metric data ----
# Metric values
model_metrics <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_metrics.csv"
    )
) %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2", "Burst_3", "Burst_4")
        ),
        correction_type = case_when(
            correction_type == "uncorrected" ~ "Unc",
            correction_type == "rotdaily" ~ "Daily",
            correction_type == "rotbasal" ~ "Basal",
        ),
        correction_type = factor(
            correction_type,
            levels = c("Unc", "Daily", "Basal")
        ),
        model = case_when(
            model == "CategoryEmbeddingModel" ~ "CatEmbed",
            model == "TSSequencerPlus" ~ "TSSeq",
            model == "HydraMultiRocketPlus" ~ "HydraMultiROCKET",
            TRUE ~ model
        ),
        model = factor(
            model,
            levels = c(
                "RF", "SVC", "XGB",
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraMultiROCKET"
            )
        )
    ) %>%
    select(
        model, burst, correction_type, fold, accuracy, roc_auc,
        balanced_acc, f1_macro, precision_macro, recall_macro
    )

# Summarise
model_metrics_summary <- model_metrics %>%
    filter(burst == "Burst_1" & correction_type == "Unc") %>%
    mutate(
        model_data = case_when(
            model %in% c("RF", "SVC", "XGB") ~ "ml_features",
            model %in% c("LSTM", "TSSeq", "HydraMultiROCKET") ~ "dl_acceleration",
            TRUE ~ "dl_features"
        ),
        model_data = factor(
            model_data,
            levels = c("ml_features", "dl_features", "dl_acceleration")
        )
    ) %>%
    group_by(model, model_data) %>%
    summarise(
        Accuracy_mean = mean(accuracy),
        Accuracy_sd = sd(accuracy),
        ROC_AUC_mean = mean(roc_auc),
        ROC_AUC_sd = sd(roc_auc),
        Balanced_Acc_mean = mean(balanced_acc),
        Balanced_Acc_sd = sd(balanced_acc),
        F1_Macro_mean = mean(f1_macro),
        F1_Macro_sd = sd(f1_macro),
        Precision_Macro_mean = mean(precision_macro),
        Precision_Macro_sd = sd(precision_macro),
        Recall_Macro_mean = mean(recall_macro),
        Recall_Macro_sd = sd(recall_macro),
        .groups = "drop"
    )

## Behaviour order ----
# Order of behaviours
beh_order <- c(
    "Resting", "Eating",
    "Walking", "Grooming actor", "Grooming receiver",
    "Sleeping", "Self-scratching", "Running"
)

# Shared F1-macro x ROC AUC limits for 3A and 3B, so the panels can be compared
# directly: the union of mean +/- SD across both panels (F1-macro 0.34-0.67,
# ROC AUC 0.83-0.95), rounded outward. LSTM and
# CatEmbed are not plotted (their SDs would stretch the axes). coord_cartesian
# clips, rather than drops, anything outside these limits
fig3_xlim <- c(0.3, 0.7)
fig3_ylim <- c(0.8, 0.97)

# Global metrics plot ----
global_roc_acc_plot <- ggplot(
    model_metrics_summary %>% filter(!model %in% c("CatEmbed", "LSTM")),
    aes(
        x = F1_Macro_mean, y = ROC_AUC_mean,
        shape = model_data,
    )
) +
    geom_errorbar(
        aes(
            ymin = ROC_AUC_mean - ROC_AUC_sd,
            ymax = ROC_AUC_mean + ROC_AUC_sd,
            colour = model,
        ),
        width = 0, linewidth = 1, alpha = 0.75,
        lineend = "round"
    ) +
    geom_errorbarh(
        aes(
            xmin = F1_Macro_mean - F1_Macro_sd,
            xmax = F1_Macro_mean + F1_Macro_sd,
            colour = model,
        ),
        height = 0, linewidth = 1, alpha = 0.75,
        lineend = "round"
    ) +
    geom_point(
        aes(colour = model),
        size = 5,
    ) +
    coord_cartesian(xlim = fig3_xlim, ylim = fig3_ylim) +
    scale_x_continuous(breaks = seq(0.3, 0.7, by = 0.1)) +
    scale_y_continuous(breaks = seq(0.8, 0.95, by = 0.05)) +
    scale_shape_manual(
        name = "Category",
        values = c(17, 15, 16),
        labels = c(
            "ml_features" = "Features + ML",
            "dl_features" = "Features + DL",
            "dl_acceleration" = "Acceleration + DL"
        ),
        guide = "none"
    ) +
    geom_label_repel(
        aes(
            label = model, fill = model,
            segment.colour = model,
            colour = I("black")
        ),
        size = 4, direction = "both", nudge_x = 0.02, nudge_y = 0.01,
        force_pull = 0.5, show.legend = FALSE,
        seed = 1
    ) +
    scale_colour_manual(
        name = "Model",
        values = model_cols,
        aesthetics = c("colour", "segment.colour")
    ) +
    scale_fill_manual(
        values = model_cols,
    ) +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        strip.text = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(x = "Macro-F1", y = "ROC AUC")
global_roc_acc_plot

# Tuning improvements ----
## Data ----
tuned_model_metrics <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_tuned_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tuned_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_tuned_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_tuned_metrics.csv"
    )
) %>%
    mutate(
        model = case_when(
            model == "HydraMultiRocket" ~ "HydraMultiROCKET",
            model == "CategoryEmbedding" ~ "CatEmbed",
            model == "TSSequencer" ~ "TSSeq",
            TRUE ~ model
        ),
        model = factor(
            model,
            levels = c(
                "RF", "SVC", "XGB",
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraMultiROCKET"
            )
        )
    ) %>%
    select(
        model, burst, correction_type, test_fold, accuracy, roc_auc,
        balanced_acc, f1_macro, precision_macro, recall_macro
    )

default_model_metrics <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_tuned_default_control_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tuned_default_control_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_tuned_default_control_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_tuned_default_control_metrics.csv"
    )
) %>%
    mutate(
        model = str_replace(model, "_default", ""),
        model = case_when(
            model == "HydraMultiRocket" ~ "HydraMultiROCKET",
            model == "CategoryEmbedding" ~ "CatEmbed",
            model == "TSSequencer" ~ "TSSeq",
            TRUE ~ model
        ),
        model = factor(
            model,
            levels = c(
                "RF", "SVC", "XGB",
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraMultiROCKET"
            )
        )
    ) %>%
    select(
        model, burst, correction_type, test_fold, accuracy, roc_auc,
        balanced_acc, f1_macro, precision_macro, recall_macro
    )

# F1-macro and ROC AUC per model, default vs tuned (mean +/- SD across the
# 5 outer folds), on the same variables and limits as 3A
tuning_metrics <- bind_rows(
    tuned_model_metrics %>% mutate(source = "Tuned"),
    default_model_metrics %>% mutate(source = "Default")
) %>%
    group_by(model, source) %>%
    summarise(
        Accuracy_mean = mean(accuracy),
        Accuracy_sd = sd(accuracy),
        ROC_AUC_mean = mean(roc_auc),
        ROC_AUC_sd = sd(roc_auc),
        Balanced_Acc_mean = mean(balanced_acc),
        Balanced_Acc_sd = sd(balanced_acc),
        F1_Macro_mean = mean(f1_macro),
        F1_Macro_sd = sd(f1_macro),
        Precision_Macro_mean = mean(precision_macro),
        Precision_Macro_sd = sd(precision_macro),
        Recall_Macro_mean = mean(recall_macro),
        Recall_Macro_sd = sd(recall_macro),
        .groups = "drop"
    )

## Plot ----
tuning_metrics_plot <- ggplot(
    tuning_metrics %>% filter(!model %in% c("LSTM", "CatEmbed")),
    aes(
        x = F1_Macro_mean, y = ROC_AUC_mean,
        shape = interaction(model, source), color = model,
        group = interaction(model, source)
    )
) +
    geom_errorbar(
        aes(
            ymin = ROC_AUC_mean - ROC_AUC_sd,
            ymax = ROC_AUC_mean + ROC_AUC_sd,
        ),
        width = 0, linewidth = 1, alpha = 0.25,
        lineend = "round"
    ) +
    geom_errorbarh(
        aes(
            xmin = F1_Macro_mean - F1_Macro_sd,
            xmax = F1_Macro_mean + F1_Macro_sd,
        ),
        height = 0, linewidth = 1, alpha = 0.25,
        lineend = "round"
    ) +
    geom_point(
        aes(colour = model),
        size = 5,
    ) +
    geom_line(
        aes(group = model),
        linewidth = 1.2, alpha = 1
    ) +
    # Invisible points that only carry the untuned (filled) vs tuned (open)
    # legend; model colours are labelled in 3A
    geom_point(aes(fill = source), shape = 21, alpha = 0) +
    coord_cartesian(xlim = fig3_xlim, ylim = fig3_ylim) +
    scale_x_continuous(breaks = seq(0.3, 0.7, by = 0.1)) +
    scale_y_continuous(breaks = seq(0.8, 0.95, by = 0.05)) +
    scale_shape_manual(
        name = "Source",
        values = c(
            17, 17, 17, 15, 15, 16, 16,
            24, 24, 24, 22, 22, 21, 21
        ),
        guide = "none"
    ) +
    scale_colour_manual(
        name = "Model",
        values = model_cols,
        guide = "none"
    ) +
    scale_fill_manual(
        name = NULL,
        values = c(Default = "black", Tuned = "black"),
        breaks = c("Default", "Tuned"),
        labels = c(Default = "Untuned", Tuned = "Tuned"),
        guide = guide_legend(override.aes = list(
            shape = c(16, 1), alpha = 1, size = 4, colour = "black", stroke = 1.2
        ))
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        strip.text = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(x = "Macro-F1", y = "ROC AUC")
tuning_metrics_plot

## Per behaviour change ----
### Data ----
# Function to obtain per-behaviour F1 for each model and test fold
per_beh_f1_from_conf <- function(conf_df) {
    conf_df %>%
        group_by(model_label, burst, correction_type, test_fold) %>%
        group_modify(~ {
            grp <- .x
            classes <- sort(unique(grp$actual_label))
            map_dfr(classes, function(b) {
                tp <- sum(
                    grp$count[grp$actual_label == b & grp$predicted_label == b]
                )
                fp <- sum(
                    grp$count[grp$actual_label != b & grp$predicted_label == b]
                )
                fn <- sum(
                    grp$count[grp$actual_label == b & grp$predicted_label != b]
                )
                tibble(
                    behaviour = b,
                    support = tp + fn,
                    f1 = if ((2 * tp + fp + fn) == 0) NA_real_ else 2 * tp / (2 * tp + fp + fn)
                )
            })
        }) %>%
        ungroup()
}

tuned_beh_conf <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_tuned_confusion.csv"
    ) %>%
        filter(model == "RF") %>%
        mutate(model_label = "RF"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_tuned_confusion.csv"
    ) %>%
        filter(model == "TabPFN") %>%
        mutate(model_label = "TabPFN"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_tuned_confusion.csv"
    ) %>%
        filter(model == "HydraMultiRocket") %>%
        mutate(model_label = "HydraMultiROCKET"),
)

default_beh_conf <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_tuned_default_control_confusion.csv"
    ) %>%
        filter(model == "RF_default") %>%
        mutate(model_label = "RF"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_tuned_default_control_confusion.csv"
    ) %>%
        filter(model == "TabPFN_default") %>%
        mutate(model_label = "TabPFN"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_tuned_default_control_confusion.csv"
    ) %>%
        filter(model == "HydraMultiRocket_default") %>%
        mutate(model_label = "HydraMultiROCKET"),
)

tuned_beh_f1 <- per_beh_f1_from_conf(tuned_beh_conf)
default_beh_f1 <- per_beh_f1_from_conf(default_beh_conf)

# Behaviours with fewer than min_test_windows test windows in any fold are
# dropped: at B1, Running and Self-scratching have only 1-2 per fold, where
# tuned and default give identical F1 (a delta of exactly 0 in every fold)
min_test_windows <- 3
beh_scored <- tuned_beh_f1 %>%
    group_by(behaviour) %>%
    summarise(min_support = min(support), .groups = "drop") %>%
    filter(min_support >= min_test_windows) %>%
    pull(behaviour)
beh_scored

beh_delta_summary <- inner_join(
    tuned_beh_f1 %>%
        select(model_label, test_fold, behaviour, tuned = f1),
    default_beh_f1 %>%
        select(model_label, test_fold, behaviour, default = f1),
    by = c("model_label", "test_fold", "behaviour")
) %>%
    filter(behaviour %in% beh_scored) %>%
    mutate(delta = tuned - default) %>%
    group_by(model_label, behaviour) %>%
    summarise(
        n_folds = sum(!is.na(delta)),
        mean_delta = mean(delta, na.rm = TRUE),
        sd_delta = sd(delta, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        model_label = factor(
            model_label, levels = c("RF", "TabPFN", "HydraMultiROCKET")
        ),
        behaviour = factor(behaviour, levels = rev(beh_order))
    )

# Output CSV: per-behaviour change in F1 with tuning (tuned - default),
# mean and SD across outer folds (values behind Fig. 3C)
write.csv(
  beh_delta_summary,
  "./data/output/comparison_behaviour/behaviour_tuning_f1_change.csv",
  row.names = FALSE
)

### Plot ----
beh_delta_plot <- ggplot(
    beh_delta_summary,
    aes(
        x = mean_delta, y = behaviour,
        colour = model_label, shape = model_label
    )
) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(
        aes(xmin = mean_delta - sd_delta, xmax = mean_delta + sd_delta),
        height = 0, linewidth = 1, alpha = 0.75, lineend = "round",
        position = position_dodge(width = 0.5)
    ) +
    geom_point(
        size = 3, position = position_dodge(width = 0.5)
    ) +
    scale_colour_manual(name = NULL, values = model_cols) +
    scale_shape_manual(name = NULL, values = c(17, 15, 16)) +
    scale_y_discrete(labels = c(
        "Grooming receiver" = "Grooming\nreceiver",
        "Grooming actor" = "Grooming\nactor",
        "Self-scratching" = "Self-\nscratching"
    )) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        legend.title = element_text(size = 14, colour = "black"),
        legend.text = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        strip.text = element_text(size = 18, colour = "black"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    ) +
    guides(
        colour = guide_legend(override.aes = list(size = 4)),
        shape = guide_legend(override.aes = list(size = 4))
    ) +
    labs(
        x = "\u0394F1, tuned \u2212 untuned  (per fold, mean \u00b1 SD)", y = NULL
    )
beh_delta_plot

# Combine 3A, 3B and 3C into Fig. 3 ----
fig3_plot <- plot_grid(
    global_roc_acc_plot, tuning_metrics_plot, beh_delta_plot,
    ncol = 3, align = "h", axis = "tb",
    labels = c("A", "B", "C"), label_size = 20,
    rel_widths = c(1, 1, 1)
)

ggsave(
    "./plots/fig3_global_metrics_tuning.png",
    plot = fig3_plot,
    width = 18, height = 6, dpi = 300
)

ggsave(
    "./plots/fig3_global_metrics_tuning.pdf",
    plot = fig3_plot,
    device = cairo_pdf,
    width = 18, height = 6, units = "in", dpi = 1000,
    fallback_resolution = 1000
)
