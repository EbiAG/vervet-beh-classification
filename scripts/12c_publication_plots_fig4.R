# We will make plots for publication figure 4 which will contain the final model selection
# (burst length, model choice and orientation correction)

library(tidyverse)
library(cowplot)

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

# Order of behaviours
beh_order <- c(
    "Resting", "Eating",
    "Walking", "Grooming actor", "Grooming receiver",
    "Sleeping", "Self-scratching", "Running"
)

# Figure 4: final model selection ----
# A: per-behaviour F1 across burst lengths (main CV): RF, TabPFN and HR on
#    uncorrected data, and HR on daily-corrected data (the final model at B2)
# B: precision vs recall at Burst_2 for the same four models
# Both panels use the out-of-fold predictions. Unlike Figure 3 (mean +/- SD
# across folds), metrics here are pooled across folds with bootstrap 95% CIs.
# Rare behaviours have only 1-2 bursts per fold, so per-fold values are near
# all-or-nothing and their SD is uninformative and can exceed [0, 1]; pooled
# bootstrap CIs reflect the number of bursts instead. Bootstraps resample
# parent bursts, keeping the sub-bursts of a burst together.
fig4_models <- c("RF", "TabPFN", "HydraMultiROCKET", "HydraMultiROCKET-Daily")
fig4_cols <- c(
    model_cols[c("RF", "TabPFN", "HydraMultiROCKET")],
    "HydraMultiROCKET-Daily" = "#00695C"
)
# Shapes follow Fig. 3 (triangle = features + ML, square = features + DL,
# circle = acceleration + DL); diamond for the daily-corrected HR, so the two
# HR models differ by more than shade
fig4_shapes <- c(
    "RF" = 17, "TabPFN" = 15,
    "HydraMultiROCKET" = 16, "HydraMultiROCKET-Daily" = 18
)
# Relative point sizes; the diamond (18) draws smaller than the other shapes
fig4_size_mult <- c(
    "RF" = 1, "TabPFN" = 1,
    "HydraMultiROCKET" = 1, "HydraMultiROCKET-Daily" = 1.3
)
n_boot <- 1000

## Data ----
# Out-of-fold predictions, all bursts; one row per sub-burst with each
# model's label, keeping sub-bursts predicted by all four models. RF, TabPFN
# and HR on uncorrected data, plus HR on daily-corrected data
fig4_oof <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_oof_predictions.csv"
    ) %>%
        filter(model == "RF"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_oof_predictions.csv"
    ) %>%
        filter(model == "TabPFN"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_oof_predictions.csv"
    ) %>%
        filter(model == "HydraMultiRocketPlus")
) %>%
    filter(
        correction_type == "uncorrected" |
            (model == "HydraMultiRocketPlus" & correction_type == "rotdaily")
    ) %>%
    mutate(
        model = case_when(
            model == "HydraMultiRocketPlus" & correction_type == "rotdaily" ~
                "HydraMultiROCKET-Daily",
            model == "HydraMultiRocketPlus" ~ "HydraMultiROCKET",
            TRUE ~ model
        )
    ) %>%
    select(
        model, burst, Ind_ID, new_burst, burst_id, actual_label,
        predicted_label
    ) %>%
    pivot_wider(names_from = model, values_from = predicted_label) %>%
    drop_na(all_of(fig4_models)) %>%
    mutate(parent = paste(Ind_ID, burst_id, sep = "_"))

# Pooled precision, recall and F1 per model and behaviour, with 95% CIs from
# resampling parent bursts. preds has one row per sub-burst and model, with
# columns model, parent, actual_label and pred_label. Every model gets the
# same resamples.
boot_pooled_metrics <- function(preds) {
    # TP/FP/FN per parent burst, so resamples keep sub-bursts together
    counts <- map_dfr(beh_order, function(b) {
        preds %>%
            group_by(model, parent) %>%
            summarise(
                tp = sum(actual_label == b & pred_label == b),
                fp = sum(actual_label != b & pred_label == b),
                fn = sum(actual_label == b & pred_label != b),
                .groups = "drop"
            ) %>%
            mutate(behaviour = b)
    })

    parents <- unique(counts$parent)
    set.seed(42)
    weights <- replicate(
        n_boot,
        tabulate(
            sample.int(length(parents), replace = TRUE),
            nbins = length(parents)
        )
    )

    ratio <- function(num, den) if_else(den == 0, NA_real_, num / den)
    counts %>%
        group_by(model, behaviour) %>%
        group_modify(~ {
            w <- weights[match(.x$parent, parents), , drop = FALSE]
            tp_b <- colSums(w * .x$tp)
            fp_b <- colSums(w * .x$fp)
            fn_b <- colSums(w * .x$fn)
            tp <- sum(.x$tp)
            fp <- sum(.x$fp)
            fn <- sum(.x$fn)
            rec_b <- ratio(tp_b, tp_b + fn_b)
            prec_b <- ratio(tp_b, tp_b + fp_b)
            f1_b <- ratio(2 * tp_b, 2 * tp_b + fp_b + fn_b)
            tibble(
                recall = ratio(tp, tp + fn),
                recall_lo = quantile(rec_b, 0.025, na.rm = TRUE),
                recall_hi = quantile(rec_b, 0.975, na.rm = TRUE),
                precision = ratio(tp, tp + fp),
                precision_lo = quantile(prec_b, 0.025, na.rm = TRUE),
                precision_hi = quantile(prec_b, 0.975, na.rm = TRUE),
                f1 = ratio(2 * tp, 2 * tp + fp + fn),
                f1_lo = quantile(f1_b, 0.025, na.rm = TRUE),
                f1_hi = quantile(f1_b, 0.975, na.rm = TRUE)
            )
        }) %>%
        ungroup()
}

## Panel A data ----
# Pooled F1 per burst length; each burst has its own parent bursts
window_f1 <- fig4_oof %>%
    pivot_longer(
        cols = all_of(fig4_models),
        names_to = "model", values_to = "pred_label"
    ) %>%
    group_by(burst) %>%
    group_modify(~ boot_pooled_metrics(.x)) %>%
    ungroup()

# Distinct bursts per behaviour at B1 and B4, for facet labels
fold_lookup <- read.csv("./data/temp/indices_cv_fold_lookup.csv")
beh_events <- fold_lookup %>%
    filter(burst %in% c("burst_1", "burst_4")) %>%
    group_by(behaviour, burst) %>%
    summarise(n_events = n_distinct(Ind_ID, burst_id), .groups = "drop") %>%
    pivot_wider(names_from = burst, values_from = n_events) %>%
    mutate(beh_label = paste0(behaviour, "\n(", burst_1, " → ", burst_4, " bursts)"))
beh_labels <- setNames(beh_events$beh_label, beh_events$behaviour)

# Two-line strip labels for panel B (bursts at B2), matching panel A's strip
# height so both panels have identical facet sizes
beh_bursts_b2 <- fold_lookup %>%
    filter(burst == "burst_2") %>%
    group_by(behaviour) %>%
    summarise(n_bursts = n_distinct(Ind_ID, burst_id), .groups = "drop")
beh_labels_b <- setNames(
    paste0(beh_bursts_b2$behaviour, "\n(", beh_bursts_b2$n_bursts, " bursts)"),
    beh_bursts_b2$behaviour
)

window_f1 <- window_f1 %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2", "Burst_3", "Burst_4"),
            labels = c("B1", "B2", "B3", "B4")
        ),
        behaviour = factor(behaviour, levels = beh_order),
        model = factor(model, levels = fig4_models)
    )

# Pooled per-behaviour precision, recall and F1 with parent-burst bootstrap
# 95% CIs, per model and burst length (Fig. 4A-B, text and Table S[new])
write.csv(
  window_f1,
  "./data/output/comparison_behaviour/model_selection_pooled_metrics.csv",
  row.names = FALSE
)

## Panel A plot ----
window_f1_plot <- ggplot(
    window_f1,
    aes(x = burst, y = f1, colour = model, group = model)
) +
    geom_vline(xintercept = 2, linetype = "dashed", colour = "grey60") +
    geom_errorbar(
        aes(ymin = f1_lo, ymax = f1_hi),
        width = 0, linewidth = 0.8, alpha = 0.5,
        position = position_dodge(width = 0.3)
    ) +
    geom_line(linewidth = 1, position = position_dodge(width = 0.3)) +
    geom_point(
        aes(shape = model, size = model),
        position = position_dodge(width = 0.3)
    ) +
    facet_wrap(
        ~behaviour,
        nrow = 2,
        labeller = labeller(behaviour = beh_labels)
    ) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_colour_manual(name = NULL, values = fig4_cols) +
    scale_shape_manual(name = NULL, values = fig4_shapes) +
    scale_size_manual(name = NULL, values = 2.5 * fig4_size_mult) +
    theme_bw() +
    theme(
        legend.position = "none",
        aspect.ratio = 1,
        plot.background = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        # Same facet gap as B (wide enough that B's neighbouring 1.0 and 0.0
        # labels do not touch), so the columns of A and B line up
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    ) +
    labs(x = "Burst Length", y = "F1")
window_f1_plot

## Panel B data ----
final_pr <- fig4_oof %>%
    filter(burst == "Burst_2") %>%
    pivot_longer(
        cols = all_of(fig4_models),
        names_to = "model", values_to = "pred_label"
    ) %>%
    boot_pooled_metrics() %>%
    mutate(
        behaviour = factor(behaviour, levels = beh_order),
        model = factor(model, levels = names(fig4_cols))
    )

final_pr %>%
    arrange(behaviour, model) %>%
    print(n = Inf)

# Output CSVs: pooled precision, recall and F1 behind Panels A and B,
# with parent-burst bootstrap CIs (values cited in the Results)
write.csv(
  window_f1,
  "./data/output/comparison_behaviour/behaviour_pooled_metrics_by_burst.csv",
  row.names = FALSE
)
write.csv(
  final_pr,
  "./data/output/comparison_behaviour/behaviour_pooled_metrics_burst2.csv",
  row.names = FALSE
)

## Panel B plot ----
final_pr_plot <- ggplot(
    final_pr,
    aes(x = precision, y = recall, colour = model)
) +
    geom_abline(
        slope = 1, intercept = 0, linetype = "dashed", colour = "grey60"
    ) +
    geom_errorbar(
        aes(ymin = recall_lo, ymax = recall_hi),
        width = 0, linewidth = 0.8, alpha = 0.5
    ) +
    geom_errorbarh(
        aes(xmin = precision_lo, xmax = precision_hi),
        height = 0, linewidth = 0.8, alpha = 0.5
    ) +
    geom_point(aes(shape = model, size = model)) +
    facet_wrap(
        ~behaviour,
        nrow = 2,
        labeller = labeller(behaviour = beh_labels_b)
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
    scale_colour_manual(name = NULL, values = fig4_cols) +
    scale_shape_manual(name = NULL, values = fig4_shapes) +
    scale_size_manual(name = NULL, values = 3 * fig4_size_mult) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        aspect.ratio = 1,
        plot.background = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA),
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        # Same facet gap as A, so the columns line up
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    ) +
    labs(x = "Precision", y = "Recall")
final_pr_plot

## Combine ----
# Shared legend below both panels so the two panels have identical layouts
fig4_legend <- get_legend(final_pr_plot)

fig4_plot <- plot_grid(
    plot_grid(
        window_f1_plot, final_pr_plot + theme(legend.position = "none"),
        ncol = 1, align = "v", axis = "lr",
        labels = c("A", "B"), label_size = 20
    ),
    fig4_legend,
    ncol = 1, rel_heights = c(1, 0.04)
) +
    theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(
    "./plots/fig4_final_model_selection.png",
    plot = fig4_plot,
    width = 12, height = 13.5, dpi = 300
)

ggsave(
    "./plots/fig4_final_model_selection.pdf",
    plot = fig4_plot,
    device = cairo_pdf,
    width = 12, height = 13.5, units = "in", dpi = 1000,
    fallback_resolution = 1000
)
