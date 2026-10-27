# We will make plots for the publication figure 2 which will contain results for experiment 1 and 2

library(tidyverse)
library(cowplot)

# Fixed seed so the jittered fold points are placed the same way in every run
set.seed(42)

# Mean +/- 1 SD across folds for stat_summary (same summary as Figs. 3 and 5;
# written out so it does not need the Hmisc package that mean_sdl requires)
mean_sd <- function(x) {
    data.frame(y = mean(x), ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
}

# Load data ----
beh_colors <- c(
    "Resting" = "#D32F2F",
    "Eating" = "#BA68C8",
    "Walking" = "#827717",
    "Grooming actor" = "#D81B60",
    "Grooming receiver" = "#512DA8",
    "Sleeping" = "#42A5F5",
    "Self-scratching" = "#64DD17",
    "Running" = "#FF6F00"
)
## Global metric comparisons ----
# Metric values (Exp 1 and 2 compare burst lengths and corrections for RF)
model_metrics <- read.csv(
    "./data/output/comparison_behaviour/behaviour_features_ml_metrics.csv"
) %>%
    filter(model == "RF") %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2","Burst_3", "Burst_4")
        ),
        correction_type = case_when(
            correction_type == "uncorrected" ~ "Unc",
            correction_type == "rotdaily" ~ "Daily",
            correction_type == "rotbasal" ~ "Basal",
        ),
        correction_type = factor(
            correction_type,
            levels = c("Unc", "Daily", "Basal")
        )
    ) %>%
    select(model, burst, correction_type, fold, accuracy, roc_auc) %>%
    pivot_longer(
        cols = c(accuracy, roc_auc),
        names_to = "metric",
        values_to = "value"
    )

## Behavioural metrics comparison ----
# Order of behaviours
beh_order <- c(
    "Resting", "Eating",
    "Walking", "Grooming actor", "Grooming receiver",
    "Sleeping", "Self-scratching", "Running"
)

# Metric values
beh_metrics <- read.csv(
    "./data/temp/statistical_analysis/exp123_behaviour_classification_precision_recall.csv"
) %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2","Burst_3", "Burst_4")
        ),
        correction_type = case_when(
            correction_type == "uncorrected" ~ "Unc",
            correction_type == "rotdaily" ~ "Daily",
            correction_type == "rotbasal" ~ "Basal"
        ),
        correction_type = factor(
            correction_type,
            levels = c("Unc", "Daily", "Basal")
        ),
        model = factor(
            model,
            levels = c(
                "RF", "SVC", "XGB",
                "CatEmbed", "GANDALF",  "TabPFN",
                "LSTM", "TSSeq", "HydraMultiROCKET"
            )
        ),
        beh_rarity = str_to_title(beh_rarity),
        beh_rarity = factor(
            beh_rarity,
            levels = c("Common", "Uncommon", "Rare")
        ),
        actual_label = factor(
            actual_label,
            levels = beh_order
        )
    ) %>%
    select(
        model, burst, correction_type, fold,
        beh_rarity, behaviour = actual_label,
        actual_count, recall, precision
    ) %>%
    pivot_longer(
        cols = c(recall, precision),
        names_to = "metric",
        values_to = "value"
    )

# Exp 1 Plots ----
## Global metrics ----
### Data ----
burst_comp_df <- model_metrics %>%
    filter(model == "RF" & correction_type == "Unc")

## Behaviour metrics ----
### Data ----
beh_burst_df <- beh_metrics %>%
    filter(model == "RF" & correction_type == "Unc") %>%
    filter(
        beh_rarity != "Rare"
    )

### Plot ----
beh_burst_plot <- ggplot(
    beh_burst_df,
    aes(
        x = burst, y = value,
        group = beh_rarity
    )
) +
    geom_point(
        aes(color = beh_rarity),
        size = 3, alpha = 0.2,
        position = position_jitterdodge(
            jitter.width = 0.1,
            dodge.width = 0.75
        )
    ) +
    stat_summary(
        aes(color = beh_rarity),
        # color = "black",
        fun.data = mean_sd,
        geom = "pointrange", shape = 15,
        position = position_dodge(width = 0.75),
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_y_continuous(
        breaks = seq(0, 1, by = 0.5)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_discrete(
        labels = c("B1", "B2", "B3", "B4")
    ) +
    scale_colour_manual(
        values = c(
            beh_colors, "Common" = "#4DD0E1",
            "Uncommon" = "#009688", "Rare" = "#9CCC65"
        ),
        breaks = c(names(beh_colors), "Common", "Uncommon", "Rare")
    ) +
    facet_wrap(
        ~metric,
        scales = "free_y", strip.position = "left",
        labeller = as_labeller(
            c(
                precision = "Precision",
                recall = "Recall"
            )
        )
    ) +
    labs(
        x = "Burst Length",
        y = "Metric Value",
        color = "Behaviour Type"
    ) +
    theme_bw() +
    theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(
        title = "",
        x = "Burst Length",
    )
beh_burst_plot

# Exp 2 Plots ----
## Global metrics plot ----
### Data ----
# Burst 1, as in the stats (06a) and the model comparison (Fig. 3); the
# behavioural metrics below use Burst 4, which has more rare-behaviour windows
correction_comp_df <- model_metrics %>%
    filter(model == "RF" & burst == "Burst_1")

## Behavioural Metrics Plot ----
### Data ----
beh_correction_df <- beh_metrics %>%
    filter(model == "RF" & burst == "Burst_4")

### Plot ----
beh_correction_plot <- ggplot(
    beh_correction_df,
    aes(
        x = correction_type, y = value,
        group = beh_rarity
    )
) +
    geom_point(
        aes(color = beh_rarity),
        size = 3, alpha = 0.2,
        position = position_jitterdodge(
            jitter.width = 0.1,
            dodge.width = 0.75
        )
    ) +
    stat_summary(
        aes(color = beh_rarity),
        # color = "black",
        fun.data = mean_sd,
        geom = "pointrange", shape = 15,
        position = position_dodge(width = 0.75),
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_y_continuous(
        breaks = seq(0, 1, by = 0.5)
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_discrete(
        labels = c("Uncorrected", "Daily", "Basal")
    ) +
    scale_colour_manual(
        values = c(
            beh_colors, "Common" = "#4DD0E1",
            "Uncommon" = "#009688", "Rare" = "#9CCC65"
        ),
        breaks = c(names(beh_colors), "Common", "Uncommon", "Rare")
    ) +
    facet_wrap(
        ~metric,
        scales = "free_y", strip.position = "left",
        labeller = as_labeller(
            c(
                precision = "Precision",
                recall = "Recall"
            )
        )
    ) +
    labs(
        x = "Correction Type",
        y = "Metric Value",
        color = "Behaviour Type"
    ) +
    theme_bw() +
    theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 13, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(
        title = "",
        x = "Correction Type",
    )
beh_correction_plot

# Combined plot ----
# For the combined plot, we will keep ROC AUC from global metrics and precision and recall from behavioural metrics
# We will create a 2x2 grid with global metrics on left and behavioural metrics on right.
# We need to recreate the plots to remove the facet and get the size of the points and error bars consistent.

# ROC AUC for Exp 1
exp1_global_rocauc_plot <- ggplot(
    burst_comp_df %>% filter(metric == "roc_auc"),
    aes(x = burst, y = value)
) +
    geom_jitter(
        size = 3, width = 0.1, alpha = 0.5,
        color = "#7E57C2"
    ) +
    stat_summary(
        fun.data = mean_sd, shape = 15,
        geom = "pointrange", color = "#7E57C2",
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_y_continuous(
        breaks = seq(0.88, 0.94, by = 0.02)
    ) +
    coord_cartesian(ylim = c(0.875, 0.955)) +
    scale_x_discrete(
        labels = c("B1", "B2", "B3", "B4")
    ) +
    labs(
        x = "Burst Length",
        y = "ROC AUC"
    ) +
    theme_bw() +
    theme(
        axis.text = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(
        title = "",
        x = "Burst Length",
    )
exp1_global_rocauc_plot

# ROC AUC for Exp 2
exp2_global_rocauc_plot <- ggplot(
    correction_comp_df %>% filter(metric == "roc_auc"),
    aes(x = correction_type, y = value)
) +
    geom_jitter(
        size = 3, width = 0.1, alpha = 0.5,
        color = "#EF5350"
    ) +
    stat_summary(
        fun.data = mean_sd, shape = 15,
        geom = "pointrange", color = "#EF5350",
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_y_continuous(
        breaks = seq(0.87, 0.95, by = 0.02)
    ) +
    coord_cartesian(ylim = c(0.865, 0.955)) +
    scale_x_discrete(
        labels = c("Uncorrected", "Daily", "Basal")
    ) +
    labs(
        x = "Correction Type",
        y = "ROC AUC"
    ) +
    theme_bw() +
    theme(
        axis.text = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 13, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(
        title = "",
        x = "Correction Type",
    )
exp2_global_rocauc_plot

# Plot combined
# Left
p2_left <- plot_grid(
    exp1_global_rocauc_plot,
    exp2_global_rocauc_plot,
    ncol = 1, labels = c("A", "B"),
    align = "v", axis = "lr"
)
p2_left

# Right
p2_right <- plot_grid(
    beh_burst_plot +
        theme(legend.position = "none"),
    beh_correction_plot +
        theme(legend.position = "none"),
    ncol = 1, labels = c("C", "D"),
    align = "v", axis = "lr"
)
p2_right

# Combine left and right
p2_combined <- plot_grid(
    p2_left, p2_right,
    ncol = 2, rel_widths = c(1, 2),
    align = "vh", axis = "tblr"
)
p2_combined

# Get legend of behavioural rarity
legend_beh_rarity <- get_legend(
    beh_correction_plot +
        theme(
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 14)
        )
)

# Combine legend row
p2_legend <- plot_grid(
    NULL, legend_beh_rarity,
    nrow = 1, rel_widths = c(1, 2)
)
p2_legend

# Final combined plot with legend
p2_final <- plot_grid(
    p2_combined,
    p2_legend,
    ncol = 1, rel_heights = c(1, 0.08)
) +
    theme(
        plot.background = element_rect(fill = "white", color = NA)
    )
p2_final
ggsave(
    "./plots/fig2_exp12_model_behaviour_metrics_combined.png",
    plot = p2_final,
    width = 12, height = 7.2, dpi = 300
)
ggsave(
    "./plots/fig2_exp12_model_behaviour_metrics_combined.pdf",
    plot = p2_final,
    device = cairo_pdf,
    width = 15,
    height = 9,
    units = "in",
    dpi = 1000,
    fallback_resolution = 1000
)
