# We will make plots for the publication figure 2 which will contain results for experiment 1 and 2

library(tidyverse)
library(cowplot)

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
# Metric values
model_metrics <- bind_rows(
    read.csv(
        "./data/output/behaviour_comparison/behaviour_features_ml_metrics.csv"
    ),
    read.csv(
        "./data/output/behaviour_comparison/behaviour_features_dl_metrics.csv"
    ),
    read.csv(
        "./data/output/behaviour_comparison/behaviour_features_dl_tabpfn_metrics.csv"
    ),
    read.csv(
        "./data/output/behaviour_comparison/behaviour_acceleration_dl_metrics.csv"
    )
) %>%
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
        ),
        model = case_when(
            model == "CategoryEmbeddingModel" ~ "CatEmbed",
            model == "TSSequencerPlus" ~ "TSSeq",
            model == "HydraMultiRocketPlus" ~ "HydraRocket",
            TRUE ~ model
        ),
        model = factor(
            model,
            levels = c(
                "RF", "SVC", "XGB",
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraRocket"
            )
        )
    ) %>%
    select(model, burst, correction_type, random_seed, accuracy, roc_auc) %>%
    pivot_longer(
        cols = c(accuracy, roc_auc),
        names_to = "metric",
        values_to = "value"
    )

# Marginal means
model_mm <- read.csv(
    "./data/output/statistical_analysis/exp123_model_metrics_comparison_mm.csv"
) %>%
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
        ),
        model = factor(
            model,
            levels = c(
                "RF", "SVC", "XGB",
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraRocket"
            )
        )
    )

# Contrasts
model_contr <- read.csv(
    "./data/output/statistical_analysis/exp123_model_metrics_comparison_contrasts.csv"
) %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2", "Burst_3", "Burst_4")
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
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraRocket"
            )
        )
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
                "LSTM", "TSSeq", "HydraRocket"
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
        model, burst, correction_type, random_seed,
        beh_rarity, behaviour = actual_label,
        actual_count, recall, precision
    ) %>%
    pivot_longer(
        cols = c(recall, precision),
        names_to = "metric",
        values_to = "value"
    )

# Marginal means
beh_mm <- read.csv(
    "./data/output/statistical_analysis/exp123_behaviour_metrics_comparison_mm.csv"
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
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraRocket"
            )
        ),
        beh_rarity = str_to_title(beh_rarity),
        beh_rarity = factor(
            beh_rarity,
            levels = c("Common", "Uncommon", "Rare")
        )
    )

# Contrasts
beh_contr <- read.csv(
    "./data/output/statistical_analysis/exp123_behaviour_metrics_comparison_contrasts.csv"
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
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraRocket"
            )
        ),
        beh_rarity = str_to_title(beh_rarity),
        beh_rarity = factor(
            beh_rarity,
            levels = c("Common", "Uncommon", "Rare")
        )
    )

# Exp 1 Plots ----
## Global metrics ----
### Data ----
burst_comp_df <- model_metrics %>%
    filter(model == "RF" & correction_type == "Unc")

# Marginal means and contrasts
burst_comp_mm <- model_mm %>%
    filter(comparison == "burst")

burst_comp_contrasts <- model_contr %>%
    filter(comparison == "burst")

### Plot ----
burst_comp_plot <- ggplot(
    burst_comp_df,
    aes(x = burst, y = value)
) +
    geom_jitter(
        size = 3, width = 0.1, alpha = 0.5,
        color = "#7E57C2"
    ) +
    # geom_point(
    #     data = burst_comp_mm,
    #     aes(x = burst, y = Proportion),
    #     color = "#7E57C2", size = 6
    # ) +
    # geom_errorbar(
    #     data = burst_comp_mm,
    #     aes(
    #         x = burst, y = Proportion,
    #         ymin = CI_low, ymax = CI_high
    #     ),
    #     width = 0, color = "#7E57C2", linewidth = 1,
    #     lineend = "round"
    # ) +
    # Add stat summary with mean and se with point and errorbar
    stat_summary(
        fun.data = "mean_se",
        geom = "pointrange", color = "#7E57C2",
        shape = 15, linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_x_discrete(
        labels = c("1", "2", "3", "4")
    ) +
    facet_wrap(
        ~metric,
        scales = "free_y", strip.position = "left",
        labeller = as_labeller(
            c(
                accuracy = "Accuracy",
                roc_auc = "ROC AUC"
            )
        )
    ) +
    labs(
        title = "Model Metrics across Burst Lengths",
        x = "Burst Length",
        y = "Metric Value"
    ) +
    theme_bw() +
    theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text =  element_text(size = 14, colour = "black"),
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
burst_comp_plot
ggsave(
    "./plots/publication/exp1_model_metrics.png",
    plot = burst_comp_plot,
    width = 8, height = 5, dpi = 300
)

## Behaviour metrics ----
### Data ----
beh_burst_df <- beh_metrics %>%
    filter(model == "RF" & correction_type == "Unc") %>%
    filter(
        beh_rarity != "Rare"
    )
# Marginal means and contrasts
beh_burst_mm <- beh_mm %>%
    filter(comparison == "burst")
beh_burst_contrasts <- beh_contr %>%
    filter(comparison == "burst")

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
        fun.data = "mean_se",
        geom = "pointrange", shape = 15,
        position = position_dodge(width = 0.75),
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_x_discrete(
        labels = c("1", "2", "3", "4")
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
        title = "Behavioural Metrics across Burst Lengths",
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
ggsave(
    "./plots/publication/exp1_behaviour_metrics.png",
    plot = beh_burst_plot,
    width = 8, height = 5, dpi = 300
)

# Exp 2 Plots ----
## Global metrics plot ----
### Data ----
correction_comp_df <- model_metrics %>%
    filter(model == "RF" & burst == "Burst_4")

# Marginal means and contrasts
correction_comp_mm <- model_mm %>%
    filter(comparison == "correction")
correction_comp_contrasts <- model_contr %>%
    filter(comparison == "correction")

### Plot ----
correction_comp_plot <- ggplot(
    correction_comp_df,
    aes(x = correction_type, y = value)
) +
    geom_jitter(
        size = 3, width = 0.1, alpha = 0.5,
        color = "#EF5350"
    ) +
    stat_summary(
        fun.data = "mean_se",
        geom = "pointrange", color = "#EF5350",
        shape = 15, linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_x_discrete(
        labels = c("Unc", "Daily", "Basal")
    ) +
    facet_wrap(
        ~metric,
        scales = "free_y", strip.position = "left",
        labeller = as_labeller(
            c(
                accuracy = "Accuracy",
                roc_auc = "ROC AUC"
            )
        )
    ) +
    labs(
        title = "Model Metrics across Correction Types",
        x = "Correction Type",
        y = "Metric Value"
    ) +
    theme_bw() +
    theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(
        title = "",
        x = "Correction Type",
    )
correction_comp_plot
ggsave(
    "./plots/publication/exp2_model_metrics.png",
    plot = correction_comp_plot,
    width = 8, height = 5, dpi = 300
)

## Behavioural Metrics Plot ----
### Data ----
beh_correction_df <- beh_metrics %>%
    filter(model == "RF" & burst == "Burst_4")
# Marginal means and contrasts
beh_correction_mm <- beh_mm %>%
    filter(comparison == "correction")
beh_correction_contrasts <- beh_contr %>%
    filter(comparison == "correction")

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
        fun.data = "mean_se",
        geom = "pointrange", shape = 15,
        position = position_dodge(width = 0.75),
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.5)
    ) +
    scale_x_discrete(
        labels = c("Unc", "Daily", "Basal")
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
        title = "Behavioural Metrics across Correction Types",
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
        axis.text.x = element_text(size = 18, colour = "black"),
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
ggsave(
    "./plots/publication/exp2_behaviour_metrics.png",
    plot = beh_correction_plot,
    width = 8, height = 5, dpi = 300
)

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
        fun.data = "mean_se", shape = 15,
        geom = "pointrange", color = "#7E57C2",
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_y_continuous(
        limits = c(0.885, 0.935),
        breaks = seq(0.89, 0.93, by = 0.01)
    ) +
    scale_x_discrete(
        labels = c("1", "2", "3", "4")
    ) +
    labs(
        title = "Model ROC AUC across Burst Lengths",
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
        fun.data = "mean_se", shape = 15,
        geom = "pointrange", color = "#EF5350",
        linewidth = 1, size = 1, lineend = "round"
    ) +
    scale_y_continuous(
        limits = c(0.885, 0.935),
        breaks = seq(0.89, 0.93, by = 0.01)
    ) +
    scale_x_discrete(
        labels = c("Unc", "Daily", "Basal")
    ) +
    labs(
        title = "Model ROC AUC across Correction Types",
        x = "Correction Type",
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
        x = "Correction Type",
    )
exp2_global_rocauc_plot

# Plot combined
# Left
p1_left <- plot_grid(
    exp1_global_rocauc_plot,
    exp2_global_rocauc_plot,
    ncol = 1, labels = c("A", "B"),
    align = "v", axis = "lr"
)
p1_left

# Right
p1_right <- plot_grid(
    beh_burst_plot +
        theme(legend.position = "none"),
    beh_correction_plot +
        theme(legend.position = "none"),
    ncol = 1, labels = c("C", "D"),
    align = "v", axis = "lr"
)
p1_right

# Combine left and right
p1_combined <- plot_grid(
    p1_left, p1_right,
    ncol = 2, rel_widths = c(1, 2),
    align = "vh", axis = "tblr"
)
p1_combined

# Get elegend of behavioural rarity
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
p1_legend <- plot_grid(
    NULL, legend_beh_rarity,
    nrow = 1, rel_widths = c(1, 2)
)
p1_legend

# Final combined plot with legend
p1_final <- plot_grid(
    p1_combined,
    p1_legend,
    ncol = 1, rel_heights = c(1, 0.2)
) +
    theme(
        plot.background = element_rect(fill = "white", color = NA)
    )
p1_final
ggsave(
    "./plots/publication/fig1_exp12_model_behaviour_metrics_combined.png",
    plot = p1_final,
    width = 12, height = 8, dpi = 300
)
