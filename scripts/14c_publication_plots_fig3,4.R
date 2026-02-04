# We will make plots for publication figure 3 and 4 which will contain results for experiment 3

library(tidyverse)
library(cowplot)
library(ggh4x)
library(ggrepel)

# Load data ----
## Global metric data ----
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
    select(model, burst, correction_type, random_seed, accuracy, roc_auc)

# Summarise
model_metrics_summary <- model_metrics %>%
    filter(burst == "Burst_4" & correction_type == "Unc") %>%
    mutate(
        model_data = case_when(
            model %in% c("RF", "SVC", "XGB") ~ "ml_features",
            model %in% c("LSTM", "TSSeq", "HydraRocket") ~ "dl_acceleration",
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
        .groups = "drop"
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
        beh_rarity,
        behaviour = actual_label,
        actual_count, recall, precision
    ) %>%
    pivot_longer(
        cols = c(recall, precision),
        names_to = "metric",
        values_to = "value"
    )

# Global metrics plot ----
global_metrics_plot <- ggplot(
    model_metrics_summary %>% filter(model != "LSTM"),
    aes(
        x = Accuracy_mean, y = ROC_AUC_mean,
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
            xmin = Accuracy_mean - Accuracy_sd,
            xmax = Accuracy_mean + Accuracy_sd,
            colour = model,
        ),
        height = 0, linewidth = 1, alpha = 0.75,
        lineend = "round"
    ) +
    geom_point(
        aes(colour = model),
        size = 5,
    ) +
    scale_y_continuous(
        limits = c(0.85, 0.95),
        breaks = seq(0.86, 0.94, by = 0.02)
    ) +
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
            # label = paste0(
            #     round(Accuracy_mean, 2), "\n", round(ROC_AUC_mean, 2)
            # ),
            label = model, fill = model,
            segment.colour = model,
            colour = I("black")
        ),
        # colour = "black",
        size = 4, direction = "both", nudge_x = 0.02, nudge_y = -0.01,
        # force = 5,
        force_pull = 0.5, show.legend = FALSE,
        seed = 1
    ) +
    scale_colour_manual(
        name = "Model",
        values = c(
            "HydraRocket" = "#26A69A",
            "TSSeq" = "#81D4FA",
            "LSTM" = "#1565C0",
            "TabPFN" = "#AB47BC",
            "GANDALF" = "#E1BEE7",
            "CatEmbed" = "#F48FB1",
            "SVC" = "#FFA000",
            "XGB" = "#FFCA28",
            "RF" = "#FF5722"
        ),
        aesthetics = c("colour", "segment.colour")
    ) +
    scale_fill_manual(
        values = c(
            "HydraRocket" = "#26A69A",
            "TSSeq" = "#81D4FA",
            "LSTM" = "#1565C0",
            "TabPFN" = "#AB47BC",
            "GANDALF" = "#E1BEE7",
            "CatEmbed" = "#F48FB1",
            "SVC" = "#FFA000",
            "XGB" = "#FFCA28",
            "RF" = "#FF5722"
        )
    ) +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text = element_text(size = 14, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        strip.text = element_text(size = 18, colour = "black"),
        panel.grid = element_blank()
    ) +
    # Combine shapes and colours in legend
    guides(
        colour = guide_legend(override.aes = list(
            size = 5,
            shape = c(16, 16, 16, 15, 15, 15, 17, 17)
        ))
    ) +
    labs(x = "Accuracy", y = "ROC AUC")
global_metrics_plot
ggsave(
    "./plots/publication/exp3_model_metrics.png",
    plot = global_metrics_plot,
    width = 8, height = 6, dpi = 300
)

# Best models in each behaviour ----
## Data ----
beh_best3_metrics <- beh_metrics %>%
    filter(
        model %in% c(
            "HydraRocket", "TabPFN", "RF"
        )
    ) %>%
    group_by(
        model, burst, correction_type,
        beh_rarity, behaviour, metric
    ) %>%
    summarise(
        value = mean(value, na.rm = TRUE),
        sd_value = sd(value, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        # Change burst to only numbers
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2", "Burst_3", "Burst_4"),
            labels = c("1", "2", "3", "4")
        )
    )

# Identifying the best combo in each model type
best_models_highlight <- beh_best3_metrics %>%
    group_by(model, behaviour, metric) %>%
    filter(
        value == max(value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(
        model, behaviour, metric
    ) %>%
    select(model, behaviour, metric, burst, correction_type, value)
# RF Resting has 2 same high recall values (Burst_3, Unc and Burst_4, Unc)
# However the Burst 3 has higher Precision so we will keep that one
best_models_highlight <- best_models_highlight %>%
    filter(
        !(
            model == "RF" & behaviour == "Resting" & metric == "recall" &
                burst == "Burst_4" & correction_type == "Unc")
    ) %>%
    group_by(model, behaviour) %>%
    distinct(burst, correction_type)

beh_best3_metrics_highlight <- beh_best3_metrics %>%
    left_join(
        best_models_highlight %>%
            mutate(highlight = TRUE),
        by = c("model", "behaviour", "burst", "correction_type")
    ) %>%
    filter(highlight == !is.na(highlight))

# Get best model per behaviour
top_highlight_df <- beh_best3_metrics %>%
    # Reshape data to have precision and recall in the same row for tie-breaking
    pivot_wider(
        names_from = metric,
        values_from = c(value, sd_value)
    ) %>%
    # Use bind_rows to combine two separate ranking operations on the wide data
    {
        bind_rows(
            # Get top 1 for precision (tie-break with recall)
            (.) %>%
                group_by(behaviour) %>%
                arrange(desc(value_precision), desc(value_recall)) %>%
                slice_head(n = 1) %>%
                mutate(metric = "precision"),

            # Get top 1 for recall (tie-break with precision)
            (.) %>%
                group_by(behaviour) %>%
                arrange(desc(value_recall), desc(value_precision)) %>%
                slice_head(n = 1) %>%
                mutate(metric = "recall")
        )
    } %>%
    # Select the final columns needed for the plot
    select(behaviour, metric, model, burst, correction_type)

# Output CSV
write.csv(
    top_highlight_df,
    "./data/temp/plotting/model_behaviour_comparison_best_pr.csv",
    row.names = FALSE
)

# model present most times
top_highlight_df %>%
    group_by(model, burst, correction_type) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
# HydraRocket Burst 2 RD and HR Burst 2 Unc are present 3 and 2 times.
# Every other model present once

## Plot ----
beh_best3_metrics_plot <- ggplot(
    beh_best3_metrics,
    aes(
        x = metric, y = burst,
    )
) +
    geom_tile(color = "white", linewidth = 0.25) +
    geom_tile(
        data = beh_best3_metrics,
        aes(
            x = metric, y = burst, fill = value
        ),
        # color = "black",
        linewidth = 0.25
    ) +
    geom_tile(
        data = top_highlight_df,
        aes(x = metric, y = burst, color = metric), # Map color to metric
        fill = "transparent",
        linewidth = 1.5,
        inherit.aes = FALSE
    ) +
    geom_text(
        data = beh_best3_metrics_highlight,
        aes(
            x = metric, y = burst, label = round(value, 2)
        ),
        color = "black",
        size = 4,
        inherit.aes = FALSE
    ) +
    scale_x_discrete(
        guide = guide_axis_nested(delim = "!"),
        expand = expansion(add = c(0, 0.5)),
        labels = c(
            "precision" = "P",
            "recall" = "R"
        ),
    ) +
    scale_y_discrete(
        guide = guide_axis_nested(delim = "!"),
        expand = c(0, 0)
    ) +
    scale_color_manual(
        values = c("precision" = "#5C6BC0", "recall" = "#9CCC65"),
        guide = "none" # Hide the legend for this scale
    ) +
    scale_fill_gradient2(
        low = "blue", mid = "white", high = "red",
        midpoint = 0.5, name = "",
        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    ) +
    facet_nested(
        behaviour ~ model * correction_type,
        switch = "both",
        # space = "free_x",
        labeller = labeller(
            behaviour = c(
                "Resting" = "Resting",
                "Eating" = "Eating",
                "Walking" = "Walking",
                "Grooming actor" = "Grooming\nactor",
                "Grooming receiver" = "Grooming\nreceiver",
                "Sleeping" = "Sleeping",
                "Self-scratching" = "Self-\nscratching",
                "Running" = "Running"
            ),
        )
        # labeller = labeller(
        #     metric = c(
        #         "precision" = "Precision",
        #         "recall" = "Recall"
        #     ),
        #     model = c(
        #         "HydraROCKET" = "HR",
        #         "TabPFN" = "Tab",
        #         "RF" = "RF"
        #     ),
        # )
    ) +
    theme_bw() +
    theme(
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = 14, colour = "black"),
        ggh4x.axis.nestline.y = element_line(),
        ggh4x.axis.nesttext.y = element_text(
            angle = 90, size = 14, vjust = 0.5, hjust = 0.5,
            margin = unit(c(0, 0.2, 0, 0.2), "cm")
        )
    )
beh_best3_metrics_plot
ggsave(
    "./plots/publication/exp3_behaviour_best3_metrics.png",
    plot = beh_best3_metrics_plot,
    width = 15, height = 10, dpi = 300
)

# Obtain gtable
library(grid)
g <- ggplotGrob(beh_best3_metrics_plot)
panel_names <- g$layout$name[grepl("panel", g$layout$name)]
# 72 panels corresponding to 8*3*3
spacer_cols <- g$layout$l[
    grepl("panel-1-5", g$layout$name) | grepl("panel-1-8", g$layout$name)
] + 1
# Set the width of these specific spacer columns
g$widths[spacer_cols] <- unit(0.75, "cm")
# Clear the current plot device
grid.newpage()
# Draw the modified gtable
grid.draw(g)
# Save the modified plot
ggsave(
    "./plots/publication/exp3_behaviour_best3_metrics_v2.png",
    plot = g,
    width = 16, height = 10, dpi = 300
)
