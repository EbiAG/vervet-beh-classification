library(tidyverse)
library(arrow)
library(ggrepel)

# This script is to analyse the VeDBA values for the annotated dataset to identify outliers in the data

# Load data ----
# Annotations information
annotations <- read.csv(
    "./data/temp/annotations/attributions_merged_majority.csv"
)
table(annotations$attribution_merged)

# Load in VeDBA values for each burst
vedba <- bind_rows(
    read_parquet(
        "./data/raw/features/Burst_1/annotated_features_burst_1_uncorrected.parquet"
    ) %>% mutate(burst = "burst_1"),
    read_parquet(
        "./data/raw/features/Burst_2/annotated_features_burst_2_uncorrected.parquet"
    ) %>% mutate(burst = "burst_2"),
    read_parquet(
        "./data/raw/features/Burst_3/annotated_features_burst_3_uncorrected.parquet"
    ) %>% mutate(burst = "burst_3"),
    read_parquet(
        "./data/raw/features/Burst_4/annotated_features_burst_4_uncorrected.parquet"
    ) %>% mutate(burst = "burst_4")
) %>%
    select(burst, Ind_ID, new_burst, mean_vedba) %>%
    left_join(annotations, by = c("burst", "Ind_ID", "new_burst")) %>%
    filter(attribution_merged != "Remove")

# Plot vedba distribution ----
ggplot(vedba, aes(x = attribution_merged, y = mean_vedba)) +
    geom_boxplot() +
    facet_wrap(burst~.) +
    theme_minimal()

# calculate amount of data above and below thresholds
calculate_proportions <- function(df, threshold) {
    df %>%
        group_by(burst, attribution_merged) %>%
        summarise(
            above_threshold = mean(mean_vedba > threshold),
            below_threshold = mean(mean_vedba <= threshold)
        ) %>%
        pivot_longer(
            cols = c(above_threshold, below_threshold), 
            names_to = "threshold_type", values_to = "proportion"
        ) %>%
        mutate(threshold = threshold)
}

# Example thresholds
thresholds <- seq(0.01, 1, 0.005)

# Calculate proportions for each threshold
proportions_list <- lapply(thresholds, function(threshold) {
    calculate_proportions(vedba, threshold)
})

# Combine the results into a single dataframe
proportions_df <- bind_rows(proportions_list) %>%
    mutate(
        attribution_merged = factor(
            attribution_merged, 
            levels = c(
                "Running", "Walking", "Self-scratching",
                "Eating", "Grooming actor", "Grooming receiver",
                "Resting", "Sleeping"
        )),
    )

# Calculate mean VeDBA for each behavior type
mean_vedba_by_behavior <- vedba %>%
    group_by(burst, attribution_merged) %>%
    summarise(mean_vedba = mean(mean_vedba, na.rm = TRUE)) %>%
    mutate(
        attribution_merged = factor(
            attribution_merged,
            levels = c(
                "Running", "Walking", "Self-scratching",
                "Eating", "Grooming actor", "Grooming receiver",
                "Resting", "Sleeping"
            )
        ),
    )

# Plot the proportion of data below the threshold
cols <- c("#F44336", "#AB47BC", "#29B6F6", "#9CCC65")
vedba_threshold_plot <- ggplot(
    proportions_df %>% filter(threshold_type == "below_threshold"),
    aes(x = threshold, y = proportion, color = burst)
) +
    geom_line(alpha = 0.75) +
    geom_point(size = 2, alpha = 0.75) +
    facet_wrap(attribution_merged ~ ., ncol = 4) +
    scale_x_continuous(
        trans = "log10",
        breaks = c(0.01, 0.05, 0.1, 0.5, 1)
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(
        values = cols, labels = c(
            "Burst 1", "Burst 2", "Burst 3", "Burst 4"
    )) +
    theme_bw() +
    labs(
        x = "VeDBA Threshold",
        y = "Proportion of Bursts",
        color = "Attribution"
    ) +
    # Add vertical lines for mean VeDBA values
    geom_vline(
        data = mean_vedba_by_behavior,
        aes(xintercept = mean_vedba, color = burst),
        linetype = "dashed",
        show.legend = FALSE
    ) +
    # Add vertical black bar at 0.1 threshold
    geom_vline(
        xintercept = 0.1,
        linetype = "solid",
        color = "black"
    ) +
    # Add labels for the vertical lines
    geom_label_repel(
        data = mean_vedba_by_behavior,
        aes(
            x = mean_vedba, y = 0.2,
            label = round(mean_vedba, 3), color = burst
        ),
        size = 3,
        show.legend = FALSE
    ) +
    theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 14, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 14, colour = "black"),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black")
    )
vedba_threshold_plot
# ggsave(
#     "./plots/vedba_threshold_plot_attribution_merged.png",
#     vedba_threshold_plot,
#     width = 25,
#     height = 15,
#     units = "cm",
#     dpi = 300
# )
