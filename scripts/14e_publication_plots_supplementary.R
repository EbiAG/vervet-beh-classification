library(tidyverse)
library(arrow)
library(cowplot)
library(ggtext)

beh_order <- c(
    "Resting", "Eating",
    "Walking", "Grooming actor", "Grooming receiver",
    "Sleeping", "Self-scratching", "Running"
)

# S1 - Validation data variation across individuals ----
## Data ----
# Load final annotations
ind_annotations <- read.csv(
    "./data/raw/annotations/annotations_merged_majority.csv"
) %>%
    mutate(burst = str_to_title(burst)) %>%
    filter(attribution_merged != "Remove") %>%
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
        name = "Number of\nsamples",
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
    "./plots/publication/figs1_individual_validation_data_variation.png",
    ind_variation_plot,
    width = 10, height = 8, dpi = 300
)

# S2 - Proportion of pure bursts ----
## Data ----
# Load burst annotations
burst_annotations <- bind_rows(
    read.csv("./data/raw/annotations/annotations_burst1_20250121.csv") %>%
        mutate(burst = "Burst_1"),
    read.csv("./data/raw/annotations/annotations_burst2_20250121.csv") %>%
        mutate(burst = "Burst_2"),
    read.csv("./data/raw/annotations/annotations_burst3_20250121.csv") %>%
        mutate(burst = "Burst_3"),
    read.csv("./data/raw/annotations/annotations_burst4_20250121.csv") %>%
        mutate(burst = "Burst_4")
) %>%
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
    "./data/raw/annotations/annotations_merged_majority.csv"
) %>%
    mutate(burst = str_to_title(burst)) %>%
    filter(attribution_merged != "Remove") %>%
    left_join(
        burst_annotations %>%
            select(
                burst, new_burst, annotation_type
            ),
        by = c("burst", "new_burst")
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
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        panel.grid = element_blank(),
    ) +
    ylab("Proportion of \"pure behavioral\" bursts")
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
    ylab("Proportion of \"pure behavioral\" bursts")
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
    "./plots/publication/figs2_pure_burst_proportions.png",
    pure_burst_combined_plot,
    width = 16, height = 8, dpi = 300
)

# S3 - VeDBA variation across behaviors ----
## Data ----
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
    left_join(
        read.csv(
            "./data/raw/annotations/annotations_merged_majority.csv"
        ),
        by = c("burst", "Ind_ID", "new_burst")
    ) %>%
    filter(attribution_merged != "Remove")

## Plot vedba by behaviour ----
vedba_threshold_plot <- ggplot(
    vedba,
    aes(x = attribution_merged, y = mean_vedba)
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
        name = "Mean VeDBA",
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
        axis.text.y = element_markdown(size = 14),
        panel.grid = element_blank(),
    )
vedba_threshold_plot
ggsave(
    "./plots/publication/figs3_vedba_by_behaviour.png",
    vedba_threshold_plot,
    width = 10, height = 8, dpi = 300
)

# S4 - Variation in orientation ----
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
    left_join(
        read.csv(
            "./data/raw/annotations/annotations_merged_majority.csv"
        ) %>%
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

# Filter individuals with at least 15 samples
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
    "./plots/publication/figs4_orientation_correction_static_acc_resting.png",
    static_acc_plot,
    width = 12, height = 15, dpi = 300
)

# S5 - Effect of correction on behaviours with TabPFN ----
## Data ----
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

# Get meand and SD for TabPFN Burst 4
tabpfn_beh_metrics_summary <- beh_metrics %>%
    filter(
        model == "TabPFN",
        burst == "Burst_4"
    ) %>%
    group_by(
        correction_type, beh_rarity, behaviour, metric
    ) %>%
    summarise(
        mean_value = mean(value),
        sd_value = sd(value),
        .groups = "drop"
    ) %>%
    arrange(
        beh_rarity, behaviour, metric, correction_type
    )

## Plot ----
tabb4_correction_plot <- ggplot(
    tabpfn_beh_metrics_summary,
    aes(
        x = behaviour,
        y = mean_value,
        group = correction_type,
        color = correction_type
    )
) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    # geom_line(aes(group = 1)) +
    geom_errorbar(
        aes(
            ymin = mean_value - sd_value,
            ymax = mean_value + sd_value
        ),
        width = 0, position = position_dodge(width = 0.5)
    ) +
    facet_grid(
        . ~ metric,
        labeller = labeller(
            metric = c(
                precision = "Precision",
                recall = "Recall"
            )
        )
    ) +
    scale_y_continuous(
        limits = c(0, 1), breaks = seq(0, 1, by = 0.5),
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
        ),
    ) +
    scale_color_brewer(
        palette = "Set1",
        name = "Correction",
        labels = c(
            "Unc" = "Uncorrected",
            "Daily" = "Daily",
            "Basal" = "Basal"
        )
    ) +
    theme_bw() +
    theme(
        strip.background = element_blank(),
        strip.text = element_text(size = 14, colour = "black"),
        legend.position = "bottom",
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, colour = "black"),
        panel.grid = element_blank(),
    )

tabb4_correction_plot
ggsave(
    "./plots/publication/figs5_tabb4_orientation_correction_behaviour_pr.png",
    tabb4_correction_plot,
    width = 16, height = 8, dpi = 300
)

# S6 - Circadian changes in behaviour ----
## Data ----
pred_tab_b4_basal <- read_parquet(
    "./data/output/inference_results/tabpfn_b4_basal_focal_sampled_random_predictions.parquet"
)
pred_hr_b1_unc <- read_parquet(
    "./data/output/inference_results/hr_b1_unc_focal_sampled_random_predictions.parquet"
)

# Function to create dataset for plotting
create_circadian_behaviour_data <- function(data) {
    circ_data <- data %>%
        mutate(
            date = as_date(burst_start_time),
            hour = hour(burst_start_time)
        ) %>%
        # Group by individual and hour
        group_by(Ind_ID, hour, predicted_behaviour) %>%
        summarise(count = n(), .groups = "drop") %>%
        # Remove nan values
        filter(predicted_behaviour != "nan") %>%
        group_by(Ind_ID, hour) %>%
        mutate(prop_beh = count / sum(count)) %>%
        ungroup() %>%
        complete(
            Ind_ID, hour, predicted_behaviour,
            fill = list(prop_beh = 0)
        ) %>%
        mutate(
            predicted_behaviour = factor(
                predicted_behaviour,
                levels = beh_order
            )
        )

    circ_data_summary <- circ_data %>%
        group_by(hour, predicted_behaviour) %>%
        summarise(
            mean_prop = mean(prop_beh),
            sd_prop = sd(prop_beh),
            .groups = "drop"
        ) %>%
        mutate(
            predicted_behaviour = factor(
                predicted_behaviour,
                levels = beh_order
            )
        )
    return(list(data = circ_data, summary = circ_data_summary))
}

# Create datasets
circ_tab_b4_basal <- create_circadian_behaviour_data(pred_tab_b4_basal)
circ_tab_b4_basal_data <- circ_tab_b4_basal$data %>%
    mutate(Model = "TabPFN_B4_Basal")
circ_tab_b4_basal_summary <- circ_tab_b4_basal$summary %>%
    mutate(Model = "TabPFN_B4_Basal")
circ_hr_b1_unc <- create_circadian_behaviour_data(pred_hr_b1_unc)
circ_hr_b1_unc_data <- circ_hr_b1_unc$data %>%
    mutate(Model = "HydraRocket_B1_Uncorrected")
circ_hr_b1_unc_summary <- circ_hr_b1_unc$summary %>%
    mutate(Model = "HydraRocket_B1_Uncorrected")

# Join datasets
circadian_behaviour_data <- bind_rows(
    circ_tab_b4_basal_data,
    circ_hr_b1_unc_data
)
circadian_behaviour_summary <- bind_rows(
    circ_tab_b4_basal_summary,
    circ_hr_b1_unc_summary
)

## Plot ----
circ_beh_plot <- ggplot() +
    geom_line(
        data = circadian_behaviour_data,
        aes(
            x = hour, y = prop_beh, colour = Model,
            group = interaction(Ind_ID, Model)
        ),
        alpha = 0.2, linewidth = 0.5,
        linetype = "dashed"
    ) +
    geom_line(
        data = circadian_behaviour_summary,
        aes(x = hour, y = mean_prop, group = Model, colour = Model),
        linewidth = 1.2
    ) +
    geom_ribbon(
        data = circadian_behaviour_summary,
        aes(
            x = hour, ymin = mean_prop - sd_prop,
            ymax = mean_prop + sd_prop, fill = Model
        ),
        alpha = 0.5
    ) +
    scale_y_continuous(
        name = "Proportion of behaviour",
        limits = c(-0.1, 1),
        breaks = c(0, 0.5, 1)
    ) +
    scale_x_continuous(
        name = "Hour of the day",
        breaks = c(0, 6, 12, 18, 24),
        limits = c(0, 24)
    ) +
    scale_colour_manual(
        name = "Model",
        values = c(
            "HydraRocket_B1_Uncorrected" = "#8BC34A",
            "TabPFN_B4_Basal" = "#F57F17"
        )
    ) +
    scale_fill_manual(
        name = "Model",
        values = c(
            "HydraRocket_B1_Uncorrected" = "#8BC34A",
            "TabPFN_B4_Basal" = "#F57F17"
        )
    ) +
    facet_wrap(
        ~predicted_behaviour,
        ncol = 4,
    ) +
    theme_bw() +
    theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        panel.grid = element_blank()
    )
circ_beh_plot
ggsave(
    "./plots/publication/figs6_model_predictions_circadian_behaviour.png",
    circ_beh_plot,
    width = 16, height = 12, dpi = 300
)

# S7 - Signals of sleeping and grooming receiver from focal sampled ----
## Data ----
# Load focal sampled predictions
start_time <- "06:00:00"
end_time <- "18:00:00"
pred_focal_sampled <- bind_rows(
    read_parquet(
        "./data/output/inference_results/hr_b1_unc_focal_sampled_random_predictions.parquet",
        col_select = c(
            "Ind_ID", "burst_id", "new_burst",
            "predicted_behaviour", "burst_start_time"
        )
    ) %>%
        mutate(model = "HydraRocket_B1_Uncorrected", burst = "Burst_1"),
    read_parquet(
        "./data/output/inference_results/tabpfn_b4_basal_focal_sampled_random_predictions.parquet",
        col_select = c(
            "Ind_ID", "burst_id", "new_burst",
            "predicted_behaviour", "burst_start_time"
        )
    ) %>%
        mutate(model = "TabPFN_B4_Basal", burst = "Burst_4")
) %>%
    filter(
        format(burst_start_time, "%H:%M:%S") > start_time,
        format(burst_start_time, "%H:%M:%S") < end_time
    ) %>%
    filter(predicted_behaviour %in% c("Sleeping", "Grooming receiver")) %>%
    select(burst, model, Ind_ID, burst_id, new_burst, predicted_behaviour)

head(pred_focal_sampled)
table(pred_focal_sampled$predicted_behaviour)
# 84k instances of grooming and 37k instances of sleeping

# Obtain Static X, Y, Z for these bursts
features_focal_sampled <- bind_rows(
    read_parquet(
        "./data/raw/focal_sampling/features/focal_sampled_features_burst_1_uncorrected.parquet",
        col_select = c(
            "Ind_ID", "burst_id", "new_burst",
            "X_static_mean", "Y_static_mean", "Z_static_mean"
        )
    ) %>%
        mutate(burst = "Burst_1"),
    read_parquet(
        "./data/raw/focal_sampling/features/focal_sampled_features_burst_4_rotbasal.parquet",
        col_select = c(
            "Ind_ID", "burst_id", "new_burst",
            "X_static_mean", "Y_static_mean", "Z_static_mean"
        )
    ) %>%
        mutate(burst = "Burst_4")
) %>%
    select(
        burst, everything()
    )

colnames(features_focal_sampled)
head(features_focal_sampled)

pred_focal_sampled <- pred_focal_sampled %>%
    left_join(
        features_focal_sampled,
        by = c("burst", "Ind_ID", "burst_id", "new_burst")
    ) %>%
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
        )
    )

# Get number of samples per individual
focal_ind_counts <- pred_focal_sampled %>%
    group_by(Ind_ID, predicted_behaviour) %>%
    summarise(n_samples = n(), .groups = "drop") %>%
    arrange(desc(n_samples)) %>%
    pivot_wider(
        names_from = predicted_behaviour,
        values_from = n_samples
    )
# Select 8 individuals with the most samples in each behaviour
focal_ind_shortlist <- unique(c(
    head(
        focal_ind_counts %>%
            arrange(desc(Sleeping)) %>%
            pull(Ind_ID),
        8
    ),
    head(
        focal_ind_counts %>%
            arrange(desc(`Grooming receiver`)) %>%
            pull(Ind_ID),
        8
    )
))

# Filter to these individuals
pred_focal_sampled_subset <- pred_focal_sampled %>%
    filter(Ind_ID %in% focal_ind_shortlist)

## Plot ----
pred_focal_sampled_acc_plot <- ggplot(
    pred_focal_sampled_subset,
    aes(
        x = interaction(model, predicted_behaviour),
        y = static_acc_value,
        fill = interaction(model, predicted_behaviour)
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
        # limits = c(-1.1, 1.02),
        # breaks = seq(-1, 1, by = 1)
    ) +
    scale_x_discrete(
        name = "Model",
        labels = c(
            "HydraRocket_B1_Uncorrected.Sleeping" = "HR_B1_Unc\nSleeping",
            "HydraRocket_B1_Uncorrected.Grooming receiver" = "HR_B1_Unc\nGrooming\nreceiver",
            "TabPFN_B4_Basal.Sleeping" = "Tab_B4_Basal\nSleeping",
            "TabPFN_B4_Basal.Grooming receiver" = "Tab_B4_Basal\nGrooming\nreceiver"
        )
    ) +
    scale_fill_manual(
        values = c(
            "HydraRocket_B1_Uncorrected.Sleeping" = "#febb71",
            "HydraRocket_B1_Uncorrected.Grooming receiver" = "#8cb9d8",
            "TabPFN_B4_Basal.Sleeping" = "#f08442",
            "TabPFN_B4_Basal.Grooming receiver" = "#0070b6"
        )
    ) +
    # scale_fill_brewer(
    #     palette = "Set1",
    #     name = "Orientation\ncorrection"
    # ) +
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
pred_focal_sampled_acc_plot
ggsave(
    "./plots/publication/figs7_focal_sampled_model_static_acc.png",
    pred_focal_sampled_acc_plot,
    width = 20, height = 25, dpi = 300
)
