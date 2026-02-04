# We will make plots for publication figure 5 which will contain results for experiment 4

library(tidyverse)
library(cowplot)
library(arrow)
library(fuzzyjoin)
library(ggh4x)
library(gghalves)
library(ggpp)

# Load data ----
## Sampling and predictions ----
focal_data <- read.csv(
    "./data/raw/focal_sampling/prop_behaviour_monthly_focal.csv"
) %>%
    mutate(Final_behaviour = ifelse(
        Final_behaviour == "Forage",
        "Eating",
        Final_behaviour
    ))

# Predictions for TabPFN_B4_Basal
pred_tab_b4_basal <- read_parquet(
    "./data/output/inference_results/tabpfn_b4_basal_focal_sampled_random_predictions.parquet"
)
table(pred_tab_b4_basal$predicted_behaviour)

# Predictions for HR_B1_Uncorrected
pred_hr_b1_unc <- read_parquet(
    "./data/output/inference_results/hr_b1_unc_focal_sampled_random_predictions.parquet"
)

## Monthly proportion of behaviour ----
# Function to extract day time proportion from prediciton data
format_pred_day_data <- function(data, start_time, end_time) {
    data %>%
        mutate(
            date = as_date(burst_start_time),
            year_month = floor_date(date, "month")
        ) %>%
        # Select data from 08:00 to 20:00
        filter(
            format(burst_start_time, "%H:%M:%S") > start_time,
            format(burst_start_time, "%H:%M:%S") < end_time
        ) %>%
        group_by(Ind_ID, year_month, predicted_behaviour) %>%
        summarise(
            count_pred = n(),
            .groups = "drop"
        ) %>%
        group_by(Ind_ID, year_month) %>%
        mutate(
            prop_pred = count_pred / sum(count_pred)
        ) %>%
        ungroup()
}

pred_tab_b4_basal_day <- format_pred_day_data(
    pred_tab_b4_basal, "06:00:00", "18:00:00"
)
pred_hr_b1_unc_day <- format_pred_day_data(
    pred_hr_b1_unc, "06:00:00", "18:00:00"
)

# Identify months in prediction data
months_in_pred <- pred_hr_b1_unc_day %>%
    select(year_month) %>%
    distinct() %>%
    pull(year_month)
months_in_pred
# 3 months

# Shortlist focal data for these months
focal_data_sel <- focal_data %>%
    filter(year_month %in% months_in_pred)

# Join the data
# Get matched IDs
IDmatch <- fuzzyjoin::regex_join(
    pred_tab_b4_basal_day %>%
        distinct(Ind_ID),
    focal_data_sel %>%
        distinct(idindividual1),
    by = c(Ind_ID = "idindividual1"),
    mode = "left"
) %>%
    filter(!(Ind_ID == "Xian" & idindividual1 == "Xia"))

# Join to focal data
focal_data_sel <- focal_data_sel %>%
    left_join(IDmatch, by = "idindividual1") %>%
    select(-idindividual1)

# Function to join predicitons and focal smapling
join_focal_pred <- function(focal_data, pred_data) {
    focal_data %>%
        select(Ind_ID, year_month, Final_behaviour, prop_duration) %>%
        mutate(year_month = as.Date(year_month)) %>%
        rename(prop_focal = prop_duration) %>%
        full_join(
            pred_data %>%
                select(Ind_ID, year_month, predicted_behaviour, prop_pred),
            by = c(
                "Ind_ID", "year_month",
                "Final_behaviour" = "predicted_behaviour"
            ),
            relationship = "many-to-many"
        ) %>%
        complete(
            Ind_ID, year_month, Final_behaviour,
            fill = list(prop_focal = 0, prop_pred = 0)
        ) %>%
        # Remove others
        filter(!(Final_behaviour %in% c("Other", "nan"))) %>%
        # Remove na if any in the data
        # filter(!is.na(Final_behaviour)) %>%
        mutate(
            Final_behaviour = factor(
                Final_behaviour,
                levels = c(
                    "Eating", "Resting", "Walking", "Grooming actor",
                    "Grooming receiver", "Sleeping", "Self-scratching",
                    "Running"
                )
            )
        ) %>%
        # Remove individuals for which all prop_focal is 0 (no data)
        group_by(Ind_ID, year_month) %>%
        filter(any(prop_focal > 0)) %>%
        ungroup()
}

focal_tabb1bas_joined <- join_focal_pred(focal_data_sel, pred_tab_b4_basal_day)
focal_hrb1unc_joined <- join_focal_pred(focal_data_sel, pred_hr_b1_unc_day)

# Save these CSVs for later reuse
write.csv(
    focal_tabb1bas_joined,
    "./data/temp/plotting/focal_comparisons_tab_b4_basal_beh_prop.csv",
    row.names = FALSE
)

write.csv(
    focal_hrb1unc_joined,
    "./data/temp/plotting/focal_comparisons_hr_b1_unc_beh_prop.csv",
    row.names = FALSE
)

## Reload data ----
pred_tab_b4_basal <- read_parquet(
    "./data/output/inference_results/tabpfn_b4_basal_focal_sampled_random_predictions.parquet"
)
pred_hr_b1_unc <- read_parquet(
    "./data/output/inference_results/hr_b1_unc_focal_sampled_random_predictions.parquet"
)
focal_tabb4bas_joined <- read.csv(
    "./data/temp/plotting/focal_comparisons_tab_b4_basal_beh_prop.csv"
) %>%
    mutate(
        Final_behaviour = factor(
            Final_behaviour,
            levels = c(
                "Eating", "Resting", "Walking", "Grooming actor",
                "Grooming receiver", "Sleeping", "Self-scratching",
                "Running"
            )
        )
    )
focal_hrb1unc_joined <- read.csv(
    "./data/temp/plotting/focal_comparisons_hr_b1_unc_beh_prop.csv"
) %>%
    mutate(
        Final_behaviour = factor(
            Final_behaviour,
            levels = c(
                "Eating", "Resting", "Walking", "Grooming actor",
                "Grooming receiver", "Sleeping", "Self-scratching",
                "Running"
            )
        )
    )

# Comparison of behaviour proportions ----
# Combine the datasets for plotting
focal_pred_all <- focal_hrb1unc_joined %>%
    pivot_longer(
        cols = c(prop_focal, prop_pred),
        names_to = "Source",
        values_to = "Proportion"
    ) %>%
    mutate(
        Source = ifelse(
            Source == "prop_focal",
            "Focal sampling",
            "HydraRocket_B1_Uncorrected"
        )
    ) %>%
    bind_rows(
        focal_tabb4bas_joined %>%
            rename(
                Proportion = prop_pred
            ) %>%
            select(!prop_focal) %>%
            mutate(
                Source = "TabPFN_B4_Basal"
            )
    )
table(focal_pred_all$Source)

monthly_day_prop_plot <- ggplot(
    focal_pred_all,
    aes(
        x = Final_behaviour,
        y = Proportion,
        fill = Source
    )
) +
    geom_boxplot(
        position = position_dodge(width = 0.75),
        width = 0.6,
        outlier.shape = NA
    ) +
    scale_x_discrete(
        labels = c(
            "Grooming actor" = "Grooming\nactor",
            "Grooming receiver" = "Grooming\nreceiver",
            "Self-scratching" = "Self-\nscratching"
        )
    ) +
    scale_y_continuous(
        name = "Proportion of behaviour",
        limits = c(0, 1),
        breaks = c(0, 0.5, 1)
    ) +
    scale_fill_manual(
        values = c(
            "Focal sampling" = "#26C6DA",
            "HydraRocket_B1_Uncorrected" = "#8BC34A",
            "TabPFN_B4_Basal" = "#F57F17"
        )
    ) +
    theme_bw() +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, colour = "black"),
        legend.background = element_blank(),
        axis.title = element_text(size = 18, colour = "black"),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 14, colour = "black"),
        panel.grid = element_blank()
    ) +
    labs(
        x = "Behaviour",
        y = "Proportion of behaviour"
    )
monthly_day_prop_plot

ggsave(
    "./plots/publication/exp4_focal_pred_prop_boxplot.png",
    monthly_day_prop_plot,
    width = 10, height = 8, dpi = 300
)

# Circadian changes in activity ----
## Data ----
inactive_beh <- c("Resting", "Sleeping", "Grooming receiver")
active_beh <- c(
    "Eating", "Grooming actor", "Self-scratching", "Running", "Walking"
)

# Create circadian data
head(pred_tab_b4_basal)
str(pred_tab_b4_basal)

# Get circadian data for Tab B4 Basal
tabb4bas_circ_data <- pred_tab_b4_basal %>%
    mutate(
        date = as_date(burst_start_time),
        hour = hour(burst_start_time),
        act_beh = ifelse(
            predicted_behaviour %in% inactive_beh, "Inactive", "Active"
        ),
        act_beh = factor(
            act_beh,
            levels = c("Active", "Inactive")
        )
    ) %>%
    # Group by individual and hour
    group_by(Ind_ID, hour, act_beh) %>%
    summarise(count = n(), .groups = "drop") %>%
    # Remove nan values
    filter(act_beh != "nan") %>%
    group_by(Ind_ID, hour) %>%
    mutate(prop_beh = count / sum(count)) %>%
    ungroup() %>%
    complete(
        Ind_ID, hour, act_beh,
        fill = list(prop_beh = 0)
    )
# Circadian data for HR B1 Uncorrected
hrb1unc_circ_data <- pred_hr_b1_unc %>%
    mutate(
        date = as_date(burst_start_time),
        hour = hour(burst_start_time),
        act_beh = ifelse(
            predicted_behaviour %in% inactive_beh, "Inactive", "Active"
        ),
        act_beh = factor(
            act_beh,
            levels = c("Active", "Inactive")
        )
    ) %>%
    # Group by individual and hour
    group_by(Ind_ID, hour, act_beh) %>%
    summarise(count = n(), .groups = "drop") %>%
    # Remove nan values
    filter(act_beh != "nan") %>%
    group_by(Ind_ID, hour) %>%
    mutate(prop_beh = count / sum(count)) %>%
    ungroup() %>%
    complete(
        Ind_ID, hour, act_beh,
        fill = list(prop_beh = 0)
    )

# Summary datasets
tabb4bas_circ_data_summary <- tabb4bas_circ_data %>%
    group_by(hour, act_beh) %>%
    summarise(
        mean_prop = mean(prop_beh),
        sd_prop = sd(prop_beh),
        .groups = "drop"
    )
hrb1unc_circ_data_summary <- hrb1unc_circ_data %>%
    group_by(hour, act_beh) %>%
    summarise(
        mean_prop = mean(prop_beh),
        sd_prop = sd(prop_beh),
        .groups = "drop"
    )

# Combine datasets for plotting
circ_data <- tabb4bas_circ_data %>%
    mutate(Model = "TabPFN_B4_Basal") %>%
    bind_rows(
        hrb1unc_circ_data %>%
            mutate(Model = "HydraRocket_B1_Uncorrected")
    )
circ_data_summary <- tabb4bas_circ_data_summary %>%
    mutate(Model = "TabPFN_B4_Basal") %>%
    bind_rows(
        hrb1unc_circ_data_summary %>%
            mutate(Model = "HydraRocket_B1_Uncorrected")
    )

# Make facetted plot for active and inactive behaviour
circ_text_annotation <- data.frame(
    hour = c(0.5, 0.5),
    mean_prop = c(0.1, 0.2),
    act_beh = factor("Inactive", levels = c("Active", "Inactive")),
    label = c("TabPFN_B4_Basal", "HydraRocket_B1_Uncorrected"),
    colour = c("#F57F17", "#8BC34A")
)

circ_activity_plot <- ggplot() +
    geom_line(
        data = circ_data,
        aes(
            x = hour, y = prop_beh, colour = Model,
            group = interaction(Ind_ID, Model)
        ),
        alpha = 0.4, linewidth = 0.5,
        linetype = "dashed"
    ) +
    geom_line(
        data = circ_data_summary,
        aes(x = hour, y = mean_prop, colour = Model),
        linewidth = 1.2
    ) +
    geom_ribbon(
        data = circ_data_summary,
        aes(
            x = hour, ymin = mean_prop - sd_prop,
            ymax = mean_prop + sd_prop, fill = Model
        ),
        alpha = 0.5
    ) +
    geom_text(
        data = circ_text_annotation,
        aes(x = hour, y = mean_prop, label = label),
        size = 5, colour = circ_text_annotation$colour,
        hjust = 0
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
    scale_y_continuous(
        name = "Proportion of behaviour",
        limits = c(0, 1),
        breaks = c(0, 0.5, 1)
    ) +
    scale_x_continuous(
        name = "Hour of the day",
        breaks = c(0, 6, 12, 18, 24),
        limits = c(0, 24)
    ) +
    facet_grid(
        act_beh ~ .,
        labeller = as_labeller(
            c(
                "Active" = "Active behaviours",
                "Inactive" = "Inactive behaviours"
            )
        )
    ) +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.title = element_text(size = 18, colour = "black"),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        strip.background = element_blank(),
        panel.grid = element_blank()
    )
circ_activity_plot
ggsave(
    "./plots/publication/exp4_circ_activity_plot.png",
    circ_activity_plot,
    width = 10, height = 8, dpi = 300
)

# Combine plots ----
p3_combined <- plot_grid(
    monthly_day_prop_plot,
    circ_activity_plot,
    labels = c("A", "B"),
    nrow = 1,
    rel_widths = c(1, 0.75),
    align = "h",
    axis = "tb"
)
p3_combined
ggsave(
    "./plots/publication/fig3_exp4_focal_sampling_beh_prop.png",
    p3_combined,
    width = 16, height = 6, dpi = 300
)
