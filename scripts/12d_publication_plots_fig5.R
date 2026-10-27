# We will make plots for publication figure 5 which will contain results for experiment 4
# A: monthly daytime proportion of behaviours from focal sampling, RF, TabPFN
#    and HR B2-Unc, and the final classifier (HR B2-Daily)
# B: circadian variation of each predicted behaviour from the same four models

library(tidyverse)
library(cowplot)
library(arrow)

beh_levels <- c(
    "Eating", "Resting", "Walking", "Grooming actor",
    "Grooming receiver", "Sleeping", "Self-scratching",
    "Running"
)

# Models, in plotting order after focal sampling (as in Fig. 4), and their
# prediction files (10b); the final classifier last
model_files <- c(
    "RF" = "final_rf_b2_unc",
    "TabPFN" = "final_tabpfn_b2_unc",
    "HydraMultiROCKET" = "final_hr_b2_unc",
    "HydraMultiROCKET-Daily" = "final_hr_b2_rotdaily"
)
source_levels <- c("Focal sampling", names(model_files))
source_cols <- c(
    "Focal sampling" = "#212121",
    "RF" = "#FF5722",
    "TabPFN" = "#AB47BC",
    "HydraMultiROCKET" = "#26A69A",
    "HydraMultiROCKET-Daily" = "#00695C"
)
# Boxplot fills in A: focal sampling as an open (white) box with a dark outline,
# so its median stays visible
box_fills <- replace(source_cols, "Focal sampling", "white")

# Load data ----
# Monthly daytime proportions joined with focal sampling, written by 11a
focal_joined <- map(model_files, function(file) {
    read.csv(paste0("./data/temp/plotting/focal_comparisons_", file, "_beh_prop.csv")) %>%
        mutate(Final_behaviour = factor(Final_behaviour, levels = beh_levels))
})

# Predicted label for every burst, written by 10b. The "nan" filter guards
# against bursts without a prediction; there are none in the current outputs
pred_labels <- imap_dfr(model_files, function(file, model) {
    read_parquet(
        paste0("./data/output/inference_results/", file, "_focal_sampled_random_predictions.parquet"),
        col_select = c("Ind_ID", "burst_start_time", "predicted_behaviour")
    ) %>%
        filter(predicted_behaviour != "nan") %>%
        mutate(Source = model)
})

# Comparison of behaviour proportions ----
# Focal proportions are the same in all files; take them from one
focal_pred_all <- bind_rows(
    focal_joined[["HydraMultiROCKET-Daily"]] %>%
        transmute(Ind_ID, year_month, Final_behaviour,
            Source = "Focal sampling", Proportion = prop_focal
        ),
    imap_dfr(focal_joined, function(joined, model) {
        joined %>%
            transmute(Ind_ID, year_month, Final_behaviour,
                Source = model, Proportion = prop_pred
            )
    })
) %>%
    mutate(Source = factor(Source, levels = source_levels))
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
        outlier.shape = NA,
        colour = "#212121"
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
    scale_fill_manual(values = box_fills) +
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


# Circadian changes in behaviour ----
## Data ----
# Proportion of each predicted behaviour per source, individual and hour
circ_sources <- names(model_files)
circ_data <- pred_labels %>%
    mutate(hour = hour(burst_start_time)) %>%
    count(Source, Ind_ID, hour, predicted_behaviour, name = "count") %>%
    group_by(Source, Ind_ID, hour) %>%
    mutate(prop_beh = count / sum(count)) %>%
    ungroup() %>%
    complete(
        Source, Ind_ID, hour, predicted_behaviour,
        fill = list(prop_beh = 0)
    ) %>%
    mutate(
        Source = factor(Source, levels = circ_sources),
        predicted_behaviour = factor(predicted_behaviour, levels = beh_levels)
    )

# Mean and SD across individuals; the ribbon is cut at 0
circ_data_summary <- circ_data %>%
    group_by(Source, hour, predicted_behaviour) %>%
    summarise(
        mean_prop = mean(prop_beh),
        sd_prop = sd(prop_beh),
        .groups = "drop"
    ) %>%
    mutate(lower = pmax(mean_prop - sd_prop, 0), upper = mean_prop + sd_prop)

# Circadian plot, one panel per behaviour (own y scale, so rare behaviours
# stay visible), coloured by source: mean across individuals with a +/- SD
# ribbon
plot_circ_behaviour <- function(line_cols) {
    ggplot() +
        geom_ribbon(
            data = circ_data_summary,
            aes(x = hour, ymin = lower, ymax = upper, fill = Source),
            alpha = 0.2
        ) +
        geom_line(
            data = circ_data_summary,
            aes(x = hour, y = mean_prop, colour = Source),
            linewidth = 1.2
        ) +
        scale_y_continuous(
            name = "Proportion of behaviour",
            limits = c(0, NA)
        ) +
        scale_x_continuous(
            name = "Hour of the day",
            breaks = c(0, 6, 12, 18, 24),
            limits = c(0, 24)
        ) +
        scale_colour_manual(values = line_cols) +
        scale_fill_manual(values = line_cols) +
        facet_wrap(~predicted_behaviour, ncol = 4, scales = "free_y") +
        theme_bw() +
        theme(strip.background = element_blank())
}

circ_behaviour_plot <- plot_circ_behaviour(source_cols[circ_sources]) +
    theme(
        legend.position = "none",
        axis.title = element_text(size = 18, colour = "black"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text = element_text(size = 16, colour = "black"),
        panel.grid = element_blank()
    )
circ_behaviour_plot

# Combine plots ----
# A above B; the legend in A covers the model colours in B
p5_combined <- plot_grid(
    monthly_day_prop_plot,
    circ_behaviour_plot,
    labels = c("A", "B"),
    ncol = 1,
    rel_heights = c(0.8, 1),
    align = "v",
    axis = "lr"
)
p5_combined
ggsave(
    "./plots/fig5_exp4_focal_sampling_beh_prop.png",
    p5_combined,
    width = 16, height = 14, dpi = 300
)
ggsave(
    "./plots/fig5_exp4_focal_sampling_beh_prop.pdf",
    plot = p5_combined,
    device = cairo_pdf,
    width = 16, height = 14, units = "in", dpi = 1000,
    fallback_resolution = 1000
)
