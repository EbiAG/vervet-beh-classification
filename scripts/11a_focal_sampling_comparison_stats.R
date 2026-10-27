# We will compare the monthly daytime proportion of behaviours from focal sampling
# with the predictions of the final classifier (HR B2-Daily) and of the B2 models
# on uncorrected data (HR, RF and TabPFN B2-Unc). Predictions come from
# 10b_final_models_inference and 10b_final_models_inference_tabpfn.

library(tidyverse)
library(arrow)
library(fuzzyjoin)
library(glmmTMB)
library(DHARMa)
library(modelbased)

# Load data ----
# Focal sampling
focal_data <- read.csv(
    "./data/raw/focal_sampling/prop_behaviour_monthly_focal.csv"
) %>%
    mutate(Final_behaviour = ifelse(
        Final_behaviour == "Forage",
        "Eating",
        Final_behaviour
    ))

focal_data %>%
    group_by(idindividual1, year_month) %>%
    summarise(
        n_scans = unique(n_scans),
        .groups = "drop"
    ) %>%
    summarise(
        tot_scans = sum(n_scans),
    )

# Predictions of the four models; the final classifier first
pred_files <- c(
    hrb2daily = "final_hr_b2_rotdaily",
    hrb2unc = "final_hr_b2_unc",
    rfb2unc = "final_rf_b2_unc",
    tabpfnb2unc = "final_tabpfn_b2_unc"
)
preds <- map(pred_files, function(name) {
    read_parquet(
        paste0(
            "./data/output/inference_results/", name,
            "_focal_sampled_random_predictions.parquet"
        ),
        col_select = c("Ind_ID", "burst_start_time", "predicted_behaviour")
    )
})

# Summary data of predictions (HR B2-Daily has 5 fewer bursts than the others)
imap_dfr(preds, function(pred, model) {
    pred %>%
        count(Ind_ID, name = "n_preds") %>%
        summarise(
            total_preds = sum(n_preds),
            mean_preds = mean(n_preds),
            sd_preds = sd(n_preds),
            min_preds = min(n_preds),
            max_preds = max(n_preds)
        ) %>%
        mutate(model = model, .before = 1)
})

# Monthly proportion of behaviour ----
# Function to extract day time proportion from prediction data
format_pred_day_data <- function(data, start_time, end_time) {
    data %>%
        mutate(
            date = as_date(burst_start_time),
            year_month = floor_date(date, "month")
        ) %>%
        # Select data between start_time and end_time
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

pred_day <- map(
    preds,
    format_pred_day_data,
    start_time = "06:00:00",
    end_time = "18:00:00"
)

# Identify months in prediction data
months_in_pred <- pred_day$hrb2daily %>%
    distinct(year_month) %>%
    pull(year_month)
months_in_pred

# Shortlist focal data for these months
focal_data_sel <- focal_data %>%
    filter(year_month %in% months_in_pred)

# Join the data
# Get matched IDs
IDmatch <- fuzzyjoin::regex_join(
    pred_day$hrb2daily %>%
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

# Function to join predictions and focal sampling
join_focal_pred <- function(focal_data, pred_data) {
    focal_data %>%
        select(
            Ind_ID, year_month,
            Final_behaviour, prop_duration
        ) %>%
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
        # Remove individual-months for which all prop_focal is 0 (no data)
        group_by(Ind_ID, year_month) %>%
        filter(any(prop_focal > 0)) %>%
        ungroup()
}

focal_pred <- map(pred_day, function(pred) {
    join_focal_pred(focal_data = focal_data_sel, pred_data = pred)
})

# Save the joined data for plotting in 12d
iwalk(focal_pred, function(joined, model) {
    write.csv(
        joined,
        paste0(
            "./data/temp/plotting/focal_comparisons_", pred_files[[model]],
            "_beh_prop.csv"
        ),
        row.names = FALSE
    )
})

# Calculate correlations ----
calc_behavior_correlations <- function(data) {
    data %>%
        group_by(Final_behaviour) %>%
        summarise(
            cor_test = list(
                cor.test(
                    prop_focal,
                    prop_pred,
                    method = "pearson"
                )
            ),
            .groups = "drop"
        ) %>%
        mutate(
            correlation = map_dbl(cor_test, ~ .x$estimate),
            p_value = map_dbl(cor_test, ~ .x$p.value)
        ) %>%
        select(Final_behaviour, correlation, p_value)
}

model_labels <- c(
    hrb2daily = "HR_B2_Daily",
    hrb2unc = "HR_B2_Unc",
    rfb2unc = "RF_B2_Unc",
    tabpfnb2unc = "TabPFN_B2_Unc"
)
behavior_cor <- imap_dfr(focal_pred, function(joined, model) {
    calc_behavior_correlations(joined) %>%
        mutate(model = model_labels[[model]])
}) %>%
    select(model, everything())
behavior_cor

write.csv(
    behavior_cor,
    "./data/output/statistical_analysis/exp4_focal_pred_correlations.csv",
    row.names = FALSE
)

# Compare distributions ----
# Focal sampling and the four models, one row per individual, month, behaviour
# and source; proportions squeezed into (0, 1) for the beta family. Focal
# proportions are the same in every joined table; take them from one
dist_focal_pred <- bind_rows(
    focal_pred$hrb2daily %>%
        transmute(Ind_ID, year_month, Final_behaviour, source = "focal", proportion = prop_focal),
    imap_dfr(focal_pred, function(joined, model) {
        joined %>%
            transmute(Ind_ID, year_month, Final_behaviour, source = model, proportion = prop_pred)
    })
) %>%
    mutate(
        source = factor(source, levels = c("focal", names(pred_files))),
        prop_mod = (proportion * (n() - 1) + 0.5) / n()
    )
table(dist_focal_pred$source)

dist_prop_mod1 <- glmmTMB(
    prop_mod ~ source * Final_behaviour + (1 | Ind_ID),
    data = dist_focal_pred,
    family = beta_family(link = "logit")
)
dist_prop_mod2 <- glmmTMB(
    prop_mod ~ source + Final_behaviour + (1 | Ind_ID),
    data = dist_focal_pred,
    family = beta_family(link = "logit")
)
anova(dist_prop_mod1, dist_prop_mod2)

sim_out <- simulateResiduals(dist_prop_mod1, n = 1000)
plot(sim_out)

estimate_means(dist_prop_mod1, by = c("source", "Final_behaviour"))
estimate_contrasts(
    dist_prop_mod1,
    contrast = "source",
    by = "Final_behaviour",
    p_adjust = "holm"
)

# Save results ----
# Interaction comparison
lrt <- anova(dist_prop_mod1, dist_prop_mod2)
interaction_comparisons <- tibble(
    model = "Focal_HRB2Daily_HRB2Unc_RFB2Unc_TabPFNB2Unc",
    model1 = "with_interaction",
    model2 = "without_interaction",
    chisq = lrt$Chisq[2],
    p_value = lrt$`Pr(>Chisq)`[2]
)

write.csv(
    interaction_comparisons,
    "./data/output/statistical_analysis/exp4_focal_pred_interaction_anova.csv",
    row.names = FALSE
)

# Main effects for the interaction model (sum-to-zero contrasts, so Type III
# main effects are tested at the average of the other factor)
main_effects <- car::Anova(
    update(
        dist_prop_mod1,
        contrasts = list(source = "contr.sum", Final_behaviour = "contr.sum")
    ),
    type = "III"
) %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    filter(term != "(Intercept)") %>%
    mutate(model_type = "Focal_HRB2Daily_HRB2Unc_RFB2Unc_TabPFNB2Unc", anova_type = "III") %>%
    select(model_type, anova_type, term, Chisq, `Pr(>Chisq)`)

write.csv(
    main_effects,
    "./data/output/statistical_analysis/exp4_focal_pred_main_effects_anova.csv",
    row.names = FALSE
)

# Marginal means
focal_pred_mm <- estimate_means(
    dist_prop_mod1,
    by = c("source", "Final_behaviour")
) %>%
    as.data.frame() %>%
    select(Final_behaviour, source, everything()) %>%
    arrange(Final_behaviour, source)

write.csv(
    focal_pred_mm,
    "./data/output/statistical_analysis/exp4_focal_pred_comparison_mm.csv",
    row.names = FALSE
)

# Contrasts
focal_pred_contrasts <- estimate_contrasts(
    dist_prop_mod1,
    contrast = "source",
    by = "Final_behaviour",
    p_adjust = "holm"
) %>%
    as.data.frame() %>%
    select(Final_behaviour, everything()) %>%
    arrange(Final_behaviour)

write.csv(
    focal_pred_contrasts,
    "./data/output/statistical_analysis/exp4_focal_pred_comparison_contrasts.csv",
    row.names = FALSE
)

# Experiment 4: number of predicted windows per model and individual ----
pred_files <- list.files(
  "./data/output/inference_results",
  pattern = "^final_.*_focal_sampled_random_predictions\\.parquet$",
  full.names = TRUE
)
exp4_prediction_counts <- map_dfr(pred_files, function(f) {
  d <- read_parquet(f, col_select = c("Ind_ID", "burst_start_time", "predicted_behaviour")) %>%
    filter(predicted_behaviour != "nan") %>%
    mutate(hour = lubridate::hour(burst_start_time))
  per_ind <- count(d, Ind_ID)
  tibble(
    model = str_remove(basename(f), "_focal_sampled_random_predictions\\.parquet$"),
    n_windows = nrow(d),
    n_day = sum(d$hour >= 6 & d$hour < 18),
    n_night = sum(d$hour >= 18 | d$hour < 6),
    n_ind = nrow(per_ind),
    mean_per_ind = mean(per_ind$n),
    sd_per_ind = sd(per_ind$n),
    min_per_ind = min(per_ind$n),
    max_per_ind = max(per_ind$n)
  )
})
write.csv(
  exp4_prediction_counts,
  "./data/output/statistical_analysis/exp4_prediction_counts.csv",
  row.names = FALSE
)
