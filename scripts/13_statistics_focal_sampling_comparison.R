# We will compare correlations between the proportion of behaviours obtained from focal sampling and the predictions

library(tidyverse)
library(arrow)
library(fuzzyjoin)

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

# Predictions from HR_B1_UNC
pred_hr_b1_unc <- read_parquet(
    "./data/output/inference_results/hr_b1_unc_focal_sampled_random_predictions.parquet"
)
# Predictions from TabPFN_B4_BASAL
pred_tab_b4_bas <- read_parquet(
    "./data/output/inference_results/tabpfn_b4_basal_focal_sampled_random_predictions.parquet"
)

# Summary data of predictions
pred_hr_b1_unc %>%
    group_by(Ind_ID) %>%
    summarise(
        n_preds = n(),
        .groups = "drop"
    ) %>%
    summarise(
        total_preds = sum(n_preds),
        mean_preds = mean(n_preds),
        sd_preds = sd(n_preds),
        min_preds = min(n_preds),
        max_preds = max(n_preds)
    )

pred_tab_b4_bas %>%
    group_by(Ind_ID) %>%
    summarise(
        n_preds = n(),
        .groups = "drop"
    ) %>%
    summarise(
        total_preds = sum(n_preds),
        mean_preds = mean(n_preds),
        sd_preds = sd(n_preds),
        min_preds = min(n_preds),
        max_preds = max(n_preds)
    )

# Monthly proportion of behaviour ----
# Function to extract day time proportion from prediction data
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

pred_hr_b1_unc_day <- format_pred_day_data(
    pred_hr_b1_unc,
    start_time = "06:00:00",
    end_time = "18:00:00"
)
pred_tab_b4_bas_day <- format_pred_day_data(
    pred_tab_b4_bas,
    start_time = "06:00:00",
    end_time = "18:00:00"
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
    pred_hr_b1_unc_day %>%
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

focal_pred_hr_b1_unc <- join_focal_pred(
    focal_data = focal_data_sel,
    pred_data = pred_hr_b1_unc_day
)
focal_pred_tab_b4_bas <- join_focal_pred(
    focal_data = focal_data_sel,
    pred_data = pred_tab_b4_bas_day
)

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

behavior_cor_hr_b1_unc <- calc_behavior_correlations(
    focal_pred_hr_b1_unc
)
behavior_cor_tab_b4_bas <- calc_behavior_correlations(
    focal_pred_tab_b4_bas
)

# Compare distributions ----
dist_hr_b1_unc <- focal_pred_hr_b1_unc %>%
    pivot_longer(
        cols = c(prop_focal, prop_pred),
        names_to = "source",
        values_to = "proportion"
    ) %>%
    mutate(
        prop_mod = (proportion * (n() - 1) + 0.5) / n()
    )
dist_tab_b4_bas <- focal_pred_tab_b4_bas %>%
    pivot_longer(
        cols = c(prop_focal, prop_pred),
        names_to = "source",
        values_to = "proportion"
    ) %>%
    mutate(
        prop_mod = (proportion * (n() - 1) + 0.5) / n()
    )

# Combine into 1 dataset
dist_focal_pred <- dist_hr_b1_unc %>%
    mutate(source = ifelse(
        source == "prop_focal",
        "focal",
        "hrb1unc"
    )) %>%
    bind_rows(
        dist_tab_b4_bas %>%
            filter(source == "prop_pred") %>%
            mutate(source = "tabb4bas")
    )
table(dist_focal_pred$source)

## HR B2 RD ----
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
# Significant interaction between source and behaviour

check_model(dist_prop_mod1)
sim_out <- simulateResiduals(dist_prop_mod1, n = 1000)
plot(sim_out)

estimate_means(dist_prop_mod1, by = c("source", "Final_behaviour"))
estimate_contrasts(
    dist_prop_mod1,
    contrast = "source",
    by = "Final_behaviour",
    adjust = "holm"
)
emmip(
    dist_prop_mod1,
    Final_behaviour ~ source,
    CIs = TRUE
)

# Save results ----
# Interaction comparisons
get_anova_results <- function(mod1, mod2) {
    anova_result <- anova(mod1, mod2)
    list(
        chisq = anova_result$Chisq[2],
        p_value = anova_result$`Pr(>Chisq)`[2]
    )
}

# Calculate anova results
interaction_comparisons <- tribble(
    ~model, ~mod1, ~mod2,
    "Focal_HRB1Unc_TabB4Bas", dist_prop_mod1, dist_prop_mod2
) %>%
    mutate(
        anova_result = pmap(list(mod1, mod2), get_anova_results),
        chisq = map_dbl(anova_result, "chisq"),
        p_value = map_dbl(anova_result, "p_value"),
        model1 = "with_interaction",
        model2 = "without_interaction"
    ) %>%
    select(model, model1, model2, chisq, p_value)

write.csv(
    interaction_comparisons,
    "./data/output/statistical_analysis/exp4_focal_pred_interaction_anova.csv",
    row.names = FALSE
)

# main effects for interaction model
get_main_effects <- function(model, anova_type) {
    anova_result <- car::Anova(model, type = anova_type)
    anova_result %>%
        as.data.frame() %>%
        rownames_to_column("term") %>%
        filter(term != "(Intercept)")
}

main_effects <- tribble(
    ~model_type, ~model, ~anova_type,
    "Focal_HRB1Unc_TabB4Bas", dist_prop_mod1, "III",
    # "HR_B3_RD", hr_b3_rd_mod1, "III"
) %>%
    mutate(
        anova_result = pmap(
            list(model, anova_type),
            get_main_effects
        )
    ) %>%
    unnest(anova_result) %>%
    select(model_type, anova_type, term, Chisq, `Pr(>Chisq)`)

write.csv(
    main_effects,
    "./data/output/statistical_analysis/exp4_focal_pred_main_effects_anova.csv",
    row.names = FALSE
)

# Marginal means
focal_pred_mm <- bind_rows(
    estimate_means(
        dist_prop_mod1,
        by = c("source", "Final_behaviour")
    )
) %>%
    select(Final_behaviour, source, everything()) %>%
    arrange(Final_behaviour, source)

write.csv(
    focal_pred_mm,
    "./data/output/statistical_analysis/exp4_focal_pred_comparison_mm.csv",
    row.names = FALSE
)

# Contrasts
focal_pred_contrasts <- bind_rows(
    estimate_contrasts(
        dist_prop_mod1,
        contrast = "source",
        by = "Final_behaviour",
    )
) %>%
    select(Final_behaviour, everything()) %>%
    arrange(Final_behaviour)

write.csv(
    focal_pred_contrasts,
    "./data/output/statistical_analysis/exp4_focal_pred_comparison_contrasts.csv",
    row.names = FALSE
)
