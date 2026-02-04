# We will statistically compare the behaviours in each of the three experiments
# (effect of burst length, rotation correction, model algorithm)

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(performance)
library(modelbased)
library(car)

# Load data ----
## Confusion Matrix ----
beh_conf <- bind_rows(
    read.csv(
        "./data/output/behaviour_comparison/behaviour_features_ml_confusion.csv"
    ) %>% mutate(data = "features", model_type = "ml"),
    read.csv(
        "./data/output/behaviour_comparison/behaviour_features_dl_confusion.csv"
    ) %>% mutate(data = "features", model_type = "dl"),
    read.csv(
        "./data/output/behaviour_comparison/behaviour_features_dl_tabpfn_confusion.csv"
    ) %>% mutate(data = "features", model_type = "dl"),
    read.csv(
        "./data/output/behaviour_comparison/behaviour_acceleration_dl_confusion.csv"
    ) %>% mutate(data = "acceleration", model_type = "dl"),
)

## P and R for each behaviour ----
beh_conf_pr <- beh_conf %>%
    group_by(
        data, model_type, model, burst,
        correction_type, random_seed, actual_label
    ) %>%
    mutate(
        actual_count = sum(count),
        recall = if_else(
            actual_label == predicted_label,
            count / actual_count,
            NA_real_
        )
    ) %>%
    group_by(
        data, model_type, model, burst,
        correction_type, random_seed, predicted_label
    ) %>%
    mutate(
        predicted_count = sum(count),
        precision = if_else(
            actual_label == predicted_label,
            count / predicted_count,
            NA_real_
        )
    ) %>%
    filter(actual_label == predicted_label) %>%
    ungroup() %>%
    dplyr::select(!c(count, predicted_count, predicted_label)) %>%
    group_by(
        data, model_type, model, burst,
        correction_type, random_seed
    ) %>%
    replace_na(list(precision = 0)) %>% # Add 0 as Precison when no predictions
    mutate(
        actual_prop = round(actual_count / sum(actual_count), 2),
        precision = round(precision, 2),
        recall = round(recall, 2)
    ) %>%
    ungroup()

# get actual counts for Burst 1 for each behaviour
beh_prop <- beh_conf_pr %>%
    filter(
        burst == "Burst_1" & correction_type == "uncorrected" &
            model == "RF" & random_seed == 42
    ) %>%
    dplyr::select(actual_label, actual_count) %>%
    distinct() %>%
    arrange(actual_count) %>%
    mutate(
        prop_beh = actual_count / sum(actual_count)
    ) %>%
    mutate(
        beh_rarity = case_when(
            prop_beh < 0.05 ~ "rare",
            prop_beh >= 0.05 & prop_beh < 0.15 ~ "uncommon",
            prop_beh >= 0.15 ~ "common"
        )
    )

# Join with behaviour rarity labels
beh_conf_pr <- beh_conf_pr %>%
    left_join(
        beh_prop %>% dplyr::select(actual_label, beh_rarity),
        by = "actual_label"
    )

# Output CSV
write.csv(
    beh_conf_pr,
    "./data/temp/statistical_analysis/exp123_behaviour_classification_precision_recall.csv",
    row.names = FALSE
)


## Burst comparison dataframe ----
# For running, self-scratching and sleeping we have only 6 or fewer observations in Burst 1. So for the burst comparison, we remove these

burst_comp_df <- beh_conf_pr %>%
    filter(
        correction_type == "uncorrected" &
            model == "RF"
    ) %>%
    filter(
        !(actual_label %in% c("Running", "Self-scratching", "Sleeping"))
    ) %>%
    mutate(
        precision_mod = (precision * (n() - 1) + 0.5) / n(),
        recall_mod = (recall * (n() - 1) + 0.5) / n(),
        burst = factor(
            burst,
            levels = c(
                "Burst_1", "Burst_2", "Burst_3", "Burst_4"
            )
        ),
        beh_rarity = factor(
            beh_rarity,
            levels = c("common", "uncommon")
        )
    )

table(burst_comp_df$beh_rarity, burst_comp_df$burst)

## Correction comparison dataframe ----
correction_comp_df <- beh_conf_pr %>%
    filter(
        burst == "Burst_4" &
            model == "RF"
    ) %>%
    mutate(
        precision_mod = (precision * (n() - 1) + 0.5) / n(),
        recall_mod = (recall * (n() - 1) + 0.5) / n(),
        correction_type = factor(
            correction_type,
            levels = c("uncorrected", "rotdaily", "rotbasal")
        ),
        beh_rarity = factor(
            beh_rarity,
            levels = c("common", "uncommon", "rare")
        )
    )

## Model comparison dataframe ----
model_comp_df <- beh_conf_pr %>%
    filter(
        burst == "Burst_4" &
            correction_type == "uncorrected"
    ) %>%
    mutate(
        precision_mod = (precision * (n() - 1) + 0.5) / n(),
        recall_mod = (recall * (n() - 1) + 0.5) / n(),
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
        beh_rarity = factor(
            beh_rarity,
            levels = c("common", "uncommon", "rare")
        )
    )

# Effect of burst on behaviour ----
## Precision ----
burst_prec_mod1 <- glmmTMB(
    precision_mod ~ beh_rarity * burst + (1 | random_seed),
    data = burst_comp_df,
    family = beta_family()
)
burst_prec_mod2 <- glmmTMB(
    precision_mod ~ beh_rarity + burst + (1 | random_seed),
    data = burst_comp_df,
    family = beta_family()
)

anova(burst_prec_mod1, burst_prec_mod2)
# No interaction effect. So common behaviours and rare behaviours are being classified similarly across bursts

check_model(burst_prec_mod2)
sim_out <- simulateResiduals(burst_prec_mod2, n = 1000)
plot(sim_out)
# Deviation present. maybe not the best model due to the interaction term

estimate_means(burst_prec_mod2, c("burst", "beh_rarity"))
estimate_contrasts(burst_prec_mod2, "beh_rarity", by = "burst")
# Uncommon behaviours have higher precision than common behaviours at all bursts

## Recall ----
burst_recall_mod1 <- glmmTMB(
    recall_mod ~ beh_rarity * burst + (1 | random_seed),
    data = burst_comp_df,
    family = beta_family()
)
burst_recall_mod2 <- glmmTMB(
    recall_mod ~ beh_rarity + burst + (1 | random_seed),
    data = burst_comp_df,
    family = beta_family()
)
anova(burst_recall_mod1, burst_recall_mod2)
# No interaction effect. So common behaviours and rare behaviours are being classified similarly across bursts

check_model(burst_recall_mod2)
sim_out <- simulateResiduals(burst_recall_mod2, n = 1000)
plot(sim_out)

estimate_means(burst_recall_mod2, c("burst", "beh_rarity"))
estimate_contrasts(burst_recall_mod2, "beh_rarity", by = "burst")
# uncommon behaviours have much lower recall across all bursts

# Effect of rotation correction on behaviour ----
## Precision ----
correction_prec_mod1 <- glmmTMB(
    precision_mod ~ beh_rarity * correction_type + (1 | random_seed),
    data = correction_comp_df,
    family = beta_family()
)
correction_prec_mod2 <- glmmTMB(
    precision_mod ~ beh_rarity + correction_type + (1 | random_seed),
    data = correction_comp_df,
    family = beta_family()
)
anova(correction_prec_mod1, correction_prec_mod2)
# No interaction effect. So common behaviours and rare behaviours are being classified similarly across correction types

check_model(correction_prec_mod2)
sim_out <- simulateResiduals(correction_prec_mod2, n = 1000)
plot(sim_out)

estimate_means(correction_prec_mod2, c("correction_type", "beh_rarity"))
estimate_contrasts(correction_prec_mod2, "beh_rarity")
estimate_contrasts(correction_prec_mod2, "correction_type")
emmip(correction_prec_mod2, beh_rarity ~ correction_type)
# rare > uncommon > common behaviours in precision across all correction types
# Some weak effect of correction. Uncorrected has higher precision than rotdaily and rotbasal (significant and not significant respectively)

## Recall ----
correction_recall_mod1 <- glmmTMB(
    recall_mod ~ beh_rarity * correction_type + (1 | random_seed),
    data = correction_comp_df,
    family = beta_family()
)
correction_recall_mod2 <- glmmTMB(
    recall_mod ~ beh_rarity + correction_type + (1 | random_seed),
    data = correction_comp_df,
    family = beta_family()
)
anova(correction_recall_mod1, correction_recall_mod2)
# No interaction effect. So common behaviours and rare behaviours are being classified similarly across correction types

check_model(correction_recall_mod2)
sim_out <- simulateResiduals(correction_recall_mod2, n = 1000)
plot(sim_out)

estimate_means(correction_recall_mod2, c("correction_type", "beh_rarity"))
estimate_contrasts(correction_recall_mod2, "beh_rarity")
estimate_contrasts(correction_recall_mod2, "correction_type")
emmip(correction_recall_mod2, beh_rarity ~ correction_type)
# common > uncommon = rare in recall across all correction types
# Same correction type effect as before. Uncorrected has higher recall than rotdaily and rotbasal (significant and not significant respectively)

# Effect of model algorithm on behaviour ----
## Precision ----
model_prec_mod1 <- glmmTMB(
    precision_mod ~ beh_rarity * model + (1 | random_seed),
    data = model_comp_df,
    family = beta_family()
)
model_prec_mod2 <- glmmTMB(
    precision_mod ~ beh_rarity + model + (1 | random_seed),
    data = model_comp_df,
    family = beta_family()
)
anova(model_prec_mod1, model_prec_mod2)
# Significant difference between models

check_model(model_prec_mod1)
sim_out <- simulateResiduals(model_prec_mod1, n = 1000)
plot(sim_out)

estimate_means(model_prec_mod1, c("model", "beh_rarity"))
estimate_contrasts(model_prec_mod1, "beh_rarity", by = "model")
# Some models show differences between common, uncommon and rare behaviours while others don't
# RF, SVC, GANDALF, LSTM show significant differences
# XGBoost, CatEmbed, TabPFN, TSSeq, HydraRocket do not show significant differences
estimate_contrasts(model_prec_mod1, "model", by = "beh_rarity")
emmip(model_prec_mod1, beh_rarity ~ model)
# Variation in common behaviours across models is less compared to variation in rare behaviours.
# So most models are having siimilar precision across common behaviours but differ more in uncommon and rare behaviours

## Recall ----
model_recall_mod1 <- glmmTMB(
    recall_mod ~ beh_rarity * model + (1 | random_seed),
    data = model_comp_df,
    family = beta_family()
)
model_recall_mod2 <- glmmTMB(
    recall_mod ~ beh_rarity + model + (1 | random_seed),
    data = model_comp_df,
    family = beta_family()
)
anova(model_recall_mod1, model_recall_mod2)
# Significant difference between models

check_model(model_recall_mod1)
sim_out <- simulateResiduals(model_recall_mod1, n = 1000)
plot(sim_out)

estimate_means(model_recall_mod1, c("model", "beh_rarity"))
estimate_contrasts(model_recall_mod1, "beh_rarity", by = "model")
# All models show significant differences between at least one comparison of (common, uncommon and rare behaviours)
estimate_contrasts(model_recall_mod1, "model", by = "beh_rarity")
emmip(model_recall_mod1, beh_rarity ~ model)
# Similar to precision, variation in recall for common behaviours is less compared to uncommon and rare behaviours across models
# The values are shifted much more significnatly than for precision

# Output statistical results ----
# Interaction effect comparisons
get_anova_results <- function(mod1, mod2) {
    anova_result <- anova(mod1, mod2)
    list(
        chisq = anova_result$Chisq[2],
        p_value = anova_result$`Pr(>Chisq)`[2]
    )
}

# Create interaction comparisons
interaction_comparisons <- tribble(
    ~comparison, ~metric, ~mod1, ~mod2,
    "burst", "precision", burst_prec_mod1, burst_prec_mod2,
    "burst", "recall", burst_recall_mod1, burst_recall_mod2,
    "correction", "precision", correction_prec_mod1, correction_prec_mod2,
    "correction", "recall", correction_recall_mod1, correction_recall_mod2,
    "model", "precision", model_prec_mod1, model_prec_mod2,
    "model", "recall", model_recall_mod1, model_recall_mod2,
) %>%
    mutate(
        anova_result = pmap(list(mod1, mod2), get_anova_results),
        chisq = map_dbl(anova_result, "chisq"),
        p_value = map_dbl(anova_result, "p_value"),
        model1 = "with_interaction",
        model2 = "without_interaction"
    ) %>%
    select(comparison, metric, model1, model2, chisq, p_value)

write.csv(
    interaction_comparisons,
    "./data/output/statistical_analysis/exp123_behaviour_metrics_interaction_anova.csv",
    row.names = FALSE
)

# get main effects for best model for each comparison
get_main_effects <- function(model, anova_type) {
    anova_result <- car::Anova(model, type = anova_type)
    anova_result %>%
        as.data.frame() %>%
        rownames_to_column("term") %>%
        filter(term != "(Intercept)")
}

# Create main effects comparisons
main_effects <- tribble(
    ~comparison, ~metric, ~model, ~anova_type,
    "burst", "precision", burst_prec_mod2, "II",
    "burst", "recall", burst_recall_mod2, "II",
    "correction", "precision", correction_prec_mod2, "II",
    "correction", "recall", correction_recall_mod2, "II",
    "model", "precision", model_prec_mod1, "III",
    "model", "recall", model_recall_mod1, "III",
) %>%
    mutate(
        anova_result = pmap(
            list(model, anova_type),
            get_main_effects
        )
    ) %>%
    unnest(anova_result) %>%
    select(comparison, metric, anova_type, term, Chisq, `Pr(>Chisq)`)

write.csv(
    main_effects,
    "./data/output/statistical_analysis/exp123_behaviour_metrics_main_effects_anova.csv",
    row.names = FALSE
)

# Marginal means
beh_metrics_mm <- bind_rows(
    estimate_means(
        burst_prec_mod2,
        c("burst", "beh_rarity"),
    ) %>%
        mutate(
            metric = "precision", model = "RF",
            correction_type = "uncorrected",
            comparison = "burst"
        ),
    estimate_means(
        burst_recall_mod2,
        c("burst", "beh_rarity"),
        response_name = "recall"
    ) %>%
        mutate(
            metric = "recall", model = "RF",
            correction_type = "uncorrected",
            comparison = "burst"
        ),
    estimate_means(
        correction_prec_mod2,
        c("correction_type", "beh_rarity"),
        response_name = "precision"
    ) %>%
        mutate(
            metric = "precision", model = "RF",
            burst = "Burst_4", comparison = "correction"
        ),
    estimate_means(
        correction_recall_mod2,
        c("correction_type", "beh_rarity"),
        response_name = "recall"
    ) %>%
        mutate(
            metric = "recall", model = "RF",
            burst = "Burst_4", comparison = "correction"
        ),
    estimate_means(
        model_prec_mod1,
        c("model", "beh_rarity"),
        response_name = "precision"
    ) %>%
        mutate(
            metric = "precision",
            correction_type = "uncorrected",
            burst = "Burst_4", comparison = "model"
        ),
    estimate_means(
        model_recall_mod1,
        c("model", "beh_rarity"),
        response_name = "recall"
    ) %>%
        mutate(
            metric = "recall",
            correction_type = "uncorrected",
            burst = "Burst_4", comparison = "model"
        )
) %>%
    select(
        comparison, model, burst, correction_type,
        beh_rarity, metric, everything()
    )

# Contrasts across behaviour rarity
beh_metrics_contrasts_model <- bind_rows(
    estimate_contrasts(
        burst_prec_mod2, contrast = "beh_rarity"
    ) %>%
        mutate(
            metric = "precision", model = "RF",
            correction_type = "uncorrected",
            comparison = "burst"
        ),
    estimate_contrasts(
        burst_recall_mod2, contrast = "beh_rarity"
    ) %>%
        mutate(
            metric = "recall", model = "RF",
            correction_type = "uncorrected",
            comparison = "burst"
        ),
    estimate_contrasts(
        correction_prec_mod2, contrast = "beh_rarity"
    ) %>%
        mutate(
            metric = "precision", model = "RF",
            burst = "Burst_4", comparison = "correction"
        ),
    estimate_contrasts(
        correction_recall_mod2, contrast = "beh_rarity"
    ) %>%
        mutate(
            metric = "recall", model = "RF",
            burst = "Burst_4", comparison = "correction"
        ),
    estimate_contrasts(
        model_prec_mod1, contrast = "beh_rarity"
    ) %>%
        mutate(
            metric = "precision",
            correction_type = "uncorrected",
            burst = "Burst_4", comparison = "model"
        ),
    estimate_contrasts(
        model_recall_mod1, contrast = "beh_rarity"
    ) %>%
        mutate(
            metric = "recall",
            correction_type = "uncorrected",
            burst = "Burst_4", comparison = "model"
        ),
) %>%
    mutate(
        contrast_level = "across_behavioural_rarity"
    ) %>%
    select(
        contrast_level, comparison, model, burst,
        correction_type, metric, everything()
    )

# Contrasts within behaviour rarity
beh_metrics_contrasts_beh <- bind_rows(
    estimate_contrasts(
        burst_prec_mod2, contrast = "burst",
    ) %>%
        mutate(
            metric = "precision", model = "RF",
            correction_type = "uncorrected",
            comparison = "burst"
        ),
    estimate_contrasts(
        burst_recall_mod2, contrast = "burst"
    ) %>%
        mutate(
            metric = "recall", model = "RF",
            correction_type = "uncorrected",
            comparison = "burst"
        ),
    estimate_contrasts(
        correction_prec_mod2, contrast = "correction_type"
    ) %>%
        mutate(
            metric = "precision", model = "RF",
            burst = "Burst_4",
            comparison = "correction"
        ),
    estimate_contrasts(
        correction_recall_mod2, contrast = "correction_type"
    ) %>%
        mutate(
            metric = "recall", model = "RF",
            burst = "Burst_4",
            comparison = "correction"
        ),
    estimate_contrasts(
        model_prec_mod1, contrast = "model",
        by = "beh_rarity"
    ) %>%
        mutate(
            metric = "precision",
            correction_type = "uncorrected",
            burst = "Burst_4", comparison = "model"
        ),
    estimate_contrasts(
        model_recall_mod1, contrast = "model",
        by = "beh_rarity"
    ) %>%
        mutate(
            metric = "recall",
            correction_type = "uncorrected",
            burst = "Burst_4", comparison = "model"
        ),
) %>%
    mutate(
        contrast_level = "within_behaviour_rarity"
    ) %>%
    select(
        contrast_level, comparison, burst,
        correction_type, beh_rarity, metric, everything()
    )

# Save outputs
write.csv(
    beh_metrics_mm,
    "./data/output/statistical_analysis/exp123_behaviour_metrics_comparison_mm.csv",
    row.names = FALSE
)

write.csv(
    bind_rows(beh_metrics_contrasts_model, beh_metrics_contrasts_beh) %>%
        select(
            contrast_level, comparison, model, burst,
            correction_type, beh_rarity, metric, everything()
        ),
    "./data/output/statistical_analysis/exp123_behaviour_metrics_comparison_contrasts.csv",
    row.names = FALSE
)
