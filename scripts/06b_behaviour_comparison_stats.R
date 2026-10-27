# We will statistically compare per-behaviour precision and recall in each of the three experiments
# (effect of burst length, rotation correction, model algorithm)
# Behaviours are grouped by rarity (common, uncommon, rare) from their share of the Burst 1 windows
# The burst comparison drops the rare behaviours, which have 7 or fewer windows per fold in Burst 1
# The correction and model comparisons use Burst 4, so that we have maximal data
#
# Random effects: every model and correction type is evaluated on the same CV
# folds of the same windows (indices_cv_fold_lookup.csv), so fold enters as
# (1 | fold). Different bursts contain different windows, so the burst
# comparison uses (1 | burst:fold).
# For each comparison and metric, the rarity x term interaction is kept when the
# likelihood-ratio test against the additive model is significant (p < 0.05).

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(modelbased)

# Sum-to-zero contrasts, so Type III main effects are tested at the average of
# the other factor (marginal means and contrasts do not depend on this)
options(contrasts = c("contr.sum", "contr.poly"))

# Load data ----
## Confusion Matrix ----
beh_conf <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_confusion.csv"
    ) %>% mutate(data = "features", model_type = "ml"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_confusion.csv"
    ) %>% mutate(data = "features", model_type = "dl"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_confusion.csv"
    ) %>% mutate(data = "features", model_type = "dl"),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_confusion.csv"
    ) %>% mutate(data = "acceleration", model_type = "dl"),
)

## P and R for each behaviour ----
beh_conf_pr <- beh_conf %>%
    group_by(
        data, model_type, model, burst,
        correction_type, fold, actual_label
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
        correction_type, fold, predicted_label
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
        correction_type, fold
    ) %>%
    replace_na(list(precision = 0)) %>% # Add 0 as Precision when no predictions
    mutate(
        actual_prop = round(actual_count / sum(actual_count), 2),
        precision = round(precision, 2),
        recall = round(recall, 2)
    ) %>%
    ungroup()

# Behaviour rarity from the Burst 1 window counts (all folds; every model sees
# the same windows, so RF uncorrected is used to count them once)
beh_prop <- beh_conf_pr %>%
    filter(
        burst == "Burst_1" & correction_type == "uncorrected" & model == "RF"
    ) %>%
    group_by(actual_label) %>%
    summarise(actual_count = sum(actual_count), .groups = "drop") %>%
    arrange(actual_count) %>%
    mutate(
        prop_beh = actual_count / sum(actual_count),
        beh_rarity = case_when(
            prop_beh < 0.05 ~ "rare",
            prop_beh >= 0.05 & prop_beh < 0.15 ~ "uncommon",
            prop_beh >= 0.15 ~ "common"
        )
    )
beh_prop

# Join with behaviour rarity labels
beh_conf_pr <- beh_conf_pr %>%
    left_join(
        beh_prop %>% dplyr::select(actual_label, beh_rarity),
        by = "actual_label"
    )

# Output CSV (intermediate, read by 12a)
write.csv(
    beh_conf_pr,
    "./data/temp/statistical_analysis/exp123_behaviour_classification_precision_recall.csv",
    row.names = FALSE
)

# Comparison dataframes ----
# Squeeze precision and recall into (0, 1) for the beta family
# (Smithson & Verkuilen 2006)
squeeze <- function(df) {
    df %>%
        mutate(
            precision_mod = (precision * (n() - 1) + 0.5) / n(),
            recall_mod = (recall * (n() - 1) + 0.5) / n(),
            fold = factor(fold)
        )
}

burst_comp_df <- beh_conf_pr %>%
    filter(
        correction_type == "uncorrected" & model == "RF" &
            beh_rarity != "rare"
    ) %>%
    squeeze() %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2", "Burst_3", "Burst_4")
        ),
        beh_rarity = factor(beh_rarity, levels = c("common", "uncommon"))
    )

table(burst_comp_df$beh_rarity, burst_comp_df$burst)

correction_comp_df <- beh_conf_pr %>%
    filter(burst == "Burst_4" & model == "RF") %>%
    squeeze() %>%
    mutate(
        correction_type = factor(
            correction_type,
            levels = c("uncorrected", "rotdaily", "rotbasal")
        ),
        beh_rarity = factor(
            beh_rarity,
            levels = c("common", "uncommon", "rare")
        )
    )

model_comp_df <- beh_conf_pr %>%
    filter(burst == "Burst_4" & correction_type == "uncorrected") %>%
    squeeze() %>%
    mutate(
        model = case_when(
            model == "CategoryEmbeddingModel" ~ "CatEmbed",
            model == "TSSequencerPlus" ~ "TSSeq",
            model == "HydraMultiRocketPlus" ~ "HydraMultiROCKET",
            TRUE ~ model
        ),
        model = factor(
            model,
            levels = c(
                "RF", "SVC", "XGB",
                "CatEmbed", "GANDALF", "TabPFN",
                "LSTM", "TSSeq", "HydraMultiROCKET"
            )
        ),
        beh_rarity = factor(
            beh_rarity,
            levels = c("common", "uncommon", "rare")
        )
    )

# Fit models ----
# Each comparison: the data, the term compared, its random effect, and the
# identifying columns added to the output tables
comparisons <- list(
    burst = list(
        data = burst_comp_df, term = "burst", re = "(1 | burst:fold)",
        labels = list(model = "RF", correction_type = "uncorrected")
    ),
    correction = list(
        data = correction_comp_df, term = "correction_type", re = "(1 | fold)",
        labels = list(model = "RF", burst = "Burst_4")
    ),
    model = list(
        data = model_comp_df, term = "model", re = "(1 | fold)",
        labels = list(burst = "Burst_4", correction_type = "uncorrected")
    )
)

fit_beta <- function(metric, rhs, data) {
    glmmTMB(
        reformulate(rhs, response = paste0(metric, "_mod")),
        data = data,
        family = beta_family()
    )
}

fits <- expand_grid(
    comparison = names(comparisons),
    metric = c("precision", "recall")
) %>%
    mutate(
        cmp = map(comparison, ~ comparisons[[.x]]),
        fit_int = map2(metric, cmp, function(metric, cmp) {
            fit_beta(
                metric, c(paste("beh_rarity *", cmp$term), cmp$re), cmp$data
            )
        }),
        fit_add = map2(metric, cmp, function(metric, cmp) {
            fit_beta(metric, c("beh_rarity", cmp$term, cmp$re), cmp$data)
        }),
        lrt = map2(fit_int, fit_add, anova),
        chisq = map_dbl(lrt, ~ .x$Chisq[2]),
        p_value = map_dbl(lrt, ~ .x$`Pr(>Chisq)`[2]),
        interaction = p_value < 0.05,
        fit = if_else(interaction, fit_int, fit_add)
    )
fits %>% select(comparison, metric, chisq, p_value, interaction)

# Diagnostics ----
# Convergence (positive-definite Hessian), DHARMa uniformity and dispersion
# tests, and the random-effect SD (fold, or burst:fold for the burst comparison),
# for the selected model
diagnostics <- fits %>%
    transmute(
        comparison, metric, interaction,
        pd_hessian = map_lgl(fit, ~ .x$sdr$pdHess),
        ks_p = map_dbl(fit, function(f) {
            testUniformity(simulateResiduals(f, n = 1000), plot = FALSE)$p.value
        }),
        dispersion_p = map_dbl(fit, function(f) {
            testDispersion(simulateResiduals(f, n = 1000), plot = FALSE)$p.value
        }),
        re_sd = map_dbl(fit, ~ attr(VarCorr(.x)$cond[[1]], "stddev")[[1]])
    )
print(diagnostics, n = Inf)

# Inspect any model flagged above, e.g.
sim_out <- simulateResiduals(fits$fit[[6]], n = 1000)
testDispersion(sim_out, plot = TRUE)
plot(sim_out)

# Output statistical results ----
# Adds the comparison's identifying columns (e.g. model = "RF") to a table
add_labels <- function(df, comp, metric) {
    df <- as.data.frame(df) %>% mutate(comparison = comp, metric = metric)
    for (col in names(comparisons[[comp]]$labels)) {
        df[[col]] <- comparisons[[comp]]$labels[[col]]
    }
    df
}

# Interaction effect comparisons
interaction_comparisons <- fits %>%
    transmute(
        comparison, metric,
        model1 = "with_interaction", model2 = "without_interaction",
        chisq, p_value, selected = if_else(interaction, model1, model2)
    )

write.csv(
    interaction_comparisons,
    "./data/output/statistical_analysis/exp123_behaviour_metrics_interaction_anova.csv",
    row.names = FALSE
)

# Main effects for the selected model (Type III with the interaction, else II)
main_effects <- fits %>%
    mutate(
        anova_type = if_else(interaction, "III", "II"),
        anova_result = map2(fit, anova_type, function(fit, type) {
            car::Anova(fit, type = type) %>%
                as.data.frame() %>%
                rownames_to_column("term") %>%
                filter(term != "(Intercept)")
        })
    ) %>%
    unnest(anova_result) %>%
    select(comparison, metric, anova_type, term, Chisq, `Pr(>Chisq)`)

write.csv(
    main_effects,
    "./data/output/statistical_analysis/exp123_behaviour_metrics_main_effects_anova.csv",
    row.names = FALSE
)

# Marginal means
beh_metrics_mm <- pmap_dfr(fits, function(comparison, metric, cmp, fit, ...) {
    estimate_means(fit, c(cmp$term, "beh_rarity")) %>%
        add_labels(comparison, metric)
}) %>%
    select(
        comparison, model, burst, correction_type,
        beh_rarity, metric, everything()
    )

# Contrasts across behavioural rarity (averaged over the compared term)
beh_metrics_contrasts_rarity <- pmap_dfr(
    fits, function(comparison, metric, fit, ...) {
    estimate_contrasts(fit, contrast = "beh_rarity", p_adjust = "holm") %>%
        add_labels(comparison, metric)
}) %>%
    mutate(contrast_level = "across_behavioural_rarity")

# Contrasts of the compared term (within each rarity class when the model has
# the interaction, otherwise averaged over rarity)
beh_metrics_contrasts_term <- pmap_dfr(
    fits, function(comparison, metric, cmp, interaction, fit, ...) {
    estimate_contrasts(
        fit,
        contrast = cmp$term,
        by = if (interaction) "beh_rarity" else NULL,
        p_adjust = "holm"
    ) %>%
        add_labels(comparison, metric) %>%
        mutate(contrast_level = if (interaction) {
            "within_behaviour_rarity"
        } else {
            "averaged_over_rarity"
        })
})

# Save outputs
write.csv(
    beh_metrics_mm,
    "./data/output/statistical_analysis/exp123_behaviour_metrics_comparison_mm.csv",
    row.names = FALSE
)

write.csv(
    bind_rows(beh_metrics_contrasts_rarity, beh_metrics_contrasts_term) %>%
        select(
            contrast_level, comparison, model, burst,
            correction_type, beh_rarity, metric, everything()
        ),
    "./data/output/statistical_analysis/exp123_behaviour_metrics_comparison_contrasts.csv",
    row.names = FALSE
)
