# We will statistically compare the global (per-fold) metrics and create tables to use for the manuscript
# In exp 1, we compare the effect of burst length. For this, we will use RF models trained on the uncorrected dataset
# In exp 2, we compare the effect of the corrections and will use RF with Burst 1, the raw-data standard
# also used for exp 3 (the per-behaviour metrics in 06b use Burst 4, which has more windows of the rare behaviours)
# In exp 3, we compare across model algorithms, using Burst 1 uncorrected and all 9 models.
# We also compare the top 5 models (any burst and correction) for each metric
# Metrics: accuracy, ROC AUC and F1-macro, each modelled with a beta GLMM
#
# Random effects: every model and correction type is evaluated on the same CV
# folds of the same windows (indices_cv_fold_lookup.csv), so fold is a paired
# unit and enters as (1 | fold). Different bursts contain different windows,
# so fold k of one burst is not the same data as fold k of another: the burst
# comparison has no random effect, and the top 5 comparison (which mixes
# bursts) uses (1 | burst:fold).

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(modelbased)
library(car)

metrics <- c("accuracy", "roc_auc", "f1_macro")

# Load the model results ----
model_metrics <- bind_rows(
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_ml_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_features_dl_tabpfn_metrics.csv"
    ),
    read.csv(
        "./data/output/comparison_behaviour/behaviour_acceleration_dl_metrics.csv"
    )
) %>%
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
        fold = factor(fold)
    )

# Every (model, burst, correction) should have all 5 folds
stopifnot(all(count(model_metrics, model, burst, correction_type)$n == 5))

burst_comp_df <- model_metrics %>%
    filter(model == "RF" & correction_type == "uncorrected") %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2", "Burst_3", "Burst_4")
        )
    )

correction_comp_df <- model_metrics %>%
    filter(model == "RF" & burst == "Burst_1") %>%
    mutate(
        correction_type = factor(
            correction_type,
            levels = c("uncorrected", "rotdaily", "rotbasal")
        )
    )

model_comp_df <- model_metrics %>%
    filter(burst == "Burst_1" & correction_type == "uncorrected")

# Top 5 (model, burst, correction) combinations by mean value of one metric
top5_df <- function(metric) {
    top5 <- model_metrics %>%
        group_by(model, burst, correction_type) %>%
        summarise(mean_value = mean(.data[[metric]]), .groups = "drop") %>%
        slice_max(mean_value, n = 5, with_ties = FALSE)

    model_metrics %>%
        semi_join(top5, by = c("model", "burst", "correction_type")) %>%
        mutate(
            model_type = paste(
                model,
                gsub("Burst_", "B", burst),
                dplyr::recode(
                    correction_type,
                    uncorrected = "Unc", rotdaily = "Daily", rotbasal = "Basal"
                ),
                sep = "_"
            )
        )
}

# Fit models ----
# Each comparison: the data, the term compared, and the random effect (if any)
comparisons <- list(
    burst = list(
        data = burst_comp_df, term = "burst", re = NULL,
        labels = list(model = "RF", correction_type = "uncorrected")
    ),
    correction = list(
        data = correction_comp_df, term = "correction_type", re = "(1 | fold)",
        labels = list(model = "RF", burst = "Burst_1")
    ),
    model = list(
        data = model_comp_df, term = "model", re = "(1 | fold)",
        labels = list(burst = "Burst_1", correction_type = "uncorrected")
    )
)

fit_beta <- function(metric, term, re, data) {
    glmmTMB(
        reformulate(c(term, re), response = metric),
        data = data,
        family = beta_family(link = "logit")
    )
}

fits <- expand_grid(comparison = names(comparisons), metric = metrics) %>%
    mutate(
        fit = map2(comparison, metric, function(comp, metric) {
            cmp <- comparisons[[comp]]
            fit_beta(metric, cmp$term, cmp$re, cmp$data)
        })
    )

# Diagnostics ----
# Convergence (positive-definite Hessian), DHARMa uniformity and dispersion
# tests, and the random-effect SD (fold, or burst:fold for the top 5; NA for the
# burst comparison, which has none; near 0 = little shared fold variation)
diagnostics <- fits %>%
    mutate(
        pd_hessian = map_lgl(fit, ~ .x$sdr$pdHess),
        ks_p = map_dbl(fit, function(f) {
            testUniformity(simulateResiduals(f, n = 1000), plot = FALSE)$p.value
        }),
        dispersion_p = map_dbl(fit, function(f) {
            testDispersion(simulateResiduals(f, n = 1000), plot = FALSE)$p.value
        }),
        fold_sd = map_dbl(fit, function(f) {
            vc <- VarCorr(f)$cond
            if (length(vc) == 0) NA_real_ else attr(vc[[1]], "stddev")[[1]]
        })
    ) %>%
    select(-fit)
print(diagnostics, n = Inf)

# Inspect any model flagged above, e.g.
# plot(simulateResiduals(fits$fit[[1]], n = 1000))

# Output statistical results ----
# Adds the comparison's identifying columns (e.g. model = "RF") to a table
add_labels <- function(df, comp, metric) {
    df <- as.data.frame(df) %>% mutate(comparison = comp, metric = metric)
    for (col in names(comparisons[[comp]]$labels)) {
        df[[col]] <- comparisons[[comp]]$labels[[col]]
    }
    df
}

model_summary <- pmap_dfr(fits, function(comparison, metric, fit) {
    Anova(fit, type = "II") %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        mutate(comparison = comparison, metric = metric)
}) %>%
    select(comparison, term, metric, everything())

write.csv(
    model_summary,
    "./data/output/statistical_analysis/exp123_model_metrics_comparison_anova.csv",
    row.names = FALSE
)

# Marginal means
model_metrics_mm <- pmap_dfr(fits, function(comparison, metric, fit) {
    estimate_means(fit, by = comparisons[[comparison]]$term) %>%
        add_labels(comparison, metric)
}) %>%
    select(
        comparison, model, burst, correction_type,
        metric, everything()
    )

write.csv(
    model_metrics_mm,
    "./data/output/statistical_analysis/exp123_model_metrics_comparison_mm.csv",
    row.names = FALSE
)

# Contrasts
model_metrics_contrasts <- pmap_dfr(fits, function(comparison, metric, fit) {
    estimate_contrasts(
        fit,
        contrast = comparisons[[comparison]]$term, p_adjust = "holm"
    ) %>%
        add_labels(comparison, metric)
}) %>%
    select(comparison, model, burst, correction_type, metric, everything())

write.csv(
    model_metrics_contrasts,
    "./data/output/statistical_analysis/exp123_model_metrics_comparison_contrasts.csv",
    row.names = FALSE
)

# Comparison of top 5 models ----
# Not written to file; quoted in the text
for (metric in metrics) {
    top5_fit <- fit_beta(
        metric, "model_type", "(1 | burst:fold)", 
        top5_df(metric)
    )
    cat("\n== Top 5 by", metric, "\n")
    print(estimate_means(top5_fit, by = "model_type"))
    print(
        estimate_contrasts(top5_fit,
            contrast = "model_type",
            p_adjust = "holm"
        )
    )
}

