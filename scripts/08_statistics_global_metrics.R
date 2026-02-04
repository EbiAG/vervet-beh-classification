# We will statsitically compare the models and create tables to use for the manuscript
# In exp 1, we compare the effect of burst length on accuracy and ROC_AUC. For this, we will use RF models which are trained on the uncorrected dataset
# In exp 2, we will compare the effect of the corrections and will use RF with burst 1
# In exp 3 we compare across model algorithms. For this we will use Burst 1, uncorrected and compare across the 9 models.

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(performance)
library(modelbased)
library(car)

# Load the model results ----
burst_comp_df <- read.csv(
    "./data/output/behaviour_comparison/behaviour_features_ml_metrics.csv"
) %>%
    filter(model == "RF" & correction_type == "uncorrected") %>%
    mutate(
        burst = factor(
            burst,
            levels = c("Burst_1", "Burst_2","Burst_3", "Burst_4")
        )
    )

correction_comp_df <- read.csv(
    "./data/output/behaviour_comparison/behaviour_features_ml_metrics.csv"
) %>%
    filter(model == "RF" & burst == "Burst_1") %>%
    mutate(
        correction_type = factor(
            correction_type,
            levels = c("uncorrected", "rotdaily", "rotbasal")
        )
    )

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
    )

model_comp_df <- model_metrics %>%
    filter(burst == "Burst_1" & correction_type == "uncorrected")

# Comparison of burst length ----
## Accuracy ----
burst_acc_mod1 <- glmmTMB(
    accuracy ~ burst,
    data = burst_comp_df,
    family = beta_family(link = "logit")
)
check_model(burst_acc_mod1)
sim_out <- simulateResiduals(burst_acc_mod1, n = 1000)
plot(sim_out)

estimate_means(burst_acc_mod1, by = "burst")
estimate_contrasts(burst_acc_mod1, contrast = "burst", adjust = "holm")
# No significant difference, due to the small sample size?

## ROC AUC ----
burst_roc_mod1 <- glmmTMB(
    roc_auc ~ burst,
    data = burst_comp_df,
    family = beta_family(link = "logit")
)
check_model(burst_roc_mod1)
sim_out <- simulateResiduals(burst_roc_mod1, n = 1000)
plot(sim_out)

estimate_means(burst_roc_mod1, by = "burst")
estimate_contrasts(burst_roc_mod1, contrast = "burst", adjust = "holm")
# No significant difference, due to the small sample size?

# Comparison of correction types ----
## Accuracy ----
correction_acc_mod1 <- glmmTMB(
    accuracy ~ correction_type,
    data = correction_comp_df,
    family = beta_family(link = "logit")
)
check_model(correction_acc_mod1)
sim_out <- simulateResiduals(correction_acc_mod1, n = 1000)
plot(sim_out)

estimate_means(correction_acc_mod1, by = "correction_type")
estimate_contrasts(
    correction_acc_mod1,
    contrast = "correction_type", adjust = "holm"
)
# Uncorrected is ssignificantly better

## ROC AUC ----
correction_roc_mod1 <- glmmTMB(
    roc_auc ~ correction_type,
    data = correction_comp_df,
    family = beta_family(link = "logit")
)
check_model(correction_roc_mod1)
sim_out <- simulateResiduals(correction_roc_mod1, n = 1000)
plot(sim_out)

estimate_means(correction_roc_mod1, by = "correction_type")
estimate_contrasts(
    correction_roc_mod1,
    contrast = "correction_type", adjust = "holm"
)
# Uncorrected is significantly better than both corrections

# Comparison of models ----
## Accuracy ----
model_acc_mod1 <- glmmTMB(
    accuracy ~ model,
    data = model_comp_df,
    family = beta_family(link = "logit")
)
check_model(model_acc_mod1)
sim_out <- simulateResiduals(model_acc_mod1, n = 1000)
plot(sim_out)

estimate_means(model_acc_mod1, by = "model")
estimate_contrasts(model_acc_mod1, contrast = "model", adjust = "holm")
# Differences between the models
emmip(model_acc_mod1, ~ model, CIs = TRUE)
# HydraROCKET and TabPFN are similar and better than the rest
# DL models in general are good
# Dl with time series shows the most variation

## ROC AUC ----
model_roc_mod1 <- glmmTMB(
    roc_auc ~ model,
    data = model_comp_df,
    family = beta_family(link = "logit")
)
check_model(model_roc_mod1)
sim_out <- simulateResiduals(model_roc_mod1, n = 1000)
plot(sim_out)

estimate_means(model_roc_mod1, by = "model")
estimate_contrasts(model_roc_mod1, contrast = "model", adjust = "holm")
emmip(model_roc_mod1, ~ model, CIs = TRUE)
# Same results as accuracy.

# Output statistical results ----
model_summary <- bind_rows(
    Anova(burst_acc_mod1, type = "II") %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        mutate(metric = "accuracy"),
    Anova(burst_roc_mod1, type = "II") %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        mutate(metric = "roc_auc"),
    Anova(correction_acc_mod1, type = "II") %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        mutate(metric = "accuracy"),
    Anova(correction_roc_mod1, type = "II") %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        mutate(metric = "roc_auc"),
    Anova(model_acc_mod1, type = "II") %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        mutate(metric = "accuracy"),
    Anova(model_roc_mod1, type = "II") %>%
        as.data.frame() %>%
        rownames_to_column(var = "term") %>%
        mutate(metric = "roc_auc")
) %>%
    select(comparison = term, metric, everything())

write.csv(
    model_summary,
    "./data/output/statistical_analysis/exp123_model_metrics_comparison_anova.csv",
    row.names = FALSE
)

# Marginal means
model_metrics_mm <- bind_rows(
    estimate_means(burst_acc_mod1, by = "burst") %>%
        mutate(
            comparison = "burst",
            metric = "accuracy", model = "RF",
            correction_type = "uncorrected"
        ),
    estimate_means(burst_roc_mod1, by = "burst") %>%
        mutate(
            comparison = "burst",
            metric = "roc_auc", model = "RF",
            correction_type = "uncorrected"
        ),
    estimate_means(correction_acc_mod1, by = "correction_type") %>%
        mutate(
            comparison = "correction",
            metric = "accuracy", model = "RF",
            burst = "Burst_1"
        ),
    estimate_means(correction_roc_mod1, by = "correction_type") %>%
        mutate(
            comparison = "correction",
            metric = "roc_auc", model = "RF",
            burst = "Burst_1"
        ),
    estimate_means(model_acc_mod1, by = "model") %>%
        mutate(
            comparison = "model",
            metric = "accuracy",
            burst = "Burst_1",
            correction_type = "uncorrected"
        ),
    estimate_means(model_roc_mod1, by = "model") %>%
        mutate(
            comparison = "model",
            metric = "roc_auc",
            burst = "Burst_1",
            correction_type = "uncorrected"
        )
) %>%
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
model_metrics_contrasts <- bind_rows(
    estimate_contrasts(burst_acc_mod1, contrast = "burst", adjust = "holm") %>%
        mutate(
            comparison = "burst",
            metric = "accuracy", model = "RF",
            correction_type = "uncorrected"
        ),
    estimate_contrasts(burst_roc_mod1, contrast = "burst", adjust = "holm") %>%
        mutate(
            comparison = "burst",
            metric = "roc_auc", model = "RF",
            correction_type = "uncorrected"
        ),
    estimate_contrasts(
        correction_acc_mod1,
        contrast = "correction_type", adjust = "holm"
    ) %>%
        mutate(
            comparison = "correction",
            metric = "accuracy", model = "RF",
            burst = "Burst_1"
        ),
    estimate_contrasts(
        correction_roc_mod1,
        contrast = "correction_type", adjust = "holm"
    ) %>%
        mutate(
            comparison = "correction",
            metric = "roc_auc", model = "RF",
            burst = "Burst_1"
        ),
    estimate_contrasts(model_acc_mod1, contrast = "model", adjust = "holm") %>%
        mutate(
            comparison = "model",
            metric = "accuracy",
            burst = "Burst_1",
            correction_type = "uncorrected"
        ),
    estimate_contrasts(model_roc_mod1, contrast = "model", adjust = "holm") %>%
        mutate(
            comparison = "model",
            metric = "roc_auc",
            burst = "Burst_1",
            correction_type = "uncorrected"
        )
) %>%
    select(comparison, model, burst, correction_type, metric, everything())

write.csv(
    model_metrics_contrasts,
    "./data/output/statistical_analysis/exp123_model_metrics_comparison_contrasts.csv",
    row.names = FALSE
)
