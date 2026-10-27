# Vervet Accelerometer Classification
Data and code for comparing the effect of pre-processing and model choice on behavioural classification from accelerometer data in vervet monkeys.

**Version 2.0.0** accompanies the revised manuscript. Models are now evaluated with grouped, stratified 5-fold cross-validation (all sub-bursts of a parent burst share a fold), and the revision adds hyperparameter tuning, leave-one-individual-out validation, a class-weighting ablation and a night-time analysis of orientation correction. The final classifier is **HydraMultiROCKET trained on Burst 2 with daily orientation correction (HydraMultiROCKET-Daily)**. Version 1.0.2 contains the code and data of the original submission.

The repository has the following folder structure:
```
├── data/
│   ├── output/
│   ├── raw/
│   └── temp/
├── models/
├── plots/
├── scripts/
├── .gitignore
├── environment_tabpfn.yml
├── environment_torch_tabular.yml
├── environment_tsai.yml
├── R_sessionInfo.txt
├── README.md
└── VervetBehClass.Rproj
```

## Setting up the environments

### Python
The three `environment_*.yml` files recreate the Python environments for the notebooks. An easy way to get started is [mamba](https://mamba.readthedocs.io/en/latest/index.html); see [this page](https://mamba.readthedocs.io/en/latest/user_guide/mamba.html#conda-yaml-spec-files) for creating an environment from a YML file. Each notebook needs one environment:

| Environment | Notebooks |
|---|---|
| `torch_tabular` | 04, 05a, 05b, 07a, 07b, 08a |
| `tabpfn` | 05c, 07c, 08b, 10a_tabpfn, 10b_tabpfn |
| `tsai` | 05d, 07d, 08c, 10a, 10b |

Notebooks are run from the [scripts](/scripts/) folder, so their paths start with `../data/`. The deep-learning notebooks (05b, 05d, 07b, 07d, 08c, 10a, 10b) expect a CUDA GPU; TabPFN uses a GPU if one is available.

### R
The R scripts are run from the repository root: open [VervetBehClass.Rproj](/VervetBehClass.Rproj) in RStudio so that paths starting with `./data/` resolve. The R version and package versions used are listed in [R_sessionInfo.txt](/R_sessionInfo.txt).

## Data folder
The data folder contains the data needed to run the scripts:

- [raw](/data/raw/):
    + [acc](/data/raw/acc/): annotated raw accelerometer data, with one subfolder per burst length (B1-B4) and three files per subfolder, one per orientation correction (uncorrected, daily, basal). The uncorrected datasets are produced by [01a_burst_division.R](/scripts/01a_burst_division.R) and the daily and basal corrected datasets by [01b_orientation_correction.R](/scripts/01b_orientation_correction.R), both run on the whole dataset. The whole dataset is too large for this repository; it is stored on Movebank (study ID 2111088601) and is available from the authors on request (see [Data available on request](#data-available-on-request)).
    + [features](/data/raw/features/): the 60 summary features calculated from the accelerometer data, with the same subfolders and files as `acc`. The feature calculation is in [01c_feature_calculation.R](/scripts/01c_feature_calculation.R).
    + [annotations](/data/raw/annotations/): behavioural annotations of each sample within each burst, one file per burst length, and a merged file that assigns one behaviour per burst by majority rule ([03_combine_burst_attributions.ipynb](/scripts/03_combine_burst_attributions.ipynb)).
    + [focal_sampling](/data/raw/focal_sampling/): the dates used for the focal-sampling comparison ([focal_sampled_days.csv](/data/raw/focal_sampling/focal_sampled_days.csv)) and the monthly proportion of each behaviour from focal sampling ([prop_behaviour_monthly_focal.csv](/data/raw/focal_sampling/prop_behaviour_monthly_focal.csv)). The accelerometer data and features of the focal-sampled days are too large for the repository and are available on request (see [Data available on request](#data-available-on-request)).
    + [annotated_burst_id_lookup_table.csv](/data/raw/annotated_burst_id_lookup_table.csv): matches `burst_id` with `new_burst`, so bursts can be matched across burst lengths.
- [temp](/data/temp/):
    + [acc_tsai](/data/temp/acc_tsai/): the raw accelerometer data converted to the format of the tsai package, written by [03_combine_burst_attributions.ipynb](/scripts/03_combine_burst_attributions.ipynb), with the same subfolders and files as `raw/acc`.
    + [annotations](/data/temp/annotations/): intermediate files used to identify and remove Resting outliers based on VeDBA, written by [02_vedba_outliers.R](/scripts/02_vedba_outliers.R) and [03_combine_burst_attributions.ipynb](/scripts/03_combine_burst_attributions.ipynb).
    + [focal_sampling](/data/temp/focal_sampling/): intermediate files for choosing the focal-sampled days, written by [09_focal_sampling_days.R](/scripts/09_focal_sampling_days.R).
    + [indices_cv_fold_lookup.csv](/data/temp/indices_cv_fold_lookup.csv): the cross-validation fold of every window, used by all model scripts (written by [04_build_cv_fold_lookup.ipynb](/scripts/04_build_cv_fold_lookup.ipynb)).
    + `indices_loio_fold_lookup_burst_1.csv`, `indices_loio_fold_lookup_burst_2.csv`: the held-out individual of every window for the leave-one-individual-out validation (written by [08a_individual_validation_features_ml.ipynb](/scripts/08a_individual_validation_features_ml.ipynb)).
    + [plotting](/data/temp/plotting/) and [statistical_analysis](/data/temp/statistical_analysis/): intermediate files for the figures and statistical analyses.
- [output](/data/output/):
    + [comparison_behaviour](/data/output/comparison_behaviour/): results of the behaviour classification. For each model family (`features_ml`, `features_dl`, `features_dl_tabpfn`, `acceleration_dl`) there are per-fold metrics (`*_metrics.csv`), per-fold confusion matrices (`*_confusion.csv`) and out-of-fold predictions with class probabilities for every window (`*_oof_predictions.csv`). The same folder holds the class-weighting ablation (`*_nocw_*`), the leave-one-individual-out validation (`*_loio_*`), the hyperparameter tuning (`*_tuned_*`, with the default-hyperparameter controls in `*_tuned_default_control_*`), the class counts and proportions per fold (`behaviour_class_counts_per_fold.csv`, `behaviour_class_props_per_fold.csv`) and the class-weighting summary (`behaviour_class_weight_ablation_summary.csv`). It also holds the per-behaviour change in F1 with tuning behind Fig. 3C, as the mean and SD across outer folds (`behaviour_tuning_f1_change.csv`), the pooled precision, recall and F1 per behaviour behind Fig. 4, with parent-burst bootstrap 95% CIs, across burst lengths (`behaviour_pooled_metrics_by_burst.csv`) and at Burst 2 (`behaviour_pooled_metrics_burst2.csv`), and the macro-F1 behind Fig. S5, over the five scored behaviours, for the main cross-validation and the leave-one-individual-out validation on the same individuals (`behaviour_loio_vs_main_cv_f1macro.csv`).
    + [inference_results](/data/output/inference_results/): behaviour predictions and class probabilities for every burst of the focal-sampled days, one file per model (written by [10b_final_models_inference.ipynb](/scripts/10b_final_models_inference.ipynb) and [10b_final_models_inference_tabpfn.ipynb](/scripts/10b_final_models_inference_tabpfn.ipynb)).
    + [statistical_analysis](/data/output/statistical_analysis/): results of the statistical comparisons (ANOVA tables, marginal means, contrasts) for Experiments 1-3 (`exp123_*`) and the focal-sampling comparison (`exp4_*`, including the number of predicted windows per model and individual, `exp4_prediction_counts.csv`), the tables of the night-time analysis (`night_*`), the proportion of pure windows per burst length behind Fig. S2, overall and per behaviour (`annotation_pure_window_proportions.csv`), and the number of annotated windows at each filtering step behind Table S1 (`annotation_window_counts.csv`).

The activity (active/inactive) target is fitted in 05a-05d but its outputs are not used in the paper and are not saved; see the save cell of each notebook to re-enable them.

## Models folder
The [models](/models/) folder contains the five models trained on all labelled Burst 2 windows and run on the focal-sampled days:

| Folder | Model | Role |
|---|---|---|
| `final_hr_b2_rotdaily` | HydraMultiROCKET, Burst 2, daily correction | **final classifier** (HydraMultiROCKET-Daily) |
| `final_hr_b2_unc` | HydraMultiROCKET, Burst 2, uncorrected | comparison |
| `final_hr_b2_rotbasal` | HydraMultiROCKET, Burst 2, basal correction | comparison (night-time analysis) |
| `final_rf_b2_unc` | Random Forest, Burst 2, uncorrected | comparison |
| `final_tabpfn_b2_unc` | TabPFN, Burst 2, uncorrected | comparison |

The HydraMultiROCKET models are saved with tsai (`load_learner_all`, see [10b_final_models_inference.ipynb](/scripts/10b_final_models_inference.ipynb)); the Random Forest and TabPFN models are saved with joblib together with their label encoders.

## Scripts
The [scripts](/scripts/) folder contains the Jupyter notebooks and R scripts for creating the datasets, training and evaluating the models, the statistical analyses, the inference on new data and the figures. They are numbered in the order in which they are run. Every script is commented with details on how to run it. The steps are:

1. **Create the training datasets**: the Burst 1 uncorrected accelerometer data are divided into 12 datasets (4 burst lengths × 3 orientation corrections) and the features are calculated for each: [01a_burst_division.R](/scripts/01a_burst_division.R), [01b_orientation_correction.R](/scripts/01b_orientation_correction.R) and [01c_feature_calculation.R](/scripts/01c_feature_calculation.R). These run on the whole 1.5-year dataset (stored on Movebank, study ID 2111088601, and available on request). Their outputs for the annotated bursts are in [data/raw/acc](/data/raw/acc/) and [data/raw/features](/data/raw/features/), so steps 2-7 and the model training in step 9 can be run without them. Their outputs for the focal-sampled days (step 8) are available on request (see [Data available on request](#data-available-on-request)).
2. **Assign behaviour to bursts**: after removing behaviours that are not modelled, each burst is assigned one of 8 behaviours by majority rule, and Resting outliers are removed based on VeDBA: [02_vedba_outliers.R](/scripts/02_vedba_outliers.R) and [03_combine_burst_attributions.ipynb](/scripts/03_combine_burst_attributions.ipynb).
3. **Build the cross-validation folds**: [04_build_cv_fold_lookup.ipynb](/scripts/04_build_cv_fold_lookup.ipynb) assigns every parent burst to one of 5 folds, stratified by behaviour, so all sub-bursts of a parent burst share a fold and never appear in both training and test data. It also writes the class counts and proportions per fold.
4. **Train and evaluate the models**: the 9 models are trained and tested on each of the 12 datasets with the same 5 folds, using class weighting: RF, XGB and SVC ([05a_feature_classification_ml.ipynb](/scripts/05a_feature_classification_ml.ipynb)), Category Embedding and GANDALF ([05b_feature_classification_dl.ipynb](/scripts/05b_feature_classification_dl.ipynb)), TabPFN ([05c_feature_classification_dl_tabpfn.ipynb](/scripts/05c_feature_classification_dl_tabpfn.ipynb)), and HydraMultiROCKET, LSTM and TSSequencer on the raw acceleration ([05d_timeseries_classification_dl.ipynb](/scripts/05d_timeseries_classification_dl.ipynb)). The end of 05a, 05c and 05d repeats the B1 and B2 uncorrected runs without class weighting (the class-weighting ablation).
5. **Statistical analysis of Experiments 1-3**: the effect of burst length, orientation correction and model on the global metrics and on per-behaviour precision and recall: [06a_model_comparison_stats.R](/scripts/06a_model_comparison_stats.R) and [06b_behaviour_comparison_stats.R](/scripts/06b_behaviour_comparison_stats.R).
6. **Hyperparameter tuning**: nested cross-validation on Burst 1 uncorrected with the same 5 folds, 50 Optuna trials per outer fold and model, and the default hyperparameters as a control: [07a_tuning_features_ml.ipynb](/scripts/07a_tuning_features_ml.ipynb), [07b_tuning_features_dl.ipynb](/scripts/07b_tuning_features_dl.ipynb), [07c_tuning_features_tabpfn.ipynb](/scripts/07c_tuning_features_tabpfn.ipynb) and [07d_tuning_timeseries_dl.ipynb](/scripts/07d_tuning_timeseries_dl.ipynb).
7. **Leave-one-individual-out validation**: RF, TabPFN and HydraMultiROCKET on B1 and B2 uncorrected. Each individual with at least 30 labelled windows at that burst length is held out in turn (13 of 20 individuals in B1, 17 in B2), and the others stay in training only. The models are trained on all 8 behaviours, but the metrics, pooled across held-out individuals, exclude Running, Self-scratching and Sleeping, which few individuals show: [08a_individual_validation_features_ml.ipynb](/scripts/08a_individual_validation_features_ml.ipynb) (which also builds the fold lookups), [08b_individual_validation_features_tabpfn.ipynb](/scripts/08b_individual_validation_features_tabpfn.ipynb) and [08c_individual_validation_timeseries_dl.ipynb](/scripts/08c_individual_validation_timeseries_dl.ipynb).
8. **Choose the focal-sampled days**: days are randomly chosen from the months with the most focal-sampling data: [09_focal_sampling_days.R](/scripts/09_focal_sampling_days.R). The accelerometer data and features of these days are taken from the outputs of step 1 on the whole dataset, and are available on request.
9. **Train the final models and run inference**: the final classifier and the comparison models (see [Models folder](#models-folder)) are trained on all labelled Burst 2 windows and run on every burst of the focal-sampled days: [10a_final_models_training.ipynb](/scripts/10a_final_models_training.ipynb) and [10b_final_models_inference.ipynb](/scripts/10b_final_models_inference.ipynb), with TabPFN in [10a_final_models_training_tabpfn.ipynb](/scripts/10a_final_models_training_tabpfn.ipynb) and [10b_final_models_inference_tabpfn.ipynb](/scripts/10b_final_models_inference_tabpfn.ipynb).
10. **Compare predictions with focal sampling (Experiment 4)**: the monthly daytime proportions of behaviour predicted by the final classifier and the comparison models are compared with focal sampling, and the predicted windows are counted per model and individual: [11a_focal_sampling_comparison_stats.R](/scripts/11a_focal_sampling_comparison_stats.R).
11. **Night-time analysis**: how orientation correction changes the night-time Sleeping predictions of HydraMultiROCKET, through collar tilt and movement: [11b_night_prediction_comparisons.R](/scripts/11b_night_prediction_comparisons.R).
12. **Figures**: [12a_publication_plots_fig2.R](/scripts/12a_publication_plots_fig2.R), [12b_publication_plots_fig3.R](/scripts/12b_publication_plots_fig3.R), [12c_publication_plots_fig4.R](/scripts/12c_publication_plots_fig4.R), [12d_publication_plots_fig5.R](/scripts/12d_publication_plots_fig5.R) and [12e_publication_plots_supplementary.R](/scripts/12e_publication_plots_supplementary.R). The figures are written to the [plots](/plots/) folder. 12b, 12c and 12e also write the values behind Fig. 3C, Fig. 4, Fig. S2, Fig. S5, Fig. S8 and Table S1 to [comparison_behaviour](/data/output/comparison_behaviour/) and [statistical_analysis](/data/output/statistical_analysis/).

The outputs of every step are in the repository, so the statistics (steps 5 and 10) and the figures (step 12) can be reproduced without retraining any model. Only the inference (10b), the night-time analysis (11b) and Fig. S7 need the files listed under [Data available on request](#data-available-on-request).

## Figures
| File | Figure | Script |
|---|---|---|
| `Fig1_behaviour_attribution_burst_sankey_plot.svg` | Fig. 1: behaviour attribution across burst lengths | - |
| `fig2_exp12_model_behaviour_metrics_combined` | Fig. 2: effect of burst length and orientation correction (Experiments 1 and 2) | 12a |
| `fig3_global_metrics_tuning` | Fig. 3: model comparison and hyperparameter tuning (Experiment 3) | 12b |
| `fig4_final_model_selection` | Fig. 4: final model selection, per-behaviour F1 across burst lengths and precision vs recall at Burst 2 | 12c |
| `fig5_exp4_focal_sampling_beh_prop` | Fig. 5: predictions vs focal sampling and circadian patterns (Experiment 4) | 12d |
| `figs1_individual_validation_data_variation` | Fig. S1: annotated windows per individual and behaviour | 12e |
| `figs2_pure_burst_proportions` | Fig. S2: proportion of pure windows | 12e |
| `figs3_orientation_correction_static_acc_resting` | Fig. S3: static acceleration of Resting windows by orientation correction | 12e |
| `figs4_vedba_by_behaviour` | Fig. S4: smoothed VeDBA by behaviour and the walking range used for orientation correction | 12e |
| `figs5_main_cv_vs_loio_f1macro` | Fig. S5: main cross-validation vs leave-one-individual-out | 12e |
| `figs6_night_time_sleep_tilt_movement` | Fig. S6: night-time Sleeping, collar tilt and movement | 12e |
| `figs7_night_orientation_by_model` | Fig. S7: night-time collar orientation by model | 12e |
| `figs8_class_weight_ablation` | Fig. S8: class-weighting ablation | 12e |

Main figures are saved as PNG and PDF, supplementary figures as PNG.

## Hyperparameter tuning
All nine models are tuned on the Burst 1 uncorrected dataset with nested cross-validation (5 outer folds, 2 inner validation folds), 50 Optuna trials (multivariate TPE) per outer fold and the inner-validation macro-F1 as the objective ([07a](/scripts/07a_tuning_features_ml.ipynb)-[07d](/scripts/07d_tuning_timeseries_dl.ipynb)). Every model except TabPFN tunes two parameters: one that sets capacity (the size or flexibility of the fitted function) and one that regularises the fit through a separate mechanism (a penalty, shrinkage, randomisation or dropout). All other parameters, including class weighting and the training settings of the deep learning models, are kept as in the main cross-validation ([05a](/scripts/05a_feature_classification_ml.ipynb)-[05d](/scripts/05d_timeseries_classification_dl.ipynb)). The one exception is that during the search, GANDALF and Category Embedding early-stop on the inner validation fold that scores each trial. The default configuration, given in brackets, is enqueued as the first trial, and the tuned and default configurations are retrained on the same split with the same seed.

| Model | Script | Capacity | Regularisation |
|---|---|---|---|
| RF | 07a | `min_samples_leaf`: 1-10, log (1) | `max_features`: 1-60, log (7, i.e. √60 rounded down) |
| XGBoost | 07a | `max_depth`: 3-20 (6) | `learning_rate`: 0.01-1, log (0.3) |
| SVC | 07a | `gamma`: 0.0001-1, log ("scale")¹ | `C`: 0.1-100, log (1) |
| GANDALF | 07b | `gflu_stages`: 5-25 (15) | `gflu_dropout`: 0-0.5 (0) |
| Category Embedding | 07b | `layers`: 5 presets (1024-512-256)² | `dropout`: 0-0.5 (0) |
| TabPFN | 07c | `n_estimators`: 4-32, log (4)³ | `outlier_removal_std`: 3-20 (12)³ |
| HydraMultiROCKET | 07d | `num_features`: 10,000-50,000, log (50,000)⁴ | `fc_dropout`: 0-0.5 (0) |
| LSTM | 07d | `hidden_size`: 50-300 (100) | `fc_dropout`: 0-0.5 (0) |
| TSSequencer | 07d | `d_model`: 32-256 (128) | `fc_dropout`: 0-0.5 (0) |

1. `gamma="scale"` depends on the data and lies outside the search range, so the SVC default is evaluated directly rather than enqueued.
2. Hidden-layer widths: 256-128, 512-256-128, 1024-512-256, 1024-512-256-128 and 2048-1024-512-256.
3. TabPFN's pretrained network has no tunable capacity, so it tunes ensemble size and outlier clipping instead.
4. The default is the upper bound of the range (the tsai default), so tuning can only reduce `num_features`.

## Data available on request
The following data are too large for the repository and are available from the authors on request:
- the whole 1.5-year accelerometer dataset, stored on Movebank (study ID 2111088601), which step 1 runs on;
- the accelerometer data and features of the focal-sampled days, taken from the outputs of step 1:
    + `data/raw/focal_sampling/acc/focal_sampled_acc_burst_2_{uncorrected,rotbasal,rotdaily}.parquet`
    + `data/raw/focal_sampling/features/focal_sampled_features_burst_2_uncorrected.parquet`

The outputs of step 1 for the annotated bursts are in the repository, so the whole dataset is needed only to rerun step 1. The focal-sampled files are needed to rerun the inference (10b), the night-time analysis (11b) and Fig. S7 (12e). The outputs of these scripts are in the repository, so all other figures and tables, including Fig. 5 and Fig. S6, can be reproduced without them. Place the files at the paths above; they are excluded from version control by `.gitignore`.

## Applying the pipeline to new data
The pipeline runs on any tri-axial accelerometer data recorded in bursts, given per-sample behavioural annotations for a subset of bursts.

1. **Raw data**: [01a_burst_division.R](/scripts/01a_burst_division.R) reads e-obs acceleration exports from Movebank (column `eobs:accelerations-raw`) and writes one parquet file per individual and burst length, with one row per sample and the columns `Ind_ID`, `burst_id`, `new_burst`, `timestamp`, `X`, `Y` and `Z`. Data from other sources can enter the pipeline as parquet files in this format. [01b_orientation_correction.R](/scripts/01b_orientation_correction.R) converts e-obs raw counts to g (`convert_to_g`), so adapt this function to the calibration of other loggers.
2. **Settings**: the sampling frequency (`sampling_frequency` in 01a, `sample_freq` in 01c), the burst divisions (`burst_dividers` in 01a), the static-acceleration window (`static_window_size`, 6 samples here), the walking range for orientation correction (`vedbas_threshold`, 0.17-0.30 g here) and the basal correction window (`basal_window_size`, 20, 40, 60 and 80 walking windows for Bursts 1-4 here) are function arguments in 01a-01c.
3. **Annotations**: provide per-sample annotations in the format of [data/raw/annotations](/data/raw/annotations/) and adapt the mapping from annotated behaviours to modelled classes in [03_combine_burst_attributions.ipynb](/scripts/03_combine_burst_attributions.ipynb) to your ethogram.
4. **Training and prediction**: run steps 2-7 to build the folds and train and evaluate the models, and steps 9-10 to train the final models and predict behaviour on unannotated data.
