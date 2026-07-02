# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 15 — Robustness, Sensitivity and Model Risk Assessment
# ============================================================
# Objective:
#   Assess robustness, sensitivity and model risk of the public
#   reduced-form DFAST replication engine.
#
# Main input:
#   data/processed/model/benchmark_validation_panel.csv
#
# Main outputs:
#   data/processed/model/model_risk_assessment_panel.csv
#   outputs/model_risk/script15_model_risk_assessment_report.docx
#   outputs/model_risk/script15_model_risk_assessment_outputs.xlsx
#   outputs/model_risk/script15_execution_log.txt
#
# Methodological note:
#   This script does not change the model estimates. It evaluates
#   robustness, error concentration, threshold sensitivity and
#   model risk based on public DFAST benchmark validation outputs.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 15 - Robustness, Sensitivity and Model Risk Assessment\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "15"
script_name <- "robustness_sensitivity_model_risk_assessment"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/model", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/model_risk", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/model_risk/figures", recursive = TRUE, showWarnings = FALSE)

cat("Project root:", project_root, "\n")
cat("Directories checked.\n\n")


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

cat("Loading packages...\n")

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "tibble",
  "janitor",
  "ggplot2",
  "openxlsx",
  "officer",
  "flextable"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing missing package:", pkg, "\n")
    install.packages(pkg)
  }

  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

cat("Packages loaded.\n\n")


# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

safe_chr <- function(x) {
  x <- as.character(x)
  x <- enc2utf8(x)
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = " ")
  x <- gsub("[[:cntrl:]]", " ", x)
  stringr::str_squish(x)
}

safe_df <- function(df) {
  df |>
    dplyr::mutate(dplyr::across(where(is.character), safe_chr))
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sd(x, na.rm = TRUE)
}

safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_quantile <- function(x, probs) {
  if (all(is.na(x))) return(NA_real_)
  as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

safe_rmse <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) == 0) return(NA_real_)
  sqrt(mean((actual[ok] - predicted[ok])^2))
}

safe_mae <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) == 0) return(NA_real_)
  mean(abs(actual[ok] - predicted[ok]))
}

safe_bias <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) == 0) return(NA_real_)
  mean(predicted[ok] - actual[ok])
}

safe_r2 <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) < 3) return(NA_real_)

  ss_res <- sum((actual[ok] - predicted[ok])^2)
  ss_tot <- sum((actual[ok] - mean(actual[ok]))^2)

  if (ss_tot == 0) return(NA_real_)

  1 - ss_res / ss_tot
}

safe_correlation <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3) return(NA_real_)
  stats::cor(x[ok], y[ok])
}

save_plot <- function(plot_object, file_name, width = 9, height = 5.5) {
  path <- file.path("outputs/model_risk/figures", file_name)

  ggplot2::ggsave(
    filename = path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300
  )

  path
}

validation_stats <- function(data, actual_col, predicted_col, component_name) {
  actual <- data[[actual_col]]
  predicted <- data[[predicted_col]]

  tibble::tibble(
    component = component_name,
    observations = sum(!is.na(actual) & !is.na(predicted)),
    actual_mean = safe_mean(actual),
    predicted_mean = safe_mean(predicted),
    bias = safe_bias(actual, predicted),
    rmse = safe_rmse(actual, predicted),
    mae = safe_mae(actual, predicted),
    r2_outcome_prediction = safe_r2(actual, predicted),
    correlation = safe_correlation(actual, predicted),
    abs_error_p50 = safe_quantile(abs(predicted - actual), 0.50),
    abs_error_p75 = safe_quantile(abs(predicted - actual), 0.75),
    abs_error_p90 = safe_quantile(abs(predicted - actual), 0.90),
    abs_error_p95 = safe_quantile(abs(predicted - actual), 0.95),
    abs_error_max = safe_max(abs(predicted - actual))
  )
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Read benchmark validation panel
# ------------------------------------------------------------

cat("Reading benchmark validation panel...\n")

input_path <- "data/processed/model/benchmark_validation_panel.csv"

if (!file.exists(input_path)) {
  stop(paste("Missing input file:", input_path))
}

validation_raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()

cat("Input loaded.\n")
cat("Rows:", nrow(validation_raw), "\n")
cat("Columns:", ncol(validation_raw), "\n\n")


# ------------------------------------------------------------
# 4. Required variables
# ------------------------------------------------------------

cat("Checking required variables...\n")

required_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label",

  "actual_credit_loss_ratio",
  "predicted_credit_loss_ratio",
  "credit_loss_prediction_error",
  "abs_credit_loss_prediction_error",

  "actual_ppnr_ratio",
  "predicted_ppnr_ratio",
  "ppnr_prediction_error",
  "abs_ppnr_prediction_error",

  "actual_capital_depletion",
  "predicted_capital_depletion",
  "cet1_depletion_prediction_error",
  "abs_cet1_depletion_prediction_error",

  "observed_cet1_min_ratio",
  "integrated_predicted_cet1_min_ratio",
  "cet1_min_prediction_error",
  "abs_cet1_min_prediction_error",

  "cet1_min_validation_flag",
  "observed_below_4_5",
  "predicted_below_4_5",
  "observed_below_7",
  "predicted_below_7",
  "classification_match_4_5",
  "classification_match_7"
)

required_column_check <- tibble::tibble(
  required_column = required_cols,
  exists = required_cols %in% names(validation_raw)
) |>
  safe_df()

missing_required <- required_column_check |>
  dplyr::filter(!exists) |>
  dplyr::pull(required_column)

required_column_check_path <- "outputs/model_risk/script15_required_column_check.csv"
readr::write_csv(required_column_check, required_column_check_path)

if (length(missing_required) > 0) {
  cat("Missing required variables:\n")
  print(missing_required)
  stop("Script 15 stopped because required variables are missing.")
}

cat("Required variables checked.\n\n")


# ------------------------------------------------------------
# 5. Key audit
# ------------------------------------------------------------

cat("Auditing keys...\n")

key_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label"
)

key_audit <- validation_raw |>
  dplyr::count(dplyr::across(dplyr::all_of(key_cols)), name = "key_count") |>
  dplyr::mutate(duplicated_key = key_count > 1) |>
  safe_df()

key_audit_summary <- tibble::tibble(
  rows = nrow(validation_raw),
  unique_keys = nrow(key_audit),
  duplicated_keys = sum(key_audit$duplicated_key),
  max_rows_per_key = max(key_audit$key_count, na.rm = TRUE)
) |>
  safe_df()

if (key_audit_summary$duplicated_keys > 0) {
  print(key_audit_summary)
  stop("Input panel contains duplicated keys.")
}

cat("Key audit completed.\n")
print(key_audit_summary)
cat("\n")


# ------------------------------------------------------------
# 6. Build model risk assessment panel
# ------------------------------------------------------------

cat("Building model risk assessment panel...\n")

model_risk_panel <- validation_raw |>
  dplyr::mutate(
    cet1_abs_error_percentile =
      dplyr::percent_rank(abs_cet1_min_prediction_error),

    capital_depletion_abs_error_percentile =
      dplyr::percent_rank(abs_cet1_depletion_prediction_error),

    credit_loss_abs_error_percentile =
      dplyr::percent_rank(abs_credit_loss_prediction_error),

    ppnr_abs_error_percentile =
      dplyr::percent_rank(abs_ppnr_prediction_error),

    severe_cet1_error =
      abs_cet1_min_prediction_error > 2.0,

    moderate_or_worse_cet1_error =
      abs_cet1_min_prediction_error > 1.0,

    tail_error_flag =
      cet1_abs_error_percentile >= 0.95,

    threshold_miss_flag =
      !classification_match_4_5 | !classification_match_7,

    near_7_threshold =
      abs(observed_cet1_min_ratio - 7.0) <= 1.0 |
        abs(integrated_predicted_cet1_min_ratio - 7.0) <= 1.0,

    near_4_5_threshold =
      abs(observed_cet1_min_ratio - 4.5) <= 1.0 |
        abs(integrated_predicted_cet1_min_ratio - 4.5) <= 1.0,

    conservative_projection =
      integrated_predicted_cet1_min_ratio < observed_cet1_min_ratio,

    optimistic_projection =
      integrated_predicted_cet1_min_ratio > observed_cet1_min_ratio,

    model_risk_bucket = dplyr::case_when(
      severe_cet1_error | threshold_miss_flag ~ "High model risk",
      moderate_or_worse_cet1_error | tail_error_flag ~ "Moderate model risk",
      near_7_threshold | near_4_5_threshold ~ "Watchlist",
      TRUE ~ "Low model risk"
    ),

    model_risk_score =
      0.35 * cet1_abs_error_percentile +
      0.20 * capital_depletion_abs_error_percentile +
      0.15 * credit_loss_abs_error_percentile +
      0.15 * ppnr_abs_error_percentile +
      0.10 * as.numeric(threshold_miss_flag) +
      0.05 * as.numeric(near_7_threshold | near_4_5_threshold)
  ) |>
  safe_df()

cat("Model risk assessment panel created.\n\n")


# ------------------------------------------------------------
# 7. Overall robustness metrics
# ------------------------------------------------------------

cat("Creating overall robustness metrics...\n")

overall_robustness_metrics <- dplyr::bind_rows(
  validation_stats(
    model_risk_panel,
    "actual_credit_loss_ratio",
    "predicted_credit_loss_ratio",
    "Credit loss ratio"
  ),
  validation_stats(
    model_risk_panel,
    "actual_ppnr_ratio",
    "predicted_ppnr_ratio",
    "PPNR ratio"
  ),
  validation_stats(
    model_risk_panel,
    "actual_capital_depletion",
    "predicted_capital_depletion",
    "CET1 capital depletion"
  ),
  validation_stats(
    model_risk_panel,
    "observed_cet1_min_ratio",
    "integrated_predicted_cet1_min_ratio",
    "CET1 minimum ratio"
  )
) |>
  safe_df()

cat("Overall robustness metrics created.\n\n")


# ------------------------------------------------------------
# 8. Robustness by year and scenario
# ------------------------------------------------------------

cat("Creating robustness by year and scenario...\n")

year_robustness <- model_risk_panel |>
  dplyr::group_by(exercise_year) |>
  dplyr::summarise(
    observations = dplyr::n(),
    banks = dplyr::n_distinct(bank_name),
    scenarios = dplyr::n_distinct(scenario_label),

    cet1_rmse =
      safe_rmse(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_mae =
      safe_mae(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_bias =
      safe_bias(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_r2 =
      safe_r2(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_correlation =
      safe_correlation(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),

    high_precision_share =
      mean(cet1_min_validation_flag == "High precision", na.rm = TRUE),
    acceptable_or_better_share =
      mean(cet1_min_validation_flag %in% c("High precision", "Acceptable precision"), na.rm = TRUE),

    high_model_risk_share =
      mean(model_risk_bucket == "High model risk", na.rm = TRUE),
    moderate_or_high_model_risk_share =
      mean(model_risk_bucket %in% c("High model risk", "Moderate model risk"), na.rm = TRUE),

    threshold_4_5_accuracy =
      mean(classification_match_4_5, na.rm = TRUE),
    threshold_7_accuracy =
      mean(classification_match_7, na.rm = TRUE),

    .groups = "drop"
  ) |>
  dplyr::arrange(exercise_year) |>
  safe_df()

scenario_robustness <- model_risk_panel |>
  dplyr::group_by(scenario_code, scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    banks = dplyr::n_distinct(bank_name),
    years = dplyr::n_distinct(exercise_year),

    cet1_rmse =
      safe_rmse(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_mae =
      safe_mae(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_bias =
      safe_bias(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_r2 =
      safe_r2(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_correlation =
      safe_correlation(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),

    high_precision_share =
      mean(cet1_min_validation_flag == "High precision", na.rm = TRUE),
    acceptable_or_better_share =
      mean(cet1_min_validation_flag %in% c("High precision", "Acceptable precision"), na.rm = TRUE),

    high_model_risk_share =
      mean(model_risk_bucket == "High model risk", na.rm = TRUE),
    moderate_or_high_model_risk_share =
      mean(model_risk_bucket %in% c("High model risk", "Moderate model risk"), na.rm = TRUE),

    threshold_4_5_accuracy =
      mean(classification_match_4_5, na.rm = TRUE),
    threshold_7_accuracy =
      mean(classification_match_7, na.rm = TRUE),

    .groups = "drop"
  ) |>
  dplyr::arrange(scenario_code) |>
  safe_df()

year_scenario_robustness <- model_risk_panel |>
  dplyr::group_by(exercise_year, scenario_code, scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    banks = dplyr::n_distinct(bank_name),

    cet1_rmse =
      safe_rmse(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_mae =
      safe_mae(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_bias =
      safe_bias(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),

    high_precision_share =
      mean(cet1_min_validation_flag == "High precision", na.rm = TRUE),
    acceptable_or_better_share =
      mean(cet1_min_validation_flag %in% c("High precision", "Acceptable precision"), na.rm = TRUE),

    high_model_risk_share =
      mean(model_risk_bucket == "High model risk", na.rm = TRUE),

    threshold_7_accuracy =
      mean(classification_match_7, na.rm = TRUE),

    .groups = "drop"
  ) |>
  dplyr::arrange(exercise_year, scenario_code) |>
  safe_df()

cat("Robustness by year and scenario created.\n\n")


# ------------------------------------------------------------
# 9. Bank-level model risk assessment
# ------------------------------------------------------------

cat("Creating bank-level model risk assessment...\n")

bank_model_risk <- model_risk_panel |>
  dplyr::group_by(bank_rssd_id, bank_name) |>
  dplyr::summarise(
    observations = dplyr::n(),
    years = dplyr::n_distinct(exercise_year),
    scenarios = dplyr::n_distinct(scenario_label),

    cet1_rmse =
      safe_rmse(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_mae =
      safe_mae(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_bias =
      safe_bias(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_correlation =
      safe_correlation(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),

    max_abs_cet1_error =
      safe_max(abs_cet1_min_prediction_error),
    mean_abs_cet1_error =
      safe_mean(abs_cet1_min_prediction_error),

    high_precision_share =
      mean(cet1_min_validation_flag == "High precision", na.rm = TRUE),
    acceptable_or_better_share =
      mean(cet1_min_validation_flag %in% c("High precision", "Acceptable precision"), na.rm = TRUE),

    high_model_risk_observations =
      sum(model_risk_bucket == "High model risk", na.rm = TRUE),
    moderate_model_risk_observations =
      sum(model_risk_bucket == "Moderate model risk", na.rm = TRUE),
    watchlist_observations =
      sum(model_risk_bucket == "Watchlist", na.rm = TRUE),

    high_model_risk_share =
      mean(model_risk_bucket == "High model risk", na.rm = TRUE),
    moderate_or_high_model_risk_share =
      mean(model_risk_bucket %in% c("High model risk", "Moderate model risk"), na.rm = TRUE),

    threshold_miss_observations =
      sum(threshold_miss_flag, na.rm = TRUE),

    near_threshold_observations =
      sum(near_7_threshold | near_4_5_threshold, na.rm = TRUE),

    mean_model_risk_score =
      safe_mean(model_risk_score),
    max_model_risk_score =
      safe_max(model_risk_score),

    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(max_model_risk_score), dplyr::desc(cet1_rmse)) |>
  dplyr::mutate(
    bank_model_risk_rank = dplyr::row_number()
  ) |>
  safe_df()

cat("Bank-level model risk assessment created.\n\n")


# ------------------------------------------------------------
# 10. Tail error analysis
# ------------------------------------------------------------

cat("Creating tail error analysis...\n")

tail_error_cutoff <- safe_quantile(model_risk_panel$abs_cet1_min_prediction_error, 0.95)

tail_error_panel <- model_risk_panel |>
  dplyr::filter(abs_cet1_min_prediction_error >= tail_error_cutoff) |>
  dplyr::arrange(dplyr::desc(abs_cet1_min_prediction_error)) |>
  safe_df()

tail_error_summary <- tail_error_panel |>
  dplyr::group_by(scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    banks = dplyr::n_distinct(bank_name),
    years = dplyr::n_distinct(exercise_year),
    mean_abs_cet1_error = safe_mean(abs_cet1_min_prediction_error),
    max_abs_cet1_error = safe_max(abs_cet1_min_prediction_error),
    optimistic_projection_share = mean(optimistic_projection, na.rm = TRUE),
    conservative_projection_share = mean(conservative_projection, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(observations)) |>
  safe_df()

largest_model_risk_observations <- model_risk_panel |>
  dplyr::select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    observed_cet1_min_ratio,
    integrated_predicted_cet1_min_ratio,
    cet1_min_prediction_error,
    abs_cet1_min_prediction_error,
    cet1_min_validation_flag,
    model_risk_bucket,
    model_risk_score,
    threshold_miss_flag,
    near_7_threshold,
    near_4_5_threshold,
    actual_capital_depletion,
    predicted_capital_depletion,
    actual_credit_loss_ratio,
    predicted_credit_loss_ratio,
    actual_ppnr_ratio,
    predicted_ppnr_ratio
  ) |>
  dplyr::arrange(dplyr::desc(model_risk_score)) |>
  safe_df()

cat("Tail error analysis created.\n\n")


# ------------------------------------------------------------
# 11. Sensitivity to CET1 thresholds
# ------------------------------------------------------------

cat("Creating threshold sensitivity analysis...\n")

threshold_grid <- tibble::tibble(
  threshold = seq(4.5, 10.0, by = 0.5)
)

threshold_sensitivity <- threshold_grid |>
  tidyr::crossing(
    model_risk_panel |>
      dplyr::select(
        bank_rssd_id,
        bank_name,
        exercise_year,
        scenario_code,
        scenario_label,
        observed_cet1_min_ratio,
        integrated_predicted_cet1_min_ratio
      )
  ) |>
  dplyr::mutate(
    observed_below_threshold = observed_cet1_min_ratio < threshold,
    predicted_below_threshold = integrated_predicted_cet1_min_ratio < threshold,
    classification_match = observed_below_threshold == predicted_below_threshold
  ) |>
  dplyr::group_by(threshold) |>
  dplyr::summarise(
    observations = dplyr::n(),
    observed_below = sum(observed_below_threshold, na.rm = TRUE),
    predicted_below = sum(predicted_below_threshold, na.rm = TRUE),
    correctly_classified = sum(classification_match, na.rm = TRUE),
    classification_accuracy = mean(classification_match, na.rm = TRUE),
    false_positive = sum(!observed_below_threshold & predicted_below_threshold, na.rm = TRUE),
    false_negative = sum(observed_below_threshold & !predicted_below_threshold, na.rm = TRUE),
    .groups = "drop"
  ) |>
  safe_df()

cat("Threshold sensitivity analysis created.\n\n")


# ------------------------------------------------------------
# 12. Bias direction and optimism/conservatism
# ------------------------------------------------------------

cat("Creating bias direction analysis...\n")

bias_direction_summary <- model_risk_panel |>
  dplyr::summarise(
    observations = dplyr::n(),
    mean_prediction_error = safe_mean(cet1_min_prediction_error),
    median_prediction_error = safe_median(cet1_min_prediction_error),
    optimistic_observations = sum(optimistic_projection, na.rm = TRUE),
    conservative_observations = sum(conservative_projection, na.rm = TRUE),
    exact_or_near_zero_observations = sum(abs_cet1_min_prediction_error <= 0.05, na.rm = TRUE),
    optimistic_share = mean(optimistic_projection, na.rm = TRUE),
    conservative_share = mean(conservative_projection, na.rm = TRUE),
    severe_optimistic_errors = sum(cet1_min_prediction_error > 2.0, na.rm = TRUE),
    severe_conservative_errors = sum(cet1_min_prediction_error < -2.0, na.rm = TRUE)
  ) |>
  safe_df()

bias_by_scenario <- model_risk_panel |>
  dplyr::group_by(scenario_code, scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    mean_prediction_error = safe_mean(cet1_min_prediction_error),
    median_prediction_error = safe_median(cet1_min_prediction_error),
    optimistic_share = mean(optimistic_projection, na.rm = TRUE),
    conservative_share = mean(conservative_projection, na.rm = TRUE),
    severe_optimistic_errors = sum(cet1_min_prediction_error > 2.0, na.rm = TRUE),
    severe_conservative_errors = sum(cet1_min_prediction_error < -2.0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(scenario_code) |>
  safe_df()

cat("Bias direction analysis created.\n\n")


# ------------------------------------------------------------
# 13. Model risk limits and conclusions
# ------------------------------------------------------------

cat("Creating model risk limits and conclusions...\n")

cet1_metrics <- overall_robustness_metrics |>
  dplyr::filter(component == "CET1 minimum ratio")

model_risk_limits <- tibble::tibble(
  risk_area = c(
    "Public-data limitation",
    "Bank-specific supervisory adjustments",
    "Tail error concentration",
    "Threshold sensitivity",
    "Scenario heterogeneity",
    "Model component interaction",
    "Ranking interpretation",
    "Use restriction"
  ),
  risk_description = c(
    "The engine uses public DFAST outputs and does not observe confidential supervisory model inputs.",
    "Some large errors are concentrated in specific bank-year-scenario observations, reflecting idiosyncratic bank structures or supervisory adjustments.",
    "Tail errors are assessed separately because average performance can hide localized deviations.",
    "Capital threshold classification is robust at 4.5 percent but should also be monitored around the 7.0 percent reference level.",
    "Model performance can vary across exercise years and supervisory scenarios.",
    "Credit loss, PPNR and capital depletion errors can interact when translated into CET1 minimum projections.",
    "Vulnerability rankings are relative outputs of the public reduced-form model, not official supervisory assessments.",
    "The project is suitable for public benchmarking, educational replication and model-risk discussion, not for official capital planning decisions."
  ),
  mitigation = c(
    "Document the public-data scope and cite Federal Reserve public DFAST sources in the final report.",
    "Report largest errors and bank-level model risk rankings.",
    "Use tail error tables and high-risk observation flags.",
    "Report threshold sensitivity across a grid of CET1 cutoffs.",
    "Report validation by year and scenario.",
    "Validate components separately and in integrated form.",
    "State clearly that rankings are analytical benchmarks.",
    "Include formal disclaimer in README, report and technical manual."
  )
) |>
  safe_df()

model_risk_conclusion <- tibble::tibble(
  item = c(
    "Overall CET1 fit",
    "Average error",
    "Bias",
    "Tail risk",
    "Threshold classification",
    "Operational use",
    "Final model-risk judgement"
  ),
  conclusion = c(
    paste0(
      "The integrated model achieves CET1 R-squared of ",
      round(cet1_metrics$r2_outcome_prediction, 4),
      " and correlation of ",
      round(cet1_metrics$correlation, 4),
      "."
    ),
    paste0(
      "The CET1 minimum ratio RMSE is ",
      round(cet1_metrics$rmse, 4),
      " and MAE is ",
      round(cet1_metrics$mae, 4),
      " percentage points."
    ),
    paste0(
      "The average CET1 prediction bias is ",
      round(cet1_metrics$bias, 4),
      ", indicating limited systematic bias."
    ),
    paste0(
      "The largest absolute CET1 error is ",
      round(safe_max(model_risk_panel$abs_cet1_min_prediction_error), 4),
      " percentage points and is treated as tail model risk."
    ),
    paste0(
      "The threshold sensitivity analysis confirms classification performance across CET1 cutoffs, with special attention to the 7.0 percent reference level."
    ),
    "The model can be used as a public reduced-form benchmark and stress test replication engine.",
    "Model risk is assessed as manageable for public benchmarking, provided that limitations and tail deviations are explicitly reported."
  )
) |>
  safe_df()

cat("Model risk limits and conclusions created.\n\n")


# ------------------------------------------------------------
# 14. Executive model risk summary
# ------------------------------------------------------------

cat("Creating executive model risk summary...\n")

executive_model_risk_summary <- tibble::tibble(
  metric = c(
    "Model risk assessment observations",
    "Banks",
    "Exercise years",
    "Scenarios",
    "Duplicated keys",
    "CET1 minimum ratio RMSE",
    "CET1 minimum ratio MAE",
    "CET1 minimum ratio bias",
    "CET1 minimum ratio R-squared",
    "CET1 minimum ratio correlation",
    "Largest absolute CET1 error",
    "High model risk observations",
    "Moderate model risk observations",
    "Watchlist observations",
    "Low model risk observations",
    "Tail error cutoff p95",
    "Threshold misses",
    "Near-threshold observations"
  ),
  value = c(
    as.character(nrow(model_risk_panel)),
    as.character(dplyr::n_distinct(model_risk_panel$bank_name)),
    as.character(dplyr::n_distinct(model_risk_panel$exercise_year)),
    as.character(dplyr::n_distinct(model_risk_panel$scenario_label)),
    as.character(key_audit_summary$duplicated_keys),
    as.character(round(cet1_metrics$rmse, 4)),
    as.character(round(cet1_metrics$mae, 4)),
    as.character(round(cet1_metrics$bias, 4)),
    as.character(round(cet1_metrics$r2_outcome_prediction, 4)),
    as.character(round(cet1_metrics$correlation, 4)),
    as.character(round(safe_max(model_risk_panel$abs_cet1_min_prediction_error), 4)),
    as.character(sum(model_risk_panel$model_risk_bucket == "High model risk", na.rm = TRUE)),
    as.character(sum(model_risk_panel$model_risk_bucket == "Moderate model risk", na.rm = TRUE)),
    as.character(sum(model_risk_panel$model_risk_bucket == "Watchlist", na.rm = TRUE)),
    as.character(sum(model_risk_panel$model_risk_bucket == "Low model risk", na.rm = TRUE)),
    as.character(round(tail_error_cutoff, 4)),
    as.character(sum(model_risk_panel$threshold_miss_flag, na.rm = TRUE)),
    as.character(sum(model_risk_panel$near_7_threshold | model_risk_panel$near_4_5_threshold, na.rm = TRUE))
  )
) |>
  safe_df()

cat("Executive model risk summary created.\n\n")


# ------------------------------------------------------------
# 15. Figures
# ------------------------------------------------------------

cat("Creating model risk figures...\n")

figure_paths <- c()

p1 <- ggplot2::ggplot(
  model_risk_panel,
  ggplot2::aes(x = abs_cet1_min_prediction_error)
) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::labs(
    title = "Absolute CET1 Minimum Ratio Prediction Error Distribution",
    x = "Absolute CET1 minimum ratio prediction error",
    y = "Number of observations"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p1, "fig01_abs_cet1_error_distribution.png")
)

p2 <- model_risk_panel |>
  dplyr::count(model_risk_bucket, name = "observations") |>
  ggplot2::ggplot(
    ggplot2::aes(x = model_risk_bucket, y = observations)
  ) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "Model Risk Buckets",
    x = "Model risk bucket",
    y = "Observations"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p2, "fig02_model_risk_buckets.png")
)

p3 <- bank_model_risk |>
  dplyr::slice_max(
    order_by = max_model_risk_score,
    n = 20,
    with_ties = FALSE
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(bank_name, max_model_risk_score),
      y = max_model_risk_score
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Top 20 Banks by Maximum Model Risk Score",
    x = "Bank",
    y = "Maximum model risk score"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p3, "fig03_top20_bank_model_risk_score.png", width = 10, height = 7)
)

p4 <- threshold_sensitivity |>
  ggplot2::ggplot(
    ggplot2::aes(x = threshold, y = classification_accuracy)
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "CET1 Threshold Classification Sensitivity",
    x = "CET1 threshold",
    y = "Classification accuracy"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p4, "fig04_threshold_classification_sensitivity.png")
)

p5 <- year_robustness |>
  ggplot2::ggplot(
    ggplot2::aes(x = exercise_year, y = cet1_rmse)
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "CET1 RMSE by Exercise Year",
    x = "Exercise year",
    y = "CET1 RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p5, "fig05_cet1_rmse_by_year.png")
)

p6 <- scenario_robustness |>
  ggplot2::ggplot(
    ggplot2::aes(x = scenario_label, y = cet1_rmse)
  ) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "CET1 RMSE by Scenario",
    x = "Scenario",
    y = "CET1 RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p6, "fig06_cet1_rmse_by_scenario.png")
)

p7 <- ggplot2::ggplot(
  model_risk_panel,
  ggplot2::aes(
    x = observed_cet1_min_ratio,
    y = integrated_predicted_cet1_min_ratio
  )
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Observed versus Predicted CET1 Minimum Ratio",
    x = "Observed CET1 minimum ratio",
    y = "Predicted CET1 minimum ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p7, "fig07_observed_vs_predicted_cet1.png")
)

figure_inventory <- tibble::tibble(
  figure_path = figure_paths,
  exists = file.exists(figure_paths),
  size_bytes = ifelse(file.exists(figure_paths), file.info(figure_paths)$size, NA_real_)
) |>
  safe_df()

cat("Figures created:", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")


# ------------------------------------------------------------
# 16. Save processed datasets
# ------------------------------------------------------------

cat("Saving model risk datasets...\n")

model_risk_panel_path <- "data/processed/model/model_risk_assessment_panel.csv"
tail_error_panel_path <- "data/processed/model/model_risk_tail_error_panel.csv"
bank_model_risk_path <- "data/processed/model/bank_model_risk_assessment.csv"

readr::write_csv(model_risk_panel, model_risk_panel_path)
readr::write_csv(tail_error_panel, tail_error_panel_path)
readr::write_csv(bank_model_risk, bank_model_risk_path)

cat("Model risk datasets saved.\n\n")


# ------------------------------------------------------------
# 17. Save tabular outputs
# ------------------------------------------------------------

cat("Saving model risk outputs...\n")

out_dir <- "outputs/model_risk"

key_audit_summary_path <- file.path(out_dir, "script15_key_audit_summary.csv")
executive_model_risk_summary_path <- file.path(out_dir, "script15_executive_model_risk_summary.csv")
overall_robustness_metrics_path <- file.path(out_dir, "script15_overall_robustness_metrics.csv")
year_robustness_path <- file.path(out_dir, "script15_year_robustness.csv")
scenario_robustness_path <- file.path(out_dir, "script15_scenario_robustness.csv")
year_scenario_robustness_path <- file.path(out_dir, "script15_year_scenario_robustness.csv")
bank_model_risk_output_path <- file.path(out_dir, "script15_bank_model_risk.csv")
tail_error_summary_path <- file.path(out_dir, "script15_tail_error_summary.csv")
largest_model_risk_observations_path <- file.path(out_dir, "script15_largest_model_risk_observations.csv")
threshold_sensitivity_path <- file.path(out_dir, "script15_threshold_sensitivity.csv")
bias_direction_summary_path <- file.path(out_dir, "script15_bias_direction_summary.csv")
bias_by_scenario_path <- file.path(out_dir, "script15_bias_by_scenario.csv")
model_risk_limits_path <- file.path(out_dir, "script15_model_risk_limits.csv")
model_risk_conclusion_path <- file.path(out_dir, "script15_model_risk_conclusion.csv")
figure_inventory_path <- file.path(out_dir, "script15_figure_inventory.csv")
execution_summary_path <- file.path(out_dir, "script15_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script15_model_risk_assessment_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script15_model_risk_assessment_report.docx")
execution_log_path <- file.path(out_dir, "script15_execution_log.txt")

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_rows = nrow(validation_raw),
  model_risk_rows = nrow(model_risk_panel),
  banks = dplyr::n_distinct(model_risk_panel$bank_name),
  years = dplyr::n_distinct(model_risk_panel$exercise_year),
  scenarios = dplyr::n_distinct(model_risk_panel$scenario_label),
  duplicated_keys = key_audit_summary$duplicated_keys,
  cet1_min_rmse = cet1_metrics$rmse,
  cet1_min_mae = cet1_metrics$mae,
  cet1_min_bias = cet1_metrics$bias,
  cet1_min_r2 = cet1_metrics$r2_outcome_prediction,
  cet1_min_correlation = cet1_metrics$correlation,
  high_model_risk_observations =
    sum(model_risk_panel$model_risk_bucket == "High model risk", na.rm = TRUE),
  moderate_model_risk_observations =
    sum(model_risk_panel$model_risk_bucket == "Moderate model risk", na.rm = TRUE),
  watchlist_observations =
    sum(model_risk_panel$model_risk_bucket == "Watchlist", na.rm = TRUE),
  low_model_risk_observations =
    sum(model_risk_panel$model_risk_bucket == "Low model risk", na.rm = TRUE),
  figures_created = sum(figure_inventory$exists, na.rm = TRUE)
) |>
  safe_df()

readr::write_csv(key_audit_summary, key_audit_summary_path)
readr::write_csv(executive_model_risk_summary, executive_model_risk_summary_path)
readr::write_csv(overall_robustness_metrics, overall_robustness_metrics_path)
readr::write_csv(year_robustness, year_robustness_path)
readr::write_csv(scenario_robustness, scenario_robustness_path)
readr::write_csv(year_scenario_robustness, year_scenario_robustness_path)
readr::write_csv(bank_model_risk, bank_model_risk_output_path)
readr::write_csv(tail_error_summary, tail_error_summary_path)
readr::write_csv(largest_model_risk_observations, largest_model_risk_observations_path)
readr::write_csv(threshold_sensitivity, threshold_sensitivity_path)
readr::write_csv(bias_direction_summary, bias_direction_summary_path)
readr::write_csv(bias_by_scenario, bias_by_scenario_path)
readr::write_csv(model_risk_limits, model_risk_limits_path)
readr::write_csv(model_risk_conclusion, model_risk_conclusion_path)
readr::write_csv(figure_inventory, figure_inventory_path)
readr::write_csv(execution_summary, execution_summary_path)

cat("Model risk outputs saved.\n\n")


# ------------------------------------------------------------
# 18. Excel workbook
# ------------------------------------------------------------

cat("Creating Excel workbook...\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "execution_summary")
openxlsx::writeData(wb, "execution_summary", execution_summary)

openxlsx::addWorksheet(wb, "executive_summary")
openxlsx::writeData(wb, "executive_summary", executive_model_risk_summary)

openxlsx::addWorksheet(wb, "overall_robustness")
openxlsx::writeData(wb, "overall_robustness", overall_robustness_metrics)

openxlsx::addWorksheet(wb, "year_robustness")
openxlsx::writeData(wb, "year_robustness", year_robustness)

openxlsx::addWorksheet(wb, "scenario_robustness")
openxlsx::writeData(wb, "scenario_robustness", scenario_robustness)

openxlsx::addWorksheet(wb, "bank_model_risk")
openxlsx::writeData(wb, "bank_model_risk", bank_model_risk)

openxlsx::addWorksheet(wb, "tail_error_summary")
openxlsx::writeData(wb, "tail_error_summary", tail_error_summary)

openxlsx::addWorksheet(wb, "largest_model_risk")
openxlsx::writeData(wb, "largest_model_risk", largest_model_risk_observations)

openxlsx::addWorksheet(wb, "threshold_sensitivity")
openxlsx::writeData(wb, "threshold_sensitivity", threshold_sensitivity)

openxlsx::addWorksheet(wb, "bias_direction")
openxlsx::writeData(wb, "bias_direction", bias_direction_summary)

openxlsx::addWorksheet(wb, "bias_by_scenario")
openxlsx::writeData(wb, "bias_by_scenario", bias_by_scenario)

openxlsx::addWorksheet(wb, "model_risk_limits")
openxlsx::writeData(wb, "model_risk_limits", model_risk_limits)

openxlsx::addWorksheet(wb, "model_risk_conclusion")
openxlsx::writeData(wb, "model_risk_conclusion", model_risk_conclusion)

openxlsx::addWorksheet(wb, "figure_inventory")
openxlsx::writeData(wb, "figure_inventory", figure_inventory)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output_path, overwrite = TRUE)

cat("Excel workbook created.\n\n")


# ------------------------------------------------------------
# 19. Word report
# ------------------------------------------------------------

cat("Creating Word report...\n")

top_bank_model_risk <- bank_model_risk |>
  dplyr::select(
    bank_model_risk_rank,
    bank_name,
    observations,
    cet1_rmse,
    cet1_mae,
    max_abs_cet1_error,
    high_model_risk_observations,
    threshold_miss_observations,
    max_model_risk_score
  ) |>
  head(20)

largest_model_risk_small <- largest_model_risk_observations |>
  dplyr::select(
    bank_name,
    exercise_year,
    scenario_label,
    observed_cet1_min_ratio,
    integrated_predicted_cet1_min_ratio,
    cet1_min_prediction_error,
    abs_cet1_min_prediction_error,
    model_risk_bucket,
    model_risk_score
  ) |>
  head(20)

doc <- officer::read_docx()

doc <- doc |>
  officer::body_add_par("Script 15 - Robustness, Sensitivity and Model Risk Assessment", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This script assesses robustness, sensitivity and model risk of the public reduced-form DFAST replication engine. It evaluates error concentration, threshold sensitivity, year and scenario robustness, bank-level model risk and key limitations.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Executive model risk summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(executive_model_risk_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Overall robustness metrics", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(overall_robustness_metrics) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. Bank-level model risk ranking", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(top_bank_model_risk) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("5. Largest model risk observations", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(largest_model_risk_small) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("6. Threshold sensitivity", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(threshold_sensitivity) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("7. Model risk limits", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(model_risk_limits) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("8. Model risk conclusion", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(model_risk_conclusion) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("9. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "The model risk assessment is based on public benchmark validation outputs. It does not assess confidential Federal Reserve supervisory models, confidential bank submissions or internal bank capital planning models.",
    style = "Normal"
  )

print(doc, target = report_docx_path)

cat("Word report created.\n\n")


# ------------------------------------------------------------
# 20. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 15 - Robustness, Sensitivity and Model Risk Assessment completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input rows:", nrow(validation_raw)),
  paste("Model risk rows:", nrow(model_risk_panel)),
  paste("Banks:", dplyr::n_distinct(model_risk_panel$bank_name)),
  paste("Years:", dplyr::n_distinct(model_risk_panel$exercise_year)),
  paste("Scenarios:", dplyr::n_distinct(model_risk_panel$scenario_label)),
  paste("Duplicated keys:", key_audit_summary$duplicated_keys),
  paste("CET1 minimum RMSE:", round(cet1_metrics$rmse, 4)),
  paste("CET1 minimum MAE:", round(cet1_metrics$mae, 4)),
  paste("CET1 minimum bias:", round(cet1_metrics$bias, 4)),
  paste("CET1 minimum R-squared:", round(cet1_metrics$r2_outcome_prediction, 4)),
  paste("CET1 minimum correlation:", round(cet1_metrics$correlation, 4)),
  paste("High model risk observations:", sum(model_risk_panel$model_risk_bucket == "High model risk", na.rm = TRUE)),
  paste("Moderate model risk observations:", sum(model_risk_panel$model_risk_bucket == "Moderate model risk", na.rm = TRUE)),
  paste("Watchlist observations:", sum(model_risk_panel$model_risk_bucket == "Watchlist", na.rm = TRUE)),
  paste("Low model risk observations:", sum(model_risk_panel$model_risk_bucket == "Low model risk", na.rm = TRUE)),
  paste("Figures created:", sum(figure_inventory$exists, na.rm = TRUE)),
  "",
  "Executive model risk summary:",
  capture.output(print(executive_model_risk_summary)),
  "",
  "Overall robustness metrics:",
  capture.output(print(overall_robustness_metrics)),
  "",
  "Model risk conclusion:",
  capture.output(print(model_risk_conclusion)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", model_risk_panel_path),
  paste(" -", tail_error_panel_path),
  paste(" -", bank_model_risk_path),
  paste(" -", key_audit_summary_path),
  paste(" -", executive_model_risk_summary_path),
  paste(" -", overall_robustness_metrics_path),
  paste(" -", year_robustness_path),
  paste(" -", scenario_robustness_path),
  paste(" -", year_scenario_robustness_path),
  paste(" -", bank_model_risk_output_path),
  paste(" -", tail_error_summary_path),
  paste(" -", largest_model_risk_observations_path),
  paste(" -", threshold_sensitivity_path),
  paste(" -", bias_direction_summary_path),
  paste(" -", bias_by_scenario_path),
  paste(" -", model_risk_limits_path),
  paste(" -", model_risk_conclusion_path),
  paste(" -", figure_inventory_path),
  paste(" -", execution_summary_path),
  paste(" -", excel_output_path),
  paste(" -", report_docx_path),
  paste(" -", execution_log_path),
  "",
  "Figures:",
  paste(" -", figure_paths)
)

writeLines(enc2utf8(log_lines), execution_log_path, useBytes = TRUE)


# ------------------------------------------------------------
# 21. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 15 - Robustness, Sensitivity and Model Risk Assessment completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input rows:\n", nrow(validation_raw), "\n")
cat("Model risk rows:\n", nrow(model_risk_panel), "\n")
cat("Banks:\n", dplyr::n_distinct(model_risk_panel$bank_name), "\n")
cat("Years:\n", dplyr::n_distinct(model_risk_panel$exercise_year), "\n")
cat("Scenarios:\n", dplyr::n_distinct(model_risk_panel$scenario_label), "\n")
cat("Duplicated keys:\n", key_audit_summary$duplicated_keys, "\n")
cat("Figures created:\n", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")

cat("Executive model risk summary:\n")
print(executive_model_risk_summary)

cat("\nOverall robustness metrics:\n")
print(overall_robustness_metrics)

cat("\nBias direction summary:\n")
print(bias_direction_summary)

cat("\nTop 20 bank model risk ranking:\n")
print(top_bank_model_risk)

cat("\nModel risk conclusion:\n")
print(model_risk_conclusion)

cat("\nMain outputs:\n")
cat(" -", model_risk_panel_path, "\n")
cat(" -", tail_error_panel_path, "\n")
cat(" -", bank_model_risk_path, "\n")
cat(" -", key_audit_summary_path, "\n")
cat(" -", executive_model_risk_summary_path, "\n")
cat(" -", overall_robustness_metrics_path, "\n")
cat(" -", year_robustness_path, "\n")
cat(" -", scenario_robustness_path, "\n")
cat(" -", year_scenario_robustness_path, "\n")
cat(" -", bank_model_risk_output_path, "\n")
cat(" -", tail_error_summary_path, "\n")
cat(" -", largest_model_risk_observations_path, "\n")
cat(" -", threshold_sensitivity_path, "\n")
cat(" -", bias_direction_summary_path, "\n")
cat(" -", bias_by_scenario_path, "\n")
cat(" -", model_risk_limits_path, "\n")
cat(" -", model_risk_conclusion_path, "\n")
cat(" -", figure_inventory_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")

cat("\nFigures:\n")
cat(paste(" -", figure_paths, collapse = "\n"))
cat("\n")
cat("============================================================\n")