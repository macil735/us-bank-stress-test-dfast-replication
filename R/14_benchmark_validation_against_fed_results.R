# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 14 — Benchmark Validation Against Federal Reserve Public Results
# ============================================================
# Objective:
#   Validate the integrated stress test projection engine against
#   public Federal Reserve DFAST benchmark outcomes.
#
# Main input:
#   data/processed/model/final_stress_test_results_panel.csv
#
# Main benchmark variables:
#   - observed CET1 minimum ratio
#   - predicted CET1 minimum ratio
#   - observed CET1 depletion
#   - predicted capital depletion
#   - actual and predicted credit loss ratios
#   - actual and predicted PPNR ratios
#
# Main outputs:
#   outputs/benchmark_validation/script14_benchmark_validation_report.docx
#   outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx
#   outputs/benchmark_validation/script14_execution_log.txt
#
# Methodological note:
#   This script validates a public reduced-form replication layer
#   against public DFAST benchmark outcomes. It does not validate
#   against confidential Federal Reserve supervisory models.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 14 - Benchmark Validation Against Fed Results\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "14"
script_name <- "benchmark_validation_against_fed_results"
start_time <- Sys.time()

setwd(project_root)

dir.create("outputs/benchmark_validation", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/benchmark_validation/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed/model", recursive = TRUE, showWarnings = FALSE)

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

safe_divide <- function(num, den) {
  ifelse(!is.na(den) & den != 0, num / den, NA_real_)
}

save_plot <- function(plot_object, file_name, width = 9, height = 5.5) {
  path <- file.path("outputs/benchmark_validation/figures", file_name)

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
    actual_variable = actual_col,
    predicted_variable = predicted_col,
    observations = sum(!is.na(actual) & !is.na(predicted)),
    actual_mean = safe_mean(actual),
    predicted_mean = safe_mean(predicted),
    actual_median = safe_median(actual),
    predicted_median = safe_median(predicted),
    actual_min = safe_min(actual),
    predicted_min = safe_min(predicted),
    actual_max = safe_max(actual),
    predicted_max = safe_max(predicted),
    bias = safe_bias(actual, predicted),
    rmse = safe_rmse(actual, predicted),
    mae = safe_mae(actual, predicted),
    r2_outcome_prediction = safe_r2(actual, predicted),
    correlation = safe_correlation(actual, predicted)
  )
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Read final stress test results
# ------------------------------------------------------------

cat("Reading final stress test results...\n")

input_path <- "data/processed/model/final_stress_test_results_panel.csv"

if (!file.exists(input_path)) {
  stop(paste("Missing input file:", input_path))
}

results_raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()

cat("Input loaded.\n")
cat("Rows:", nrow(results_raw), "\n")
cat("Columns:", ncol(results_raw), "\n\n")


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

  "actual_ppnr_ratio",
  "predicted_ppnr_ratio",

  "actual_capital_depletion",
  "predicted_capital_depletion",

  "observed_cet1_min_ratio",
  "integrated_predicted_cet1_min_ratio",

  "observed_cet1_depletion",
  "predicted_cet1_depletion",

  "integrated_predicted_cet1_gap",
  "capital_result_flag",
  "vulnerability_bucket"
)

required_column_check <- tibble::tibble(
  required_column = required_cols,
  exists = required_cols %in% names(results_raw)
) |>
  safe_df()

missing_required <- required_column_check |>
  dplyr::filter(!exists) |>
  dplyr::pull(required_column)

required_column_check_path <- "outputs/benchmark_validation/script14_required_column_check.csv"
readr::write_csv(required_column_check, required_column_check_path)

if (length(missing_required) > 0) {
  cat("Missing required variables:\n")
  print(missing_required)
  stop("Script 14 stopped because required variables are missing.")
}

cat("Required variables checked.\n\n")


# ------------------------------------------------------------
# 5. Key audit
# ------------------------------------------------------------

cat("Auditing benchmark validation keys...\n")

key_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label"
)

key_audit <- results_raw |>
  dplyr::count(dplyr::across(dplyr::all_of(key_cols)), name = "key_count") |>
  dplyr::mutate(
    duplicated_key = key_count > 1
  ) |>
  safe_df()

key_audit_summary <- tibble::tibble(
  rows = nrow(results_raw),
  unique_keys = nrow(key_audit),
  duplicated_keys = sum(key_audit$duplicated_key),
  max_rows_per_key = max(key_audit$key_count, na.rm = TRUE)
) |>
  safe_df()

if (key_audit_summary$duplicated_keys > 0) {
  print(key_audit_summary)
  stop("Input results contain duplicated bank-year-scenario keys.")
}

cat("Key audit completed.\n")
print(key_audit_summary)
cat("\n")


# ------------------------------------------------------------
# 6. Build benchmark validation panel
# ------------------------------------------------------------

cat("Building benchmark validation panel...\n")

benchmark_validation_panel <- results_raw |>
  dplyr::mutate(
    cet1_min_prediction_error =
      integrated_predicted_cet1_min_ratio - observed_cet1_min_ratio,

    cet1_depletion_prediction_error =
      predicted_capital_depletion - actual_capital_depletion,

    credit_loss_prediction_error =
      predicted_credit_loss_ratio - actual_credit_loss_ratio,

    ppnr_prediction_error =
      predicted_ppnr_ratio - actual_ppnr_ratio,

    abs_cet1_min_prediction_error =
      abs(cet1_min_prediction_error),

    abs_cet1_depletion_prediction_error =
      abs(cet1_depletion_prediction_error),

    abs_credit_loss_prediction_error =
      abs(credit_loss_prediction_error),

    abs_ppnr_prediction_error =
      abs(ppnr_prediction_error),

    cet1_min_error_bucket = dplyr::case_when(
      abs_cet1_min_prediction_error <= 0.50 ~ "Error <= 0.50 pp",
      abs_cet1_min_prediction_error <= 1.00 ~ "0.50 pp < error <= 1.00 pp",
      abs_cet1_min_prediction_error <= 2.00 ~ "1.00 pp < error <= 2.00 pp",
      TRUE ~ "Error > 2.00 pp"
    ),

    cet1_min_validation_flag = dplyr::case_when(
      abs_cet1_min_prediction_error <= 0.50 ~ "High precision",
      abs_cet1_min_prediction_error <= 1.00 ~ "Acceptable precision",
      abs_cet1_min_prediction_error <= 2.00 ~ "Moderate deviation",
      TRUE ~ "Large deviation"
    ),

    observed_below_4_5 =
      observed_cet1_min_ratio < 4.5,

    predicted_below_4_5 =
      integrated_predicted_cet1_min_ratio < 4.5,

    observed_below_7 =
      observed_cet1_min_ratio < 7.0,

    predicted_below_7 =
      integrated_predicted_cet1_min_ratio < 7.0,

    classification_match_4_5 =
      observed_below_4_5 == predicted_below_4_5,

    classification_match_7 =
      observed_below_7 == predicted_below_7
  ) |>
  safe_df()

cat("Benchmark validation panel created.\n\n")


# ------------------------------------------------------------
# 7. Overall validation metrics
# ------------------------------------------------------------

cat("Creating overall validation metrics...\n")

overall_validation_metrics <- dplyr::bind_rows(
  validation_stats(
    benchmark_validation_panel,
    "actual_credit_loss_ratio",
    "predicted_credit_loss_ratio",
    "Credit loss ratio"
  ),
  validation_stats(
    benchmark_validation_panel,
    "actual_ppnr_ratio",
    "predicted_ppnr_ratio",
    "PPNR ratio"
  ),
  validation_stats(
    benchmark_validation_panel,
    "actual_capital_depletion",
    "predicted_capital_depletion",
    "CET1 capital depletion"
  ),
  validation_stats(
    benchmark_validation_panel,
    "observed_cet1_min_ratio",
    "integrated_predicted_cet1_min_ratio",
    "CET1 minimum ratio"
  )
) |>
  safe_df()

cat("Overall validation metrics created.\n\n")


# ------------------------------------------------------------
# 8. Scenario validation metrics
# ------------------------------------------------------------

cat("Creating scenario validation metrics...\n")

scenario_validation_metrics <- benchmark_validation_panel |>
  dplyr::group_by(exercise_year, scenario_code, scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    banks = dplyr::n_distinct(bank_name),

    cet1_min_rmse =
      safe_rmse(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_mae =
      safe_mae(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_bias =
      safe_bias(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_r2 =
      safe_r2(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_correlation =
      safe_correlation(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),

    capital_depletion_rmse =
      safe_rmse(actual_capital_depletion, predicted_capital_depletion),
    credit_loss_rmse =
      safe_rmse(actual_credit_loss_ratio, predicted_credit_loss_ratio),
    ppnr_rmse =
      safe_rmse(actual_ppnr_ratio, predicted_ppnr_ratio),

    mean_observed_cet1_min_ratio =
      safe_mean(observed_cet1_min_ratio),
    mean_predicted_cet1_min_ratio =
      safe_mean(integrated_predicted_cet1_min_ratio),

    min_observed_cet1_min_ratio =
      safe_min(observed_cet1_min_ratio),
    min_predicted_cet1_min_ratio =
      safe_min(integrated_predicted_cet1_min_ratio),

    high_precision_share =
      mean(cet1_min_validation_flag == "High precision", na.rm = TRUE),
    acceptable_or_better_share =
      mean(cet1_min_validation_flag %in% c("High precision", "Acceptable precision"), na.rm = TRUE),

    classification_accuracy_4_5 =
      mean(classification_match_4_5, na.rm = TRUE),
    classification_accuracy_7 =
      mean(classification_match_7, na.rm = TRUE),

    .groups = "drop"
  ) |>
  dplyr::arrange(exercise_year, scenario_code) |>
  safe_df()

cat("Scenario validation metrics created.\n\n")


# ------------------------------------------------------------
# 9. Bank validation metrics
# ------------------------------------------------------------

cat("Creating bank validation metrics...\n")

bank_validation_metrics <- benchmark_validation_panel |>
  dplyr::group_by(bank_rssd_id, bank_name) |>
  dplyr::summarise(
    observations = dplyr::n(),
    years = dplyr::n_distinct(exercise_year),
    scenarios = dplyr::n_distinct(scenario_label),

    cet1_min_rmse =
      safe_rmse(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_mae =
      safe_mae(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_bias =
      safe_bias(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_r2 =
      safe_r2(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),
    cet1_min_correlation =
      safe_correlation(observed_cet1_min_ratio, integrated_predicted_cet1_min_ratio),

    capital_depletion_rmse =
      safe_rmse(actual_capital_depletion, predicted_capital_depletion),
    credit_loss_rmse =
      safe_rmse(actual_credit_loss_ratio, predicted_credit_loss_ratio),
    ppnr_rmse =
      safe_rmse(actual_ppnr_ratio, predicted_ppnr_ratio),

    mean_abs_cet1_min_error =
      safe_mean(abs_cet1_min_prediction_error),
    max_abs_cet1_min_error =
      safe_max(abs_cet1_min_prediction_error),

    high_precision_share =
      mean(cet1_min_validation_flag == "High precision", na.rm = TRUE),
    acceptable_or_better_share =
      mean(cet1_min_validation_flag %in% c("High precision", "Acceptable precision"), na.rm = TRUE),

    classification_accuracy_4_5 =
      mean(classification_match_4_5, na.rm = TRUE),
    classification_accuracy_7 =
      mean(classification_match_7, na.rm = TRUE),

    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(cet1_min_rmse)) |>
  safe_df()

cat("Bank validation metrics created.\n\n")


# ------------------------------------------------------------
# 10. Error distribution tables
# ------------------------------------------------------------

cat("Creating error distribution tables...\n")

error_bucket_summary <- benchmark_validation_panel |>
  dplyr::count(cet1_min_error_bucket, cet1_min_validation_flag, name = "observations") |>
  dplyr::mutate(
    share = observations / sum(observations)
  ) |>
  dplyr::arrange(cet1_min_error_bucket) |>
  safe_df()

validation_flag_by_scenario <- benchmark_validation_panel |>
  dplyr::count(exercise_year, scenario_code, scenario_label, cet1_min_validation_flag, name = "observations") |>
  dplyr::group_by(exercise_year, scenario_code, scenario_label) |>
  dplyr::mutate(
    share = observations / sum(observations)
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(exercise_year, scenario_code, cet1_min_validation_flag) |>
  safe_df()

largest_cet1_errors <- benchmark_validation_panel |>
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
    observed_cet1_depletion,
    predicted_capital_depletion,
    actual_credit_loss_ratio,
    predicted_credit_loss_ratio,
    actual_ppnr_ratio,
    predicted_ppnr_ratio
  ) |>
  dplyr::arrange(dplyr::desc(abs_cet1_min_prediction_error)) |>
  safe_df()

cat("Error distribution tables created.\n\n")


# ------------------------------------------------------------
# 11. Threshold classification validation
# ------------------------------------------------------------

cat("Creating threshold classification validation...\n")

threshold_classification_summary <- tibble::tibble(
  threshold = c("CET1 4.5 percent", "CET1 7.0 percent"),
  observations = c(
    sum(!is.na(benchmark_validation_panel$observed_below_4_5) &
          !is.na(benchmark_validation_panel$predicted_below_4_5)),
    sum(!is.na(benchmark_validation_panel$observed_below_7) &
          !is.na(benchmark_validation_panel$predicted_below_7))
  ),
  observed_below_threshold = c(
    sum(benchmark_validation_panel$observed_below_4_5, na.rm = TRUE),
    sum(benchmark_validation_panel$observed_below_7, na.rm = TRUE)
  ),
  predicted_below_threshold = c(
    sum(benchmark_validation_panel$predicted_below_4_5, na.rm = TRUE),
    sum(benchmark_validation_panel$predicted_below_7, na.rm = TRUE)
  ),
  correctly_classified = c(
    sum(benchmark_validation_panel$classification_match_4_5, na.rm = TRUE),
    sum(benchmark_validation_panel$classification_match_7, na.rm = TRUE)
  ),
  classification_accuracy = c(
    mean(benchmark_validation_panel$classification_match_4_5, na.rm = TRUE),
    mean(benchmark_validation_panel$classification_match_7, na.rm = TRUE)
  )
) |>
  safe_df()

threshold_confusion_long <- benchmark_validation_panel |>
  dplyr::transmute(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,

    observed_below_4_5,
    predicted_below_4_5,
    observed_below_7,
    predicted_below_7
  ) |>
  tidyr::pivot_longer(
    cols = c(
      observed_below_4_5,
      predicted_below_4_5,
      observed_below_7,
      predicted_below_7
    ),
    names_to = "classification_variable",
    values_to = "classification_value"
  ) |>
  safe_df()

cat("Threshold classification validation created.\n\n")


# ------------------------------------------------------------
# 12. Latest exercise validation
# ------------------------------------------------------------

cat("Creating latest exercise validation...\n")

latest_year <- max(benchmark_validation_panel$exercise_year, na.rm = TRUE)

latest_validation_panel <- benchmark_validation_panel |>
  dplyr::filter(exercise_year == latest_year) |>
  safe_df()

latest_validation_metrics <- dplyr::bind_rows(
  validation_stats(
    latest_validation_panel,
    "actual_credit_loss_ratio",
    "predicted_credit_loss_ratio",
    "Latest credit loss ratio"
  ),
  validation_stats(
    latest_validation_panel,
    "actual_ppnr_ratio",
    "predicted_ppnr_ratio",
    "Latest PPNR ratio"
  ),
  validation_stats(
    latest_validation_panel,
    "actual_capital_depletion",
    "predicted_capital_depletion",
    "Latest CET1 capital depletion"
  ),
  validation_stats(
    latest_validation_panel,
    "observed_cet1_min_ratio",
    "integrated_predicted_cet1_min_ratio",
    "Latest CET1 minimum ratio"
  )
) |>
  safe_df()

latest_largest_errors <- latest_validation_panel |>
  dplyr::select(
    bank_name,
    scenario_label,
    observed_cet1_min_ratio,
    integrated_predicted_cet1_min_ratio,
    cet1_min_prediction_error,
    abs_cet1_min_prediction_error,
    cet1_min_validation_flag
  ) |>
  dplyr::arrange(dplyr::desc(abs_cet1_min_prediction_error)) |>
  head(20) |>
  safe_df()

cat("Latest exercise validation created.\n")
cat("Latest exercise year:", latest_year, "\n\n")


# ------------------------------------------------------------
# 13. Executive validation summary
# ------------------------------------------------------------

cat("Creating executive validation summary...\n")

cet1_overall <- overall_validation_metrics |>
  dplyr::filter(component == "CET1 minimum ratio")

executive_validation_summary <- tibble::tibble(
  metric = c(
    "Benchmark validation observations",
    "Banks",
    "Exercise years",
    "Scenarios",
    "Latest exercise year",
    "CET1 minimum ratio RMSE",
    "CET1 minimum ratio MAE",
    "CET1 minimum ratio bias",
    "CET1 minimum ratio R-squared",
    "CET1 minimum ratio correlation",
    "High precision share",
    "Acceptable or better share",
    "CET1 4.5 percent classification accuracy",
    "CET1 7.0 percent classification accuracy",
    "Largest absolute CET1 minimum ratio error"
  ),
  value = c(
    as.character(nrow(benchmark_validation_panel)),
    as.character(dplyr::n_distinct(benchmark_validation_panel$bank_name)),
    as.character(dplyr::n_distinct(benchmark_validation_panel$exercise_year)),
    as.character(dplyr::n_distinct(benchmark_validation_panel$scenario_label)),
    as.character(latest_year),
    as.character(round(cet1_overall$rmse, 4)),
    as.character(round(cet1_overall$mae, 4)),
    as.character(round(cet1_overall$bias, 4)),
    as.character(round(cet1_overall$r2_outcome_prediction, 4)),
    as.character(round(cet1_overall$correlation, 4)),
    as.character(round(mean(benchmark_validation_panel$cet1_min_validation_flag == "High precision", na.rm = TRUE), 4)),
    as.character(round(mean(benchmark_validation_panel$cet1_min_validation_flag %in% c("High precision", "Acceptable precision"), na.rm = TRUE), 4)),
    as.character(round(mean(benchmark_validation_panel$classification_match_4_5, na.rm = TRUE), 4)),
    as.character(round(mean(benchmark_validation_panel$classification_match_7, na.rm = TRUE), 4)),
    as.character(round(safe_max(benchmark_validation_panel$abs_cet1_min_prediction_error), 4))
  )
) |>
  safe_df()

cat("Executive validation summary created.\n\n")


# ------------------------------------------------------------
# 14. Figures
# ------------------------------------------------------------

cat("Creating benchmark validation figures...\n")

figure_paths <- c()

p1 <- ggplot2::ggplot(
  benchmark_validation_panel,
  ggplot2::aes(
    x = observed_cet1_min_ratio,
    y = integrated_predicted_cet1_min_ratio
  )
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Benchmark Validation: Observed versus Predicted CET1 Minimum Ratio",
    x = "Observed Fed DFAST CET1 minimum ratio",
    y = "Predicted CET1 minimum ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p1, "fig01_observed_vs_predicted_cet1_min_ratio.png")
)

p2 <- ggplot2::ggplot(
  benchmark_validation_panel,
  ggplot2::aes(x = cet1_min_prediction_error)
) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::labs(
    title = "Distribution of CET1 Minimum Ratio Prediction Errors",
    x = "Predicted minus observed CET1 minimum ratio",
    y = "Number of observations"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p2, "fig02_cet1_prediction_error_distribution.png")
)

p3 <- overall_validation_metrics |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(component, rmse),
      y = rmse
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Benchmark Validation RMSE by Component",
    x = "Component",
    y = "RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p3, "fig03_validation_rmse_by_component.png")
)

p4 <- error_bucket_summary |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = cet1_min_validation_flag,
      y = observations
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = "CET1 Minimum Ratio Validation Precision Buckets",
    x = "Validation flag",
    y = "Observations"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p4, "fig04_cet1_validation_precision_buckets.png")
)

p5 <- bank_validation_metrics |>
  dplyr::slice_max(
    order_by = cet1_min_rmse,
    n = 20,
    with_ties = FALSE
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(bank_name, cet1_min_rmse),
      y = cet1_min_rmse
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Top 20 Bank-Level CET1 Minimum Ratio RMSE",
    x = "Bank",
    y = "CET1 minimum ratio RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p5, "fig05_top20_bank_cet1_validation_rmse.png", width = 10, height = 7)
)

p6 <- scenario_validation_metrics |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = exercise_year,
      y = cet1_min_rmse,
      group = scenario_label
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "CET1 Minimum Ratio RMSE by Exercise Year and Scenario",
    x = "Exercise year",
    y = "CET1 minimum ratio RMSE",
    group = "Scenario"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p6, "fig06_cet1_rmse_by_year_scenario.png")
)

p7 <- ggplot2::ggplot(
  benchmark_validation_panel,
  ggplot2::aes(
    x = observed_cet1_depletion,
    y = predicted_capital_depletion
  )
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Observed versus Predicted CET1 Capital Depletion",
    x = "Observed CET1 depletion",
    y = "Predicted CET1 depletion"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p7, "fig07_observed_vs_predicted_capital_depletion.png")
)

figure_inventory <- tibble::tibble(
  figure_path = figure_paths,
  exists = file.exists(figure_paths),
  size_bytes = ifelse(file.exists(figure_paths), file.info(figure_paths)$size, NA_real_)
) |>
  safe_df()

cat("Figures created:", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")


# ------------------------------------------------------------
# 15. Save processed datasets
# ------------------------------------------------------------

cat("Saving benchmark validation datasets...\n")

benchmark_validation_panel_path <- "data/processed/model/benchmark_validation_panel.csv"
benchmark_largest_errors_path <- "data/processed/model/benchmark_validation_largest_cet1_errors.csv"

readr::write_csv(benchmark_validation_panel, benchmark_validation_panel_path)
readr::write_csv(largest_cet1_errors, benchmark_largest_errors_path)

cat("Benchmark validation datasets saved.\n\n")


# ------------------------------------------------------------
# 16. Save tabular outputs
# ------------------------------------------------------------

cat("Saving benchmark validation outputs...\n")

out_dir <- "outputs/benchmark_validation"

key_audit_summary_path <- file.path(out_dir, "script14_key_audit_summary.csv")
executive_validation_summary_path <- file.path(out_dir, "script14_executive_validation_summary.csv")
overall_validation_metrics_path <- file.path(out_dir, "script14_overall_validation_metrics.csv")
scenario_validation_metrics_path <- file.path(out_dir, "script14_scenario_validation_metrics.csv")
bank_validation_metrics_path <- file.path(out_dir, "script14_bank_validation_metrics.csv")
error_bucket_summary_path <- file.path(out_dir, "script14_error_bucket_summary.csv")
validation_flag_by_scenario_path <- file.path(out_dir, "script14_validation_flag_by_scenario.csv")
largest_cet1_errors_path <- file.path(out_dir, "script14_largest_cet1_errors.csv")
threshold_classification_summary_path <- file.path(out_dir, "script14_threshold_classification_summary.csv")
latest_validation_metrics_path <- file.path(out_dir, "script14_latest_validation_metrics.csv")
latest_largest_errors_path <- file.path(out_dir, "script14_latest_largest_errors.csv")
figure_inventory_path <- file.path(out_dir, "script14_figure_inventory.csv")
execution_summary_path <- file.path(out_dir, "script14_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script14_benchmark_validation_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script14_benchmark_validation_report.docx")
execution_log_path <- file.path(out_dir, "script14_execution_log.txt")

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_rows = nrow(results_raw),
  validation_rows = nrow(benchmark_validation_panel),
  banks = dplyr::n_distinct(benchmark_validation_panel$bank_name),
  years = dplyr::n_distinct(benchmark_validation_panel$exercise_year),
  scenarios = dplyr::n_distinct(benchmark_validation_panel$scenario_label),
  latest_exercise_year = latest_year,
  duplicated_keys = key_audit_summary$duplicated_keys,
  cet1_min_rmse = cet1_overall$rmse,
  cet1_min_mae = cet1_overall$mae,
  cet1_min_bias = cet1_overall$bias,
  cet1_min_r2 = cet1_overall$r2_outcome_prediction,
  cet1_min_correlation = cet1_overall$correlation,
  figures_created = sum(figure_inventory$exists, na.rm = TRUE)
) |>
  safe_df()

readr::write_csv(key_audit_summary, key_audit_summary_path)
readr::write_csv(executive_validation_summary, executive_validation_summary_path)
readr::write_csv(overall_validation_metrics, overall_validation_metrics_path)
readr::write_csv(scenario_validation_metrics, scenario_validation_metrics_path)
readr::write_csv(bank_validation_metrics, bank_validation_metrics_path)
readr::write_csv(error_bucket_summary, error_bucket_summary_path)
readr::write_csv(validation_flag_by_scenario, validation_flag_by_scenario_path)
readr::write_csv(largest_cet1_errors, largest_cet1_errors_path)
readr::write_csv(threshold_classification_summary, threshold_classification_summary_path)
readr::write_csv(latest_validation_metrics, latest_validation_metrics_path)
readr::write_csv(latest_largest_errors, latest_largest_errors_path)
readr::write_csv(figure_inventory, figure_inventory_path)
readr::write_csv(execution_summary, execution_summary_path)

cat("Benchmark validation outputs saved.\n\n")


# ------------------------------------------------------------
# 17. Excel workbook
# ------------------------------------------------------------

cat("Creating Excel workbook...\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "execution_summary")
openxlsx::writeData(wb, "execution_summary", execution_summary)

openxlsx::addWorksheet(wb, "executive_summary")
openxlsx::writeData(wb, "executive_summary", executive_validation_summary)

openxlsx::addWorksheet(wb, "overall_validation")
openxlsx::writeData(wb, "overall_validation", overall_validation_metrics)

openxlsx::addWorksheet(wb, "scenario_validation")
openxlsx::writeData(wb, "scenario_validation", scenario_validation_metrics)

openxlsx::addWorksheet(wb, "bank_validation")
openxlsx::writeData(wb, "bank_validation", bank_validation_metrics)

openxlsx::addWorksheet(wb, "error_buckets")
openxlsx::writeData(wb, "error_buckets", error_bucket_summary)

openxlsx::addWorksheet(wb, "threshold_classification")
openxlsx::writeData(wb, "threshold_classification", threshold_classification_summary)

openxlsx::addWorksheet(wb, "largest_cet1_errors")
openxlsx::writeData(wb, "largest_cet1_errors", largest_cet1_errors)

openxlsx::addWorksheet(wb, "latest_validation")
openxlsx::writeData(wb, "latest_validation", latest_validation_metrics)

openxlsx::addWorksheet(wb, "latest_largest_errors")
openxlsx::writeData(wb, "latest_largest_errors", latest_largest_errors)

openxlsx::addWorksheet(wb, "figure_inventory")
openxlsx::writeData(wb, "figure_inventory", figure_inventory)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output_path, overwrite = TRUE)

cat("Excel workbook created.\n\n")


# ------------------------------------------------------------
# 18. Word report
# ------------------------------------------------------------

cat("Creating Word report...\n")

largest_errors_small <- largest_cet1_errors |>
  dplyr::select(
    bank_name,
    exercise_year,
    scenario_label,
    observed_cet1_min_ratio,
    integrated_predicted_cet1_min_ratio,
    cet1_min_prediction_error,
    abs_cet1_min_prediction_error,
    cet1_min_validation_flag
  ) |>
  head(20)

top_bank_validation <- bank_validation_metrics |>
  dplyr::select(
    bank_name,
    observations,
    cet1_min_rmse,
    cet1_min_mae,
    cet1_min_bias,
    mean_abs_cet1_min_error,
    max_abs_cet1_min_error,
    acceptable_or_better_share
  ) |>
  head(20)

doc <- officer::read_docx()

doc <- doc |>
  officer::body_add_par("Script 14 - Benchmark Validation Against Federal Reserve Public Results", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This script validates the integrated public stress test projection engine against Federal Reserve DFAST public benchmark outcomes. The validation focuses on CET1 minimum ratios, capital depletion, credit loss ratios and PPNR ratios.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Executive validation summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(executive_validation_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Overall validation metrics", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(overall_validation_metrics) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. Threshold classification summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(threshold_classification_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("5. Largest CET1 minimum ratio prediction errors", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(largest_errors_small) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("6. Bank-level validation ranking", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(top_bank_validation) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("7. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "The benchmark validation compares public model projections with public DFAST outcome variables. It does not validate against confidential Federal Reserve models, confidential supervisory assumptions or bank internal capital planning systems.",
    style = "Normal"
  )

print(doc, target = report_docx_path)

cat("Word report created.\n\n")


# ------------------------------------------------------------
# 19. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 14 - Benchmark Validation Against Federal Reserve Public Results completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input rows:", nrow(results_raw)),
  paste("Validation rows:", nrow(benchmark_validation_panel)),
  paste("Banks:", dplyr::n_distinct(benchmark_validation_panel$bank_name)),
  paste("Years:", dplyr::n_distinct(benchmark_validation_panel$exercise_year)),
  paste("Scenarios:", dplyr::n_distinct(benchmark_validation_panel$scenario_label)),
  paste("Latest exercise year:", latest_year),
  paste("Duplicated keys:", key_audit_summary$duplicated_keys),
  paste("CET1 minimum RMSE:", round(cet1_overall$rmse, 4)),
  paste("CET1 minimum MAE:", round(cet1_overall$mae, 4)),
  paste("CET1 minimum bias:", round(cet1_overall$bias, 4)),
  paste("CET1 minimum R-squared:", round(cet1_overall$r2_outcome_prediction, 4)),
  paste("CET1 minimum correlation:", round(cet1_overall$correlation, 4)),
  paste("Figures created:", sum(figure_inventory$exists, na.rm = TRUE)),
  "",
  "Executive validation summary:",
  capture.output(print(executive_validation_summary)),
  "",
  "Overall validation metrics:",
  capture.output(print(overall_validation_metrics)),
  "",
  "Threshold classification summary:",
  capture.output(print(threshold_classification_summary)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", benchmark_validation_panel_path),
  paste(" -", benchmark_largest_errors_path),
  paste(" -", key_audit_summary_path),
  paste(" -", executive_validation_summary_path),
  paste(" -", overall_validation_metrics_path),
  paste(" -", scenario_validation_metrics_path),
  paste(" -", bank_validation_metrics_path),
  paste(" -", error_bucket_summary_path),
  paste(" -", validation_flag_by_scenario_path),
  paste(" -", largest_cet1_errors_path),
  paste(" -", threshold_classification_summary_path),
  paste(" -", latest_validation_metrics_path),
  paste(" -", latest_largest_errors_path),
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
# 20. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 14 - Benchmark Validation Against Federal Reserve Public Results completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input rows:\n", nrow(results_raw), "\n")
cat("Validation rows:\n", nrow(benchmark_validation_panel), "\n")
cat("Banks:\n", dplyr::n_distinct(benchmark_validation_panel$bank_name), "\n")
cat("Years:\n", dplyr::n_distinct(benchmark_validation_panel$exercise_year), "\n")
cat("Scenarios:\n", dplyr::n_distinct(benchmark_validation_panel$scenario_label), "\n")
cat("Latest exercise year:\n", latest_year, "\n")
cat("Duplicated keys:\n", key_audit_summary$duplicated_keys, "\n")
cat("Figures created:\n", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")

cat("Executive validation summary:\n")
print(executive_validation_summary)

cat("\nOverall validation metrics:\n")
print(overall_validation_metrics)

cat("\nThreshold classification summary:\n")
print(threshold_classification_summary)

cat("\nLargest CET1 errors:\n")
print(largest_cet1_errors |> head(10))

cat("\nMain outputs:\n")
cat(" -", benchmark_validation_panel_path, "\n")
cat(" -", benchmark_largest_errors_path, "\n")
cat(" -", key_audit_summary_path, "\n")
cat(" -", executive_validation_summary_path, "\n")
cat(" -", overall_validation_metrics_path, "\n")
cat(" -", scenario_validation_metrics_path, "\n")
cat(" -", bank_validation_metrics_path, "\n")
cat(" -", error_bucket_summary_path, "\n")
cat(" -", validation_flag_by_scenario_path, "\n")
cat(" -", largest_cet1_errors_path, "\n")
cat(" -", threshold_classification_summary_path, "\n")
cat(" -", latest_validation_metrics_path, "\n")
cat(" -", latest_largest_errors_path, "\n")
cat(" -", figure_inventory_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")

cat("\nFigures:\n")
cat(paste(" -", figure_paths, collapse = "\n"))
cat("\n")
cat("============================================================\n")