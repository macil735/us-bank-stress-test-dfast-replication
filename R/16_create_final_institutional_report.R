# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 16 — Create Final Institutional Report
# ============================================================
# Objective:
#   Create the final institutional report for the public DFAST
#   stress test replication project.
#
# The report consolidates:
#   1. Project motivation and scope
#   2. Data and regulatory framework
#   3. Transmission layer
#   4. Econometric models
#   5. Integrated stress test engine
#   6. Final results and vulnerability ranking
#   7. Benchmark validation against Federal Reserve public results
#   8. Robustness, sensitivity and model risk assessment
#   9. Limitations, disclaimers and pedagogical notes
#
# Main inputs:
#   data/processed/model/final_stress_test_results_panel.csv
#   data/processed/model/final_bank_vulnerability_ranking.csv
#   data/processed/model/latest_exercise_bank_vulnerability_ranking.csv
#   data/processed/model/benchmark_validation_panel.csv
#   data/processed/model/model_risk_assessment_panel.csv
#   data/processed/model/bank_model_risk_assessment.csv
#
# Main outputs:
#   report/final_institutional_report_dfast_replication.docx
#   outputs/final_report/script16_final_report_outputs.xlsx
#   outputs/final_report/script16_execution_log.txt
#
# Methodological note:
#   The report is based entirely on public data and public DFAST
#   benchmark outcomes. It does not reproduce confidential Federal
#   Reserve supervisory models or bank internal stress testing models.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 16 - Create Final Institutional Report\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "16"
script_name <- "create_final_institutional_report"
start_time <- Sys.time()

setwd(project_root)

dir.create("report", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/final_report", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/final_report/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/final_report/figures", recursive = TRUE, showWarnings = FALSE)

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

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop(paste("Missing required input file:", path))
  }

  readr::read_csv(
    path,
    show_col_types = FALSE,
    guess_max = 100000
  ) |>
    janitor::clean_names() |>
    safe_df()
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
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

safe_cor <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) < 3) return(NA_real_)
  stats::cor(actual[ok], predicted[ok])
}

round_numeric <- function(df, digits = 4) {
  df |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, digits)
      )
    )
}

shorten_names <- function(df) {
  names(df) <- names(df) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_sentence()

  df
}

make_ft <- function(df, digits = 4, font_size = 8) {
  df2 <- df |>
    round_numeric(digits = digits) |>
    shorten_names()

  flextable::flextable(df2) |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::autofit()
}

add_table <- function(doc, df, title = NULL, digits = 4, font_size = 8) {
  if (!is.null(title)) {
    doc <- doc |>
      officer::body_add_par(title, style = "heading 3")
  }

  doc <- flextable::body_add_flextable(
    x = doc,
    value = make_ft(df, digits = digits, font_size = font_size)
  )

  doc
}

add_existing_image <- function(doc, image_path, caption = NULL, width = 6.5, height = 4.2) {
  if (file.exists(image_path)) {
    if (!is.null(caption)) {
      doc <- doc |>
        officer::body_add_par(caption, style = "heading 3")
    }

    doc <- doc |>
      officer::body_add_img(src = image_path, width = width, height = height)
  } else {
    doc <- doc |>
      officer::body_add_par(
        paste("Figure not found:", image_path),
        style = "Normal"
      )
  }

  doc
}

add_bullets <- function(doc, bullets) {
  for (b in bullets) {
    doc <- doc |>
      officer::body_add_par(b, style = "Normal")
  }

  doc
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Input paths
# ------------------------------------------------------------

cat("Defining input paths...\n")

paths <- list(
  final_results_panel =
    "data/processed/model/final_stress_test_results_panel.csv",

  final_bank_vulnerability_ranking =
    "data/processed/model/final_bank_vulnerability_ranking.csv",

  latest_exercise_bank_vulnerability_ranking =
    "data/processed/model/latest_exercise_bank_vulnerability_ranking.csv",

  benchmark_validation_panel =
    "data/processed/model/benchmark_validation_panel.csv",

  benchmark_validation_largest_errors =
    "data/processed/model/benchmark_validation_largest_cet1_errors.csv",

  model_risk_assessment_panel =
    "data/processed/model/model_risk_assessment_panel.csv",

  bank_model_risk_assessment =
    "data/processed/model/bank_model_risk_assessment.csv",

  model_risk_tail_error_panel =
    "data/processed/model/model_risk_tail_error_panel.csv"
)

input_check <- tibble::tibble(
  input_name = names(paths),
  input_path = unlist(paths),
  exists = file.exists(unlist(paths)),
  size_bytes = ifelse(
    file.exists(unlist(paths)),
    file.info(unlist(paths))$size,
    NA_real_
  )
) |>
  safe_df()

if (any(!input_check$exists)) {
  print(input_check)
  stop("One or more required inputs are missing.")
}

cat("Input paths checked.\n")
print(input_check)
cat("\n")


# ------------------------------------------------------------
# 4. Read inputs
# ------------------------------------------------------------

cat("Reading inputs...\n")

final_results <- safe_read_csv(paths$final_results_panel)
bank_ranking <- safe_read_csv(paths$final_bank_vulnerability_ranking)
latest_ranking <- safe_read_csv(paths$latest_exercise_bank_vulnerability_ranking)
validation_panel <- safe_read_csv(paths$benchmark_validation_panel)
largest_errors <- safe_read_csv(paths$benchmark_validation_largest_errors)
model_risk_panel <- safe_read_csv(paths$model_risk_assessment_panel)
bank_model_risk <- safe_read_csv(paths$bank_model_risk_assessment)
tail_error_panel <- safe_read_csv(paths$model_risk_tail_error_panel)

cat("Inputs loaded.\n")
cat("Final results rows:", nrow(final_results), "\n")
cat("Validation rows:", nrow(validation_panel), "\n")
cat("Model risk rows:", nrow(model_risk_panel), "\n\n")


# ------------------------------------------------------------
# 5. Required variables and key audit
# ------------------------------------------------------------

cat("Checking required variables and keys...\n")

required_final_cols <- c(
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
  "integrated_predicted_cet1_min_ratio"
)

required_validation_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label",
  "cet1_min_prediction_error",
  "abs_cet1_min_prediction_error",
  "cet1_min_validation_flag",
  "classification_match_4_5",
  "classification_match_7"
)

required_model_risk_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label",
  "model_risk_bucket",
  "model_risk_score",
  "threshold_miss_flag",
  "near_7_threshold",
  "near_4_5_threshold"
)

required_column_check <- dplyr::bind_rows(
  tibble::tibble(
    dataset = "final_results",
    required_column = required_final_cols,
    exists = required_final_cols %in% names(final_results)
  ),
  tibble::tibble(
    dataset = "validation_panel",
    required_column = required_validation_cols,
    exists = required_validation_cols %in% names(validation_panel)
  ),
  tibble::tibble(
    dataset = "model_risk_panel",
    required_column = required_model_risk_cols,
    exists = required_model_risk_cols %in% names(model_risk_panel)
  )
) |>
  safe_df()

if (any(!required_column_check$exists)) {
  print(required_column_check |> dplyr::filter(!exists))
  stop("Missing required columns.")
}

key_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label"
)

key_audit_final <- final_results |>
  dplyr::count(dplyr::across(dplyr::all_of(key_cols)), name = "key_count") |>
  dplyr::mutate(duplicated_key = key_count > 1)

key_audit_summary <- tibble::tibble(
  dataset = "final_results_panel",
  rows = nrow(final_results),
  unique_keys = nrow(key_audit_final),
  duplicated_keys = sum(key_audit_final$duplicated_key),
  max_rows_per_key = max(key_audit_final$key_count, na.rm = TRUE)
) |>
  safe_df()

if (key_audit_summary$duplicated_keys > 0) {
  print(key_audit_summary)
  stop("Final results contain duplicated bank-year-scenario keys.")
}

cat("Required variables and keys checked.\n\n")


# ------------------------------------------------------------
# 6. Core metrics for report
# ------------------------------------------------------------

cat("Creating core report metrics...\n")

latest_year <- max(final_results$exercise_year, na.rm = TRUE)

core_accuracy <- tibble::tibble(
  component = c(
    "Credit loss ratio",
    "PPNR ratio",
    "CET1 capital depletion",
    "CET1 minimum ratio"
  ),
  observations = c(
    sum(!is.na(final_results$actual_credit_loss_ratio) &
          !is.na(final_results$predicted_credit_loss_ratio)),
    sum(!is.na(final_results$actual_ppnr_ratio) &
          !is.na(final_results$predicted_ppnr_ratio)),
    sum(!is.na(final_results$actual_capital_depletion) &
          !is.na(final_results$predicted_capital_depletion)),
    sum(!is.na(final_results$observed_cet1_min_ratio) &
          !is.na(final_results$integrated_predicted_cet1_min_ratio))
  ),
  rmse = c(
    safe_rmse(final_results$actual_credit_loss_ratio, final_results$predicted_credit_loss_ratio),
    safe_rmse(final_results$actual_ppnr_ratio, final_results$predicted_ppnr_ratio),
    safe_rmse(final_results$actual_capital_depletion, final_results$predicted_capital_depletion),
    safe_rmse(final_results$observed_cet1_min_ratio, final_results$integrated_predicted_cet1_min_ratio)
  ),
  mae = c(
    safe_mae(final_results$actual_credit_loss_ratio, final_results$predicted_credit_loss_ratio),
    safe_mae(final_results$actual_ppnr_ratio, final_results$predicted_ppnr_ratio),
    safe_mae(final_results$actual_capital_depletion, final_results$predicted_capital_depletion),
    safe_mae(final_results$observed_cet1_min_ratio, final_results$integrated_predicted_cet1_min_ratio)
  ),
  bias = c(
    safe_bias(final_results$actual_credit_loss_ratio, final_results$predicted_credit_loss_ratio),
    safe_bias(final_results$actual_ppnr_ratio, final_results$predicted_ppnr_ratio),
    safe_bias(final_results$actual_capital_depletion, final_results$predicted_capital_depletion),
    safe_bias(final_results$observed_cet1_min_ratio, final_results$integrated_predicted_cet1_min_ratio)
  ),
  r_squared = c(
    safe_r2(final_results$actual_credit_loss_ratio, final_results$predicted_credit_loss_ratio),
    safe_r2(final_results$actual_ppnr_ratio, final_results$predicted_ppnr_ratio),
    safe_r2(final_results$actual_capital_depletion, final_results$predicted_capital_depletion),
    safe_r2(final_results$observed_cet1_min_ratio, final_results$integrated_predicted_cet1_min_ratio)
  ),
  correlation = c(
    safe_cor(final_results$actual_credit_loss_ratio, final_results$predicted_credit_loss_ratio),
    safe_cor(final_results$actual_ppnr_ratio, final_results$predicted_ppnr_ratio),
    safe_cor(final_results$actual_capital_depletion, final_results$predicted_capital_depletion),
    safe_cor(final_results$observed_cet1_min_ratio, final_results$integrated_predicted_cet1_min_ratio)
  )
) |>
  safe_df()

threshold_summary <- tibble::tibble(
  threshold = c("CET1 4.5 percent", "CET1 7.0 percent"),
  observations = c(
    sum(!is.na(validation_panel$classification_match_4_5)),
    sum(!is.na(validation_panel$classification_match_7))
  ),
  correctly_classified = c(
    sum(validation_panel$classification_match_4_5, na.rm = TRUE),
    sum(validation_panel$classification_match_7, na.rm = TRUE)
  ),
  classification_accuracy = c(
    mean(validation_panel$classification_match_4_5, na.rm = TRUE),
    mean(validation_panel$classification_match_7, na.rm = TRUE)
  )
) |>
  safe_df()

model_risk_bucket_summary <- model_risk_panel |>
  dplyr::count(model_risk_bucket, name = "observations") |>
  dplyr::mutate(
    share = observations / sum(observations)
  ) |>
  dplyr::arrange(dplyr::desc(observations)) |>
  safe_df()

executive_summary <- tibble::tibble(
  metric = c(
    "Panel observations",
    "Banks",
    "Exercise years",
    "Scenarios",
    "Latest exercise year",
    "Minimum predicted CET1 ratio",
    "Mean predicted CET1 ratio",
    "Maximum predicted capital depletion",
    "Mean predicted capital depletion",
    "CET1 minimum ratio RMSE",
    "CET1 minimum ratio MAE",
    "CET1 minimum ratio bias",
    "CET1 minimum ratio R-squared",
    "CET1 minimum ratio correlation",
    "CET1 4.5 percent classification accuracy",
    "CET1 7.0 percent classification accuracy",
    "High model risk observations",
    "Largest absolute CET1 error"
  ),
  value = c(
    as.character(nrow(final_results)),
    as.character(dplyr::n_distinct(final_results$bank_name)),
    as.character(dplyr::n_distinct(final_results$exercise_year)),
    as.character(dplyr::n_distinct(final_results$scenario_label)),
    as.character(latest_year),
    as.character(round(safe_min(final_results$integrated_predicted_cet1_min_ratio), 4)),
    as.character(round(safe_mean(final_results$integrated_predicted_cet1_min_ratio), 4)),
    as.character(round(safe_max(final_results$predicted_capital_depletion), 4)),
    as.character(round(safe_mean(final_results$predicted_capital_depletion), 4)),
    as.character(round(core_accuracy |> dplyr::filter(component == "CET1 minimum ratio") |> dplyr::pull(rmse), 4)),
    as.character(round(core_accuracy |> dplyr::filter(component == "CET1 minimum ratio") |> dplyr::pull(mae), 4)),
    as.character(round(core_accuracy |> dplyr::filter(component == "CET1 minimum ratio") |> dplyr::pull(bias), 4)),
    as.character(round(core_accuracy |> dplyr::filter(component == "CET1 minimum ratio") |> dplyr::pull(r_squared), 4)),
    as.character(round(core_accuracy |> dplyr::filter(component == "CET1 minimum ratio") |> dplyr::pull(correlation), 4)),
    as.character(round(threshold_summary |> dplyr::filter(threshold == "CET1 4.5 percent") |> dplyr::pull(classification_accuracy), 4)),
    as.character(round(threshold_summary |> dplyr::filter(threshold == "CET1 7.0 percent") |> dplyr::pull(classification_accuracy), 4)),
    as.character(sum(model_risk_panel$model_risk_bucket == "High model risk", na.rm = TRUE)),
    as.character(round(safe_max(validation_panel$abs_cet1_min_prediction_error), 4))
  )
) |>
  safe_df()

cat("Core report metrics created.\n\n")


# ------------------------------------------------------------
# 7. Report tables
# ------------------------------------------------------------

cat("Creating report tables...\n")

table_pipeline_map <- tibble::tibble(
  stage = c(
    "Data availability audit",
    "Regulatory documentation and raw data ingestion",
    "Federal Reserve DFAST data cleaning",
    "Macro scenario structuring",
    "DFAST benchmark dataset construction",
    "Capital and losses transmission layer",
    "Exploratory analysis",
    "Modelling sample and treatment rules",
    "Credit loss model",
    "PPNR model",
    "Capital depletion model",
    "Integrated projection engine",
    "Join-key correction",
    "Final results and vulnerability ranking",
    "Benchmark validation",
    "Robustness and model risk assessment"
  ),
  script = c(
    "Script 01",
    "Script 02 / 02b / 02c",
    "Script 03",
    "Script 04",
    "Script 05",
    "Script 06",
    "Script 07",
    "Script 08",
    "Script 09",
    "Script 10",
    "Script 11",
    "Script 12",
    "Script 12b",
    "Script 13",
    "Script 14",
    "Script 15"
  ),
  output_role = c(
    "Defines the public data universe and feasibility of the project.",
    "Downloads and records public regulatory and DFAST source documents.",
    "Cleans Federal Reserve public DFAST result files.",
    "Structures baseline and severely adverse macroeconomic scenarios.",
    "Creates bank-year-scenario public DFAST benchmark data.",
    "Builds the accounting link between PPNR, losses and CET1 depletion.",
    "Describes distributions, outliers and empirical structure.",
    "Creates clean and winsorized modelling samples.",
    "Estimates credit loss intensity using public DFAST outcomes.",
    "Estimates PPNR performance using public DFAST outcomes.",
    "Estimates CET1 capital depletion.",
    "Combines component projections into an integrated stress test engine.",
    "Corrects duplicated join keys and produces the valid integrated panel.",
    "Produces final bank vulnerability rankings.",
    "Validates model outputs against Federal Reserve public benchmark results.",
    "Assesses robustness, sensitivity, tail errors and model risk."
  )
) |>
  safe_df()

table_model_architecture <- tibble::tibble(
  model_block = c(
    "Credit losses",
    "PPNR",
    "Capital depletion",
    "Integrated CET1 projection"
  ),
  dependent_variable = c(
    "Credit loss ratio",
    "PPNR ratio",
    "CET1 capital depletion",
    "Predicted CET1 minimum ratio"
  ),
  operational_model = c(
    "Bank and year fixed effects",
    "Bank and year fixed effects",
    "Bank and year fixed effects",
    "CET1 actual ratio minus predicted capital depletion"
  ),
  role_in_engine = c(
    "Captures stress-period credit loss intensity.",
    "Captures pre-provision earnings capacity under stress.",
    "Maps losses, earnings and risk-weighted asset effects into CET1 depletion.",
    "Produces the final stressed capital adequacy measure."
  )
) |>
  safe_df()

table_top_latest <- latest_ranking |>
  dplyr::select(
    latest_year_rank_composite,
    bank_name,
    min_predicted_cet1_min_ratio,
    min_observed_cet1_min_ratio,
    max_predicted_capital_depletion,
    max_predicted_credit_loss_ratio,
    min_predicted_ppnr_ratio,
    worst_scenario_by_predicted_cet1
  ) |>
  dplyr::slice_head(n = 20) |>
  safe_df()

table_top_full_sample <- bank_ranking |>
  dplyr::select(
    vulnerability_rank_composite,
    bank_name,
    min_predicted_cet1_min_ratio,
    max_predicted_capital_depletion,
    mean_predicted_credit_loss_ratio,
    mean_predicted_ppnr_ratio,
    max_composite_vulnerability_score,
    observations_below_7
  ) |>
  dplyr::slice_head(n = 20) |>
  safe_df()

table_largest_errors <- largest_errors |>
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
  dplyr::slice_head(n = 20) |>
  safe_df()

table_bank_model_risk <- bank_model_risk |>
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
  dplyr::slice_head(n = 20) |>
  safe_df()

table_pedagogical_use <- tibble::tibble(
  teaching_dimension = c(
    "Data governance",
    "Regulatory interpretation",
    "Financial accounting logic",
    "Econometric modelling",
    "Stress testing mechanics",
    "Validation",
    "Model risk",
    "Communication"
  ),
  learning_objective = c(
    "Understand how public supervisory data are collected, audited and transformed.",
    "Connect DFAST outputs with capital regulation, scenarios and bank disclosure.",
    "Translate PPNR, losses and RWAs into capital depletion.",
    "Estimate reduced-form models and interpret fit, residuals and limitations.",
    "Build a complete stress test engine from components.",
    "Compare model outputs with public benchmark outcomes.",
    "Identify tail errors, threshold sensitivity and ranking instability.",
    "Prepare reproducible reports, tables, figures and disclaimers."
  )
) |>
  safe_df()

table_limitations <- tibble::tibble(
  limitation = c(
    "Public data only",
    "No confidential supervisory models",
    "No bank internal capital planning information",
    "Reduced-form econometric specification",
    "Bank-specific idiosyncrasies",
    "Tail errors",
    "Ranking interpretation",
    "Threshold interpretation"
  ),
  implication = c(
    "The engine relies only on public DFAST outputs and public regulatory material.",
    "The model does not reproduce Federal Reserve confidential supervisory models.",
    "Internal bank assumptions, management actions and portfolio details are not observed.",
    "The model is designed for replication and benchmarking, not for official capital decisions.",
    "Some banks have structural features that may not be fully captured by public variables.",
    "Large deviations are concentrated in specific bank-year-scenario observations.",
    "Vulnerability rankings are analytical and relative, not official supervisory rankings.",
    "The 4.5 percent and 7.0 percent thresholds are used as analytical reference points."
  )
) |>
  safe_df()

cat("Report tables created.\n\n")


# ------------------------------------------------------------
# 8. Copy selected figures to final report folder
# ------------------------------------------------------------

cat("Preparing figure inventory...\n")

figure_sources <- tibble::tibble(
  figure_id = c(
    "final_top20_min_cet1",
    "final_composite_vulnerability",
    "final_observed_vs_predicted_cet1",
    "validation_error_distribution",
    "validation_precision_buckets",
    "model_risk_buckets",
    "threshold_sensitivity",
    "bank_model_risk"
  ),
  source_path = c(
    "outputs/final_results/figures/fig01_latest_top20_min_predicted_cet1.png",
    "outputs/final_results/figures/fig02_top20_composite_vulnerability_score.png",
    "outputs/final_results/figures/fig04_observed_vs_predicted_cet1_min_ratio.png",
    "outputs/benchmark_validation/figures/fig02_cet1_prediction_error_distribution.png",
    "outputs/benchmark_validation/figures/fig04_cet1_validation_precision_buckets.png",
    "outputs/model_risk/figures/fig02_model_risk_buckets.png",
    "outputs/model_risk/figures/fig04_threshold_classification_sensitivity.png",
    "outputs/model_risk/figures/fig03_top20_bank_model_risk_score.png"
  ),
  final_path = c(
    "outputs/final_report/figures/fig01_latest_top20_min_predicted_cet1.png",
    "outputs/final_report/figures/fig02_composite_vulnerability_score.png",
    "outputs/final_report/figures/fig03_observed_vs_predicted_cet1.png",
    "outputs/final_report/figures/fig04_cet1_error_distribution.png",
    "outputs/final_report/figures/fig05_validation_precision_buckets.png",
    "outputs/final_report/figures/fig06_model_risk_buckets.png",
    "outputs/final_report/figures/fig07_threshold_sensitivity.png",
    "outputs/final_report/figures/fig08_bank_model_risk.png"
  )
) |>
  safe_df()

for (i in seq_len(nrow(figure_sources))) {
  if (file.exists(figure_sources$source_path[i])) {
    file.copy(
      from = figure_sources$source_path[i],
      to = figure_sources$final_path[i],
      overwrite = TRUE
    )
  }
}

figure_inventory <- figure_sources |>
  dplyr::mutate(
    source_exists = file.exists(source_path),
    final_exists = file.exists(final_path),
    final_size_bytes = ifelse(final_exists, file.info(final_path)$size, NA_real_)
  ) |>
  safe_df()

cat("Figure inventory prepared.\n\n")


# ------------------------------------------------------------
# 9. Save tables and Excel workbook
# ------------------------------------------------------------

cat("Saving final report tables and workbook...\n")

out_dir <- "outputs/final_report"

paths_out <- list(
  input_check = file.path(out_dir, "script16_input_check.csv"),
  required_column_check = file.path(out_dir, "script16_required_column_check.csv"),
  key_audit_summary = file.path(out_dir, "script16_key_audit_summary.csv"),
  executive_summary = file.path(out_dir, "script16_executive_summary.csv"),
  core_accuracy = file.path(out_dir, "script16_core_accuracy.csv"),
  threshold_summary = file.path(out_dir, "script16_threshold_summary.csv"),
  model_risk_bucket_summary = file.path(out_dir, "script16_model_risk_bucket_summary.csv"),
  pipeline_map = file.path(out_dir, "script16_pipeline_map.csv"),
  model_architecture = file.path(out_dir, "script16_model_architecture.csv"),
  top_latest = file.path(out_dir, "script16_top_latest_ranking.csv"),
  top_full_sample = file.path(out_dir, "script16_top_full_sample_ranking.csv"),
  largest_errors = file.path(out_dir, "script16_largest_cet1_errors.csv"),
  bank_model_risk = file.path(out_dir, "script16_bank_model_risk.csv"),
  pedagogical_use = file.path(out_dir, "script16_pedagogical_use.csv"),
  limitations = file.path(out_dir, "script16_limitations.csv"),
  figure_inventory = file.path(out_dir, "script16_figure_inventory.csv"),
  execution_summary = file.path(out_dir, "script16_execution_summary.csv"),
  excel = file.path(out_dir, "script16_final_report_outputs.xlsx"),
  report_docx = "report/final_institutional_report_dfast_replication.docx",
  execution_log = file.path(out_dir, "script16_execution_log.txt")
)

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  final_results_rows = nrow(final_results),
  validation_rows = nrow(validation_panel),
  model_risk_rows = nrow(model_risk_panel),
  banks = dplyr::n_distinct(final_results$bank_name),
  years = dplyr::n_distinct(final_results$exercise_year),
  scenarios = dplyr::n_distinct(final_results$scenario_label),
  latest_exercise_year = latest_year,
  duplicated_keys = key_audit_summary$duplicated_keys,
  figures_available = sum(figure_inventory$final_exists, na.rm = TRUE),
  final_report_path = paths_out$report_docx
) |>
  safe_df()

readr::write_csv(input_check, paths_out$input_check)
readr::write_csv(required_column_check, paths_out$required_column_check)
readr::write_csv(key_audit_summary, paths_out$key_audit_summary)
readr::write_csv(executive_summary, paths_out$executive_summary)
readr::write_csv(core_accuracy, paths_out$core_accuracy)
readr::write_csv(threshold_summary, paths_out$threshold_summary)
readr::write_csv(model_risk_bucket_summary, paths_out$model_risk_bucket_summary)
readr::write_csv(table_pipeline_map, paths_out$pipeline_map)
readr::write_csv(table_model_architecture, paths_out$model_architecture)
readr::write_csv(table_top_latest, paths_out$top_latest)
readr::write_csv(table_top_full_sample, paths_out$top_full_sample)
readr::write_csv(table_largest_errors, paths_out$largest_errors)
readr::write_csv(table_bank_model_risk, paths_out$bank_model_risk)
readr::write_csv(table_pedagogical_use, paths_out$pedagogical_use)
readr::write_csv(table_limitations, paths_out$limitations)
readr::write_csv(figure_inventory, paths_out$figure_inventory)
readr::write_csv(execution_summary, paths_out$execution_summary)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  execution_summary = execution_summary,
  executive_summary = executive_summary,
  core_accuracy = core_accuracy,
  threshold_summary = threshold_summary,
  model_risk_buckets = model_risk_bucket_summary,
  pipeline_map = table_pipeline_map,
  model_architecture = table_model_architecture,
  latest_top20 = table_top_latest,
  full_sample_top20 = table_top_full_sample,
  largest_cet1_errors = table_largest_errors,
  bank_model_risk = table_bank_model_risk,
  pedagogical_use = table_pedagogical_use,
  limitations = table_limitations,
  figure_inventory = figure_inventory
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, paths_out$excel, overwrite = TRUE)

cat("Final report tables and workbook saved.\n\n")


# ------------------------------------------------------------
# 10. Create final institutional report
# ------------------------------------------------------------

cat("Creating final institutional Word report...\n")

doc <- officer::read_docx()


# Cover
# Cover
doc <- doc |>
  officer::body_add_par(
    "USA Bank Stress Test DFAST Replication",
    style = "heading 1"
  ) |>
  officer::body_add_par(
    "Final Institutional Report",
    style = "heading 2"
  ) |>
  officer::body_add_par(
    "Public reduced-form replication of Federal Reserve DFAST-style stress testing",
    style = "Normal"
  ) |>
  officer::body_add_par(
    paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    style = "Normal"
  ) |>
  officer::body_add_par(
    "Prepared for academic, pedagogical and public benchmarking purposes.",
    style = "Normal"
  ) |>
  officer::body_add_break()

# Executive summary
doc <- doc |>
  officer::body_add_par("Executive Summary", style = "heading 1") |>
  officer::body_add_par(
    "This report presents a complete public-data replication of a DFAST-style bank stress testing framework for large banking organizations in the United States. The project uses public Federal Reserve DFAST results, public macroeconomic scenario data and reproducible R scripts to build a reduced-form stress test engine.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The purpose is not to reproduce confidential Federal Reserve supervisory models or internal bank stress testing systems. The purpose is to construct a transparent, auditable and pedagogically useful benchmark that links macro-financial stress testing concepts to public data, econometric modelling, capital depletion and model validation.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  executive_summary,
  title = "Table 1. Executive summary of final results",
  digits = 4,
  font_size = 8
)


# Scope and disclaimer
doc <- doc |>
  officer::body_add_par("1. Scope, Use and Disclaimer", style = "heading 1") |>
  officer::body_add_par(
    "The project is based exclusively on public information. It uses public DFAST results and public scenario data released by the Federal Reserve. It does not access, infer or replicate confidential supervisory models, confidential bank submissions, internal capital planning models or non-public supervisory assumptions.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The results should be interpreted as a public benchmark and educational replication exercise. They should not be interpreted as official supervisory findings, investment advice, bank ratings, capital adequacy opinions or regulatory pass/fail decisions.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The vulnerability rankings are relative outputs of the reduced-form model. They indicate which banks appear more exposed under the model's public-data assumptions, not which banks are officially weak or unsafe.",
    style = "Normal"
  )


# Pipeline
doc <- doc |>
  officer::body_add_par("2. Reproducible Pipeline", style = "heading 1") |>
  officer::body_add_par(
    "The project follows a staged pipeline. Each script creates auditable outputs, diagnostic tables and execution logs. This structure is intentionally suitable for classroom use, replication, peer review and future extension.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  table_pipeline_map,
  title = "Table 2. Script pipeline and analytical role",
  digits = 4,
  font_size = 7
)


# Data and regulatory framework
doc <- doc |>
  officer::body_add_par("3. Data and Regulatory Framework", style = "heading 1") |>
  officer::body_add_par(
    "The empirical base is built from Federal Reserve public DFAST results and macroeconomic scenario releases. The relevant regulatory context includes supervisory stress testing, capital planning, stressed capital ratios and scenario-based loss and revenue projections.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The dataset is organized at the bank-year-scenario level. Each observation contains public outcome variables such as stressed capital ratios, credit losses, PPNR, capital depletion and related balance sheet measures.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "A key data quality issue identified during the project was the presence of duplicated bank-year-scenario keys after joining model components. This was corrected in Script 12b. The final panel contains one observation per bank-year-scenario key.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  key_audit_summary,
  title = "Table 3. Final key audit",
  digits = 4,
  font_size = 8
)


# Methodology
doc <- doc |>
  officer::body_add_par("4. Stress Testing Methodology", style = "heading 1") |>
  officer::body_add_par(
    "The analytical framework follows a reduced-form transmission structure. Public DFAST outcomes are used to estimate component models for credit losses, PPNR and CET1 capital depletion. These components are then combined into an integrated stress test engine.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The core transmission logic is: stressed credit losses and revenue dynamics affect pre-tax performance; pre-tax performance and risk-weighted asset dynamics affect capital depletion; capital depletion determines the projected minimum CET1 ratio under stress.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  table_model_architecture,
  title = "Table 4. Model architecture",
  digits = 4,
  font_size = 8
)

doc <- doc |>
  officer::body_add_par("4.1. Credit Loss Model", style = "heading 2") |>
  officer::body_add_par(
    "The credit loss model estimates the intensity of stress-period credit losses using public bank-year-scenario outcomes. The operational specification uses bank and year fixed effects, allowing the model to capture persistent differences across institutions and common exercise-year effects.",
    style = "Normal"
  ) |>
  officer::body_add_par("4.2. PPNR Model", style = "heading 2") |>
  officer::body_add_par(
    "The PPNR model estimates pre-provision net revenue performance under stress. PPNR is central because it provides loss-absorbing capacity before capital is depleted.",
    style = "Normal"
  ) |>
  officer::body_add_par("4.3. Capital Depletion Model", style = "heading 2") |>
  officer::body_add_par(
    "The capital depletion model maps losses, PPNR, balance sheet dynamics and scenario effects into CET1 depletion. It is the direct bridge between stress outcomes and the final capital adequacy measure.",
    style = "Normal"
  ) |>
  officer::body_add_par("4.4. Integrated Projection Engine", style = "heading 2") |>
  officer::body_add_par(
    "The integrated engine combines component projections and computes the predicted CET1 minimum ratio. The corrected engine uses a unique bank-year-scenario key to prevent artificial row expansion.",
    style = "Normal"
  )


# Accuracy and validation
doc <- doc |>
  officer::body_add_par("5. Benchmark Validation Against Federal Reserve Public Results", style = "heading 1") |>
  officer::body_add_par(
    "The benchmark validation compares the model's predicted outcomes with public Federal Reserve DFAST benchmark outcomes. The central validation variable is the CET1 minimum ratio.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  core_accuracy,
  title = "Table 5. Model accuracy by component",
  digits = 4,
  font_size = 8
)

doc <- add_table(
  doc,
  threshold_summary,
  title = "Table 6. CET1 threshold classification accuracy",
  digits = 4,
  font_size = 8
)

doc <- doc |>
  officer::body_add_par(
    "The CET1 minimum ratio validation shows high explanatory power, low mean absolute error and negligible average bias. The model classifies the 4.5 percent CET1 threshold correctly in the final validation sample and performs strongly at the 7.0 percent reference level.",
    style = "Normal"
  )

doc <- add_existing_image(
  doc,
  "outputs/final_report/figures/fig03_observed_vs_predicted_cet1.png",
  caption = "Figure 1. Observed versus predicted CET1 minimum ratio",
  width = 6.5,
  height = 4.2
)

doc <- add_existing_image(
  doc,
  "outputs/final_report/figures/fig04_cet1_error_distribution.png",
  caption = "Figure 2. Distribution of CET1 prediction errors",
  width = 6.5,
  height = 4.2
)


# Final stress test results
doc <- doc |>
  officer::body_add_par("6. Final Stress Test Results", style = "heading 1") |>
  officer::body_add_par(
    "The final results panel contains projected and observed stress outcomes for 497 bank-year-scenario observations, covering 56 banks, 11 exercise years and 3 scenario categories.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The minimum predicted CET1 ratio in the sample is above the 4.5 percent reference threshold. However, several observations fall below the 7.0 percent analytical reference level, indicating capital pressure under stress for selected institutions and scenarios.",
    style = "Normal"
  )

doc <- add_existing_image(
  doc,
  "outputs/final_report/figures/fig01_latest_top20_min_predicted_cet1.png",
  caption = "Figure 3. Latest exercise top 20 banks by minimum predicted CET1",
  width = 6.5,
  height = 4.8
)

doc <- add_table(
  doc,
  table_top_latest,
  title = "Table 7. Latest exercise vulnerability ranking",
  digits = 4,
  font_size = 7
)

doc <- add_existing_image(
  doc,
  "outputs/final_report/figures/fig02_composite_vulnerability_score.png",
  caption = "Figure 4. Top 20 banks by composite vulnerability score",
  width = 6.5,
  height = 4.8
)

doc <- add_table(
  doc,
  table_top_full_sample,
  title = "Table 8. Full-sample composite vulnerability ranking",
  digits = 4,
  font_size = 7
)


# Model risk
doc <- doc |>
  officer::body_add_par("7. Robustness, Sensitivity and Model Risk", style = "heading 1") |>
  officer::body_add_par(
    "The model risk assessment examines whether the strong average performance hides localized errors, threshold sensitivity or bank-specific deviations. This step is necessary because a stress test model can have good average accuracy but still require caution for specific banks or scenarios.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  model_risk_bucket_summary,
  title = "Table 9. Model risk bucket summary",
  digits = 4,
  font_size = 8
)

doc <- add_existing_image(
  doc,
  "outputs/final_report/figures/fig06_model_risk_buckets.png",
  caption = "Figure 5. Model risk buckets",
  width = 6.5,
  height = 4.2
)

doc <- add_existing_image(
  doc,
  "outputs/final_report/figures/fig07_threshold_sensitivity.png",
  caption = "Figure 6. CET1 threshold sensitivity",
  width = 6.5,
  height = 4.2
)

doc <- add_table(
  doc,
  table_bank_model_risk,
  title = "Table 10. Bank-level model risk ranking",
  digits = 4,
  font_size = 7
)

doc <- add_table(
  doc,
  table_largest_errors,
  title = "Table 11. Largest CET1 prediction errors",
  digits = 4,
  font_size = 7
)

doc <- doc |>
  officer::body_add_par(
    "The model risk assessment indicates that most observations are classified as low model risk, but a non-trivial set of observations requires attention. These cases are concentrated in selected bank-year-scenario combinations and should be treated as tail model risk.",
    style = "Normal"
  )


# Pedagogical section
doc <- doc |>
  officer::body_add_par("8. Pedagogical Use", style = "heading 1") |>
  officer::body_add_par(
    "This project is suitable for teaching applied banking, macro-financial stress testing, financial econometrics, regulatory data analysis and reproducible research. It gives students a complete pipeline from raw public data to final institutional reporting.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  table_pedagogical_use,
  title = "Table 12. Pedagogical use of the project",
  digits = 4,
  font_size = 8
)

doc <- doc |>
  officer::body_add_par("8.1. Suggested Classroom Sequence", style = "heading 2")

doc <- add_bullets(
  doc,
  c(
    "Step 1: Introduce DFAST, stress testing objectives and capital adequacy concepts.",
    "Step 2: Review the structure of public Federal Reserve DFAST data.",
    "Step 3: Explain the transmission from losses and PPNR to CET1 depletion.",
    "Step 4: Estimate and interpret the credit loss, PPNR and capital depletion models.",
    "Step 5: Build the integrated projection engine.",
    "Step 6: Validate the model against public benchmark outcomes.",
    "Step 7: Discuss model risk, limitations and the difference between public replication and official supervisory modelling.",
    "Step 8: Require students to reproduce selected tables, figures and validation metrics."
  )
)

doc <- doc |>
  officer::body_add_par("8.2. Recommended Student Exercises", style = "heading 2")

doc <- add_bullets(
  doc,
  c(
    "Reproduce the key audit and explain why duplicated join keys distort an integrated panel.",
    "Compare the RMSE, MAE and R-squared of the three component models.",
    "Explain why CET1 minimum ratio accuracy can be higher than component-level accuracy.",
    "Identify the banks with the largest CET1 prediction errors and interpret possible reasons.",
    "Evaluate threshold classification at 4.5 percent, 7.0 percent and alternative CET1 thresholds.",
    "Discuss why a model can be useful even when tail errors remain.",
    "Write a short policy note distinguishing official supervisory stress testing from public reduced-form replication."
  )
)


# Limitations
doc <- doc |>
  officer::body_add_par("9. Limitations", style = "heading 1") |>
  officer::body_add_par(
    "The limitations are not weaknesses to hide. They define the valid interpretation of the project. A public reduced-form stress test can be informative and reproducible, but it cannot replace confidential supervisory modelling or bank internal stress testing.",
    style = "Normal"
  )

doc <- add_table(
  doc,
  table_limitations,
  title = "Table 13. Main limitations and implications",
  digits = 4,
  font_size = 8
)


# Institutional conclusion
doc <- doc |>
  officer::body_add_par("10. Institutional Conclusion", style = "heading 1") |>
  officer::body_add_par(
    "The project demonstrates that a transparent public-data stress test replication can approximate Federal Reserve public DFAST outcomes with high accuracy. The integrated engine achieves strong validation results for the CET1 minimum ratio and provides an interpretable framework for bank vulnerability ranking.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The final validation shows low average error, limited systematic bias and high correlation with public benchmark outcomes. At the same time, the model risk assessment identifies tail deviations and bank-specific cases where caution is required.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "The appropriate use of the project is therefore as a public benchmark, teaching tool and reproducible research framework. It is not an official supervisory stress test, not a capital planning model and not an investment recommendation.",
    style = "Normal"
  )


# Final disclaimer
doc <- doc |>
  officer::body_add_par("Final Disclaimer", style = "heading 1") |>
  officer::body_add_par(
    "This report is an independent educational and analytical project based on public data. It is not affiliated with, endorsed by or approved by the Federal Reserve, any banking organization or any supervisory authority. All results are produced for research, teaching and public benchmarking purposes only.",
    style = "Normal"
  ) |>
  officer::body_add_par(
    "No confidential supervisory information, confidential bank submissions, non-public models or internal bank data are used. The report must not be interpreted as an official regulatory assessment, investment recommendation, credit opinion or statement on the safety and soundness of any institution.",
    style = "Normal"
  )


# Technical appendix
doc <- doc |>
  officer::body_add_break() |>
  officer::body_add_par("Appendix A. Reproducibility Checklist", style = "heading 1")

repro_checklist <- tibble::tibble(
  item = c(
    "Raw public data downloaded",
    "Regulatory documentation recorded",
    "Processed DFAST benchmark dataset created",
    "Transmission layer created",
    "Modelling sample created",
    "Credit loss model estimated",
    "PPNR model estimated",
    "Capital depletion model estimated",
    "Integrated engine corrected for join keys",
    "Final rankings produced",
    "Benchmark validation completed",
    "Model risk assessment completed",
    "Final institutional report generated"
  ),
  status = rep("Completed", 13)
) |>
  safe_df()

doc <- add_table(
  doc,
  repro_checklist,
  title = "Table A1. Reproducibility checklist",
  digits = 4,
  font_size = 8
)

doc <- doc |>
  officer::body_add_par("Appendix B. Glossary", style = "heading 1")

glossary <- tibble::tibble(
  term = c(
    "DFAST",
    "CET1",
    "PPNR",
    "RWA",
    "Capital depletion",
    "Benchmark validation",
    "RMSE",
    "MAE",
    "Model risk",
    "Tail error"
  ),
  definition = c(
    "Dodd-Frank Act Stress Test; a supervisory stress testing framework used in the United States.",
    "Common Equity Tier 1 capital ratio; a core regulatory capital adequacy measure.",
    "Pre-provision net revenue; earnings before provisions and selected losses.",
    "Risk-weighted assets; asset measure adjusted for regulatory risk weights.",
    "Reduction in capital ratio under stress.",
    "Comparison of model outputs with observed public benchmark outcomes.",
    "Root mean squared error; penalizes larger errors more strongly.",
    "Mean absolute error; average absolute deviation between prediction and observation.",
    "Risk that the model gives inaccurate or unstable outputs for its intended use.",
    "Large error in the tail of the error distribution."
  )
) |>
  safe_df()

doc <- add_table(
  doc,
  glossary,
  title = "Table B1. Glossary of key terms",
  digits = 4,
  font_size = 8
)

print(doc, target = paths_out$report_docx)

cat("Final institutional Word report created.\n\n")


# ------------------------------------------------------------
# 11. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 16 - Create Final Institutional Report completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Final results rows:", nrow(final_results)),
  paste("Validation rows:", nrow(validation_panel)),
  paste("Model risk rows:", nrow(model_risk_panel)),
  paste("Banks:", dplyr::n_distinct(final_results$bank_name)),
  paste("Years:", dplyr::n_distinct(final_results$exercise_year)),
  paste("Scenarios:", dplyr::n_distinct(final_results$scenario_label)),
  paste("Latest exercise year:", latest_year),
  paste("Duplicated keys:", key_audit_summary$duplicated_keys),
  paste("Figures available:", sum(figure_inventory$final_exists, na.rm = TRUE)),
  "",
  "Executive summary:",
  capture.output(print(executive_summary)),
  "",
  "Core accuracy:",
  capture.output(print(core_accuracy)),
  "",
  "Threshold summary:",
  capture.output(print(threshold_summary)),
  "",
  "Model risk bucket summary:",
  capture.output(print(model_risk_bucket_summary)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", paths_out$input_check),
  paste(" -", paths_out$required_column_check),
  paste(" -", paths_out$key_audit_summary),
  paste(" -", paths_out$executive_summary),
  paste(" -", paths_out$core_accuracy),
  paste(" -", paths_out$threshold_summary),
  paste(" -", paths_out$model_risk_bucket_summary),
  paste(" -", paths_out$pipeline_map),
  paste(" -", paths_out$model_architecture),
  paste(" -", paths_out$top_latest),
  paste(" -", paths_out$top_full_sample),
  paste(" -", paths_out$largest_errors),
  paste(" -", paths_out$bank_model_risk),
  paste(" -", paths_out$pedagogical_use),
  paste(" -", paths_out$limitations),
  paste(" -", paths_out$figure_inventory),
  paste(" -", paths_out$execution_summary),
  paste(" -", paths_out$excel),
  paste(" -", paths_out$report_docx),
  paste(" -", paths_out$execution_log)
)

writeLines(enc2utf8(log_lines), paths_out$execution_log, useBytes = TRUE)


# ------------------------------------------------------------
# 12. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 16 - Create Final Institutional Report completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Final results rows:\n", nrow(final_results), "\n")
cat("Validation rows:\n", nrow(validation_panel), "\n")
cat("Model risk rows:\n", nrow(model_risk_panel), "\n")
cat("Banks:\n", dplyr::n_distinct(final_results$bank_name), "\n")
cat("Years:\n", dplyr::n_distinct(final_results$exercise_year), "\n")
cat("Scenarios:\n", dplyr::n_distinct(final_results$scenario_label), "\n")
cat("Latest exercise year:\n", latest_year, "\n")
cat("Duplicated keys:\n", key_audit_summary$duplicated_keys, "\n")
cat("Figures available:\n", sum(figure_inventory$final_exists, na.rm = TRUE), "\n\n")

cat("Executive summary:\n")
print(executive_summary)

cat("\nCore accuracy:\n")
print(core_accuracy)

cat("\nThreshold summary:\n")
print(threshold_summary)

cat("\nModel risk bucket summary:\n")
print(model_risk_bucket_summary)

cat("\nMain outputs:\n")
cat(" -", paths_out$excel, "\n")
cat(" -", paths_out$report_docx, "\n")
cat(" -", paths_out$execution_log, "\n")
cat("============================================================\n")