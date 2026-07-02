# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 17 — Create Final Technical and Pedagogical Manual
# ============================================================
# Objective:
#   Create a technical and pedagogical manual for students,
#   researchers and interested public readers.
#
# The manual explains:
#   1. What stress testing is
#   2. What DFAST-style public replication means
#   3. How the data pipeline works
#   4. How credit losses, PPNR and capital depletion are linked
#   5. How the econometric models are structured
#   6. How validation against public Federal Reserve outcomes is performed
#   7. How model risk and robustness are assessed
#   8. How students can reproduce and extend the project
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
#   manual/final_technical_pedagogical_manual_dfast_replication.docx
#   outputs/final_manual/script17_final_manual_outputs.xlsx
#   outputs/final_manual/script17_execution_log.txt
#
# Methodological note:
#   This manual is educational and technical. It is based only on
#   public data and public DFAST benchmark outcomes. It does not
#   reproduce confidential Federal Reserve supervisory models or
#   bank internal capital planning systems.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 17 - Create Final Technical and Pedagogical Manual\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "17"
script_name <- "create_final_technical_pedagogical_manual"
start_time <- Sys.time()

setwd(project_root)

dir.create("manual", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/final_manual", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/final_manual/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/final_manual/figures", recursive = TRUE, showWarnings = FALSE)

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

add_paragraphs <- function(doc, paragraphs) {
  for (p in paragraphs) {
    doc <- doc |>
      officer::body_add_par(p, style = "Normal")
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

key_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label"
)

required_final_cols <- c(
  key_cols,
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
  key_cols,
  "cet1_min_prediction_error",
  "abs_cet1_min_prediction_error",
  "cet1_min_validation_flag",
  "classification_match_4_5",
  "classification_match_7"
)

required_model_risk_cols <- c(
  key_cols,
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
# 6. Core metrics
# ------------------------------------------------------------

cat("Creating core manual metrics...\n")

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

cat("Core manual metrics created.\n\n")


# ------------------------------------------------------------
# 7. Pedagogical tables
# ------------------------------------------------------------

cat("Creating pedagogical tables...\n")

pipeline_table <- tibble::tibble(
  script = c(
    "Script 01", "Script 02", "Script 02b", "Script 02c",
    "Script 03", "Script 04", "Script 05", "Script 06",
    "Script 07", "Script 08", "Script 09", "Script 10",
    "Script 11", "Script 12", "Script 12b", "Script 13",
    "Script 14", "Script 15", "Script 16", "Script 17"
  ),
  title = c(
    "Data Availability Audit",
    "Regulatory Documentation and Raw Data Ingestion",
    "Download Regulatory Documentation",
    "Fix Missing Regulatory Documentation Downloads",
    "Clean Federal Reserve DFAST Results",
    "Structure Federal Reserve Macro Scenarios",
    "Build DFAST Benchmark Dataset",
    "Capital and Losses Transmission Layer",
    "Exploratory Analysis",
    "Build Modelling Sample and Treatment Rules",
    "Estimate Credit Loss Model",
    "Estimate PPNR Model",
    "Estimate Capital Depletion Model",
    "Integrated Stress Test Projection Engine",
    "Fix Integrated Projection Join Keys",
    "Stress Test Results and Bank Vulnerability Ranking",
    "Benchmark Validation Against Federal Reserve Public Results",
    "Robustness, Sensitivity and Model Risk Assessment",
    "Create Final Institutional Report",
    "Create Final Technical and Pedagogical Manual"
  ),
  student_learning_point = c(
    "How to verify whether a project is feasible before modelling.",
    "How to document source provenance and regulatory basis.",
    "How public documentation supports reproducibility.",
    "How to handle missing public source links transparently.",
    "How to clean raw public supervisory data.",
    "How macro scenarios are structured for stress testing.",
    "How to build a bank-year-scenario benchmark panel.",
    "How accounting logic links PPNR, losses and capital.",
    "How to inspect distributions, outliers and empirical structure.",
    "How to create a clean econometric modelling sample.",
    "How to estimate stress-period credit loss intensity.",
    "How to estimate revenue capacity under stress.",
    "How to estimate capital depletion under stress.",
    "How to combine component models into one engine.",
    "Why unique join keys are essential in panel data.",
    "How to rank banks under model-based vulnerability metrics.",
    "How to validate a model against a public benchmark.",
    "How to assess tail error, threshold sensitivity and model risk.",
    "How to communicate institutional results.",
    "How to convert the project into a teaching manual."
  )
) |>
  safe_df()

concept_table <- tibble::tibble(
  concept = c(
    "Stress test",
    "DFAST-style replication",
    "Scenario",
    "Severely adverse scenario",
    "Credit losses",
    "PPNR",
    "Provision",
    "CET1 ratio",
    "Capital depletion",
    "Risk-weighted assets",
    "Benchmark validation",
    "Model risk",
    "Tail error",
    "Threshold classification",
    "Vulnerability ranking"
  ),
  explanation = c(
    "A forward-looking exercise that evaluates how a bank performs under adverse economic and financial conditions.",
    "A public-data reconstruction of selected stress test outputs, without access to confidential supervisory models.",
    "A coherent set of macroeconomic and financial assumptions used to project bank outcomes.",
    "A severe hypothetical scenario designed to test resilience under material stress.",
    "Losses generated by loan portfolios and credit exposures during the stress horizon.",
    "Pre-provision net revenue; a measure of earnings available to absorb losses before provisions.",
    "Allowance or expense recognized to absorb expected credit losses.",
    "Common Equity Tier 1 capital divided by risk-weighted assets.",
    "Reduction in capital ratio between the starting point and the stressed minimum.",
    "Assets adjusted by regulatory risk weights to reflect relative riskiness.",
    "Comparison of model projections with observed public DFAST benchmark outcomes.",
    "Risk that a model produces inaccurate, unstable or misleading results.",
    "A large error located in the extreme part of the error distribution.",
    "Classification of observations as above or below selected capital thresholds.",
    "Relative ranking of banks according to model-based stress vulnerability indicators."
  )
) |>
  safe_df()

variable_table <- tibble::tibble(
  variable = c(
    "actual_credit_loss_ratio",
    "predicted_credit_loss_ratio",
    "actual_ppnr_ratio",
    "predicted_ppnr_ratio",
    "actual_capital_depletion",
    "predicted_capital_depletion",
    "observed_cet1_min_ratio",
    "integrated_predicted_cet1_min_ratio",
    "cet1_min_prediction_error",
    "abs_cet1_min_prediction_error",
    "model_risk_score",
    "model_risk_bucket"
  ),
  interpretation = c(
    "Observed credit loss intensity from public benchmark data.",
    "Model-predicted credit loss intensity.",
    "Observed PPNR ratio from public benchmark data.",
    "Model-predicted PPNR ratio.",
    "Observed CET1 capital depletion.",
    "Model-predicted CET1 capital depletion.",
    "Observed public DFAST stressed minimum CET1 ratio.",
    "Integrated model-predicted stressed minimum CET1 ratio.",
    "Predicted CET1 minimum ratio minus observed CET1 minimum ratio.",
    "Absolute value of the CET1 prediction error.",
    "Composite score measuring potential model risk at observation level.",
    "Categorical classification of model risk."
  )
) |>
  safe_df()

model_equation_table <- tibble::tibble(
  model_block = c(
    "Credit loss model",
    "PPNR model",
    "Capital depletion model",
    "Integrated CET1 projection"
  ),
  simplified_equation = c(
    "Credit loss ratio = f(bank effects, year effects, scenario indicators, balance sheet controls)",
    "PPNR ratio = f(bank effects, year effects, scenario indicators, profitability controls)",
    "CET1 depletion = f(credit losses, PPNR, RWA dynamics, initial CET1, bank effects, year effects)",
    "Predicted CET1 minimum ratio = Starting CET1 ratio - Predicted CET1 depletion"
  ),
  pedagogical_message = c(
    "Credit losses are not only macro driven; they also depend on bank-specific portfolios.",
    "PPNR is the first buffer against stress losses.",
    "Capital depletion aggregates multiple channels and is therefore harder to model than a single component.",
    "The integrated engine converts model components into a capital adequacy outcome."
  )
) |>
  safe_df()

validation_metrics_table <- tibble::tibble(
  metric = c("RMSE", "MAE", "Bias", "R-squared", "Correlation", "Classification accuracy"),
  meaning = c(
    "Root mean squared error; penalizes large errors more heavily.",
    "Mean absolute error; average size of the prediction error.",
    "Average prediction error; indicates whether predictions are systematically above or below observed values.",
    "Share of outcome variation explained by the prediction.",
    "Linear association between observed and predicted values.",
    "Share of observations correctly classified relative to a threshold."
  ),
  classroom_warning = c(
    "A low RMSE can hide important tail errors.",
    "MAE is intuitive but does not penalize large errors as strongly as RMSE.",
    "A low bias does not mean every prediction is accurate.",
    "A high R-squared does not eliminate model risk.",
    "High correlation can coexist with level errors.",
    "Accuracy depends on the chosen threshold."
  )
) |>
  safe_df()

exercise_table <- tibble::tibble(
  exercise_number = 1:12,
  exercise = c(
    "Explain why a public DFAST replication cannot reproduce confidential supervisory models.",
    "Identify the role of PPNR in the stress testing transmission mechanism.",
    "Explain why duplicated join keys increased the integrated panel from 520 to 658 rows.",
    "Compute RMSE and MAE manually for a small set of observed and predicted CET1 ratios.",
    "Compare the interpretation of R-squared and correlation.",
    "Discuss why CET1 classification at 4.5 percent can be perfect while 7.0 percent classification is not.",
    "Interpret the difference between vulnerability ranking and model risk ranking.",
    "Explain why DB USA Corporation appears as a tail error case in the validation output.",
    "Discuss why a model can have low average bias but still have severe individual errors.",
    "Propose one additional robustness test for the project.",
    "Write a short policy note explaining the limitations of public-data stress testing.",
    "Replicate one figure and explain its economic interpretation."
  ),
  expected_learning_outcome = c(
    "Understand the boundary between public replication and official supervision.",
    "Understand earnings as a loss absorption channel.",
    "Understand panel-data keys and join mechanics.",
    "Understand forecast error metrics.",
    "Understand model fit versus association.",
    "Understand threshold sensitivity.",
    "Distinguish bank vulnerability from model uncertainty.",
    "Interpret tail risk.",
    "Understand distributional model risk.",
    "Think critically about robustness.",
    "Communicate limitations clearly.",
    "Connect graphics with economic interpretation."
  )
) |>
  safe_df()

limitations_table <- tibble::tibble(
  limitation = c(
    "Public-data-only design",
    "Reduced-form specification",
    "No confidential supervisory assumptions",
    "No internal bank portfolio data",
    "Potential bank-specific idiosyncrasies",
    "Tail error concentration",
    "Ranking sensitivity",
    "Pedagogical simplification"
  ),
  consequence = c(
    "The model is reproducible but necessarily incomplete.",
    "The model explains observed outcomes but is not a structural supervisory model.",
    "The project cannot reproduce Federal Reserve confidential modelling layers.",
    "Detailed loan-level, trading, operational and management-action data are not observed.",
    "Some banks may have special features not captured by public variables.",
    "A small number of observations can have large errors.",
    "Relative rankings may change under alternative specifications.",
    "Some technical details are simplified for teaching and transparency."
  ),
  recommended_response = c(
    "Document all sources and keep the workflow reproducible.",
    "Explain the model as a benchmark, not as an official engine.",
    "Use explicit disclaimer language.",
    "Avoid overclaiming precision.",
    "Report largest errors and model risk rankings.",
    "Discuss tail risk separately from average fit.",
    "Use sensitivity checks and alternative thresholds.",
    "Provide exercises and technical appendices."
  )
) |>
  safe_df()

cat("Pedagogical tables created.\n\n")


# ------------------------------------------------------------
# 8. Rankings and report tables
# ------------------------------------------------------------

cat("Creating ranking and output tables...\n")

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

cat("Ranking and output tables created.\n\n")


# ------------------------------------------------------------
# 9. Figure inventory
# ------------------------------------------------------------

cat("Preparing manual figure inventory...\n")

figure_sources <- tibble::tibble(
  figure_id = c(
    "latest_top20_min_cet1",
    "composite_vulnerability",
    "observed_vs_predicted_cet1",
    "cet1_error_distribution",
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
    "outputs/final_manual/figures/fig01_latest_top20_min_predicted_cet1.png",
    "outputs/final_manual/figures/fig02_composite_vulnerability_score.png",
    "outputs/final_manual/figures/fig03_observed_vs_predicted_cet1.png",
    "outputs/final_manual/figures/fig04_cet1_error_distribution.png",
    "outputs/final_manual/figures/fig05_validation_precision_buckets.png",
    "outputs/final_manual/figures/fig06_model_risk_buckets.png",
    "outputs/final_manual/figures/fig07_threshold_sensitivity.png",
    "outputs/final_manual/figures/fig08_bank_model_risk.png"
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

cat("Manual figure inventory prepared.\n\n")


# ------------------------------------------------------------
# 10. Output paths and Excel workbook
# ------------------------------------------------------------

cat("Saving final manual tables and workbook...\n")

out_dir <- "outputs/final_manual"

paths_out <- list(
  input_check = file.path(out_dir, "script17_input_check.csv"),
  required_column_check = file.path(out_dir, "script17_required_column_check.csv"),
  key_audit_summary = file.path(out_dir, "script17_key_audit_summary.csv"),
  executive_summary = file.path(out_dir, "script17_executive_summary.csv"),
  core_accuracy = file.path(out_dir, "script17_core_accuracy.csv"),
  threshold_summary = file.path(out_dir, "script17_threshold_summary.csv"),
  model_risk_bucket_summary = file.path(out_dir, "script17_model_risk_bucket_summary.csv"),
  pipeline_table = file.path(out_dir, "script17_pipeline_table.csv"),
  concept_table = file.path(out_dir, "script17_concept_table.csv"),
  variable_table = file.path(out_dir, "script17_variable_table.csv"),
  model_equation_table = file.path(out_dir, "script17_model_equation_table.csv"),
  validation_metrics_table = file.path(out_dir, "script17_validation_metrics_table.csv"),
  exercise_table = file.path(out_dir, "script17_student_exercises.csv"),
  limitations_table = file.path(out_dir, "script17_limitations_table.csv"),
  top_latest = file.path(out_dir, "script17_top_latest_ranking.csv"),
  largest_errors = file.path(out_dir, "script17_largest_cet1_errors.csv"),
  bank_model_risk = file.path(out_dir, "script17_bank_model_risk.csv"),
  figure_inventory = file.path(out_dir, "script17_figure_inventory.csv"),
  execution_summary = file.path(out_dir, "script17_execution_summary.csv"),
  excel = file.path(out_dir, "script17_final_manual_outputs.xlsx"),
  manual_docx = file.path(project_root, "manual", "final_technical_pedagogical_manual_dfast_replication.docx"),
  manual_docx_copy = file.path(project_root, "outputs", "final_manual", "final_technical_pedagogical_manual_dfast_replication.docx"),
  execution_log = file.path(out_dir, "script17_execution_log.txt")
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
  manual_path = paths_out$manual_docx
) |>
  safe_df()

readr::write_csv(input_check, paths_out$input_check)
readr::write_csv(required_column_check, paths_out$required_column_check)
readr::write_csv(key_audit_summary, paths_out$key_audit_summary)
readr::write_csv(executive_summary, paths_out$executive_summary)
readr::write_csv(core_accuracy, paths_out$core_accuracy)
readr::write_csv(threshold_summary, paths_out$threshold_summary)
readr::write_csv(model_risk_bucket_summary, paths_out$model_risk_bucket_summary)
readr::write_csv(pipeline_table, paths_out$pipeline_table)
readr::write_csv(concept_table, paths_out$concept_table)
readr::write_csv(variable_table, paths_out$variable_table)
readr::write_csv(model_equation_table, paths_out$model_equation_table)
readr::write_csv(validation_metrics_table, paths_out$validation_metrics_table)
readr::write_csv(exercise_table, paths_out$exercise_table)
readr::write_csv(limitations_table, paths_out$limitations_table)
readr::write_csv(table_top_latest, paths_out$top_latest)
readr::write_csv(table_largest_errors, paths_out$largest_errors)
readr::write_csv(table_bank_model_risk, paths_out$bank_model_risk)
readr::write_csv(figure_inventory, paths_out$figure_inventory)
readr::write_csv(execution_summary, paths_out$execution_summary)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  execution_summary = execution_summary,
  executive_summary = executive_summary,
  core_accuracy = core_accuracy,
  threshold_summary = threshold_summary,
  model_risk_buckets = model_risk_bucket_summary,
  pipeline = pipeline_table,
  concepts = concept_table,
  variables = variable_table,
  model_equations = model_equation_table,
  validation_metrics = validation_metrics_table,
  student_exercises = exercise_table,
  limitations = limitations_table,
  latest_top20 = table_top_latest,
  largest_cet1_errors = table_largest_errors,
  bank_model_risk = table_bank_model_risk,
  figure_inventory = figure_inventory
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, paths_out$excel, overwrite = TRUE)

cat("Final manual tables and workbook saved.\n\n")


# ------------------------------------------------------------
# 11. Create technical and pedagogical manual
# ------------------------------------------------------------

cat("Creating final technical and pedagogical Word manual...\n")

doc <- officer::read_docx()

# Cover
doc <- doc |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "heading 1") |>
  officer::body_add_par("Final Technical and Pedagogical Manual", style = "heading 2") |>
  officer::body_add_par(
    "A public, reproducible and didactic DFAST-style stress testing project",
    style = "Normal"
  ) |>
  officer::body_add_par(
    paste("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    style = "Normal"
  ) |>
  officer::body_add_par(
    "Prepared for students, instructors, researchers and interested public readers.",
    style = "Normal"
  ) |>
  officer::body_add_break()


# Preface
doc <- doc |>
  officer::body_add_par("Preface", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "This manual explains the construction of a public DFAST-style bank stress testing engine using only public data. It is designed primarily for students, but it can also be used by analysts and interested readers who want to understand the mechanics of stress testing, supervisory data, capital ratios and model validation.",
    "The manual is intentionally technical and pedagogical. It does not only present final results; it explains the logic of the project, the role of each script, the meaning of the variables, the structure of the models, the interpretation of validation metrics and the limitations of a public replication exercise.",
    "The central message is that a useful stress testing project must combine data governance, regulatory interpretation, financial accounting, econometric modelling, validation, model risk assessment and transparent communication."
  )
)


# Executive overview
doc <- doc |>
  officer::body_add_par("1. Executive Overview", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The project builds a reduced-form replication of selected DFAST-style stress testing outputs for large banking organizations in the United States. It uses Federal Reserve public stress test results and public scenario data to estimate models for credit losses, PPNR and CET1 capital depletion.",
    "The final integrated engine predicts stressed minimum CET1 ratios and compares them with Federal Reserve public benchmark outcomes. The model achieves strong validation results, with high correlation, low average error and limited systematic bias.",
    "The results should be interpreted as a public benchmark and teaching tool, not as an official supervisory stress test or bank rating system."
  )
)

doc <- add_table(
  doc,
  executive_summary,
  title = "Table 1. Manual executive summary",
  digits = 4,
  font_size = 8
)


# Conceptual foundations
doc <- doc |>
  officer::body_add_par("2. Conceptual Foundations of Bank Stress Testing", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "A bank stress test is a forward-looking exercise. It asks how a bank's balance sheet, income statement and capital ratios would behave under adverse economic and financial conditions.",
    "The essential question is not whether a bank is profitable in normal times. The essential question is whether it can absorb losses and continue to satisfy capital adequacy standards under stress.",
    "In a simplified stress testing framework, macroeconomic stress affects borrower defaults, credit losses, revenue, provisions, trading losses, risk-weighted assets and capital ratios. The final outcome is usually expressed through capital adequacy measures such as the CET1 ratio."
  )
)

doc <- add_table(
  doc,
  concept_table,
  title = "Table 2. Core concepts",
  digits = 4,
  font_size = 8
)


# Regulatory and public data scope
doc <- doc |>
  officer::body_add_par("3. Regulatory and Public Data Scope", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The empirical scope is based on public Federal Reserve DFAST outputs. Public DFAST results are useful because they provide observed stress outcomes across banks, years and scenarios.",
    "A public replication has strict boundaries. It can use observed public outcomes and public scenario data, but it cannot reproduce confidential supervisory models, confidential bank submissions, loan-level supervisory data or internal management actions.",
    "This distinction is fundamental for students. A public-data model can be rigorous, transparent and informative, but it remains a benchmark model. It is not the official model used by supervisors."
  )
)


# Pipeline
doc <- doc |>
  officer::body_add_par("4. Reproducible Research Pipeline", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The project is organized as a sequential R pipeline. Each script performs a defined task, creates outputs and leaves an audit trail.",
    "This structure is pedagogically important because it allows students to understand that empirical modelling begins before estimation. Data availability, source documentation, cleaning, key integrity and sample construction are part of the model-building process."
  )
)

doc <- add_table(
  doc,
  pipeline_table,
  title = "Table 3. Script-by-script learning map",
  digits = 4,
  font_size = 7
)


# Data structure and variables
doc <- doc |>
  officer::body_add_par("5. Data Structure and Variables", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The final dataset is organized at the bank-year-scenario level. Each observation corresponds to one bank, one stress test exercise year and one scenario category.",
    "The most important technical requirement is key uniqueness. The final panel must contain one row per bank-year-scenario key. If duplicated keys remain, joins between model components can create artificial row expansion and distort results.",
    "This project identified and corrected such a problem in Script 12b. That correction is an important teaching example in panel data management."
  )
)

doc <- add_table(
  doc,
  key_audit_summary,
  title = "Table 4. Final key audit",
  digits = 4,
  font_size = 8
)

doc <- add_table(
  doc,
  variable_table,
  title = "Table 5. Main variables used in the manual",
  digits = 4,
  font_size = 8
)


# Transmission mechanism
doc <- doc |>
  officer::body_add_par("6. Stress Testing Transmission Mechanism", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The transmission mechanism links stress conditions to capital outcomes. In simplified form, stress increases losses, affects earnings, changes risk-weighted assets and reduces capital ratios.",
    "PPNR is important because it absorbs losses before capital is depleted. A bank with high loss-absorbing revenue may experience lower capital pressure than a bank with similar losses but weaker earnings.",
    "Capital depletion is the bridge between stressed income and regulatory capital adequacy. The integrated model therefore estimates credit losses, PPNR and capital depletion before computing the predicted stressed CET1 minimum ratio."
  )
)

doc <- add_table(
  doc,
  model_equation_table,
  title = "Table 6. Simplified model equations and teaching messages",
  digits = 4,
  font_size = 8
)


# Econometric models
doc <- doc |>
  officer::body_add_par("7. Econometric Modelling Blocks", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The project estimates three main reduced-form model blocks: credit losses, PPNR and CET1 capital depletion.",
    "The operational models use bank and year fixed effects. Bank fixed effects capture persistent differences across institutions. Year fixed effects capture common exercise-year conditions and changes in the stress testing environment.",
    "The purpose is not to discover a universal structural law of bank losses. The purpose is to build a transparent, empirically disciplined benchmark that approximates public DFAST outcomes."
  )
)

doc <- doc |>
  officer::body_add_par("7.1. Credit Loss Model", style = "heading 2")

doc <- add_paragraphs(
  doc,
  c(
    "The credit loss model estimates stress-period loss intensity. It reflects the fact that credit losses depend on both macro-financial stress and bank-specific exposure structures.",
    "In teaching terms, this block helps students understand why the same macro scenario can have different effects across banks."
  )
)

doc <- doc |>
  officer::body_add_par("7.2. PPNR Model", style = "heading 2")

doc <- add_paragraphs(
  doc,
  c(
    "The PPNR model estimates the earnings buffer available before provisions and selected losses. It is one of the most important concepts in bank stress testing because earnings can absorb part of the stress before capital is reduced.",
    "For students, PPNR shows that stress testing is not only about losses. It is also about the capacity to generate revenue under adverse conditions."
  )
)

doc <- doc |>
  officer::body_add_par("7.3. Capital Depletion Model", style = "heading 2")

doc <- add_paragraphs(
  doc,
  c(
    "The capital depletion model maps credit losses, PPNR, capital ratios and balance sheet dynamics into the reduction of CET1 capital under stress.",
    "This model is naturally more difficult than the credit loss and PPNR models because it aggregates several channels. Its lower explanatory power relative to the final CET1 ratio is therefore not surprising."
  )
)

doc <- add_table(
  doc,
  core_accuracy,
  title = "Table 7. Accuracy of the main modelling blocks",
  digits = 4,
  font_size = 8
)


# Integrated engine
doc <- doc |>
  officer::body_add_par("8. Integrated Stress Test Engine", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The integrated engine combines the component models into a final capital projection. The simplified formula is: predicted CET1 minimum ratio equals starting CET1 ratio minus predicted CET1 depletion.",
    "The first integrated version produced artificial row expansion because of duplicated bank-year-scenario keys. Script 12b corrected this by forcing one row per key before merging the component predictions.",
    "This correction is a central lesson: in panel-data modelling, correct joins are as important as correct equations."
  )
)

doc <- add_existing_image(
  doc,
  "outputs/final_manual/figures/fig03_observed_vs_predicted_cet1.png",
  caption = "Figure 1. Observed versus predicted CET1 minimum ratio",
  width = 6.5,
  height = 4.2
)


# Final results
doc <- doc |>
  officer::body_add_par("9. Final Stress Test Results and Vulnerability Ranking", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The final results identify banks with lower predicted minimum CET1 ratios, larger predicted capital depletion and weaker stress margins.",
    "The ranking is model-based and relative. It is not an official supervisory ranking, not a credit rating and not an investment recommendation.",
    "For students, the ranking is useful because it shows how model outputs can be transformed into analytical summaries. It also creates an opportunity to discuss responsible interpretation."
  )
)

doc <- add_existing_image(
  doc,
  "outputs/final_manual/figures/fig01_latest_top20_min_predicted_cet1.png",
  caption = "Figure 2. Latest exercise top 20 banks by minimum predicted CET1",
  width = 6.5,
  height = 4.8
)

doc <- add_table(
  doc,
  table_top_latest,
  title = "Table 8. Latest exercise vulnerability ranking",
  digits = 4,
  font_size = 7
)


# Validation
doc <- doc |>
  officer::body_add_par("10. Benchmark Validation", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "Benchmark validation compares the model's predictions with observed public Federal Reserve DFAST outcomes. This step is essential because a stress test engine must be assessed against actual benchmark results.",
    "The key validation variable is the CET1 minimum ratio. The model achieves high explanatory power, low average error and near-zero average bias.",
    "Students should understand that no single validation metric is sufficient. RMSE, MAE, bias, R-squared, correlation and threshold accuracy each answer a different question."
  )
)

doc <- add_table(
  doc,
  validation_metrics_table,
  title = "Table 9. Validation metrics and classroom warnings",
  digits = 4,
  font_size = 8
)

doc <- add_table(
  doc,
  threshold_summary,
  title = "Table 10. Threshold classification accuracy",
  digits = 4,
  font_size = 8
)

doc <- add_existing_image(
  doc,
  "outputs/final_manual/figures/fig04_cet1_error_distribution.png",
  caption = "Figure 3. Distribution of CET1 prediction errors",
  width = 6.5,
  height = 4.2
)

doc <- add_table(
  doc,
  table_largest_errors,
  title = "Table 11. Largest CET1 prediction errors",
  digits = 4,
  font_size = 7
)


# Model risk
doc <- doc |>
  officer::body_add_par("11. Robustness, Sensitivity and Model Risk", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "Model risk is the possibility that the model produces inaccurate, unstable or misleading outputs for its intended use.",
    "In this project, model risk is assessed through error distributions, tail errors, threshold sensitivity, bank-level error concentration and model-risk buckets.",
    "The overall fit is strong, but there are tail cases. This is a critical lesson: a model can perform well on average and still require caution for specific observations."
  )
)

doc <- add_table(
  doc,
  model_risk_bucket_summary,
  title = "Table 12. Model risk bucket summary",
  digits = 4,
  font_size = 8
)

doc <- add_existing_image(
  doc,
  "outputs/final_manual/figures/fig06_model_risk_buckets.png",
  caption = "Figure 4. Model risk buckets",
  width = 6.5,
  height = 4.2
)

doc <- add_existing_image(
  doc,
  "outputs/final_manual/figures/fig07_threshold_sensitivity.png",
  caption = "Figure 5. CET1 threshold sensitivity",
  width = 6.5,
  height = 4.2
)

doc <- add_table(
  doc,
  table_bank_model_risk,
  title = "Table 13. Bank-level model risk ranking",
  digits = 4,
  font_size = 7
)


# Student exercises
doc <- doc |>
  officer::body_add_par("12. Student Exercises", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The exercises below are designed to move students from mechanical reproduction to critical interpretation.",
    "Instructors can assign them individually or as group work. Some exercises require only interpretation, while others require students to inspect output files or rerun selected scripts."
  )
)

doc <- add_table(
  doc,
  exercise_table,
  title = "Table 14. Recommended student exercises",
  digits = 4,
  font_size = 8
)


# Limitations
doc <- doc |>
  officer::body_add_par("13. Limitations and Responsible Interpretation", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "The limitations of the project must be explicit. Public-data stress testing is valuable because it is transparent and reproducible, but it is necessarily incomplete.",
    "The model does not include confidential supervisory assumptions, internal bank data, confidential management actions or loan-level portfolio information.",
    "The correct interpretation is therefore limited: the project is a public benchmark, educational tool and reproducible research framework."
  )
)

doc <- add_table(
  doc,
  limitations_table,
  title = "Table 15. Limitations and recommended responses",
  digits = 4,
  font_size = 8
)


# Teaching plan
doc <- doc |>
  officer::body_add_par("14. Suggested Teaching Plan", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "A four-session teaching plan can be used for an applied banking or macro-financial econometrics course.",
    "Session 1: introduce DFAST, stress testing, CET1, PPNR, RWAs and public supervisory data.",
    "Session 2: reproduce Scripts 01 to 08 and discuss data governance, cleaning, sample construction and key integrity.",
    "Session 3: reproduce Scripts 09 to 12b and discuss fixed effects, component models and the integrated projection engine.",
    "Session 4: reproduce Scripts 13 to 17 and discuss validation, model risk, rankings, limitations and technical communication."
  )
)


# Glossary
doc <- doc |>
  officer::body_add_par("15. Glossary", style = "heading 1")

glossary <- tibble::tibble(
  term = c(
    "DFAST",
    "Stress test",
    "Scenario",
    "CET1",
    "PPNR",
    "RWA",
    "Credit loss ratio",
    "Capital depletion",
    "Fixed effects",
    "RMSE",
    "MAE",
    "Bias",
    "R-squared",
    "Correlation",
    "Benchmark validation",
    "Model risk",
    "Tail error",
    "Threshold sensitivity",
    "Vulnerability ranking",
    "Public replication"
  ),
  definition = c(
    "Dodd-Frank Act Stress Test; a U.S. supervisory stress testing framework.",
    "Forward-looking assessment of bank resilience under adverse conditions.",
    "Set of assumptions used to simulate economic and financial stress.",
    "Common Equity Tier 1 capital ratio.",
    "Pre-provision net revenue.",
    "Risk-weighted assets.",
    "Credit losses scaled by a relevant balance sheet or risk-weighted asset measure.",
    "Reduction in capital ratio under stress.",
    "Econometric controls for unobserved group or time-specific effects.",
    "Root mean squared error.",
    "Mean absolute error.",
    "Average prediction error.",
    "Share of variation explained by model predictions.",
    "Linear association between observed and predicted values.",
    "Comparison of model projections with observed public benchmark outcomes.",
    "Risk that model outputs are inaccurate or unstable for the intended purpose.",
    "Large error in the extreme part of the error distribution.",
    "Assessment of how classification changes under different capital thresholds.",
    "Relative ranking of banks based on model outputs.",
    "Transparent reconstruction using only public data."
  )
) |>
  safe_df()

doc <- add_table(
  doc,
  glossary,
  title = "Table 16. Glossary",
  digits = 4,
  font_size = 8
)


# Final disclaimer
doc <- doc |>
  officer::body_add_par("16. Final Disclaimer", style = "heading 1")

doc <- add_paragraphs(
  doc,
  c(
    "This manual is an independent educational and analytical document based on public data. It is not affiliated with, endorsed by or approved by the Federal Reserve, any banking organization or any supervisory authority.",
    "No confidential supervisory information, confidential bank submissions, non-public models or internal bank data are used.",
    "The manual must not be interpreted as an official regulatory assessment, investment recommendation, credit opinion or statement on the safety and soundness of any institution.",
    "All results are produced for teaching, public benchmarking and reproducible research purposes."
  )
)


# Save manual
dir.create(file.path(project_root, "manual"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(project_root, "outputs", "final_manual"), recursive = TRUE, showWarnings = FALSE)

print(doc, target = paths_out$manual_docx)

file.copy(
  from = paths_out$manual_docx,
  to = paths_out$manual_docx_copy,
  overwrite = TRUE
)

manual_check <- tibble::tibble(
  file_role = c("main_manual_docx", "copy_manual_docx"),
  file_path = c(paths_out$manual_docx, paths_out$manual_docx_copy),
  exists = file.exists(c(paths_out$manual_docx, paths_out$manual_docx_copy)),
  size_bytes = ifelse(
    file.exists(c(paths_out$manual_docx, paths_out$manual_docx_copy)),
    file.info(c(paths_out$manual_docx, paths_out$manual_docx_copy))$size,
    NA_real_
  )
) |>
  safe_df()

print(manual_check)

if (any(!manual_check$exists) |
    any(is.na(manual_check$size_bytes)) |
    any(manual_check$size_bytes == 0)) {
  stop("Manual Word file was not created correctly. Check file permissions or whether the file is open in Word.")
}

cat("Final technical and pedagogical Word manual created.\n\n")


# ------------------------------------------------------------
# 12. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 17 - Create Final Technical and Pedagogical Manual completed",
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
  "Manual check:",
  capture.output(print(manual_check)),
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
  paste(" -", paths_out$manual_docx),
  paste(" -", paths_out$manual_docx_copy),
  paste(" -", paths_out$excel),
  paste(" -", paths_out$execution_log)
)

writeLines(enc2utf8(log_lines), paths_out$execution_log, useBytes = TRUE)


# ------------------------------------------------------------
# 13. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 17 - Create Final Technical and Pedagogical Manual completed\n")
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

cat("Manual file check:\n")
print(manual_check)

cat("\nExecutive summary:\n")
print(executive_summary)

cat("\nCore accuracy:\n")
print(core_accuracy)

cat("\nThreshold summary:\n")
print(threshold_summary)

cat("\nModel risk bucket summary:\n")
print(model_risk_bucket_summary)

cat("\nMain outputs:\n")
cat(" -", paths_out$manual_docx, "\n")
cat(" -", paths_out$manual_docx_copy, "\n")
cat(" -", paths_out$excel, "\n")
cat(" -", paths_out$execution_log, "\n")
cat("============================================================\n")