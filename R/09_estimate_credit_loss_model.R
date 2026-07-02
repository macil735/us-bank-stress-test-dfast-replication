# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 09 — Estimate Credit Loss Model
# ============================================================
# Objective:
#   Estimate credit loss response models using the modelling
#   sample created in Script 08.
#
# Main input:
#   data/processed/model/modelling_sample_winsorized.csv
#
# Main dependent variables:
#   y_credit_loss_ratio_winsor
#   y_credit_loss_rate_winsor
#
# Model families:
#   1. Pooled OLS
#   2. Year fixed effects
#   3. Bank fixed effects
#   4. Bank and year fixed effects
#   5. Scenario-only benchmark model
#
# Main outputs:
#   data/processed/model/credit_loss_model_estimation_sample.csv
#   data/processed/model/credit_loss_model_predictions.csv
#   outputs/credit_loss_model/script09_credit_loss_model_report.docx
#   outputs/credit_loss_model/script09_credit_loss_model_outputs.xlsx
#   outputs/credit_loss_model/script09_execution_log.txt
#
# Methodological note:
#   This script estimates public, reduced-form credit loss models.
#   It does not reproduce confidential Federal Reserve supervisory
#   models.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 09 - Estimate Credit Loss Model\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "09"
script_name <- "estimate_credit_loss_model"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/model", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/credit_loss_model", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/credit_loss_model/figures", recursive = TRUE, showWarnings = FALSE)

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
  "purrr",
  "janitor",
  "broom",
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

safe_r2 <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) < 3) return(NA_real_)

  ss_res <- sum((actual[ok] - predicted[ok])^2)
  ss_tot <- sum((actual[ok] - mean(actual[ok]))^2)

  if (ss_tot == 0) return(NA_real_)

  1 - ss_res / ss_tot
}

tidy_lm_safe <- function(model, model_name) {
  broom::tidy(model) |>
    dplyr::mutate(model_name = model_name, .before = 1) |>
    safe_df()
}

glance_lm_safe <- function(model, model_name) {
  broom::glance(model) |>
    dplyr::mutate(model_name = model_name, .before = 1) |>
    safe_df()
}

fit_lm_safe <- function(formula, data, model_name) {
  tryCatch(
    {
      model <- stats::lm(formula, data = data)
      list(
        model_name = model_name,
        formula = deparse(formula),
        model = model,
        status = "estimated",
        error_message = NA_character_
      )
    },
    error = function(e) {
      list(
        model_name = model_name,
        formula = deparse(formula),
        model = NULL,
        status = "failed",
        error_message = conditionMessage(e)
      )
    }
  )
}

save_plot <- function(plot_object, file_name, width = 9, height = 5.5) {
  path <- file.path("outputs/credit_loss_model/figures", file_name)

  ggplot2::ggsave(
    filename = path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300
  )

  path
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Read input
# ------------------------------------------------------------

cat("Reading modelling sample...\n")

input_path <- "data/processed/model/modelling_sample_winsorized.csv"

if (!file.exists(input_path)) {
  stop(paste("Missing input file:", input_path))
}

sample_raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()

cat("Input loaded.\n")
cat("Rows:", nrow(sample_raw), "\n")
cat("Columns:", ncol(sample_raw), "\n\n")


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
  "credit_loss_model_sample",
  "y_credit_loss_ratio",
  "y_credit_loss_ratio_winsor",
  "y_credit_loss_rate",
  "y_credit_loss_rate_winsor",
  "x_initial_cet1",
  "x_initial_rwa",
  "x_rwa_growth",
  "x_rwa_growth_winsor",
  "x_ppnr_absorption",
  "x_ppnr_absorption_winsor",
  "x_ppnr_to_losses",
  "x_ppnr_to_losses_winsor",
  "x_provision_ratio",
  "x_provision_ratio_winsor",
  "total_loan_losses_amount",
  "rwa_actual_amount",
  "ppnr_amount"
)

required_column_check <- tibble::tibble(
  required_column = required_cols,
  exists = required_cols %in% names(sample_raw)
) |>
  safe_df()

missing_required <- required_column_check |>
  dplyr::filter(!exists) |>
  dplyr::pull(required_column)

required_column_check_path <- "outputs/credit_loss_model/script09_required_column_check.csv"
readr::write_csv(required_column_check, required_column_check_path)

if (length(missing_required) > 0) {
  cat("Missing required variables:\n")
  print(missing_required)
  stop("Script 09 stopped because required variables are missing.")
}

cat("Required variables checked.\n\n")


# ------------------------------------------------------------
# 5. Build estimation sample
# ------------------------------------------------------------

cat("Building credit loss estimation sample...\n")

estimation_sample <- sample_raw |>
  dplyr::filter(credit_loss_model_sample == 1) |>
  dplyr::mutate(
    bank_factor = as.factor(bank_name),
    year_factor = as.factor(exercise_year),
    scenario_factor = as.factor(scenario_label),

    log_rwa_actual = log(pmax(rwa_actual_amount, 1)),
    log_total_loan_losses = log(pmax(total_loan_losses_amount, 0) + 1),
    log_ppnr_amount = log(pmax(ppnr_amount, 0) + 1),

    y_loss_ratio_model = y_credit_loss_ratio_winsor,
    y_loss_rate_model = y_credit_loss_rate_winsor,

    x_initial_cet1_model = x_initial_cet1,
    x_rwa_growth_model = x_rwa_growth_winsor,
    x_ppnr_absorption_model = x_ppnr_absorption_winsor,
    x_ppnr_to_losses_model = x_ppnr_to_losses_winsor,
    x_provision_ratio_model = x_provision_ratio_winsor
  ) |>
  dplyr::filter(
    !is.na(y_loss_ratio_model),
    !is.na(x_initial_cet1_model),
    !is.na(x_rwa_growth_model),
    !is.na(scenario_factor),
    !is.na(year_factor),
    !is.na(bank_factor)
  ) |>
  safe_df()

cat("Estimation sample built.\n")
cat("Rows:", nrow(estimation_sample), "\n")
cat("Banks:", dplyr::n_distinct(estimation_sample$bank_name), "\n")
cat("Years:", dplyr::n_distinct(estimation_sample$exercise_year), "\n")
cat("Scenarios:", dplyr::n_distinct(estimation_sample$scenario_label), "\n\n")

if (nrow(estimation_sample) < 50) {
  stop("Estimation sample too small for Script 09 models.")
}


# ------------------------------------------------------------
# 6. Estimation sample diagnostics
# ------------------------------------------------------------

cat("Creating estimation sample diagnostics...\n")

estimation_sample_summary <- tibble::tibble(
  raw_rows = nrow(sample_raw),
  credit_loss_model_flag_rows = sum(sample_raw$credit_loss_model_sample == 1, na.rm = TRUE),
  final_estimation_rows = nrow(estimation_sample),
  banks = dplyr::n_distinct(estimation_sample$bank_name),
  rssd_ids = dplyr::n_distinct(estimation_sample$bank_rssd_id),
  years = dplyr::n_distinct(estimation_sample$exercise_year),
  scenarios = dplyr::n_distinct(estimation_sample$scenario_label),
  mean_loss_ratio = safe_mean(estimation_sample$y_loss_ratio_model),
  sd_loss_ratio = safe_sd(estimation_sample$y_loss_ratio_model),
  mean_loss_rate = safe_mean(estimation_sample$y_loss_rate_model),
  sd_loss_rate = safe_sd(estimation_sample$y_loss_rate_model)
) |>
  safe_df()

scenario_distribution <- estimation_sample |>
  dplyr::count(exercise_year, scenario_label, name = "observations") |>
  dplyr::arrange(exercise_year, scenario_label) |>
  safe_df()

bank_distribution <- estimation_sample |>
  dplyr::group_by(bank_rssd_id, bank_name) |>
  dplyr::summarise(
    observations = dplyr::n(),
    years = dplyr::n_distinct(exercise_year),
    scenarios = dplyr::n_distinct(scenario_label),
    mean_loss_ratio = safe_mean(y_loss_ratio_model),
    max_loss_ratio = max(y_loss_ratio_model, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(mean_loss_ratio)) |>
  safe_df()

cat("Diagnostics created.\n\n")


# ------------------------------------------------------------
# 7. Model specifications
# ------------------------------------------------------------

cat("Estimating credit loss models...\n")

formula_m1 <- y_loss_ratio_model ~ scenario_factor

formula_m2 <- y_loss_ratio_model ~
  scenario_factor +
  x_initial_cet1_model +
  log_rwa_actual

formula_m3 <- y_loss_ratio_model ~
  scenario_factor +
  x_initial_cet1_model +
  log_rwa_actual +
  x_rwa_growth_model +
  year_factor

formula_m4 <- y_loss_ratio_model ~
  scenario_factor +
  x_initial_cet1_model +
  log_rwa_actual +
  x_rwa_growth_model +
  bank_factor

formula_m5 <- y_loss_ratio_model ~
  scenario_factor +
  x_initial_cet1_model +
  log_rwa_actual +
  x_rwa_growth_model +
  year_factor +
  bank_factor

formula_m6 <- y_loss_rate_model ~
  scenario_factor +
  x_initial_cet1_model +
  log_rwa_actual +
  x_rwa_growth_model +
  year_factor +
  bank_factor

model_specs <- list(
  fit_lm_safe(formula_m1, estimation_sample, "M1_scenario_only"),
  fit_lm_safe(formula_m2, estimation_sample, "M2_pooled_controls"),
  fit_lm_safe(formula_m3, estimation_sample, "M3_year_fixed_effects"),
  fit_lm_safe(formula_m4, estimation_sample, "M4_bank_fixed_effects"),
  fit_lm_safe(formula_m5, estimation_sample, "M5_bank_year_fixed_effects"),
  fit_lm_safe(formula_m6, estimation_sample, "M6_loss_rate_bank_year_fixed_effects")
)

model_status <- purrr::map_dfr(
  model_specs,
  ~ tibble::tibble(
    model_name = .x$model_name,
    formula = .x$formula,
    status = .x$status,
    error_message = .x$error_message
  )
) |>
  safe_df()

estimated_models <- model_specs[
  purrr::map_chr(model_specs, "status") == "estimated"
]

if (length(estimated_models) == 0) {
  stop("No credit loss model was estimated successfully.")
}

cat("Models estimated.\n")
print(model_status)
cat("\n")


# ------------------------------------------------------------
# 8. Coefficients and fit statistics
# ------------------------------------------------------------

cat("Extracting model results...\n")

coefficient_table <- purrr::map_dfr(
  estimated_models,
  ~ tidy_lm_safe(.x$model, .x$model_name)
) |>
  dplyr::mutate(
    significant_10pct = p.value < 0.10,
    significant_5pct = p.value < 0.05,
    significant_1pct = p.value < 0.01
  ) |>
  safe_df()

fit_table <- purrr::map_dfr(
  estimated_models,
  ~ glance_lm_safe(.x$model, .x$model_name)
) |>
  dplyr::select(
    model_name,
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value,
    df,
    logLik,
    AIC,
    BIC,
    deviance,
    df.residual,
    nobs
  ) |>
  safe_df()

cat("Model results extracted.\n\n")


# ------------------------------------------------------------
# 9. Predictions
# ------------------------------------------------------------

cat("Creating model predictions...\n")

prediction_base <- estimation_sample |>
  dplyr::select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    y_loss_ratio_model,
    y_loss_rate_model,
    total_loan_losses_amount,
    rwa_actual_amount,
    ppnr_amount
  )

for (m in estimated_models) {
  pred_name <- paste0("pred_", m$model_name)
  prediction_base[[pred_name]] <- as.numeric(stats::predict(m$model, newdata = estimation_sample))
}

prediction_cols <- names(prediction_base)[stringr::str_detect(names(prediction_base), "^pred_")]

prediction_long <- prediction_base |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(prediction_cols),
    names_to = "model_name",
    values_to = "predicted_loss_ratio"
  ) |>
  dplyr::mutate(
    model_name = stringr::str_remove(model_name, "^pred_"),
    residual_loss_ratio = y_loss_ratio_model - predicted_loss_ratio
  ) |>
  safe_df()

prediction_metrics <- prediction_long |>
  dplyr::group_by(model_name) |>
  dplyr::summarise(
    observations = dplyr::n(),
    rmse = safe_rmse(y_loss_ratio_model, predicted_loss_ratio),
    mae = safe_mae(y_loss_ratio_model, predicted_loss_ratio),
    r2_outcome_prediction = safe_r2(y_loss_ratio_model, predicted_loss_ratio),
    mean_residual = safe_mean(residual_loss_ratio),
    sd_residual = safe_sd(residual_loss_ratio),
    .groups = "drop"
  ) |>
  dplyr::arrange(rmse) |>
  safe_df()

best_model_name <- prediction_metrics |>
  dplyr::arrange(rmse) |>
  dplyr::slice(1) |>
  dplyr::pull(model_name)

cat("Predictions created.\n")
cat("Best model by RMSE:", best_model_name, "\n\n")


# ------------------------------------------------------------
# 10. Scenario effects extraction
# ------------------------------------------------------------

cat("Extracting scenario effects...\n")

scenario_effects <- coefficient_table |>
  dplyr::filter(stringr::str_detect(term, "^scenario_factor")) |>
  dplyr::mutate(
    scenario_effect = stringr::str_replace(term, "^scenario_factor", "")
  ) |>
  dplyr::select(
    model_name,
    scenario_effect,
    estimate,
    std.error,
    statistic,
    p.value,
    significant_10pct,
    significant_5pct,
    significant_1pct
  ) |>
  dplyr::arrange(model_name, scenario_effect) |>
  safe_df()

cat("Scenario effects extracted.\n\n")


# ------------------------------------------------------------
# 11. Residual diagnostics
# ------------------------------------------------------------

cat("Creating residual diagnostics...\n")

residual_diagnostics <- prediction_long |>
  dplyr::group_by(model_name, scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    mean_actual = safe_mean(y_loss_ratio_model),
    mean_predicted = safe_mean(predicted_loss_ratio),
    mean_residual = safe_mean(residual_loss_ratio),
    rmse = safe_rmse(y_loss_ratio_model, predicted_loss_ratio),
    mae = safe_mae(y_loss_ratio_model, predicted_loss_ratio),
    .groups = "drop"
  ) |>
  dplyr::arrange(model_name, scenario_label) |>
  safe_df()

bank_residual_ranking <- prediction_long |>
  dplyr::filter(model_name == best_model_name) |>
  dplyr::group_by(bank_rssd_id, bank_name) |>
  dplyr::summarise(
    observations = dplyr::n(),
    mean_actual = safe_mean(y_loss_ratio_model),
    mean_predicted = safe_mean(predicted_loss_ratio),
    mean_residual = safe_mean(residual_loss_ratio),
    rmse = safe_rmse(y_loss_ratio_model, predicted_loss_ratio),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(rmse)) |>
  safe_df()

cat("Residual diagnostics created.\n\n")


# ------------------------------------------------------------
# 12. Figures
# ------------------------------------------------------------

cat("Creating figures...\n")

figure_paths <- c()

plot_data_best <- prediction_long |>
  dplyr::filter(model_name == best_model_name)

p1 <- ggplot2::ggplot(
  plot_data_best,
  ggplot2::aes(x = y_loss_ratio_model, y = predicted_loss_ratio)
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = paste("Actual versus Predicted Credit Loss Ratio -", best_model_name),
    x = "Actual credit loss ratio",
    y = "Predicted credit loss ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p1, "fig01_actual_vs_predicted_credit_loss_ratio.png")
)

p2 <- ggplot2::ggplot(
  plot_data_best,
  ggplot2::aes(x = residual_loss_ratio)
) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::labs(
    title = paste("Residual Distribution -", best_model_name),
    x = "Residual",
    y = "Number of observations"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p2, "fig02_residual_distribution.png")
)

p3 <- prediction_metrics |>
  ggplot2::ggplot(
    ggplot2::aes(x = reorder(model_name, rmse), y = rmse)
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Credit Loss Model Comparison by RMSE",
    x = "Model",
    y = "RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p3, "fig03_model_rmse_comparison.png")
)

p4 <- residual_diagnostics |>
  dplyr::filter(model_name == best_model_name) |>
  ggplot2::ggplot(
    ggplot2::aes(x = scenario_label, y = rmse)
  ) +
  ggplot2::geom_col() +
  ggplot2::labs(
    title = paste("RMSE by Scenario -", best_model_name),
    x = "Scenario",
    y = "RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p4, "fig04_rmse_by_scenario.png")
)

p5 <- bank_residual_ranking |>
  dplyr::slice_max(order_by = rmse, n = 20, with_ties = FALSE) |>
  ggplot2::ggplot(
    ggplot2::aes(x = reorder(bank_name, rmse), y = rmse)
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = paste("Top 20 Bank Residual RMSE -", best_model_name),
    x = "Bank",
    y = "RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p5, "fig05_top20_bank_residual_rmse.png", width = 10, height = 7)
)

figure_inventory <- tibble::tibble(
  figure_path = figure_paths,
  exists = file.exists(figure_paths),
  size_bytes = ifelse(file.exists(figure_paths), file.info(figure_paths)$size, NA_real_)
) |>
  safe_df()

cat("Figures created:", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")


# ------------------------------------------------------------
# 13. Save datasets
# ------------------------------------------------------------

cat("Saving processed datasets...\n")

estimation_sample_path <- "data/processed/model/credit_loss_model_estimation_sample.csv"
predictions_path <- "data/processed/model/credit_loss_model_predictions.csv"

readr::write_csv(estimation_sample, estimation_sample_path)
readr::write_csv(prediction_long, predictions_path)

cat("Processed datasets saved.\n\n")


# ------------------------------------------------------------
# 14. Save tabular outputs
# ------------------------------------------------------------

cat("Saving model outputs...\n")

out_dir <- "outputs/credit_loss_model"

estimation_sample_summary_path <- file.path(out_dir, "script09_estimation_sample_summary.csv")
scenario_distribution_path <- file.path(out_dir, "script09_scenario_distribution.csv")
bank_distribution_path <- file.path(out_dir, "script09_bank_distribution.csv")
model_status_path <- file.path(out_dir, "script09_model_status.csv")
coefficient_table_path <- file.path(out_dir, "script09_coefficient_table.csv")
fit_table_path <- file.path(out_dir, "script09_fit_table.csv")
prediction_metrics_path <- file.path(out_dir, "script09_prediction_metrics.csv")
scenario_effects_path <- file.path(out_dir, "script09_scenario_effects.csv")
residual_diagnostics_path <- file.path(out_dir, "script09_residual_diagnostics.csv")
bank_residual_ranking_path <- file.path(out_dir, "script09_bank_residual_ranking.csv")
figure_inventory_path <- file.path(out_dir, "script09_figure_inventory.csv")
execution_summary_path <- file.path(out_dir, "script09_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script09_credit_loss_model_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script09_credit_loss_model_report.docx")
execution_log_path <- file.path(out_dir, "script09_execution_log.txt")

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_rows = nrow(sample_raw),
  estimation_rows = nrow(estimation_sample),
  banks = dplyr::n_distinct(estimation_sample$bank_name),
  years = dplyr::n_distinct(estimation_sample$exercise_year),
  scenarios = dplyr::n_distinct(estimation_sample$scenario_label),
  models_attempted = length(model_specs),
  models_estimated = length(estimated_models),
  best_model_by_rmse = best_model_name,
  figures_created = sum(figure_inventory$exists, na.rm = TRUE)
) |>
  safe_df()

readr::write_csv(estimation_sample_summary, estimation_sample_summary_path)
readr::write_csv(scenario_distribution, scenario_distribution_path)
readr::write_csv(bank_distribution, bank_distribution_path)
readr::write_csv(model_status, model_status_path)
readr::write_csv(coefficient_table, coefficient_table_path)
readr::write_csv(fit_table, fit_table_path)
readr::write_csv(prediction_metrics, prediction_metrics_path)
readr::write_csv(scenario_effects, scenario_effects_path)
readr::write_csv(residual_diagnostics, residual_diagnostics_path)
readr::write_csv(bank_residual_ranking, bank_residual_ranking_path)
readr::write_csv(figure_inventory, figure_inventory_path)
readr::write_csv(execution_summary, execution_summary_path)

cat("Model outputs saved.\n\n")


# ------------------------------------------------------------
# 15. Excel workbook
# ------------------------------------------------------------

cat("Creating Excel workbook...\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "execution_summary")
openxlsx::writeData(wb, "execution_summary", execution_summary)

openxlsx::addWorksheet(wb, "sample_summary")
openxlsx::writeData(wb, "sample_summary", estimation_sample_summary)

openxlsx::addWorksheet(wb, "model_status")
openxlsx::writeData(wb, "model_status", model_status)

openxlsx::addWorksheet(wb, "fit_table")
openxlsx::writeData(wb, "fit_table", fit_table)

openxlsx::addWorksheet(wb, "prediction_metrics")
openxlsx::writeData(wb, "prediction_metrics", prediction_metrics)

openxlsx::addWorksheet(wb, "scenario_effects")
openxlsx::writeData(wb, "scenario_effects", scenario_effects)

openxlsx::addWorksheet(wb, "coefficients")
openxlsx::writeData(wb, "coefficients", coefficient_table)

openxlsx::addWorksheet(wb, "residual_diagnostics")
openxlsx::writeData(wb, "residual_diagnostics", residual_diagnostics)

openxlsx::addWorksheet(wb, "bank_residual_ranking")
openxlsx::writeData(wb, "bank_residual_ranking", bank_residual_ranking)

openxlsx::addWorksheet(wb, "figure_inventory")
openxlsx::writeData(wb, "figure_inventory", figure_inventory)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output_path, overwrite = TRUE)

cat("Excel workbook created.\n\n")


# ------------------------------------------------------------
# 16. Word report
# ------------------------------------------------------------

cat("Creating Word report...\n")

doc <- officer::read_docx()

doc <- doc |>
  officer::body_add_par("Script 09 - Estimate Credit Loss Model", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This script estimates reduced-form public credit loss models using the modelling sample prepared in Script 08. The models explain observed DFAST credit loss ratios using scenario indicators, initial capital, RWA scale, RWA dynamics and fixed effects.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Execution summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(execution_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Estimation sample summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(estimation_sample_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. Model status", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(model_status) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("5. Model fit comparison", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(
    prediction_metrics |>
      dplyr::arrange(rmse)
  ) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("6. Scenario effects", style = "heading 2")

if (nrow(scenario_effects) == 0) {
  doc <- officer::body_add_par(
    doc,
    "No scenario effects were extracted from the estimated models.",
    style = "Normal"
  )
} else {
  doc <- flextable::body_add_flextable(
    x = doc,
    value = flextable::flextable(scenario_effects) |>
      flextable::autofit()
  )
}

doc <- doc |>
  officer::body_add_par("7. Residual diagnostics by scenario", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(residual_diagnostics) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("8. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "The estimated models are reduced-form and based only on public DFAST information. They should be interpreted as benchmarking and replication tools, not as replicas of confidential supervisory models.",
    style = "Normal"
  )

print(doc, target = report_docx_path)

cat("Word report created.\n\n")


# ------------------------------------------------------------
# 17. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 09 - Estimate Credit Loss Model completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input rows:", nrow(sample_raw)),
  paste("Estimation rows:", nrow(estimation_sample)),
  paste("Banks:", dplyr::n_distinct(estimation_sample$bank_name)),
  paste("Years:", dplyr::n_distinct(estimation_sample$exercise_year)),
  paste("Scenarios:", dplyr::n_distinct(estimation_sample$scenario_label)),
  paste("Models attempted:", length(model_specs)),
  paste("Models estimated:", length(estimated_models)),
  paste("Best model by RMSE:", best_model_name),
  paste("Figures created:", sum(figure_inventory$exists, na.rm = TRUE)),
  "",
  "Model status:",
  capture.output(print(model_status)),
  "",
  "Prediction metrics:",
  capture.output(print(prediction_metrics)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", estimation_sample_path),
  paste(" -", predictions_path),
  paste(" -", estimation_sample_summary_path),
  paste(" -", model_status_path),
  paste(" -", coefficient_table_path),
  paste(" -", fit_table_path),
  paste(" -", prediction_metrics_path),
  paste(" -", scenario_effects_path),
  paste(" -", residual_diagnostics_path),
  paste(" -", bank_residual_ranking_path),
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
# 18. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 09 - Estimate Credit Loss Model completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input rows:\n", nrow(sample_raw), "\n")
cat("Estimation rows:\n", nrow(estimation_sample), "\n")
cat("Banks:\n", dplyr::n_distinct(estimation_sample$bank_name), "\n")
cat("Years:\n", dplyr::n_distinct(estimation_sample$exercise_year), "\n")
cat("Scenarios:\n", dplyr::n_distinct(estimation_sample$scenario_label), "\n")
cat("Models attempted:\n", length(model_specs), "\n")
cat("Models estimated:\n", length(estimated_models), "\n")
cat("Best model by RMSE:\n", best_model_name, "\n")
cat("Figures created:\n", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")

cat("Model status:\n")
print(model_status)

cat("\nPrediction metrics:\n")
print(prediction_metrics)

cat("\nMain outputs:\n")
cat(" -", estimation_sample_path, "\n")
cat(" -", predictions_path, "\n")
cat(" -", estimation_sample_summary_path, "\n")
cat(" -", model_status_path, "\n")
cat(" -", coefficient_table_path, "\n")
cat(" -", fit_table_path, "\n")
cat(" -", prediction_metrics_path, "\n")
cat(" -", scenario_effects_path, "\n")
cat(" -", residual_diagnostics_path, "\n")
cat(" -", bank_residual_ranking_path, "\n")
cat(" -", figure_inventory_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")

cat("\nFigures:\n")
cat(paste(" -", figure_paths, collapse = "\n"))
cat("\n")
cat("============================================================\n")