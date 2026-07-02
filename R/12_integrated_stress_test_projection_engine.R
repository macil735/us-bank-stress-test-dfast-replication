# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 12 — Integrated Stress Test Projection Engine
# ============================================================
# Objective:
#   Build an integrated public DFAST-style stress test projection
#   engine by combining:
#     - Credit loss model outputs from Script 09
#     - PPNR model outputs from Script 10
#     - Capital depletion model outputs from Script 11
#
# Main inputs:
#   data/processed/model/credit_loss_model_predictions.csv
#   data/processed/model/ppnr_model_predictions.csv
#   data/processed/model/capital_depletion_model_predictions.csv
#
# Main outputs:
#   data/processed/model/integrated_stress_test_projection_panel.csv
#   data/processed/model/integrated_stress_test_projection_long.csv
#   outputs/integrated_projection/script12_integrated_projection_report.docx
#   outputs/integrated_projection/script12_integrated_projection_outputs.xlsx
#   outputs/integrated_projection/script12_execution_log.txt
#
# Methodological note:
#   This engine is a public, reduced-form replication layer.
#   It does not reproduce confidential Federal Reserve supervisory
#   models or bank internal capital planning models.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 12 - Integrated Stress Test Projection Engine\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "12"
script_name <- "integrated_stress_test_projection_engine"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/model", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/integrated_projection", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/integrated_projection/figures", recursive = TRUE, showWarnings = FALSE)

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

safe_divide <- function(num, den) {
  ifelse(!is.na(den) & den != 0, num / den, NA_real_)
}

save_plot <- function(plot_object, file_name, width = 9, height = 5.5) {
  path <- file.path("outputs/integrated_projection/figures", file_name)

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
# 3. Input paths
# ------------------------------------------------------------

credit_loss_predictions_path <- "data/processed/model/credit_loss_model_predictions.csv"
ppnr_predictions_path <- "data/processed/model/ppnr_model_predictions.csv"
capital_predictions_path <- "data/processed/model/capital_depletion_model_predictions.csv"

input_check <- tibble::tibble(
  input_name = c(
    "credit_loss_model_predictions",
    "ppnr_model_predictions",
    "capital_depletion_model_predictions"
  ),
  input_path = c(
    credit_loss_predictions_path,
    ppnr_predictions_path,
    capital_predictions_path
  ),
  exists = file.exists(c(
    credit_loss_predictions_path,
    ppnr_predictions_path,
    capital_predictions_path
  ))
) |>
  safe_df()

if (any(!input_check$exists)) {
  print(input_check)
  stop("Missing one or more required model prediction files.")
}

cat("Input files checked.\n")
print(input_check)
cat("\n")


# ------------------------------------------------------------
# 4. Read model prediction files
# ------------------------------------------------------------

cat("Reading model prediction files...\n")

credit_pred_raw <- readr::read_csv(
  credit_loss_predictions_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()

ppnr_pred_raw <- readr::read_csv(
  ppnr_predictions_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()

capital_pred_raw <- readr::read_csv(
  capital_predictions_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()

cat("Files loaded.\n")
cat("Credit loss prediction rows:", nrow(credit_pred_raw), "\n")
cat("PPNR prediction rows:", nrow(ppnr_pred_raw), "\n")
cat("Capital depletion prediction rows:", nrow(capital_pred_raw), "\n\n")


# ------------------------------------------------------------
# 5. Select operational models
# ------------------------------------------------------------

cat("Selecting operational model predictions...\n")

credit_operational_model <- "M5_bank_year_fixed_effects"
ppnr_operational_model <- "M5_bank_year_fixed_effects"
capital_operational_model <- "M6_bank_year_fixed_effects"

credit_pred <- credit_pred_raw |>
  dplyr::filter(model_name == credit_operational_model) |>
  dplyr::select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    actual_credit_loss_ratio = y_loss_ratio_model,
    predicted_credit_loss_ratio = predicted_loss_ratio,
    residual_credit_loss_ratio = residual_loss_ratio,
    total_loan_losses_amount,
    rwa_actual_amount,
    ppnr_amount
  ) |>
  safe_df()

ppnr_pred <- ppnr_pred_raw |>
  dplyr::filter(model_name == ppnr_operational_model) |>
  dplyr::select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    actual_ppnr_ratio = y_ppnr_ratio_model,
    predicted_ppnr_ratio,
    residual_ppnr_ratio,
    ppnr_amount,
    rwa_actual_amount,
    total_loan_losses_amount,
    pretax_net_income_amount
  ) |>
  safe_df()

capital_pred <- capital_pred_raw |>
  dplyr::filter(model_name == capital_operational_model) |>
  dplyr::select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    actual_capital_depletion = y_capital_depletion_model,
    predicted_capital_depletion,
    residual_capital_depletion,
    predicted_cet1_min_ratio,
    cet1_actual_ratio,
    cet1_min_ratio,
    total_loan_losses_amount,
    ppnr_amount,
    rwa_actual_amount,
    rwa_end_amount
  ) |>
  safe_df()

cat("Operational models selected.\n")
cat("Credit operational model:", credit_operational_model, "\n")
cat("PPNR operational model:", ppnr_operational_model, "\n")
cat("Capital operational model:", capital_operational_model, "\n\n")


# ------------------------------------------------------------
# 6. Validate selected model rows
# ------------------------------------------------------------

selected_model_check <- tibble::tibble(
  selected_component = c("credit_loss", "ppnr", "capital_depletion"),
  selected_model = c(
    credit_operational_model,
    ppnr_operational_model,
    capital_operational_model
  ),
  rows_selected = c(
    nrow(credit_pred),
    nrow(ppnr_pred),
    nrow(capital_pred)
  )
) |>
  safe_df()

if (any(selected_model_check$rows_selected == 0)) {
  print(selected_model_check)
  stop("At least one operational model returned zero rows.")
}

cat("Selected model rows checked.\n")
print(selected_model_check)
cat("\n")


# ------------------------------------------------------------
# 7. Merge model outputs
# ------------------------------------------------------------

cat("Merging model outputs...\n")

join_keys <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label"
)

integrated_panel <- capital_pred |>
  dplyr::left_join(
    credit_pred,
    by = join_keys,
    suffix = c("", "_credit")
  ) |>
  dplyr::left_join(
    ppnr_pred,
    by = join_keys,
    suffix = c("", "_ppnr")
  ) |>
  dplyr::mutate(
    observed_cet1_min_ratio = cet1_min_ratio,

    integrated_predicted_cet1_min_ratio =
      cet1_actual_ratio - predicted_capital_depletion,

    integrated_predicted_cet1_gap =
      integrated_predicted_cet1_min_ratio - observed_cet1_min_ratio,

    integrated_capital_depletion_gap =
      predicted_capital_depletion - actual_capital_depletion,

    integrated_credit_loss_gap =
      predicted_credit_loss_ratio - actual_credit_loss_ratio,

    integrated_ppnr_gap =
      predicted_ppnr_ratio - actual_ppnr_ratio,

    predicted_credit_loss_amount =
      predicted_credit_loss_ratio * rwa_actual_amount,

    predicted_ppnr_amount =
      predicted_ppnr_ratio * rwa_actual_amount,

    actual_credit_loss_to_ppnr_ratio =
      safe_divide(actual_credit_loss_ratio, actual_ppnr_ratio),

    predicted_credit_loss_to_ppnr_ratio =
      safe_divide(predicted_credit_loss_ratio, predicted_ppnr_ratio),

    predicted_net_stress_margin =
      predicted_ppnr_ratio - predicted_credit_loss_ratio,

    actual_net_stress_margin =
      actual_ppnr_ratio - actual_credit_loss_ratio,

    predicted_rwa_growth =
      safe_divide(rwa_end_amount - rwa_actual_amount, rwa_actual_amount),

    model_stack_id = paste(
      credit_operational_model,
      ppnr_operational_model,
      capital_operational_model,
      sep = " | "
    )
  ) |>
  safe_df()

cat("Model outputs merged.\n")
cat("Integrated panel rows:", nrow(integrated_panel), "\n")
cat("Banks:", dplyr::n_distinct(integrated_panel$bank_name), "\n")
cat("Years:", dplyr::n_distinct(integrated_panel$exercise_year), "\n")
cat("Scenarios:", dplyr::n_distinct(integrated_panel$scenario_label), "\n\n")


# ------------------------------------------------------------
# 8. Integrated validation metrics
# ------------------------------------------------------------

cat("Creating integrated validation metrics...\n")

integrated_validation_metrics <- tibble::tibble(
  component = c(
    "credit_loss_ratio",
    "ppnr_ratio",
    "capital_depletion",
    "cet1_min_ratio"
  ),
  observations = c(
    sum(!is.na(integrated_panel$actual_credit_loss_ratio) &
          !is.na(integrated_panel$predicted_credit_loss_ratio)),
    sum(!is.na(integrated_panel$actual_ppnr_ratio) &
          !is.na(integrated_panel$predicted_ppnr_ratio)),
    sum(!is.na(integrated_panel$actual_capital_depletion) &
          !is.na(integrated_panel$predicted_capital_depletion)),
    sum(!is.na(integrated_panel$observed_cet1_min_ratio) &
          !is.na(integrated_panel$integrated_predicted_cet1_min_ratio))
  ),
  rmse = c(
    safe_rmse(
      integrated_panel$actual_credit_loss_ratio,
      integrated_panel$predicted_credit_loss_ratio
    ),
    safe_rmse(
      integrated_panel$actual_ppnr_ratio,
      integrated_panel$predicted_ppnr_ratio
    ),
    safe_rmse(
      integrated_panel$actual_capital_depletion,
      integrated_panel$predicted_capital_depletion
    ),
    safe_rmse(
      integrated_panel$observed_cet1_min_ratio,
      integrated_panel$integrated_predicted_cet1_min_ratio
    )
  ),
  mae = c(
    safe_mae(
      integrated_panel$actual_credit_loss_ratio,
      integrated_panel$predicted_credit_loss_ratio
    ),
    safe_mae(
      integrated_panel$actual_ppnr_ratio,
      integrated_panel$predicted_ppnr_ratio
    ),
    safe_mae(
      integrated_panel$actual_capital_depletion,
      integrated_panel$predicted_capital_depletion
    ),
    safe_mae(
      integrated_panel$observed_cet1_min_ratio,
      integrated_panel$integrated_predicted_cet1_min_ratio
    )
  ),
  r2_outcome_prediction = c(
    safe_r2(
      integrated_panel$actual_credit_loss_ratio,
      integrated_panel$predicted_credit_loss_ratio
    ),
    safe_r2(
      integrated_panel$actual_ppnr_ratio,
      integrated_panel$predicted_ppnr_ratio
    ),
    safe_r2(
      integrated_panel$actual_capital_depletion,
      integrated_panel$predicted_capital_depletion
    ),
    safe_r2(
      integrated_panel$observed_cet1_min_ratio,
      integrated_panel$integrated_predicted_cet1_min_ratio
    )
  )
) |>
  safe_df()

cat("Validation metrics created.\n\n")


# ------------------------------------------------------------
# 9. Scenario summary
# ------------------------------------------------------------

cat("Creating scenario summary...\n")

scenario_summary <- integrated_panel |>
  dplyr::group_by(exercise_year, scenario_code, scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    banks = dplyr::n_distinct(bank_name),

    mean_actual_credit_loss_ratio = safe_mean(actual_credit_loss_ratio),
    mean_predicted_credit_loss_ratio = safe_mean(predicted_credit_loss_ratio),

    mean_actual_ppnr_ratio = safe_mean(actual_ppnr_ratio),
    mean_predicted_ppnr_ratio = safe_mean(predicted_ppnr_ratio),

    mean_actual_capital_depletion = safe_mean(actual_capital_depletion),
    mean_predicted_capital_depletion = safe_mean(predicted_capital_depletion),

    mean_observed_cet1_min_ratio = safe_mean(observed_cet1_min_ratio),
    mean_predicted_cet1_min_ratio = safe_mean(integrated_predicted_cet1_min_ratio),

    mean_predicted_net_stress_margin = safe_mean(predicted_net_stress_margin),
    mean_actual_net_stress_margin = safe_mean(actual_net_stress_margin),

    rmse_cet1_min_ratio = safe_rmse(
      observed_cet1_min_ratio,
      integrated_predicted_cet1_min_ratio
    ),

    .groups = "drop"
  ) |>
  dplyr::arrange(exercise_year, scenario_code) |>
  safe_df()

cat("Scenario summary created.\n\n")


# ------------------------------------------------------------
# 10. Bank vulnerability ranking
# ------------------------------------------------------------

cat("Creating bank vulnerability rankings...\n")

bank_vulnerability_ranking <- integrated_panel |>
  dplyr::group_by(bank_rssd_id, bank_name) |>
  dplyr::summarise(
    observations = dplyr::n(),
    years = dplyr::n_distinct(exercise_year),
    scenarios = dplyr::n_distinct(scenario_label),

    mean_predicted_cet1_min_ratio =
      safe_mean(integrated_predicted_cet1_min_ratio),

    min_predicted_cet1_min_ratio =
      safe_min(integrated_predicted_cet1_min_ratio),

    mean_observed_cet1_min_ratio =
      safe_mean(observed_cet1_min_ratio),

    min_observed_cet1_min_ratio =
      safe_min(observed_cet1_min_ratio),

    max_predicted_capital_depletion =
      safe_max(predicted_capital_depletion),

    mean_predicted_credit_loss_ratio =
      safe_mean(predicted_credit_loss_ratio),

    mean_predicted_ppnr_ratio =
      safe_mean(predicted_ppnr_ratio),

    mean_predicted_net_stress_margin =
      safe_mean(predicted_net_stress_margin),

    rmse_predicted_cet1_min_ratio =
      safe_rmse(
        observed_cet1_min_ratio,
        integrated_predicted_cet1_min_ratio
      ),

    .groups = "drop"
  ) |>
  dplyr::arrange(min_predicted_cet1_min_ratio, dplyr::desc(max_predicted_capital_depletion)) |>
  dplyr::mutate(
    vulnerability_rank = dplyr::row_number()
  ) |>
  safe_df()

bank_model_error_ranking <- integrated_panel |>
  dplyr::group_by(bank_rssd_id, bank_name) |>
  dplyr::summarise(
    observations = dplyr::n(),
    rmse_credit_loss_ratio = safe_rmse(
      actual_credit_loss_ratio,
      predicted_credit_loss_ratio
    ),
    rmse_ppnr_ratio = safe_rmse(
      actual_ppnr_ratio,
      predicted_ppnr_ratio
    ),
    rmse_capital_depletion = safe_rmse(
      actual_capital_depletion,
      predicted_capital_depletion
    ),
    rmse_cet1_min_ratio = safe_rmse(
      observed_cet1_min_ratio,
      integrated_predicted_cet1_min_ratio
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(rmse_cet1_min_ratio)) |>
  safe_df()

cat("Bank rankings created.\n\n")


# ------------------------------------------------------------
# 11. Integrated long format
# ------------------------------------------------------------

cat("Creating integrated long dataset...\n")

id_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label",
  "model_stack_id"
)

integrated_long <- integrated_panel |>
  dplyr::select(
    dplyr::all_of(id_cols),
    actual_credit_loss_ratio,
    predicted_credit_loss_ratio,
    residual_credit_loss_ratio,
    actual_ppnr_ratio,
    predicted_ppnr_ratio,
    residual_ppnr_ratio,
    actual_capital_depletion,
    predicted_capital_depletion,
    residual_capital_depletion,
    observed_cet1_min_ratio,
    integrated_predicted_cet1_min_ratio,
    integrated_predicted_cet1_gap,
    predicted_credit_loss_amount,
    predicted_ppnr_amount,
    predicted_net_stress_margin,
    actual_net_stress_margin
  ) |>
  tidyr::pivot_longer(
    cols = -dplyr::all_of(id_cols),
    names_to = "projection_variable",
    values_to = "value"
  ) |>
  dplyr::mutate(
    projection_block = dplyr::case_when(
      stringr::str_detect(projection_variable, "credit_loss") ~ "Credit losses",
      stringr::str_detect(projection_variable, "ppnr") ~ "PPNR",
      stringr::str_detect(projection_variable, "capital_depletion") ~ "Capital depletion",
      stringr::str_detect(projection_variable, "cet1") ~ "CET1 capital ratio",
      stringr::str_detect(projection_variable, "stress_margin") ~ "Stress margin",
      TRUE ~ "Other"
    )
  ) |>
  dplyr::arrange(
    exercise_year,
    bank_name,
    scenario_code,
    projection_block,
    projection_variable
  ) |>
  safe_df()

cat("Integrated long dataset created.\n\n")


# ------------------------------------------------------------
# 12. Projection variable map
# ------------------------------------------------------------

projection_variable_map <- integrated_long |>
  dplyr::group_by(projection_block, projection_variable) |>
  dplyr::summarise(
    observations = dplyr::n(),
    non_missing_observations = sum(!is.na(value)),
    missing_share = mean(is.na(value)),
    mean_value = safe_mean(value),
    sd_value = safe_sd(value),
    min_value = safe_min(value),
    max_value = safe_max(value),
    .groups = "drop"
  ) |>
  dplyr::arrange(projection_block, projection_variable) |>
  safe_df()


# ------------------------------------------------------------
# 13. Figures
# ------------------------------------------------------------

cat("Creating figures...\n")

figure_paths <- c()

p1 <- ggplot2::ggplot(
  integrated_panel,
  ggplot2::aes(
    x = observed_cet1_min_ratio,
    y = integrated_predicted_cet1_min_ratio
  )
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Observed versus Integrated Predicted CET1 Minimum Ratio",
    x = "Observed CET1 minimum ratio",
    y = "Integrated predicted CET1 minimum ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p1, "fig01_observed_vs_predicted_cet1_min_ratio.png")
)

p2 <- ggplot2::ggplot(
  integrated_panel,
  ggplot2::aes(x = integrated_predicted_cet1_gap)
) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::labs(
    title = "Distribution of Integrated CET1 Prediction Gap",
    x = "Predicted CET1 minimum ratio minus observed CET1 minimum ratio",
    y = "Number of observations"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p2, "fig02_cet1_prediction_gap_distribution.png")
)

p3 <- integrated_validation_metrics |>
  ggplot2::ggplot(
    ggplot2::aes(x = reorder(component, rmse), y = rmse)
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Integrated Projection RMSE by Component",
    x = "Component",
    y = "RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p3, "fig03_integrated_rmse_by_component.png")
)

p4 <- bank_vulnerability_ranking |>
  dplyr::slice_min(
    order_by = min_predicted_cet1_min_ratio,
    n = 20,
    with_ties = FALSE
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(bank_name, min_predicted_cet1_min_ratio),
      y = min_predicted_cet1_min_ratio
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Top 20 Most Vulnerable Banks by Predicted Minimum CET1",
    x = "Bank",
    y = "Minimum predicted CET1 ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p4, "fig04_top20_vulnerable_banks_predicted_cet1.png", width = 10, height = 7)
)

p5 <- scenario_summary |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = exercise_year,
      y = mean_predicted_cet1_min_ratio,
      group = scenario_label
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "Mean Predicted CET1 Minimum Ratio by Year and Scenario",
    x = "Exercise year",
    y = "Mean predicted CET1 minimum ratio",
    group = "Scenario"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p5, "fig05_predicted_cet1_by_year_scenario.png")
)

p6 <- ggplot2::ggplot(
  integrated_panel,
  ggplot2::aes(
    x = predicted_credit_loss_ratio,
    y = predicted_ppnr_ratio
  )
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::labs(
    title = "Predicted PPNR Ratio versus Predicted Credit Loss Ratio",
    x = "Predicted credit loss ratio",
    y = "Predicted PPNR ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p6, "fig06_predicted_ppnr_vs_credit_losses.png")
)

figure_inventory <- tibble::tibble(
  figure_path = figure_paths,
  exists = file.exists(figure_paths),
  size_bytes = ifelse(file.exists(figure_paths), file.info(figure_paths)$size, NA_real_)
) |>
  safe_df()

cat("Figures created:", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")


# ------------------------------------------------------------
# 14. Save processed datasets
# ------------------------------------------------------------

cat("Saving processed datasets...\n")

integrated_panel_path <- "data/processed/model/integrated_stress_test_projection_panel.csv"
integrated_long_path <- "data/processed/model/integrated_stress_test_projection_long.csv"

readr::write_csv(integrated_panel, integrated_panel_path)
readr::write_csv(integrated_long, integrated_long_path)

cat("Processed datasets saved.\n\n")


# ------------------------------------------------------------
# 15. Save tabular outputs
# ------------------------------------------------------------

cat("Saving integrated projection outputs...\n")

out_dir <- "outputs/integrated_projection"

input_check_path <- file.path(out_dir, "script12_input_check.csv")
selected_model_check_path <- file.path(out_dir, "script12_selected_model_check.csv")
validation_metrics_path <- file.path(out_dir, "script12_integrated_validation_metrics.csv")
scenario_summary_path <- file.path(out_dir, "script12_scenario_summary.csv")
bank_vulnerability_path <- file.path(out_dir, "script12_bank_vulnerability_ranking.csv")
bank_error_ranking_path <- file.path(out_dir, "script12_bank_model_error_ranking.csv")
projection_variable_map_path <- file.path(out_dir, "script12_projection_variable_map.csv")
figure_inventory_path <- file.path(out_dir, "script12_figure_inventory.csv")
execution_summary_path <- file.path(out_dir, "script12_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script12_integrated_projection_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script12_integrated_projection_report.docx")
execution_log_path <- file.path(out_dir, "script12_execution_log.txt")

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  integrated_panel_rows = nrow(integrated_panel),
  integrated_long_rows = nrow(integrated_long),
  banks = dplyr::n_distinct(integrated_panel$bank_name),
  years = dplyr::n_distinct(integrated_panel$exercise_year),
  scenarios = dplyr::n_distinct(integrated_panel$scenario_label),
  credit_operational_model = credit_operational_model,
  ppnr_operational_model = ppnr_operational_model,
  capital_operational_model = capital_operational_model,
  figures_created = sum(figure_inventory$exists, na.rm = TRUE)
) |>
  safe_df()

readr::write_csv(input_check, input_check_path)
readr::write_csv(selected_model_check, selected_model_check_path)
readr::write_csv(integrated_validation_metrics, validation_metrics_path)
readr::write_csv(scenario_summary, scenario_summary_path)
readr::write_csv(bank_vulnerability_ranking, bank_vulnerability_path)
readr::write_csv(bank_model_error_ranking, bank_error_ranking_path)
readr::write_csv(projection_variable_map, projection_variable_map_path)
readr::write_csv(figure_inventory, figure_inventory_path)
readr::write_csv(execution_summary, execution_summary_path)

cat("Integrated projection outputs saved.\n\n")


# ------------------------------------------------------------
# 16. Excel workbook
# ------------------------------------------------------------

cat("Creating Excel workbook...\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "execution_summary")
openxlsx::writeData(wb, "execution_summary", execution_summary)

openxlsx::addWorksheet(wb, "input_check")
openxlsx::writeData(wb, "input_check", input_check)

openxlsx::addWorksheet(wb, "selected_models")
openxlsx::writeData(wb, "selected_models", selected_model_check)

openxlsx::addWorksheet(wb, "validation_metrics")
openxlsx::writeData(wb, "validation_metrics", integrated_validation_metrics)

openxlsx::addWorksheet(wb, "scenario_summary")
openxlsx::writeData(wb, "scenario_summary", scenario_summary)

openxlsx::addWorksheet(wb, "bank_vulnerability")
openxlsx::writeData(wb, "bank_vulnerability", bank_vulnerability_ranking)

openxlsx::addWorksheet(wb, "bank_model_errors")
openxlsx::writeData(wb, "bank_model_errors", bank_model_error_ranking)

openxlsx::addWorksheet(wb, "projection_variable_map")
openxlsx::writeData(wb, "projection_variable_map", projection_variable_map)

openxlsx::addWorksheet(wb, "figure_inventory")
openxlsx::writeData(wb, "figure_inventory", figure_inventory)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output_path, overwrite = TRUE)

cat("Excel workbook created.\n\n")


# ------------------------------------------------------------
# 17. Word report
# ------------------------------------------------------------

cat("Creating Word report...\n")

doc <- officer::read_docx()

doc <- doc |>
  officer::body_add_par("Script 12 - Integrated Stress Test Projection Engine", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This script builds the integrated public stress test projection engine by combining the estimated credit loss, PPNR and capital depletion models. The engine produces predicted CET1 minimum ratios and compares them with observed Federal Reserve DFAST benchmark outcomes.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Execution summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(execution_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Selected operational models", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(selected_model_check) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. Integrated validation metrics", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(integrated_validation_metrics) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("5. Scenario summary", style = "heading 2")

scenario_summary_small <- scenario_summary |>
  dplyr::arrange(exercise_year, scenario_code) |>
  head(30)

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(scenario_summary_small) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("6. Most vulnerable banks", style = "heading 2")

top_vulnerable <- bank_vulnerability_ranking |>
  dplyr::select(
    vulnerability_rank,
    bank_name,
    observations,
    min_predicted_cet1_min_ratio,
    min_observed_cet1_min_ratio,
    max_predicted_capital_depletion,
    mean_predicted_credit_loss_ratio,
    mean_predicted_ppnr_ratio
  ) |>
  head(20)

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(top_vulnerable) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("7. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "The integrated projection engine is a reduced-form public replication layer. It combines separately estimated model components and compares the resulting capital projections with public DFAST outcomes. It should not be interpreted as a replica of confidential Federal Reserve supervisory models.",
    style = "Normal"
  )

print(doc, target = report_docx_path)

cat("Word report created.\n\n")


# ------------------------------------------------------------
# 18. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 12 - Integrated Stress Test Projection Engine completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Integrated panel rows:", nrow(integrated_panel)),
  paste("Integrated long rows:", nrow(integrated_long)),
  paste("Banks:", dplyr::n_distinct(integrated_panel$bank_name)),
  paste("Years:", dplyr::n_distinct(integrated_panel$exercise_year)),
  paste("Scenarios:", dplyr::n_distinct(integrated_panel$scenario_label)),
  paste("Credit operational model:", credit_operational_model),
  paste("PPNR operational model:", ppnr_operational_model),
  paste("Capital operational model:", capital_operational_model),
  paste("Figures created:", sum(figure_inventory$exists, na.rm = TRUE)),
  "",
  "Selected model check:",
  capture.output(print(selected_model_check)),
  "",
  "Integrated validation metrics:",
  capture.output(print(integrated_validation_metrics)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", integrated_panel_path),
  paste(" -", integrated_long_path),
  paste(" -", input_check_path),
  paste(" -", selected_model_check_path),
  paste(" -", validation_metrics_path),
  paste(" -", scenario_summary_path),
  paste(" -", bank_vulnerability_path),
  paste(" -", bank_error_ranking_path),
  paste(" -", projection_variable_map_path),
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
# 19. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 12 - Integrated Stress Test Projection Engine completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Integrated panel rows:\n", nrow(integrated_panel), "\n")
cat("Integrated long rows:\n", nrow(integrated_long), "\n")
cat("Banks:\n", dplyr::n_distinct(integrated_panel$bank_name), "\n")
cat("Years:\n", dplyr::n_distinct(integrated_panel$exercise_year), "\n")
cat("Scenarios:\n", dplyr::n_distinct(integrated_panel$scenario_label), "\n")
cat("Credit operational model:\n", credit_operational_model, "\n")
cat("PPNR operational model:\n", ppnr_operational_model, "\n")
cat("Capital operational model:\n", capital_operational_model, "\n")
cat("Figures created:\n", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")

cat("Selected model check:\n")
print(selected_model_check)

cat("\nIntegrated validation metrics:\n")
print(integrated_validation_metrics)

cat("\nMain outputs:\n")
cat(" -", integrated_panel_path, "\n")
cat(" -", integrated_long_path, "\n")
cat(" -", input_check_path, "\n")
cat(" -", selected_model_check_path, "\n")
cat(" -", validation_metrics_path, "\n")
cat(" -", scenario_summary_path, "\n")
cat(" -", bank_vulnerability_path, "\n")
cat(" -", bank_error_ranking_path, "\n")
cat(" -", projection_variable_map_path, "\n")
cat(" -", figure_inventory_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")

cat("\nFigures:\n")
cat(paste(" -", figure_paths, collapse = "\n"))
cat("\n")
cat("============================================================\n")