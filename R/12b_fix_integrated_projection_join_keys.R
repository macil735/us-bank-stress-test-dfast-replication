# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 12b — Fix Integrated Projection Join Keys
# ============================================================
# Objective:
#   Diagnose and fix duplicated join keys in the integrated
#   projection engine created in Script 12.
#
# Problem identified in Script 12:
#   Each selected model component had 520 rows, but the merged
#   integrated panel had 658 rows. This indicates duplicated
#   join keys in at least one component.
#
# Main inputs:
#   data/processed/model/credit_loss_model_predictions.csv
#   data/processed/model/ppnr_model_predictions.csv
#   data/processed/model/capital_depletion_model_predictions.csv
#
# Main outputs:
#   data/processed/model/integrated_stress_test_projection_panel_fixed.csv
#   data/processed/model/integrated_stress_test_projection_long_fixed.csv
#   outputs/integrated_projection_fix/script12b_join_key_audit.csv
#   outputs/integrated_projection_fix/script12b_duplicate_key_details.csv
#   outputs/integrated_projection_fix/script12b_integrated_validation_metrics_fixed.csv
#   outputs/integrated_projection_fix/script12b_integrated_projection_fix_report.docx
#   outputs/integrated_projection_fix/script12b_execution_log.txt
#
# Methodological note:
#   The script preserves the operational model choices from
#   Script 12, but forces one row per bank-year-scenario key
#   before merging model components.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 12b - Fix Integrated Projection Join Keys\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "12b"
script_name <- "fix_integrated_projection_join_keys"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/model", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/integrated_projection_fix", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/integrated_projection_fix/figures", recursive = TRUE, showWarnings = FALSE)

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
  path <- file.path("outputs/integrated_projection_fix/figures", file_name)

  ggplot2::ggsave(
    filename = path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300
  )

  path
}

audit_keys <- function(df, component_name, key_cols) {
  df |>
    dplyr::count(dplyr::across(dplyr::all_of(key_cols)), name = "key_count") |>
    dplyr::mutate(
      component = component_name,
      duplicated_key = key_count > 1,
      .before = 1
    ) |>
    safe_df()
}

aggregate_to_unique_keys <- function(df, key_cols) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::summarise(
      dplyr::across(
        where(is.numeric),
        ~ mean(.x, na.rm = TRUE)
      ),
      dplyr::across(
        where(is.character),
        ~ dplyr::first(.x)
      ),
      source_rows_collapsed = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)
      )
    ) |>
    safe_df()
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Input paths and operational models
# ------------------------------------------------------------

credit_loss_predictions_path <- "data/processed/model/credit_loss_model_predictions.csv"
ppnr_predictions_path <- "data/processed/model/ppnr_model_predictions.csv"
capital_predictions_path <- "data/processed/model/capital_depletion_model_predictions.csv"

credit_operational_model <- "M5_bank_year_fixed_effects"
ppnr_operational_model <- "M5_bank_year_fixed_effects"
capital_operational_model <- "M6_bank_year_fixed_effects"

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
# 4. Read prediction files
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
# 5. Select operational model rows
# ------------------------------------------------------------

cat("Selecting operational model rows...\n")

credit_pred_selected <- credit_pred_raw |>
  dplyr::filter(model_name == credit_operational_model) |>
  safe_df()

ppnr_pred_selected <- ppnr_pred_raw |>
  dplyr::filter(model_name == ppnr_operational_model) |>
  safe_df()

capital_pred_selected <- capital_pred_raw |>
  dplyr::filter(model_name == capital_operational_model) |>
  safe_df()

selected_model_check <- tibble::tibble(
  selected_component = c("credit_loss", "ppnr", "capital_depletion"),
  selected_model = c(
    credit_operational_model,
    ppnr_operational_model,
    capital_operational_model
  ),
  rows_selected = c(
    nrow(credit_pred_selected),
    nrow(ppnr_pred_selected),
    nrow(capital_pred_selected)
  )
) |>
  safe_df()

print(selected_model_check)

if (any(selected_model_check$rows_selected == 0)) {
  stop("At least one operational model returned zero rows.")
}

cat("Operational model rows selected.\n\n")


# ------------------------------------------------------------
# 6. Define and audit join keys
# ------------------------------------------------------------

cat("Auditing join keys...\n")

key_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label"
)

missing_key_cols <- list(
  credit_loss = setdiff(key_cols, names(credit_pred_selected)),
  ppnr = setdiff(key_cols, names(ppnr_pred_selected)),
  capital_depletion = setdiff(key_cols, names(capital_pred_selected))
)

if (length(unlist(missing_key_cols)) > 0) {
  print(missing_key_cols)
  stop("At least one selected prediction file is missing required join keys.")
}

credit_key_audit <- audit_keys(credit_pred_selected, "credit_loss", key_cols)
ppnr_key_audit <- audit_keys(ppnr_pred_selected, "ppnr", key_cols)
capital_key_audit <- audit_keys(capital_pred_selected, "capital_depletion", key_cols)

join_key_audit <- dplyr::bind_rows(
  credit_key_audit,
  ppnr_key_audit,
  capital_key_audit
) |>
  safe_df()

join_key_summary <- join_key_audit |>
  dplyr::group_by(component) |>
  dplyr::summarise(
    rows_after_model_selection = dplyr::case_when(
      component == "credit_loss" ~ nrow(credit_pred_selected),
      component == "ppnr" ~ nrow(ppnr_pred_selected),
      component == "capital_depletion" ~ nrow(capital_pred_selected),
      TRUE ~ NA_integer_
    )[1],
    unique_keys = dplyr::n(),
    duplicated_keys = sum(duplicated_key),
    max_rows_per_key = max(key_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  safe_df()

duplicate_key_details <- join_key_audit |>
  dplyr::filter(duplicated_key) |>
  dplyr::arrange(component, dplyr::desc(key_count), bank_name, exercise_year) |>
  safe_df()

cat("Join key audit completed.\n")
print(join_key_summary)
cat("\n")


# ------------------------------------------------------------
# 7. Aggregate selected model rows to unique keys
# ------------------------------------------------------------

cat("Aggregating selected rows to unique join keys...\n")

credit_pred_unique_raw <- aggregate_to_unique_keys(credit_pred_selected, key_cols)
ppnr_pred_unique_raw <- aggregate_to_unique_keys(ppnr_pred_selected, key_cols)
capital_pred_unique_raw <- aggregate_to_unique_keys(capital_pred_selected, key_cols)

post_aggregation_check <- tibble::tibble(
  component = c("credit_loss", "ppnr", "capital_depletion"),
  rows_before = c(
    nrow(credit_pred_selected),
    nrow(ppnr_pred_selected),
    nrow(capital_pred_selected)
  ),
  rows_after_unique_key_aggregation = c(
    nrow(credit_pred_unique_raw),
    nrow(ppnr_pred_unique_raw),
    nrow(capital_pred_unique_raw)
  ),
  duplicate_rows_collapsed = rows_before - rows_after_unique_key_aggregation
) |>
  safe_df()

cat("Aggregation completed.\n")
print(post_aggregation_check)
cat("\n")


# ------------------------------------------------------------
# 8. Build component-specific unique datasets
# ------------------------------------------------------------

cat("Building component-specific datasets...\n")

credit_pred <- credit_pred_unique_raw |>
  dplyr::select(
    dplyr::all_of(key_cols),
    actual_credit_loss_ratio = y_loss_ratio_model,
    predicted_credit_loss_ratio = predicted_loss_ratio,
    residual_credit_loss_ratio = residual_loss_ratio,
    total_loan_losses_amount,
    rwa_actual_amount,
    ppnr_amount,
    source_rows_collapsed_credit = source_rows_collapsed
  ) |>
  safe_df()

ppnr_pred <- ppnr_pred_unique_raw |>
  dplyr::select(
    dplyr::all_of(key_cols),
    actual_ppnr_ratio = y_ppnr_ratio_model,
    predicted_ppnr_ratio,
    residual_ppnr_ratio,
    ppnr_amount,
    rwa_actual_amount,
    total_loan_losses_amount,
    pretax_net_income_amount,
    source_rows_collapsed_ppnr = source_rows_collapsed
  ) |>
  safe_df()

capital_pred <- capital_pred_unique_raw |>
  dplyr::select(
    dplyr::all_of(key_cols),
    actual_capital_depletion = y_capital_depletion_model,
    predicted_capital_depletion,
    residual_capital_depletion,
    predicted_cet1_min_ratio,
    cet1_actual_ratio,
    cet1_min_ratio,
    total_loan_losses_amount,
    ppnr_amount,
    rwa_actual_amount,
    rwa_end_amount,
    source_rows_collapsed_capital = source_rows_collapsed
  ) |>
  safe_df()

cat("Component datasets built.\n\n")


# ------------------------------------------------------------
# 9. Merge with unique keys
# ------------------------------------------------------------

cat("Merging unique-key component datasets...\n")

integrated_panel_fixed <- capital_pred |>
  dplyr::left_join(
    credit_pred,
    by = key_cols,
    suffix = c("", "_credit")
  ) |>
  dplyr::left_join(
    ppnr_pred,
    by = key_cols,
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

fixed_key_audit <- audit_keys(
  integrated_panel_fixed,
  "integrated_panel_fixed",
  key_cols
)

fixed_integrated_check <- tibble::tibble(
  dataset = "integrated_panel_fixed",
  rows = nrow(integrated_panel_fixed),
  unique_keys = nrow(fixed_key_audit),
  duplicated_keys = sum(fixed_key_audit$duplicated_key),
  max_rows_per_key = max(fixed_key_audit$key_count, na.rm = TRUE)
) |>
  safe_df()

if (fixed_integrated_check$duplicated_keys > 0) {
  print(fixed_integrated_check)
  stop("Fixed integrated panel still contains duplicated keys.")
}

cat("Unique-key merge completed.\n")
print(fixed_integrated_check)
cat("\n")


# ------------------------------------------------------------
# 10. Validation metrics after fix
# ------------------------------------------------------------

cat("Creating fixed validation metrics...\n")

integrated_validation_metrics_fixed <- tibble::tibble(
  component = c(
    "credit_loss_ratio",
    "ppnr_ratio",
    "capital_depletion",
    "cet1_min_ratio"
  ),
  observations = c(
    sum(!is.na(integrated_panel_fixed$actual_credit_loss_ratio) &
          !is.na(integrated_panel_fixed$predicted_credit_loss_ratio)),
    sum(!is.na(integrated_panel_fixed$actual_ppnr_ratio) &
          !is.na(integrated_panel_fixed$predicted_ppnr_ratio)),
    sum(!is.na(integrated_panel_fixed$actual_capital_depletion) &
          !is.na(integrated_panel_fixed$predicted_capital_depletion)),
    sum(!is.na(integrated_panel_fixed$observed_cet1_min_ratio) &
          !is.na(integrated_panel_fixed$integrated_predicted_cet1_min_ratio))
  ),
  rmse = c(
    safe_rmse(
      integrated_panel_fixed$actual_credit_loss_ratio,
      integrated_panel_fixed$predicted_credit_loss_ratio
    ),
    safe_rmse(
      integrated_panel_fixed$actual_ppnr_ratio,
      integrated_panel_fixed$predicted_ppnr_ratio
    ),
    safe_rmse(
      integrated_panel_fixed$actual_capital_depletion,
      integrated_panel_fixed$predicted_capital_depletion
    ),
    safe_rmse(
      integrated_panel_fixed$observed_cet1_min_ratio,
      integrated_panel_fixed$integrated_predicted_cet1_min_ratio
    )
  ),
  mae = c(
    safe_mae(
      integrated_panel_fixed$actual_credit_loss_ratio,
      integrated_panel_fixed$predicted_credit_loss_ratio
    ),
    safe_mae(
      integrated_panel_fixed$actual_ppnr_ratio,
      integrated_panel_fixed$predicted_ppnr_ratio
    ),
    safe_mae(
      integrated_panel_fixed$actual_capital_depletion,
      integrated_panel_fixed$predicted_capital_depletion
    ),
    safe_mae(
      integrated_panel_fixed$observed_cet1_min_ratio,
      integrated_panel_fixed$integrated_predicted_cet1_min_ratio
    )
  ),
  r2_outcome_prediction = c(
    safe_r2(
      integrated_panel_fixed$actual_credit_loss_ratio,
      integrated_panel_fixed$predicted_credit_loss_ratio
    ),
    safe_r2(
      integrated_panel_fixed$actual_ppnr_ratio,
      integrated_panel_fixed$predicted_ppnr_ratio
    ),
    safe_r2(
      integrated_panel_fixed$actual_capital_depletion,
      integrated_panel_fixed$predicted_capital_depletion
    ),
    safe_r2(
      integrated_panel_fixed$observed_cet1_min_ratio,
      integrated_panel_fixed$integrated_predicted_cet1_min_ratio
    )
  )
) |>
  safe_df()

cat("Validation metrics created.\n\n")


# ------------------------------------------------------------
# 11. Scenario and bank summaries
# ------------------------------------------------------------

cat("Creating scenario and bank summaries...\n")

scenario_summary_fixed <- integrated_panel_fixed |>
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

    rmse_cet1_min_ratio = safe_rmse(
      observed_cet1_min_ratio,
      integrated_predicted_cet1_min_ratio
    ),

    .groups = "drop"
  ) |>
  dplyr::arrange(exercise_year, scenario_code) |>
  safe_df()

bank_vulnerability_ranking_fixed <- integrated_panel_fixed |>
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

bank_model_error_ranking_fixed <- integrated_panel_fixed |>
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

cat("Scenario and bank summaries created.\n\n")


# ------------------------------------------------------------
# 12. Long fixed dataset
# ------------------------------------------------------------

cat("Creating fixed integrated long dataset...\n")

id_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label",
  "model_stack_id"
)

integrated_long_fixed <- integrated_panel_fixed |>
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

projection_variable_map_fixed <- integrated_long_fixed |>
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

cat("Fixed long dataset created.\n\n")


# ------------------------------------------------------------
# 13. Figures
# ------------------------------------------------------------

cat("Creating fixed projection figures...\n")

figure_paths <- c()

p1 <- ggplot2::ggplot(
  integrated_panel_fixed,
  ggplot2::aes(
    x = observed_cet1_min_ratio,
    y = integrated_predicted_cet1_min_ratio
  )
) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title = "Observed versus Fixed Integrated Predicted CET1 Minimum Ratio",
    x = "Observed CET1 minimum ratio",
    y = "Fixed integrated predicted CET1 minimum ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p1, "fig01_fixed_observed_vs_predicted_cet1_min_ratio.png")
)

p2 <- ggplot2::ggplot(
  integrated_panel_fixed,
  ggplot2::aes(x = integrated_predicted_cet1_gap)
) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::labs(
    title = "Fixed Integrated CET1 Prediction Gap Distribution",
    x = "Predicted CET1 minimum ratio minus observed CET1 minimum ratio",
    y = "Number of observations"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p2, "fig02_fixed_cet1_prediction_gap_distribution.png")
)

p3 <- integrated_validation_metrics_fixed |>
  ggplot2::ggplot(
    ggplot2::aes(x = reorder(component, rmse), y = rmse)
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Fixed Integrated Projection RMSE by Component",
    x = "Component",
    y = "RMSE"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p3, "fig03_fixed_integrated_rmse_by_component.png")
)

p4 <- bank_vulnerability_ranking_fixed |>
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
    title = "Top 20 Most Vulnerable Banks - Fixed Integrated Projection",
    x = "Bank",
    y = "Minimum predicted CET1 ratio"
  ) +
  ggplot2::theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p4, "fig04_fixed_top20_vulnerable_banks.png", width = 10, height = 7)
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

cat("Saving fixed processed datasets...\n")

integrated_panel_fixed_path <- "data/processed/model/integrated_stress_test_projection_panel_fixed.csv"
integrated_long_fixed_path <- "data/processed/model/integrated_stress_test_projection_long_fixed.csv"

readr::write_csv(integrated_panel_fixed, integrated_panel_fixed_path)
readr::write_csv(integrated_long_fixed, integrated_long_fixed_path)

cat("Fixed processed datasets saved.\n\n")


# ------------------------------------------------------------
# 15. Save tabular outputs
# ------------------------------------------------------------

cat("Saving fixed integrated projection outputs...\n")

out_dir <- "outputs/integrated_projection_fix"

input_check_path <- file.path(out_dir, "script12b_input_check.csv")
selected_model_check_path <- file.path(out_dir, "script12b_selected_model_check.csv")
join_key_audit_path <- file.path(out_dir, "script12b_join_key_audit.csv")
join_key_summary_path <- file.path(out_dir, "script12b_join_key_summary.csv")
duplicate_key_details_path <- file.path(out_dir, "script12b_duplicate_key_details.csv")
post_aggregation_check_path <- file.path(out_dir, "script12b_post_aggregation_check.csv")
fixed_integrated_check_path <- file.path(out_dir, "script12b_fixed_integrated_check.csv")
validation_metrics_fixed_path <- file.path(out_dir, "script12b_integrated_validation_metrics_fixed.csv")
scenario_summary_fixed_path <- file.path(out_dir, "script12b_scenario_summary_fixed.csv")
bank_vulnerability_fixed_path <- file.path(out_dir, "script12b_bank_vulnerability_ranking_fixed.csv")
bank_error_ranking_fixed_path <- file.path(out_dir, "script12b_bank_model_error_ranking_fixed.csv")
projection_variable_map_fixed_path <- file.path(out_dir, "script12b_projection_variable_map_fixed.csv")
figure_inventory_path <- file.path(out_dir, "script12b_figure_inventory.csv")
execution_summary_path <- file.path(out_dir, "script12b_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script12b_integrated_projection_fix_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script12b_integrated_projection_fix_report.docx")
execution_log_path <- file.path(out_dir, "script12b_execution_log.txt")

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  credit_rows_selected = nrow(credit_pred_selected),
  ppnr_rows_selected = nrow(ppnr_pred_selected),
  capital_rows_selected = nrow(capital_pred_selected),
  credit_unique_keys = nrow(credit_pred_unique_raw),
  ppnr_unique_keys = nrow(ppnr_pred_unique_raw),
  capital_unique_keys = nrow(capital_pred_unique_raw),
  fixed_integrated_panel_rows = nrow(integrated_panel_fixed),
  fixed_integrated_long_rows = nrow(integrated_long_fixed),
  fixed_integrated_unique_keys = fixed_integrated_check$unique_keys,
  fixed_integrated_duplicated_keys = fixed_integrated_check$duplicated_keys,
  banks = dplyr::n_distinct(integrated_panel_fixed$bank_name),
  years = dplyr::n_distinct(integrated_panel_fixed$exercise_year),
  scenarios = dplyr::n_distinct(integrated_panel_fixed$scenario_label),
  figures_created = sum(figure_inventory$exists, na.rm = TRUE)
) |>
  safe_df()

readr::write_csv(input_check, input_check_path)
readr::write_csv(selected_model_check, selected_model_check_path)
readr::write_csv(join_key_audit, join_key_audit_path)
readr::write_csv(join_key_summary, join_key_summary_path)
readr::write_csv(duplicate_key_details, duplicate_key_details_path)
readr::write_csv(post_aggregation_check, post_aggregation_check_path)
readr::write_csv(fixed_integrated_check, fixed_integrated_check_path)
readr::write_csv(integrated_validation_metrics_fixed, validation_metrics_fixed_path)
readr::write_csv(scenario_summary_fixed, scenario_summary_fixed_path)
readr::write_csv(bank_vulnerability_ranking_fixed, bank_vulnerability_fixed_path)
readr::write_csv(bank_model_error_ranking_fixed, bank_error_ranking_fixed_path)
readr::write_csv(projection_variable_map_fixed, projection_variable_map_fixed_path)
readr::write_csv(figure_inventory, figure_inventory_path)
readr::write_csv(execution_summary, execution_summary_path)

cat("Fixed integrated projection outputs saved.\n\n")


# ------------------------------------------------------------
# 16. Excel workbook
# ------------------------------------------------------------

cat("Creating Excel workbook...\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "execution_summary")
openxlsx::writeData(wb, "execution_summary", execution_summary)

openxlsx::addWorksheet(wb, "selected_models")
openxlsx::writeData(wb, "selected_models", selected_model_check)

openxlsx::addWorksheet(wb, "join_key_summary")
openxlsx::writeData(wb, "join_key_summary", join_key_summary)

openxlsx::addWorksheet(wb, "post_aggregation")
openxlsx::writeData(wb, "post_aggregation", post_aggregation_check)

openxlsx::addWorksheet(wb, "fixed_integrated_check")
openxlsx::writeData(wb, "fixed_integrated_check", fixed_integrated_check)

openxlsx::addWorksheet(wb, "validation_metrics")
openxlsx::writeData(wb, "validation_metrics", integrated_validation_metrics_fixed)

openxlsx::addWorksheet(wb, "scenario_summary")
openxlsx::writeData(wb, "scenario_summary", scenario_summary_fixed)

openxlsx::addWorksheet(wb, "bank_vulnerability")
openxlsx::writeData(wb, "bank_vulnerability", bank_vulnerability_ranking_fixed)

openxlsx::addWorksheet(wb, "bank_model_errors")
openxlsx::writeData(wb, "bank_model_errors", bank_model_error_ranking_fixed)

openxlsx::addWorksheet(wb, "projection_variable_map")
openxlsx::writeData(wb, "projection_variable_map", projection_variable_map_fixed)

openxlsx::addWorksheet(wb, "duplicate_key_details")
openxlsx::writeData(wb, "duplicate_key_details", duplicate_key_details)

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
  officer::body_add_par("Script 12b - Fix Integrated Projection Join Keys", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This script diagnoses and fixes duplicated join keys in the integrated stress test projection engine. It forces one row per bank-year-scenario key before merging credit loss, PPNR and capital depletion model outputs.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Execution summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(execution_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Join key summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(join_key_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. Post-aggregation check", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(post_aggregation_check) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("5. Fixed integrated panel check", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(fixed_integrated_check) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("6. Fixed validation metrics", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(integrated_validation_metrics_fixed) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("7. Most vulnerable banks after join-key fix", style = "heading 2")

top_vulnerable <- bank_vulnerability_ranking_fixed |>
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
  officer::body_add_par("8. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "The correction aggregates duplicated bank-year-scenario keys within each model component before merging. This prevents artificial row expansion in the integrated projection panel while preserving the operational model choices from Script 12.",
    style = "Normal"
  )

print(doc, target = report_docx_path)

cat("Word report created.\n\n")


# ------------------------------------------------------------
# 18. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 12b - Fix Integrated Projection Join Keys completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Selected model check:",
  capture.output(print(selected_model_check)),
  "",
  "Join key summary:",
  capture.output(print(join_key_summary)),
  "",
  "Post aggregation check:",
  capture.output(print(post_aggregation_check)),
  "",
  "Fixed integrated check:",
  capture.output(print(fixed_integrated_check)),
  "",
  "Fixed validation metrics:",
  capture.output(print(integrated_validation_metrics_fixed)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", integrated_panel_fixed_path),
  paste(" -", integrated_long_fixed_path),
  paste(" -", input_check_path),
  paste(" -", selected_model_check_path),
  paste(" -", join_key_audit_path),
  paste(" -", join_key_summary_path),
  paste(" -", duplicate_key_details_path),
  paste(" -", post_aggregation_check_path),
  paste(" -", fixed_integrated_check_path),
  paste(" -", validation_metrics_fixed_path),
  paste(" -", scenario_summary_fixed_path),
  paste(" -", bank_vulnerability_fixed_path),
  paste(" -", bank_error_ranking_fixed_path),
  paste(" -", projection_variable_map_fixed_path),
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
cat("Script 12b - Fix Integrated Projection Join Keys completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Selected model check:\n")
print(selected_model_check)

cat("\nJoin key summary:\n")
print(join_key_summary)

cat("\nPost aggregation check:\n")
print(post_aggregation_check)

cat("\nFixed integrated check:\n")
print(fixed_integrated_check)

cat("\nFixed validation metrics:\n")
print(integrated_validation_metrics_fixed)

cat("\nMain outputs:\n")
cat(" -", integrated_panel_fixed_path, "\n")
cat(" -", integrated_long_fixed_path, "\n")
cat(" -", input_check_path, "\n")
cat(" -", selected_model_check_path, "\n")
cat(" -", join_key_audit_path, "\n")
cat(" -", join_key_summary_path, "\n")
cat(" -", duplicate_key_details_path, "\n")
cat(" -", post_aggregation_check_path, "\n")
cat(" -", fixed_integrated_check_path, "\n")
cat(" -", validation_metrics_fixed_path, "\n")
cat(" -", scenario_summary_fixed_path, "\n")
cat(" -", bank_vulnerability_fixed_path, "\n")
cat(" -", bank_error_ranking_fixed_path, "\n")
cat(" -", projection_variable_map_fixed_path, "\n")
cat(" -", figure_inventory_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")

cat("\nFigures:\n")
cat(paste(" -", figure_paths, collapse = "\n"))
cat("\n")
cat("============================================================\n")