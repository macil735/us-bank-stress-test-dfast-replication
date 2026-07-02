# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 08 — Build Modelling Sample and Treatment Rules
# ============================================================

cat("\n============================================================\n")
cat("Starting Script 08 - Build Modelling Sample\n")
cat("============================================================\n")

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "08"
script_name <- "build_modelling_sample"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/model", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/modelling_sample", recursive = TRUE, showWarnings = FALSE)

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

safe_divide <- function(num, den) {
  ifelse(!is.na(den) & den != 0, num / den, NA_real_)
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

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

winsorize_vec <- function(x, p_low = 0.01, p_high = 0.99) {
  if (all(is.na(x))) return(x)

  q_low <- suppressWarnings(quantile(x, p_low, na.rm = TRUE, names = FALSE))
  q_high <- suppressWarnings(quantile(x, p_high, na.rm = TRUE, names = FALSE))

  if (is.na(q_low) | is.na(q_high)) return(x)

  pmin(pmax(x, q_low), q_high)
}

flag_outlier_vec <- function(x, p_low = 0.01, p_high = 0.99) {
  if (all(is.na(x))) return(rep(FALSE, length(x)))

  q_low <- suppressWarnings(quantile(x, p_low, na.rm = TRUE, names = FALSE))
  q_high <- suppressWarnings(quantile(x, p_high, na.rm = TRUE, names = FALSE))

  if (is.na(q_low) | is.na(q_high)) return(rep(FALSE, length(x)))

  x < q_low | x > q_high
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Read input
# ------------------------------------------------------------

cat("Reading input file...\n")

input_path <- "data/processed/model/dfast_capital_losses_transmission_panel.csv"

if (!file.exists(input_path)) {
  stop(paste("Missing input file:", input_path))
}

panel_raw <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()

cat("Input loaded.\n")
cat("Rows:", nrow(panel_raw), "\n")
cat("Columns:", ncol(panel_raw), "\n\n")


# ------------------------------------------------------------
# 4. Replace Inf with NA
# ------------------------------------------------------------

cat("Cleaning non-finite numeric values...\n")

panel <- panel_raw |>
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)
    )
  )

cat("Non-finite values converted to NA.\n\n")


# ------------------------------------------------------------
# 5. Required column check
# ------------------------------------------------------------

cat("Checking required modelling variables...\n")

required_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "exercise_quarter",
  "scenario_code",
  "scenario_label",
  "cet1_actual_ratio",
  "cet1_min_ratio",
  "cet1_depletion_min",
  "total_loan_losses_amount",
  "total_loan_losses_rate",
  "ppnr_amount",
  "ppnr_rate",
  "provision_amount",
  "pretax_net_income_amount",
  "rwa_actual_amount",
  "rwa_end_amount",
  "rwa_growth_rate",
  "ppnr_loss_absorption_ratio",
  "loan_loss_burden_on_ppnr",
  "observed_loss_burden_on_ppnr"
)

required_column_check <- tibble::tibble(
  required_column = required_cols,
  exists = required_cols %in% names(panel)
)

missing_required <- required_column_check |>
  dplyr::filter(!exists) |>
  dplyr::pull(required_column)

readr::write_csv(
  required_column_check,
  "outputs/modelling_sample/script08_required_column_check.csv"
)

if (length(missing_required) > 0) {
  cat("Missing required columns:\n")
  print(missing_required)
  stop("Script 08 stopped because required modelling variables are missing.")
}

cat("Required variables checked.\n\n")


# ------------------------------------------------------------
# 6. Create modelling variables
# ------------------------------------------------------------

cat("Creating dependent and explanatory variables...\n")

modelling_sample <- panel |>
  dplyr::mutate(
    y_credit_loss_ratio = safe_divide(
      total_loan_losses_amount,
      rwa_actual_amount
    ),

    y_credit_loss_rate = total_loan_losses_rate,

    y_ppnr_ratio = safe_divide(
      ppnr_amount,
      rwa_actual_amount
    ),

    y_ppnr_rate = ppnr_rate,

    y_capital_depletion = cet1_depletion_min,

    y_pretax_margin = safe_divide(
      pretax_net_income_amount,
      rwa_actual_amount
    ),

    x_initial_cet1 = cet1_actual_ratio,

    x_initial_rwa = rwa_actual_amount,

    x_rwa_growth = rwa_growth_rate,

    x_ppnr_absorption = ppnr_loss_absorption_ratio,

    x_loan_loss_burden = loan_loss_burden_on_ppnr,

    x_observed_loss_burden = observed_loss_burden_on_ppnr,

    x_provision_ratio = safe_divide(
      provision_amount,
      total_loan_losses_amount
    ),

    x_ppnr_to_losses = safe_divide(
      ppnr_amount,
      total_loan_losses_amount
    )
  ) |>
  safe_df()

cat("Modelling variables created.\n\n")

# ------------------------------------------------------------
# 7. Define modelling samples
# ------------------------------------------------------------

cat("Defining modelling samples...\n")

credit_loss_required <- c(
  "y_credit_loss_ratio",
  "cet1_actual_ratio",
  "rwa_actual_amount",
  "scenario_label",
  "exercise_year"
)

ppnr_required <- c(
  "y_ppnr_ratio",
  "rwa_actual_amount",
  "scenario_label",
  "exercise_year"
)

capital_required <- c(
  "y_capital_depletion",
  "total_loan_losses_amount",
  "ppnr_amount",
  "rwa_growth_rate",
  "scenario_label",
  "exercise_year"
)

credit_loss_required <- credit_loss_required[
  credit_loss_required %in% names(modelling_sample)
]

ppnr_required <- ppnr_required[
  ppnr_required %in% names(modelling_sample)
]

capital_required <- capital_required[
  capital_required %in% names(modelling_sample)
]

credit_loss_flag <- as.integer(
  stats::complete.cases(
    modelling_sample[, credit_loss_required, drop = FALSE]
  )
)

ppnr_flag <- as.integer(
  stats::complete.cases(
    modelling_sample[, ppnr_required, drop = FALSE]
  )
)

capital_flag <- as.integer(
  stats::complete.cases(
    modelling_sample[, capital_required, drop = FALSE]
  )
)

modelling_sample <- modelling_sample |>
  dplyr::mutate(
    credit_loss_model_sample = credit_loss_flag,
    ppnr_model_sample = ppnr_flag,
    capital_model_sample = capital_flag,
    full_core_model_sample = as.integer(
      credit_loss_model_sample == 1 &
        ppnr_model_sample == 1 &
        capital_model_sample == 1
    )
  )

cat("Samples defined.\n")
cat("Credit loss model sample:", sum(modelling_sample$credit_loss_model_sample, na.rm = TRUE), "\n")
cat("PPNR model sample:", sum(modelling_sample$ppnr_model_sample, na.rm = TRUE), "\n")
cat("Capital model sample:", sum(modelling_sample$capital_model_sample, na.rm = TRUE), "\n")
cat("Full core model sample:", sum(modelling_sample$full_core_model_sample, na.rm = TRUE), "\n\n")

# ------------------------------------------------------------
# 8. Outlier flags
# ------------------------------------------------------------

cat("Creating outlier flags...\n")

treatment_vars <- c(
  "y_credit_loss_ratio",
  "y_credit_loss_rate",
  "y_ppnr_ratio",
  "y_ppnr_rate",
  "y_capital_depletion",
  "y_pretax_margin",
  "x_rwa_growth",
  "x_ppnr_absorption",
  "x_loan_loss_burden",
  "x_observed_loss_burden",
  "x_provision_ratio",
  "x_ppnr_to_losses"
)

treatment_vars <- treatment_vars[treatment_vars %in% names(modelling_sample)]

outlier_flags <- modelling_sample |>
  dplyr::select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    dplyr::all_of(treatment_vars)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(treatment_vars),
    names_to = "variable",
    values_to = "value"
  ) |>
  dplyr::group_by(variable) |>
  dplyr::mutate(
    outlier_1_99_flag = flag_outlier_vec(value, 0.01, 0.99)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(outlier_1_99_flag) |>
  dplyr::arrange(variable, dplyr::desc(abs(value))) |>
  safe_df()

outlier_summary <- outlier_flags |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    outlier_count = dplyr::n(),
    banks_affected = dplyr::n_distinct(bank_name),
    years_affected = dplyr::n_distinct(exercise_year),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(outlier_count)) |>
  safe_df()

cat("Outlier flags created.\n")
cat("Outliers detected:", nrow(outlier_flags), "\n\n")


# ------------------------------------------------------------
# 9. Winsorized sample
# ------------------------------------------------------------

cat("Creating winsorized sample...\n")

modelling_sample_winsorized <- modelling_sample

for (v in treatment_vars) {
  modelling_sample_winsorized[[paste0(v, "_winsor")]] <-
    winsorize_vec(modelling_sample_winsorized[[v]], 0.01, 0.99)
}

cat("Winsorized variables created:", length(treatment_vars), "\n\n")


# ------------------------------------------------------------
# 10. Sample summaries
# ------------------------------------------------------------

cat("Creating summaries...\n")

sample_summary <- tibble::tibble(
  sample_name = c(
    "raw_transmission_panel",
    "credit_loss_model_sample",
    "ppnr_model_sample",
    "capital_model_sample",
    "full_core_model_sample"
  ),
  observations = c(
    nrow(modelling_sample),
    sum(modelling_sample$credit_loss_model_sample, na.rm = TRUE),
    sum(modelling_sample$ppnr_model_sample, na.rm = TRUE),
    sum(modelling_sample$capital_model_sample, na.rm = TRUE),
    sum(modelling_sample$full_core_model_sample, na.rm = TRUE)
  ),
  share_of_raw_sample = observations / nrow(modelling_sample)
) |>
  safe_df()

scenario_sample_summary <- modelling_sample |>
  dplyr::group_by(exercise_year, scenario_code, scenario_label) |>
  dplyr::summarise(
    observations = dplyr::n(),
    credit_loss_model_sample = sum(credit_loss_model_sample, na.rm = TRUE),
    ppnr_model_sample = sum(ppnr_model_sample, na.rm = TRUE),
    capital_model_sample = sum(capital_model_sample, na.rm = TRUE),
    full_core_model_sample = sum(full_core_model_sample, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(exercise_year, scenario_code) |>
  safe_df()

variable_treatment_summary <- modelling_sample |>
  dplyr::select(dplyr::all_of(treatment_vars)) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "value"
  ) |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    observations = dplyr::n(),
    non_missing = sum(!is.na(value)),
    missing_share = mean(is.na(value)),
    mean_raw = safe_mean(value),
    median_raw = safe_median(value),
    min_raw = safe_min(value),
    max_raw = safe_max(value),
    p01 = suppressWarnings(quantile(value, 0.01, na.rm = TRUE, names = FALSE)),
    p99 = suppressWarnings(quantile(value, 0.99, na.rm = TRUE, names = FALSE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  dplyr::arrange(variable) |>
  safe_df()

cat("Summaries created.\n\n")


# ------------------------------------------------------------
# 11. Save datasets
# ------------------------------------------------------------

cat("Saving datasets...\n")

clean_path <- "data/processed/model/modelling_sample_clean.csv"
winsorized_path <- "data/processed/model/modelling_sample_winsorized.csv"
flags_path <- "data/processed/model/modelling_sample_outlier_flags.csv"

readr::write_csv(modelling_sample, clean_path)
readr::write_csv(modelling_sample_winsorized, winsorized_path)
readr::write_csv(outlier_flags, flags_path)

cat("Datasets saved.\n\n")


# ------------------------------------------------------------
# 12. Save audit outputs
# ------------------------------------------------------------

cat("Saving audit outputs...\n")

out_dir <- "outputs/modelling_sample"

sample_summary_path <- file.path(out_dir, "script08_sample_summary.csv")
scenario_sample_summary_path <- file.path(out_dir, "script08_scenario_sample_summary.csv")
variable_treatment_summary_path <- file.path(out_dir, "script08_variable_treatment_summary.csv")
outlier_summary_path <- file.path(out_dir, "script08_outlier_summary.csv")
outlier_flags_path <- file.path(out_dir, "script08_outlier_flags.csv")
execution_summary_path <- file.path(out_dir, "script08_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script08_modelling_sample_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script08_modelling_sample_report.docx")
execution_log_path <- file.path(out_dir, "script08_execution_log.txt")

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_rows = nrow(panel_raw),
  input_columns = ncol(panel_raw),
  modelling_sample_rows = nrow(modelling_sample),
  winsorized_sample_rows = nrow(modelling_sample_winsorized),
  treatment_variables = length(treatment_vars),
  outliers_detected = nrow(outlier_flags),
  credit_loss_model_sample = sum(modelling_sample$credit_loss_model_sample, na.rm = TRUE),
  ppnr_model_sample = sum(modelling_sample$ppnr_model_sample, na.rm = TRUE),
  capital_model_sample = sum(modelling_sample$capital_model_sample, na.rm = TRUE),
  full_core_model_sample = sum(modelling_sample$full_core_model_sample, na.rm = TRUE)
) |>
  safe_df()

readr::write_csv(sample_summary, sample_summary_path)
readr::write_csv(scenario_sample_summary, scenario_sample_summary_path)
readr::write_csv(variable_treatment_summary, variable_treatment_summary_path)
readr::write_csv(outlier_summary, outlier_summary_path)
readr::write_csv(outlier_flags, outlier_flags_path)
readr::write_csv(execution_summary, execution_summary_path)

cat("Audit outputs saved.\n\n")


# ------------------------------------------------------------
# 13. Excel workbook
# ------------------------------------------------------------

cat("Creating Excel workbook...\n")

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "execution_summary")
openxlsx::writeData(wb, "execution_summary", execution_summary)

openxlsx::addWorksheet(wb, "sample_summary")
openxlsx::writeData(wb, "sample_summary", sample_summary)

openxlsx::addWorksheet(wb, "scenario_summary")
openxlsx::writeData(wb, "scenario_summary", scenario_sample_summary)

openxlsx::addWorksheet(wb, "variable_treatment")
openxlsx::writeData(wb, "variable_treatment", variable_treatment_summary)

openxlsx::addWorksheet(wb, "outlier_summary")
openxlsx::writeData(wb, "outlier_summary", outlier_summary)

openxlsx::addWorksheet(wb, "required_columns")
openxlsx::writeData(wb, "required_columns", required_column_check)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output_path, overwrite = TRUE)

cat("Excel workbook created.\n\n")


# ------------------------------------------------------------
# 14. Word report
# ------------------------------------------------------------

cat("Creating Word report...\n")

doc <- officer::read_docx()

doc <- doc |>
  officer::body_add_par("Script 08 - Build Modelling Sample and Treatment Rules", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This script constructs modelling-ready samples from the DFAST transmission layer. It defines dependent variables, treatment variables, outlier flags and winsorized variables for later econometric estimation.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Execution summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(execution_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Modelling sample summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(sample_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. Variable treatment summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(variable_treatment_summary) |>
    flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("5. Outlier summary", style = "heading 2")

if (nrow(outlier_summary) == 0) {
  doc <- officer::body_add_par(
    doc,
    "No outliers detected under the 1st/99th percentile rule.",
    style = "Normal"
  )
} else {
  doc <- flextable::body_add_flextable(
    x = doc,
    value = flextable::flextable(outlier_summary) |>
      flextable::autofit()
  )
}

doc <- doc |>
  officer::body_add_par("6. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "The script preserves the raw modelling sample and creates winsorized variables separately. This avoids overwriting public DFAST values while providing a robust estimation-ready dataset.",
    style = "Normal"
  )

print(doc, target = report_docx_path)

cat("Word report created.\n\n")

# ------------------------------------------------------------
# 15. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 08 - Build Modelling Sample and Treatment Rules completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input rows:", nrow(panel_raw)),
  paste("Input columns:", ncol(panel_raw)),
  paste("Modelling sample rows:", nrow(modelling_sample)),
  paste("Winsorized sample rows:", nrow(modelling_sample_winsorized)),
  paste("Treatment variables:", length(treatment_vars)),
  paste("Outliers detected:", nrow(outlier_flags)),
  paste("Credit loss model sample:", sum(modelling_sample$credit_loss_model_sample, na.rm = TRUE)),
  paste("PPNR model sample:", sum(modelling_sample$ppnr_model_sample, na.rm = TRUE)),
  paste("Capital model sample:", sum(modelling_sample$capital_model_sample, na.rm = TRUE)),
  paste("Full core model sample:", sum(modelling_sample$full_core_model_sample, na.rm = TRUE)),
  "",
  "Sample summary:",
  capture.output(print(sample_summary)),
  "",
  "Outlier summary:",
  capture.output(print(outlier_summary)),
  "",
  "Main outputs:",
  paste(" -", clean_path),
  paste(" -", winsorized_path),
  paste(" -", flags_path),
  paste(" -", sample_summary_path),
  paste(" -", scenario_sample_summary_path),
  paste(" -", variable_treatment_summary_path),
  paste(" -", outlier_summary_path),
  paste(" -", execution_summary_path),
  paste(" -", excel_output_path),
  paste(" -", report_docx_path),
  paste(" -", execution_log_path)
)

writeLines(enc2utf8(log_lines), execution_log_path, useBytes = TRUE)


# ------------------------------------------------------------
# 16. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 08 - Build Modelling Sample and Treatment Rules completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input rows:\n", nrow(panel_raw), "\n")
cat("Input columns:\n", ncol(panel_raw), "\n")
cat("Modelling sample rows:\n", nrow(modelling_sample), "\n")
cat("Winsorized sample rows:\n", nrow(modelling_sample_winsorized), "\n")
cat("Treatment variables:\n", length(treatment_vars), "\n")
cat("Outliers detected:\n", nrow(outlier_flags), "\n")
cat("Credit loss model sample:\n", sum(modelling_sample$credit_loss_model_sample, na.rm = TRUE), "\n")
cat("PPNR model sample:\n", sum(modelling_sample$ppnr_model_sample, na.rm = TRUE), "\n")
cat("Capital model sample:\n", sum(modelling_sample$capital_model_sample, na.rm = TRUE), "\n")
cat("Full core model sample:\n", sum(modelling_sample$full_core_model_sample, na.rm = TRUE), "\n\n")

cat("Sample summary:\n")
print(sample_summary)

cat("\nOutlier summary:\n")
print(outlier_summary)

cat("\nMain outputs:\n")
cat(" -", clean_path, "\n")
cat(" -", winsorized_path, "\n")
cat(" -", flags_path, "\n")
cat(" -", sample_summary_path, "\n")
cat(" -", scenario_sample_summary_path, "\n")
cat(" -", variable_treatment_summary_path, "\n")
cat(" -", outlier_summary_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")
cat("============================================================\n")