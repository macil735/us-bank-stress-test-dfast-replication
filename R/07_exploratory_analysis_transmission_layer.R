# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 07 — Exploratory Analysis of DFAST Transmission Layer
# ============================================================
# Objective:
#   Conduct exploratory analysis of the DFAST transmission layer
#   created in Script 06.
#
# Main input:
#   data/processed/model/dfast_capital_losses_transmission_panel.csv
#
# Main outputs:
#   outputs/exploratory_analysis/
#   outputs/exploratory_analysis/figures/
#   outputs/exploratory_analysis/script07_exploratory_analysis_report.docx
#   outputs/exploratory_analysis/script07_execution_log.txt
#
# Methodological note:
#   This script does not estimate stress test models.
#   It diagnoses distributions, outliers, scenario patterns and
#   transmission relationships before econometric modelling.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "07"
script_name <- "exploratory_analysis_transmission_layer"
start_time <- Sys.time()

setwd(project_root)

dir.create("outputs/exploratory_analysis", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/exploratory_analysis/figures", recursive = TRUE, showWarnings = FALSE)


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

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

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, install_if_missing))

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(tibble)
library(purrr)
library(janitor)
library(ggplot2)
library(openxlsx)
library(officer)
library(flextable)


# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

safe_chr <- function(x) {
  x <- as.character(x)
  x <- enc2utf8(x)
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = " ")
  x <- gsub("[[:cntrl:]]", " ", x)
  x <- stringr::str_squish(x)
  x
}

safe_df <- function(df) {
  df |>
    mutate(across(where(is.character), safe_chr))
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

winsor_flag <- function(x, p_low = 0.01, p_high = 0.99) {
  q_low <- quantile(x, probs = p_low, na.rm = TRUE, names = FALSE)
  q_high <- quantile(x, probs = p_high, na.rm = TRUE, names = FALSE)

  if (is.na(q_low) || is.na(q_high)) {
    return(rep(FALSE, length(x)))
  }

  x < q_low | x > q_high
}

save_plot <- function(plot_object, file_name, width = 9, height = 5.5) {
  path <- file.path("outputs/exploratory_analysis/figures", file_name)
  ggsave(
    filename = path,
    plot = plot_object,
    width = width,
    height = height,
    dpi = 300
  )
  path
}


# ------------------------------------------------------------
# 3. Read input
# ------------------------------------------------------------

input_path <- "data/processed/model/dfast_capital_losses_transmission_panel.csv"

if (!file.exists(input_path)) {
  stop(paste("Missing input file:", input_path))
}

panel <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()


# ------------------------------------------------------------
# 4. Basic structure audit
# ------------------------------------------------------------

required_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "scenario_code",
  "scenario_label",
  "cet1_actual_ratio",
  "cet1_min_ratio",
  "cet1_depletion_min",
  "total_loan_losses_amount",
  "ppnr_amount",
  "provision_amount",
  "pretax_net_income_amount",
  "rwa_actual_amount",
  "rwa_end_amount",
  "rwa_growth_rate",
  "ppnr_loss_absorption_ratio"
)

required_column_check <- tibble(
  required_column = required_cols,
  exists = required_cols %in% names(panel)
) |>
  safe_df()

missing_required <- required_column_check |>
  filter(!exists) |>
  pull(required_column)

if (length(missing_required) > 0) {
  stop(
    paste(
      "Missing required columns in transmission panel:",
      paste(missing_required, collapse = ", ")
    )
  )
}

structure_audit <- tibble(
  input_path = input_path,
  rows = nrow(panel),
  columns = ncol(panel),
  banks = n_distinct(panel$bank_name),
  rssd_ids = n_distinct(panel$bank_rssd_id),
  years = n_distinct(panel$exercise_year),
  scenarios = n_distinct(panel$scenario_label),
  min_year = min(panel$exercise_year, na.rm = TRUE),
  max_year = max(panel$exercise_year, na.rm = TRUE)
) |>
  safe_df()


# ------------------------------------------------------------
# 5. Missing values audit
# ------------------------------------------------------------

missing_audit <- panel |>
  summarise(
    across(
      everything(),
      ~ mean(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_share"
  ) |>
  mutate(
    missing_count = map_int(variable, ~ sum(is.na(panel[[.x]]))),
    variable_class = map_chr(variable, ~ paste(class(panel[[.x]]), collapse = "|"))
  ) |>
  arrange(desc(missing_share), variable) |>
  safe_df()


# ------------------------------------------------------------
# 6. Core variable descriptive statistics
# ------------------------------------------------------------

core_vars <- c(
  "cet1_actual_ratio",
  "cet1_end_ratio",
  "cet1_min_ratio",
  "cet1_depletion_end",
  "cet1_depletion_min",
  "tier1_depletion_min",
  "total_capital_depletion_min",
  "leverage_depletion_min",
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
  "provision_to_loan_loss_ratio",
  "loan_loss_burden_on_ppnr",
  "observed_loss_burden_on_ppnr"
)

core_vars <- core_vars[core_vars %in% names(panel)]

core_descriptives <- panel |>
  select(all_of(core_vars)) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(variable) |>
  summarise(
    observations = n(),
    non_missing = sum(!is.na(value)),
    missing_share = mean(is.na(value)),
    mean = safe_mean(value),
    median = safe_median(value),
    sd = safe_sd(value),
    min = safe_min(value),
    max = safe_max(value),
    p05 = quantile(value, 0.05, na.rm = TRUE, names = FALSE),
    p25 = quantile(value, 0.25, na.rm = TRUE, names = FALSE),
    p75 = quantile(value, 0.75, na.rm = TRUE, names = FALSE),
    p95 = quantile(value, 0.95, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  arrange(variable) |>
  safe_df()


# ------------------------------------------------------------
# 7. Scenario-level summary
# ------------------------------------------------------------

scenario_summary <- panel |>
  group_by(exercise_year, scenario_code, scenario_label) |>
  summarise(
    n_banks = n_distinct(bank_name),

    mean_cet1_actual_ratio = safe_mean(cet1_actual_ratio),
    mean_cet1_min_ratio = safe_mean(cet1_min_ratio),
    mean_cet1_depletion_min = safe_mean(cet1_depletion_min),

    median_cet1_depletion_min = safe_median(cet1_depletion_min),
    max_cet1_depletion_min = safe_max(cet1_depletion_min),

    total_loan_losses_amount = sum(total_loan_losses_amount, na.rm = TRUE),
    total_ppnr_amount = sum(ppnr_amount, na.rm = TRUE),
    total_provision_amount = sum(provision_amount, na.rm = TRUE),
    total_pretax_net_income_amount = sum(pretax_net_income_amount, na.rm = TRUE),

    mean_ppnr_loss_absorption_ratio = safe_mean(ppnr_loss_absorption_ratio),
    mean_rwa_growth_rate = safe_mean(rwa_growth_rate),

    .groups = "drop"
  ) |>
  mutate(
    aggregate_ppnr_to_losses_ratio =
      ifelse(total_loan_losses_amount != 0, total_ppnr_amount / total_loan_losses_amount, NA_real_)
  ) |>
  arrange(exercise_year, scenario_code) |>
  safe_df()


# ------------------------------------------------------------
# 8. Bank-level summary across all years
# ------------------------------------------------------------

bank_summary_all_years <- panel |>
  group_by(bank_rssd_id, bank_name) |>
  summarise(
    n_years = n_distinct(exercise_year),
    n_scenarios = n_distinct(scenario_label),
    observations = n(),

    mean_cet1_actual_ratio = safe_mean(cet1_actual_ratio),
    mean_cet1_min_ratio = safe_mean(cet1_min_ratio),
    mean_cet1_depletion_min = safe_mean(cet1_depletion_min),
    max_cet1_depletion_min = safe_max(cet1_depletion_min),

    total_loan_losses_amount = sum(total_loan_losses_amount, na.rm = TRUE),
    total_ppnr_amount = sum(ppnr_amount, na.rm = TRUE),
    total_provision_amount = sum(provision_amount, na.rm = TRUE),
    total_pretax_net_income_amount = sum(pretax_net_income_amount, na.rm = TRUE),

    mean_ppnr_loss_absorption_ratio = safe_mean(ppnr_loss_absorption_ratio),
    mean_rwa_growth_rate = safe_mean(rwa_growth_rate),

    .groups = "drop"
  ) |>
  mutate(
    aggregate_ppnr_to_losses_ratio =
      ifelse(total_loan_losses_amount != 0, total_ppnr_amount / total_loan_losses_amount, NA_real_)
  ) |>
  arrange(desc(max_cet1_depletion_min)) |>
  safe_df()


# ------------------------------------------------------------
# 9. Bank-year stress ranking
# ------------------------------------------------------------

bank_year_stress_ranking <- panel |>
  select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    cet1_actual_ratio,
    cet1_min_ratio,
    cet1_depletion_min,
    total_loan_losses_amount,
    ppnr_amount,
    ppnr_loss_absorption_ratio,
    rwa_growth_rate
  ) |>
  arrange(desc(cet1_depletion_min), desc(total_loan_losses_amount)) |>
  mutate(
    stress_rank = row_number()
  ) |>
  safe_df()


# ------------------------------------------------------------
# 10. Outlier detection
# ------------------------------------------------------------

outlier_vars <- c(
  "cet1_depletion_min",
  "total_loan_losses_amount",
  "ppnr_amount",
  "ppnr_loss_absorption_ratio",
  "rwa_growth_rate",
  "pretax_net_income_amount"
)

outlier_vars <- outlier_vars[outlier_vars %in% names(panel)]

outlier_table <- panel |>
  select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,
    all_of(outlier_vars)
  ) |>
  pivot_longer(
    cols = all_of(outlier_vars),
    names_to = "variable",
    values_to = "value"
  ) |>
  group_by(variable) |>
  mutate(
    is_outlier = winsor_flag(value, 0.01, 0.99)
  ) |>
  ungroup() |>
  filter(is_outlier) |>
  arrange(variable, desc(abs(value))) |>
  safe_df()

outlier_summary <- outlier_table |>
  group_by(variable) |>
  summarise(
    outlier_count = n(),
    banks_affected = n_distinct(bank_name),
    years_affected = n_distinct(exercise_year),
    .groups = "drop"
  ) |>
  arrange(desc(outlier_count)) |>
  safe_df()


# ------------------------------------------------------------
# 11. Correlation matrix for core numeric variables
# ------------------------------------------------------------

corr_vars <- c(
  "cet1_depletion_min",
  "total_loan_losses_amount",
  "total_loan_losses_rate",
  "ppnr_amount",
  "ppnr_rate",
  "provision_amount",
  "pretax_net_income_amount",
  "rwa_growth_rate",
  "ppnr_loss_absorption_ratio",
  "loan_loss_burden_on_ppnr"
)

corr_vars <- corr_vars[corr_vars %in% names(panel)]

correlation_matrix <- panel |>
  select(all_of(corr_vars)) |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  tibble::rownames_to_column("variable") |>
  safe_df()

correlation_long <- correlation_matrix |>
  pivot_longer(
    cols = -variable,
    names_to = "variable_2",
    values_to = "correlation"
  ) |>
  arrange(variable, variable_2) |>
  safe_df()


# ------------------------------------------------------------
# 12. Figures
# ------------------------------------------------------------

figure_paths <- c()

p1 <- ggplot(panel, aes(x = cet1_depletion_min)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of CET1 Capital Depletion",
    x = "CET1 actual ratio minus CET1 minimum ratio",
    y = "Number of observations"
  ) +
  theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p1, "fig01_cet1_depletion_distribution.png")
)

p2 <- panel |>
  filter(!is.na(cet1_depletion_min)) |>
  group_by(exercise_year, scenario_label) |>
  summarise(
    mean_cet1_depletion_min = mean(cet1_depletion_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = exercise_year, y = mean_cet1_depletion_min, group = scenario_label)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average CET1 Depletion by Exercise Year and Scenario",
    x = "Exercise year",
    y = "Average CET1 depletion",
    group = "Scenario"
  ) +
  theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p2, "fig02_cet1_depletion_by_year_scenario.png")
)

p3 <- ggplot(panel, aes(x = total_loan_losses_amount, y = ppnr_amount)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "PPNR versus Loan Losses",
    x = "Total loan losses",
    y = "Pre-provision net revenue"
  ) +
  theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p3, "fig03_ppnr_vs_loan_losses.png")
)

p4 <- panel |>
  filter(!is.na(ppnr_loss_absorption_ratio)) |>
  ggplot(aes(x = ppnr_loss_absorption_ratio)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of PPNR Loss Absorption Ratio",
    x = "PPNR / observed losses",
    y = "Number of observations"
  ) +
  theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p4, "fig04_ppnr_loss_absorption_distribution.png")
)

p5 <- bank_summary_all_years |>
  slice_max(order_by = max_cet1_depletion_min, n = 20, with_ties = FALSE) |>
  ggplot(aes(x = reorder(bank_name, max_cet1_depletion_min), y = max_cet1_depletion_min)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Banks by Maximum CET1 Depletion",
    x = "Bank",
    y = "Maximum CET1 depletion"
  ) +
  theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p5, "fig05_top20_banks_cet1_depletion.png", width = 10, height = 7)
)

p6 <- panel |>
  filter(!is.na(rwa_growth_rate)) |>
  ggplot(aes(x = rwa_growth_rate)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of RWA Growth Rate",
    x = "RWA growth rate",
    y = "Number of observations"
  ) +
  theme_minimal()

figure_paths <- c(
  figure_paths,
  save_plot(p6, "fig06_rwa_growth_distribution.png")
)

figure_inventory <- tibble(
  figure_path = figure_paths,
  exists = file.exists(figure_paths),
  size_bytes = ifelse(file.exists(figure_paths), file.info(figure_paths)$size, NA_real_)
) |>
  safe_df()


# ------------------------------------------------------------
# 13. Save tabular outputs
# ------------------------------------------------------------

out_dir <- "outputs/exploratory_analysis"

required_column_check_path <- file.path(out_dir, "script07_required_column_check.csv")
structure_audit_path <- file.path(out_dir, "script07_structure_audit.csv")
missing_audit_path <- file.path(out_dir, "script07_missing_values_audit.csv")
core_descriptives_path <- file.path(out_dir, "script07_core_descriptive_statistics.csv")
scenario_summary_path <- file.path(out_dir, "script07_scenario_summary.csv")
bank_summary_path <- file.path(out_dir, "script07_bank_summary_all_years.csv")
stress_ranking_path <- file.path(out_dir, "script07_bank_year_stress_ranking.csv")
outlier_table_path <- file.path(out_dir, "script07_outlier_table.csv")
outlier_summary_path <- file.path(out_dir, "script07_outlier_summary.csv")
correlation_matrix_path <- file.path(out_dir, "script07_correlation_matrix.csv")
correlation_long_path <- file.path(out_dir, "script07_correlation_long.csv")
figure_inventory_path <- file.path(out_dir, "script07_figure_inventory.csv")
execution_summary_path <- file.path(out_dir, "script07_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script07_exploratory_analysis_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script07_exploratory_analysis_report.docx")
execution_log_path <- file.path(out_dir, "script07_execution_log.txt")

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_rows = nrow(panel),
  input_columns = ncol(panel),
  banks = n_distinct(panel$bank_name),
  years = n_distinct(panel$exercise_year),
  scenarios = n_distinct(panel$scenario_label),
  core_variables_analyzed = length(core_vars),
  outlier_variables_analyzed = length(outlier_vars),
  outliers_detected = nrow(outlier_table),
  figures_created = sum(figure_inventory$exists, na.rm = TRUE)
) |>
  safe_df()

write_csv(required_column_check, required_column_check_path)
write_csv(structure_audit, structure_audit_path)
write_csv(missing_audit, missing_audit_path)
write_csv(core_descriptives, core_descriptives_path)
write_csv(scenario_summary, scenario_summary_path)
write_csv(bank_summary_all_years, bank_summary_path)
write_csv(bank_year_stress_ranking, stress_ranking_path)
write_csv(outlier_table, outlier_table_path)
write_csv(outlier_summary, outlier_summary_path)
write_csv(correlation_matrix, correlation_matrix_path)
write_csv(correlation_long, correlation_long_path)
write_csv(figure_inventory, figure_inventory_path)
write_csv(execution_summary, execution_summary_path)


# ------------------------------------------------------------
# 14. Excel workbook
# ------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", execution_summary)

addWorksheet(wb, "structure_audit")
writeData(wb, "structure_audit", structure_audit)

addWorksheet(wb, "missing_audit")
writeData(wb, "missing_audit", missing_audit)

addWorksheet(wb, "core_descriptives")
writeData(wb, "core_descriptives", core_descriptives)

addWorksheet(wb, "scenario_summary")
writeData(wb, "scenario_summary", scenario_summary)

addWorksheet(wb, "bank_summary")
writeData(wb, "bank_summary", bank_summary_all_years)

addWorksheet(wb, "stress_ranking")
writeData(wb, "stress_ranking", bank_year_stress_ranking)

addWorksheet(wb, "outlier_summary")
writeData(wb, "outlier_summary", outlier_summary)

addWorksheet(wb, "correlation_matrix")
writeData(wb, "correlation_matrix", correlation_matrix)

addWorksheet(wb, "figure_inventory")
writeData(wb, "figure_inventory", figure_inventory)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_output_path, overwrite = TRUE)


# ------------------------------------------------------------
# 15. Word report
# ------------------------------------------------------------

doc <- read_docx()

doc <- doc |>
  body_add_par("Script 07 - Exploratory Analysis of DFAST Transmission Layer", style = "heading 1") |>
  body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  body_add_par("1. Objective", style = "heading 2") |>
  body_add_par(
    "This script conducts exploratory analysis of the DFAST transmission layer before econometric modelling. It examines data structure, distributions, outliers, scenario patterns and core transmission relationships.",
    style = "Normal"
  ) |>
  body_add_par("2. Execution summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(execution_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("3. Structure audit", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(structure_audit) |>
    autofit()
)

doc <- doc |>
  body_add_par("4. Core descriptive statistics", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(core_descriptives) |>
    autofit()
)

doc <- doc |>
  body_add_par("5. Scenario summary", style = "heading 2")

scenario_summary_small <- scenario_summary |>
  head(30)

doc <- body_add_flextable(
  doc,
  flextable(scenario_summary_small) |>
    autofit()
)

doc <- doc |>
  body_add_par("6. Largest CET1 depletion observations", style = "heading 2")

stress_top <- bank_year_stress_ranking |>
  head(20)

doc <- body_add_flextable(
  doc,
  flextable(stress_top) |>
    autofit()
)

doc <- doc |>
  body_add_par("7. Outlier summary", style = "heading 2")

if (nrow(outlier_summary) == 0) {
  doc <- body_add_par(doc, "No outliers detected under the selected 1st/99th percentile rule.", style = "Normal")
} else {
  doc <- body_add_flextable(
    doc,
    flextable(outlier_summary) |>
      autofit()
  )
}

doc <- doc |>
  body_add_par("8. Figures created", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(figure_inventory) |>
    autofit()
)

doc <- doc |>
  body_add_par("9. Methodological note", style = "heading 2") |>
  body_add_par(
    "The analysis is descriptive. It does not estimate credit loss, PPNR or capital models. Its role is to identify patterns, outliers and modelling risks before moving to formal estimation.",
    style = "Normal"
  )

print(doc, target = report_docx_path)


# ------------------------------------------------------------
# 16. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 07 - Exploratory Analysis of DFAST Transmission Layer completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input rows:", nrow(panel)),
  paste("Input columns:", ncol(panel)),
  paste("Banks:", n_distinct(panel$bank_name)),
  paste("Years:", n_distinct(panel$exercise_year)),
  paste("Scenarios:", n_distinct(panel$scenario_label)),
  paste("Core variables analyzed:", length(core_vars)),
  paste("Outlier variables analyzed:", length(outlier_vars)),
  paste("Outliers detected:", nrow(outlier_table)),
  paste("Figures created:", sum(figure_inventory$exists, na.rm = TRUE)),
  "",
  "Structure audit:",
  capture.output(print(structure_audit)),
  "",
  "Outlier summary:",
  capture.output(print(outlier_summary)),
  "",
  "Main outputs:",
  paste(" -", required_column_check_path),
  paste(" -", structure_audit_path),
  paste(" -", missing_audit_path),
  paste(" -", core_descriptives_path),
  paste(" -", scenario_summary_path),
  paste(" -", bank_summary_path),
  paste(" -", stress_ranking_path),
  paste(" -", outlier_table_path),
  paste(" -", outlier_summary_path),
  paste(" -", correlation_matrix_path),
  paste(" -", correlation_long_path),
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
# 17. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 07 - Exploratory Analysis of DFAST Transmission Layer completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input rows:\n", nrow(panel), "\n")
cat("Input columns:\n", ncol(panel), "\n")
cat("Banks:\n", n_distinct(panel$bank_name), "\n")
cat("Years:\n", n_distinct(panel$exercise_year), "\n")
cat("Scenarios:\n", n_distinct(panel$scenario_label), "\n")
cat("Core variables analyzed:\n", length(core_vars), "\n")
cat("Outlier variables analyzed:\n", length(outlier_vars), "\n")
cat("Outliers detected:\n", nrow(outlier_table), "\n")
cat("Figures created:\n", sum(figure_inventory$exists, na.rm = TRUE), "\n\n")

cat("Structure audit:\n")
print(structure_audit)

cat("\nOutlier summary:\n")
print(outlier_summary)

cat("\nMain outputs:\n")
cat(" -", required_column_check_path, "\n")
cat(" -", structure_audit_path, "\n")
cat(" -", missing_audit_path, "\n")
cat(" -", core_descriptives_path, "\n")
cat(" -", scenario_summary_path, "\n")
cat(" -", bank_summary_path, "\n")
cat(" -", stress_ranking_path, "\n")
cat(" -", outlier_table_path, "\n")
cat(" -", outlier_summary_path, "\n")
cat(" -", correlation_matrix_path, "\n")
cat(" -", correlation_long_path, "\n")
cat(" -", figure_inventory_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")

cat("\nFigures:\n")
cat(paste(" -", figure_paths, collapse = "\n"))
cat("\n")