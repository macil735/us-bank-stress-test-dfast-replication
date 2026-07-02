# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 04 — Structure Federal Reserve Macro Scenarios
# ============================================================
# Objective:
#   Structure Federal Reserve domestic macroeconomic scenarios
#   into analytical long and wide formats for the DFAST-style
#   replication pipeline.
#
# Inputs from Script 03:
#   data/processed/fed/fed_macro_historic_domestic_clean.csv
#   data/processed/fed/fed_macro_baseline_domestic_clean.csv
#   data/processed/fed/fed_macro_severely_adverse_domestic_clean.csv
#
# Outputs:
#   data/processed/fed/fed_macro_scenarios_long.csv
#   data/processed/fed/fed_macro_scenarios_wide.csv
#   data/processed/fed/fed_macro_scenario_shocks.csv
#   outputs/scenario_cleaning/fed_macro_scenario_variable_map.csv
#   outputs/scenario_cleaning/fed_macro_scenario_shock_summary.csv
#   outputs/scenario_cleaning/script04_macro_scenario_cleaning_report.docx
#   outputs/scenario_cleaning/script04_execution_log.txt
#
# Methodological note:
#   This script structures official Federal Reserve macro scenarios.
#   It does not estimate credit losses, PPNR or capital ratios.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "04"
script_name <- "structure_fed_macro_scenarios"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/fed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/scenario_cleaning", recursive = TRUE, showWarnings = FALSE)


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

read_clean_csv <- function(path, dataset_name) {
  if (!file.exists(path)) {
    stop(paste("Missing required input:", path))
  }

  readr::read_csv(path, show_col_types = FALSE, guess_max = 100000) |>
    janitor::clean_names() |>
    mutate(source_dataset = dataset_name, .before = 1)
}

detect_time_column <- function(df) {
  names_df <- names(df)

  candidates <- c(
    "date",
    "quarter",
    "period",
    "time",
    "year",
    "date_quarter",
    "scenario_quarter",
    "projection_quarter",
    "quarter_date"
  )

  found <- intersect(candidates, names_df)

  if (length(found) > 0) {
    return(found[1])
  }

  possible <- names_df[str_detect(names_df, "date|quarter|period|year|time")]

  if (length(possible) > 0) {
    return(possible[1])
  }

  return(NA_character_)
}

create_period_index <- function(df, time_col) {
  if (!is.na(time_col) && time_col %in% names(df)) {
    df |>
      mutate(
        period_raw = as.character(.data[[time_col]]),
        period_index = row_number(),
        .after = scenario
      )
  } else {
    df |>
      mutate(
        period_raw = as.character(row_number()),
        period_index = row_number(),
        .after = scenario
      )
  }
}

is_macro_numeric_col <- function(df, col_name) {
  if (!col_name %in% names(df)) return(FALSE)

  x <- df[[col_name]]

  if (!is.numeric(x)) return(FALSE)

  excluded_patterns <- paste(
    c(
      "year",
      "index",
      "id",
      "rssd",
      "cik",
      "code"
    ),
    collapse = "|"
  )

  !str_detect(col_name, excluded_patterns)
}


# ------------------------------------------------------------
# 3. Input paths
# ------------------------------------------------------------

historic_path <- "data/processed/fed/fed_macro_historic_domestic_clean.csv"
baseline_path <- "data/processed/fed/fed_macro_baseline_domestic_clean.csv"
severe_path <- "data/processed/fed/fed_macro_severely_adverse_domestic_clean.csv"


# ------------------------------------------------------------
# 4. Read inputs
# ------------------------------------------------------------

historic_raw <- read_clean_csv(historic_path, "fed_macro_historic_domestic_clean")
baseline_raw <- read_clean_csv(baseline_path, "fed_macro_baseline_domestic_clean")
severe_raw <- read_clean_csv(severe_path, "fed_macro_severely_adverse_domestic_clean")


# ------------------------------------------------------------
# 5. Add scenario labels
# ------------------------------------------------------------

historic <- historic_raw |>
  mutate(
    scenario = "historic",
    scenario_order = 1,
    .before = 1
  )

baseline <- baseline_raw |>
  mutate(
    scenario = "baseline",
    scenario_order = 2,
    .before = 1
  )

severe <- severe_raw |>
  mutate(
    scenario = "severely_adverse",
    scenario_order = 3,
    .before = 1
  )


# ------------------------------------------------------------
# 6. Harmonise time structure
# ------------------------------------------------------------

historic_time_col <- detect_time_column(historic)
baseline_time_col <- detect_time_column(baseline)
severe_time_col <- detect_time_column(severe)

historic <- create_period_index(historic, historic_time_col)
baseline <- create_period_index(baseline, baseline_time_col)
severe <- create_period_index(severe, severe_time_col)


# ------------------------------------------------------------
# 7. Combine scenarios
# ------------------------------------------------------------

scenarios_all <- bind_rows(historic, baseline, severe) |>
  safe_df()


# ------------------------------------------------------------
# 8. Identify macro scenario variables
# ------------------------------------------------------------
# Important:
#   Only numeric macro variables are pivoted.
#   Metadata columns are deliberately excluded.

metadata_cols <- c(
  "scenario",
  "scenario_order",
  "period_index",
  "period_raw",
  "source_dataset",
  "source_file",
  "dataset_role",
  "scenario_type",
  "cleaned_at",
  "scenario_name"
)

metadata_cols <- metadata_cols[metadata_cols %in% names(scenarios_all)]

numeric_cols <- names(scenarios_all)[map_lgl(scenarios_all, is.numeric)]

candidate_macro_vars <- setdiff(numeric_cols, metadata_cols)

candidate_macro_vars <- candidate_macro_vars[
  map_lgl(candidate_macro_vars, ~ is_macro_numeric_col(scenarios_all, .x))
]

if (length(candidate_macro_vars) == 0) {
  stop("No numeric macro scenario variables detected. Check input file structure.")
}


# ------------------------------------------------------------
# 9. Long format
# ------------------------------------------------------------

scenarios_long <- scenarios_all |>
  select(all_of(metadata_cols), all_of(candidate_macro_vars)) |>
  pivot_longer(
    cols = all_of(candidate_macro_vars),
    names_to = "macro_variable",
    values_to = "value"
  ) |>
  mutate(
    value = as.numeric(value)
  ) |>
  arrange(scenario_order, period_index, macro_variable) |>
  safe_df()


# ------------------------------------------------------------
# 10. Wide format
# ------------------------------------------------------------

scenarios_wide <- scenarios_long |>
  select(
    scenario,
    scenario_order,
    period_index,
    period_raw,
    macro_variable,
    value
  ) |>
  pivot_wider(
    names_from = macro_variable,
    values_from = value
  ) |>
  arrange(scenario_order, period_index) |>
  safe_df()


# ------------------------------------------------------------
# 11. Baseline versus severely adverse shock dataset
# ------------------------------------------------------------

scenario_shocks <- scenarios_long |>
  filter(scenario %in% c("baseline", "severely_adverse")) |>
  select(scenario, period_index, period_raw, macro_variable, value) |>
  pivot_wider(
    names_from = scenario,
    values_from = value
  ) |>
  mutate(
    shock_level = severely_adverse - baseline,
    shock_ratio = ifelse(
      !is.na(baseline) & baseline != 0,
      severely_adverse / baseline,
      NA_real_
    ),
    absolute_shock = abs(shock_level)
  ) |>
  arrange(macro_variable, period_index) |>
  safe_df()


# ------------------------------------------------------------
# 12. Variable map
# ------------------------------------------------------------

variable_map <- scenarios_long |>
  group_by(macro_variable, scenario) |>
  summarise(
    n_obs = n(),
    n_non_missing = sum(!is.na(value)),
    n_missing = sum(is.na(value)),
    min_value = suppressWarnings(min(value, na.rm = TRUE)),
    max_value = suppressWarnings(max(value, na.rm = TRUE)),
    mean_value = suppressWarnings(mean(value, na.rm = TRUE)),
    first_value = dplyr::first(value),
    last_value = dplyr::last(value),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(min_value, max_value, mean_value, first_value, last_value),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  arrange(macro_variable, scenario) |>
  safe_df()


# ------------------------------------------------------------
# 13. Shock summary
# ------------------------------------------------------------

shock_summary <- scenario_shocks |>
  group_by(macro_variable) |>
  summarise(
    n_periods = n(),
    baseline_mean = mean(baseline, na.rm = TRUE),
    severely_adverse_mean = mean(severely_adverse, na.rm = TRUE),
    mean_shock = mean(shock_level, na.rm = TRUE),
    max_adverse_shock = shock_level[which.max(abs(shock_level))][1],
    max_absolute_shock = max(absolute_shock, na.rm = TRUE),
    period_of_max_absolute_shock = period_index[which.max(absolute_shock)][1],
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(
        baseline_mean,
        severely_adverse_mean,
        mean_shock,
        max_adverse_shock,
        max_absolute_shock
      ),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  arrange(desc(max_absolute_shock)) |>
  safe_df()


# ------------------------------------------------------------
# 14. Scenario structure audit
# ------------------------------------------------------------

scenario_structure_audit <- scenarios_long |>
  group_by(scenario) |>
  summarise(
    n_periods = n_distinct(period_index),
    n_macro_variables = n_distinct(macro_variable),
    n_observations = n(),
    n_missing_values = sum(is.na(value)),
    missing_share = mean(is.na(value)),
    .groups = "drop"
  ) |>
  arrange(scenario) |>
  safe_df()

input_structure_audit <- tibble(
  input_dataset = c(
    "historic",
    "baseline",
    "severely_adverse"
  ),
  input_path = c(
    historic_path,
    baseline_path,
    severe_path
  ),
  detected_time_column = c(
    historic_time_col,
    baseline_time_col,
    severe_time_col
  ),
  rows = c(
    nrow(historic_raw),
    nrow(baseline_raw),
    nrow(severe_raw)
  ),
  columns = c(
    ncol(historic_raw),
    ncol(baseline_raw),
    ncol(severe_raw)
  )
) |>
  safe_df()


# ------------------------------------------------------------
# 15. Save processed datasets
# ------------------------------------------------------------

long_path <- "data/processed/fed/fed_macro_scenarios_long.csv"
wide_path <- "data/processed/fed/fed_macro_scenarios_wide.csv"
shocks_path <- "data/processed/fed/fed_macro_scenario_shocks.csv"

write_csv(scenarios_long, long_path)
write_csv(scenarios_wide, wide_path)
write_csv(scenario_shocks, shocks_path)


# ------------------------------------------------------------
# 16. Save audit outputs
# ------------------------------------------------------------

out_dir <- "outputs/scenario_cleaning"

variable_map_path <- file.path(out_dir, "fed_macro_scenario_variable_map.csv")
shock_summary_path <- file.path(out_dir, "fed_macro_scenario_shock_summary.csv")
structure_audit_path <- file.path(out_dir, "fed_macro_scenario_structure_audit.csv")
input_audit_path <- file.path(out_dir, "script04_input_structure_audit.csv")
macro_vars_path <- file.path(out_dir, "script04_detected_macro_variables.csv")
execution_summary_path <- file.path(out_dir, "script04_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script04_macro_scenario_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script04_macro_scenario_cleaning_report.docx")
execution_log_path <- file.path(out_dir, "script04_execution_log.txt")

detected_macro_variables <- tibble(
  macro_variable = candidate_macro_vars
) |>
  arrange(macro_variable) |>
  safe_df()

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_files = 3,
  scenarios_structured = n_distinct(scenarios_long$scenario),
  macro_variables_detected = length(candidate_macro_vars),
  long_rows = nrow(scenarios_long),
  wide_rows = nrow(scenarios_wide),
  shock_rows = nrow(scenario_shocks)
) |>
  safe_df()

write_csv(variable_map, variable_map_path)
write_csv(shock_summary, shock_summary_path)
write_csv(scenario_structure_audit, structure_audit_path)
write_csv(input_structure_audit, input_audit_path)
write_csv(detected_macro_variables, macro_vars_path)
write_csv(execution_summary, execution_summary_path)


# ------------------------------------------------------------
# 17. Excel workbook
# ------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", execution_summary)

addWorksheet(wb, "input_structure")
writeData(wb, "input_structure", input_structure_audit)

addWorksheet(wb, "scenario_structure")
writeData(wb, "scenario_structure", scenario_structure_audit)

addWorksheet(wb, "detected_macro_vars")
writeData(wb, "detected_macro_vars", detected_macro_variables)

addWorksheet(wb, "variable_map")
writeData(wb, "variable_map", variable_map)

addWorksheet(wb, "shock_summary")
writeData(wb, "shock_summary", shock_summary)

addWorksheet(wb, "scenario_shocks")
writeData(wb, "scenario_shocks", scenario_shocks)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_output_path, overwrite = TRUE)


# ------------------------------------------------------------
# 18. Word report
# ------------------------------------------------------------

doc <- read_docx()

doc <- doc |>
  body_add_par("Script 04 - Structure Federal Reserve Macro Scenarios", style = "heading 1") |>
  body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  body_add_par("1. Objective", style = "heading 2") |>
  body_add_par(
    "This script structures Federal Reserve domestic macroeconomic scenarios into analytical long and wide datasets for the DFAST-style stress test replication pipeline.",
    style = "Normal"
  ) |>
  body_add_par("2. Execution summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(execution_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("3. Input structure audit", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(input_structure_audit) |>
    autofit()
)

doc <- doc |>
  body_add_par("4. Scenario structure audit", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(scenario_structure_audit) |>
    autofit()
)

doc <- doc |>
  body_add_par("5. Largest scenario shocks", style = "heading 2")

shock_top <- shock_summary |>
  arrange(desc(max_absolute_shock)) |>
  head(15)

doc <- body_add_flextable(
  doc,
  flextable(shock_top) |>
    autofit()
)

doc <- doc |>
  body_add_par("6. Methodological note", style = "heading 2") |>
  body_add_par(
    "The script transforms only numeric macroeconomic variables into long format. Metadata columns such as source files, cleaning timestamps and scenario descriptors are preserved but excluded from the macro variable pivot.",
    style = "Normal"
  )

print(doc, target = report_docx_path)


# ------------------------------------------------------------
# 19. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 04 - Structure Federal Reserve Macro Scenarios completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input files:", 3),
  paste("Scenarios structured:", n_distinct(scenarios_long$scenario)),
  paste("Macro variables detected:", length(candidate_macro_vars)),
  paste("Long rows:", nrow(scenarios_long)),
  paste("Wide rows:", nrow(scenarios_wide)),
  paste("Shock rows:", nrow(scenario_shocks)),
  "",
  "Input structure audit:",
  capture.output(print(input_structure_audit)),
  "",
  "Scenario structure audit:",
  capture.output(print(scenario_structure_audit)),
  "",
  "Processed outputs:",
  paste(" -", long_path),
  paste(" -", wide_path),
  paste(" -", shocks_path),
  "",
  "Audit outputs:",
  paste(" -", variable_map_path),
  paste(" -", shock_summary_path),
  paste(" -", structure_audit_path),
  paste(" -", input_audit_path),
  paste(" -", macro_vars_path),
  paste(" -", execution_summary_path),
  paste(" -", excel_output_path),
  paste(" -", report_docx_path),
  paste(" -", execution_log_path)
)

writeLines(enc2utf8(log_lines), execution_log_path, useBytes = TRUE)


# ------------------------------------------------------------
# 20. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 04 - Structure Federal Reserve Macro Scenarios completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input files:\n", 3, "\n")
cat("Scenarios structured:\n", n_distinct(scenarios_long$scenario), "\n")
cat("Macro variables detected:\n", length(candidate_macro_vars), "\n")
cat("Long rows:\n", nrow(scenarios_long), "\n")
cat("Wide rows:\n", nrow(scenarios_wide), "\n")
cat("Shock rows:\n", nrow(scenario_shocks), "\n\n")

cat("Input structure audit:\n")
print(input_structure_audit)

cat("\nScenario structure audit:\n")
print(scenario_structure_audit)

cat("\nProcessed outputs:\n")
cat(" -", long_path, "\n")
cat(" -", wide_path, "\n")
cat(" -", shocks_path, "\n")

cat("\nAudit outputs:\n")
cat(" -", variable_map_path, "\n")
cat(" -", shock_summary_path, "\n")
cat(" -", structure_audit_path, "\n")
cat(" -", input_audit_path, "\n")
cat(" -", macro_vars_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")