# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 05 — Build DFAST Benchmark Dataset
# ============================================================
# Objective:
#   Build a structured benchmark dataset from the Federal Reserve
#   public DFAST results.
#
# Input:
#   data/processed/fed/fed_dfast_results_clean.csv
#
# Key identifiers in the Fed file:
#   id_rssd
#   disclosure_legal_name
#   exercise_name
#   dt_exercise_quarter
#   scenario_id
#   scenario_name
#
# Outputs:
#   data/processed/fed/fed_dfast_benchmark_long.csv
#   data/processed/fed/fed_dfast_benchmark_wide.csv
#   outputs/benchmarking/dfast_benchmark_variable_map.csv
#   outputs/benchmarking/dfast_benchmark_bank_summary.csv
#   outputs/benchmarking/dfast_benchmark_year_summary.csv
#   outputs/benchmarking/script05_benchmark_report.docx
#   outputs/benchmarking/script05_execution_log.txt
#
# Methodological note:
#   This script does not estimate a stress test model.
#   It structures the official Fed DFAST results as the benchmark
#   layer for later comparison.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "05"
script_name <- "build_dfast_benchmark_dataset"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/fed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/benchmarking", recursive = TRUE, showWarnings = FALSE)


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

make_column_audit <- function(df) {
  tibble(
    column_name = names(df),
    column_class = map_chr(df, ~ paste(class(.x), collapse = "|")),
    n_obs = nrow(df),
    n_missing = map_int(df, ~ sum(is.na(.x))),
    missing_share = map_dbl(df, ~ mean(is.na(.x))),
    n_unique = map_int(df, ~ dplyr::n_distinct(.x, na.rm = TRUE)),
    sample_values = map_chr(
      df,
      ~ paste(head(unique(as.character(.x[!is.na(.x)])), 5), collapse = " | ")
    )
  ) |>
    safe_df()
}

derive_year_from_text <- function(x) {
  out <- stringr::str_extract(as.character(x), "\\d{4}")
  suppressWarnings(as.integer(out))
}

classify_metric_block <- function(metric) {
  case_when(
    str_detect(metric, "cet1|common_equity|tier1_common|tier_1_common") ~ "Capital and solvency",
    str_detect(metric, "tier1|tier_1|total_capital|capital_rat|leverage|supp_leverage") ~ "Capital and solvency",
    str_detect(metric, "loss|provision|charge_off") ~ "Losses and provisions",
    str_detect(metric, "ppnr|preprovision|pre_provision|revenue|income|expense|pretax") ~ "Income and PPNR",
    str_detect(metric, "rwa|risk_weighted") ~ "Risk-weighted assets",
    str_detect(metric, "asset|loan|balance|exposure") ~ "Balance sheet",
    str_detect(metric, "aoci|comprehensive") ~ "Other comprehensive income",
    TRUE ~ "Other"
  )
}

metric_unit_type <- function(metric) {
  case_when(
    str_detect(metric, "_rat$|_rate$") ~ "ratio_or_rate",
    str_detect(metric, "_amt$") ~ "amount",
    TRUE ~ "other"
  )
}


# ------------------------------------------------------------
# 3. Read input
# ------------------------------------------------------------

input_path <- "data/processed/fed/fed_dfast_results_clean.csv"

if (!file.exists(input_path)) {
  stop(paste("Missing input file:", input_path))
}

dfast <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  guess_max = 100000
) |>
  janitor::clean_names() |>
  safe_df()


# ------------------------------------------------------------
# 4. Audit columns
# ------------------------------------------------------------

column_audit <- make_column_audit(dfast)

column_audit_path <- "outputs/benchmarking/dfast_benchmark_column_audit.csv"
write_csv(column_audit, column_audit_path)


# ------------------------------------------------------------
# 5. Validate required Fed columns
# ------------------------------------------------------------

required_cols <- c(
  "id_rssd",
  "disclosure_legal_name",
  "exercise_name",
  "dt_exercise_quarter",
  "scenario_id",
  "scenario_name"
)

missing_required_cols <- setdiff(required_cols, names(dfast))

required_column_check <- tibble(
  required_column = required_cols,
  exists = required_cols %in% names(dfast)
)

required_column_check_path <- "outputs/benchmarking/script05_required_column_check.csv"
write_csv(required_column_check, required_column_check_path)

if (length(missing_required_cols) > 0) {
  stop(
    paste(
      "Missing required Fed identifier columns:",
      paste(missing_required_cols, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# 6. Create standardized identifiers
# ------------------------------------------------------------

dfast_std <- dfast |>
  mutate(
    bank_rssd_id = as.character(id_rssd),
    bank_name = disclosure_legal_name,
    exercise_name_std = exercise_name,
    exercise_quarter = as.character(dt_exercise_quarter),
    exercise_year = derive_year_from_text(dt_exercise_quarter),
    scenario_code = as.character(scenario_id),
    scenario_label = scenario_name
  )

# Fallback: if year was not extracted from dt_exercise_quarter,
# try exercise_name.
if (all(is.na(dfast_std$exercise_year))) {
  dfast_std <- dfast_std |>
    mutate(
      exercise_year = derive_year_from_text(exercise_name_std)
    )
}

dfast_std <- dfast_std |>
  mutate(
    bank_rssd_id = safe_chr(bank_rssd_id),
    bank_name = safe_chr(bank_name),
    exercise_name_std = safe_chr(exercise_name_std),
    exercise_quarter = safe_chr(exercise_quarter),
    scenario_code = safe_chr(scenario_code),
    scenario_label = safe_chr(scenario_label)
  )


# ------------------------------------------------------------
# 7. Identify benchmark metrics
# ------------------------------------------------------------

metadata_cols <- c(
  "source_file",
  "dataset_role",
  "scenario_type",
  "cleaned_at",
  "exercise_name",
  "dt_exercise_quarter",
  "id_rssd",
  "disclosure_legal_name",
  "scenario_id",
  "scenario_name",
  "bank_rssd_id",
  "bank_name",
  "exercise_name_std",
  "exercise_quarter",
  "exercise_year",
  "scenario_code",
  "scenario_label"
)

metadata_cols <- metadata_cols[metadata_cols %in% names(dfast_std)]

numeric_cols <- names(dfast_std)[map_lgl(dfast_std, is.numeric)]

benchmark_vars <- setdiff(
  numeric_cols,
  c(metadata_cols, "id_rssd", "scenario_id", "exercise_year")
)

if (length(benchmark_vars) == 0) {
  stop("No numeric DFAST benchmark metrics found.")
}

detected_metrics <- tibble(
  metric = benchmark_vars,
  metric_block = classify_metric_block(benchmark_vars),
  unit_type = metric_unit_type(benchmark_vars)
) |>
  arrange(metric_block, unit_type, metric) |>
  safe_df()


# ------------------------------------------------------------
# 8. Build long benchmark dataset
# ------------------------------------------------------------

dfast_long <- dfast_std |>
  select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    exercise_quarter,
    exercise_name_std,
    scenario_code,
    scenario_label,
    all_of(benchmark_vars)
  ) |>
  pivot_longer(
    cols = all_of(benchmark_vars),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric_block = classify_metric_block(metric),
    unit_type = metric_unit_type(metric),
    value = as.numeric(value)
  ) |>
  arrange(
    exercise_year,
    bank_name,
    scenario_code,
    metric_block,
    metric
  ) |>
  safe_df()


# ------------------------------------------------------------
# 9. Build wide benchmark dataset
# ------------------------------------------------------------

dfast_wide <- dfast_long |>
  select(
    bank_rssd_id,
    bank_name,
    exercise_year,
    exercise_quarter,
    exercise_name_std,
    scenario_code,
    scenario_label,
    metric,
    value
  ) |>
  pivot_wider(
    names_from = metric,
    values_from = value,
    values_fn = list(value = mean),
    values_fill = NA_real_
  ) |>
  arrange(
    exercise_year,
    bank_name,
    scenario_code
  ) |>
  safe_df()


# ------------------------------------------------------------
# 10. Bank-level summary
# ------------------------------------------------------------

bank_summary <- dfast_long |>
  group_by(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label
  ) |>
  summarise(
    n_metrics = n_distinct(metric),
    n_non_missing_values = sum(!is.na(value)),
    n_missing_values = sum(is.na(value)),
    missing_share = mean(is.na(value)),
    .groups = "drop"
  ) |>
  arrange(exercise_year, bank_name, scenario_code) |>
  safe_df()


# ------------------------------------------------------------
# 11. Variable map
# ------------------------------------------------------------

variable_map <- dfast_long |>
  group_by(metric_block, unit_type, metric) |>
  summarise(
    observations = n(),
    non_missing_observations = sum(!is.na(value)),
    missing_share = mean(is.na(value)),
    n_banks = n_distinct(bank_name),
    n_years = n_distinct(exercise_year),
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(mean_value, sd_value, min_value, max_value),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  arrange(metric_block, unit_type, metric) |>
  safe_df()


# ------------------------------------------------------------
# 12. Year and scenario summary
# ------------------------------------------------------------

year_summary <- dfast_long |>
  group_by(exercise_year, scenario_code, scenario_label, metric_block) |>
  summarise(
    n_banks = n_distinct(bank_name),
    n_metrics = n_distinct(metric),
    n_observations = n(),
    missing_share = mean(is.na(value)),
    .groups = "drop"
  ) |>
  arrange(exercise_year, scenario_code, metric_block) |>
  safe_df()


# ------------------------------------------------------------
# 13. Metric block summary
# ------------------------------------------------------------

metric_block_summary <- detected_metrics |>
  count(metric_block, unit_type, name = "number_of_metrics") |>
  arrange(metric_block, unit_type) |>
  safe_df()


# ------------------------------------------------------------
# 14. Save processed datasets
# ------------------------------------------------------------

long_path <- "data/processed/fed/fed_dfast_benchmark_long.csv"
wide_path <- "data/processed/fed/fed_dfast_benchmark_wide.csv"

write_csv(dfast_long, long_path)
write_csv(dfast_wide, wide_path)


# ------------------------------------------------------------
# 15. Save audit outputs
# ------------------------------------------------------------

out_dir <- "outputs/benchmarking"

detected_metrics_path <- file.path(out_dir, "dfast_benchmark_detected_metrics.csv")
variable_map_path <- file.path(out_dir, "dfast_benchmark_variable_map.csv")
bank_summary_path <- file.path(out_dir, "dfast_benchmark_bank_summary.csv")
year_summary_path <- file.path(out_dir, "dfast_benchmark_year_summary.csv")
metric_block_summary_path <- file.path(out_dir, "dfast_benchmark_metric_block_summary.csv")
execution_summary_path <- file.path(out_dir, "script05_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script05_benchmark_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script05_benchmark_report.docx")
execution_log_path <- file.path(out_dir, "script05_execution_log.txt")

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_rows = nrow(dfast),
  input_columns = ncol(dfast),
  banks = n_distinct(dfast_long$bank_name),
  bank_rssd_ids = n_distinct(dfast_long$bank_rssd_id),
  years = n_distinct(dfast_long$exercise_year),
  scenarios = n_distinct(dfast_long$scenario_label),
  benchmark_metrics = length(benchmark_vars),
  long_rows = nrow(dfast_long),
  wide_rows = nrow(dfast_wide)
) |>
  safe_df()

write_csv(detected_metrics, detected_metrics_path)
write_csv(variable_map, variable_map_path)
write_csv(bank_summary, bank_summary_path)
write_csv(year_summary, year_summary_path)
write_csv(metric_block_summary, metric_block_summary_path)
write_csv(execution_summary, execution_summary_path)


# ------------------------------------------------------------
# 16. Excel workbook
# ------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", execution_summary)

addWorksheet(wb, "required_column_check")
writeData(wb, "required_column_check", required_column_check)

addWorksheet(wb, "column_audit")
writeData(wb, "column_audit", column_audit)

addWorksheet(wb, "detected_metrics")
writeData(wb, "detected_metrics", detected_metrics)

addWorksheet(wb, "metric_block_summary")
writeData(wb, "metric_block_summary", metric_block_summary)

addWorksheet(wb, "variable_map")
writeData(wb, "variable_map", variable_map)

addWorksheet(wb, "bank_summary")
writeData(wb, "bank_summary", bank_summary)

addWorksheet(wb, "year_summary")
writeData(wb, "year_summary", year_summary)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_output_path, overwrite = TRUE)


# ------------------------------------------------------------
# 17. Word report
# ------------------------------------------------------------

doc <- read_docx()

doc <- doc |>
  body_add_par("Script 05 - Build DFAST Benchmark Dataset", style = "heading 1") |>
  body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  body_add_par("1. Objective", style = "heading 2") |>
  body_add_par(
    "This script builds a structured benchmark dataset from the Federal Reserve public DFAST results. The benchmark will later be used to compare the public replication outputs with official published stress test results.",
    style = "Normal"
  ) |>
  body_add_par("2. Execution summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(execution_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("3. Required column check", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(required_column_check) |>
    autofit()
)

doc <- doc |>
  body_add_par("4. Metric block summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(metric_block_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("5. Year and scenario summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(year_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("6. Methodological note", style = "heading 2") |>
  body_add_par(
    "This script does not estimate a stress test model. It prepares the official Federal Reserve DFAST results as a benchmark layer for later comparison with the public replication outputs.",
    style = "Normal"
  )

print(doc, target = report_docx_path)


# ------------------------------------------------------------
# 18. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 05 - Build DFAST Benchmark Dataset completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input rows:", nrow(dfast)),
  paste("Input columns:", ncol(dfast)),
  paste("Banks:", n_distinct(dfast_long$bank_name)),
  paste("RSSD IDs:", n_distinct(dfast_long$bank_rssd_id)),
  paste("Years:", n_distinct(dfast_long$exercise_year)),
  paste("Scenarios:", n_distinct(dfast_long$scenario_label)),
  paste("Benchmark metrics:", length(benchmark_vars)),
  paste("Long rows:", nrow(dfast_long)),
  paste("Wide rows:", nrow(dfast_wide)),
  "",
  "Metric block summary:",
  capture.output(print(metric_block_summary)),
  "",
  "Processed outputs:",
  paste(" -", long_path),
  paste(" -", wide_path),
  "",
  "Audit outputs:",
  paste(" -", column_audit_path),
  paste(" -", required_column_check_path),
  paste(" -", detected_metrics_path),
  paste(" -", variable_map_path),
  paste(" -", bank_summary_path),
  paste(" -", year_summary_path),
  paste(" -", metric_block_summary_path),
  paste(" -", execution_summary_path),
  paste(" -", excel_output_path),
  paste(" -", report_docx_path),
  paste(" -", execution_log_path)
)

writeLines(enc2utf8(log_lines), execution_log_path, useBytes = TRUE)


# ------------------------------------------------------------
# 19. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 05 - Build DFAST Benchmark Dataset completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input rows:\n", nrow(dfast), "\n")
cat("Input columns:\n", ncol(dfast), "\n")
cat("Banks:\n", n_distinct(dfast_long$bank_name), "\n")
cat("RSSD IDs:\n", n_distinct(dfast_long$bank_rssd_id), "\n")
cat("Years:\n", n_distinct(dfast_long$exercise_year), "\n")
cat("Scenarios:\n", n_distinct(dfast_long$scenario_label), "\n")
cat("Benchmark metrics:\n", length(benchmark_vars), "\n")
cat("Long rows:\n", nrow(dfast_long), "\n")
cat("Wide rows:\n", nrow(dfast_wide), "\n\n")

cat("Metric block summary:\n")
print(metric_block_summary)

cat("\nProcessed outputs:\n")
cat(" -", long_path, "\n")
cat(" -", wide_path, "\n")

cat("\nAudit outputs:\n")
cat(" -", column_audit_path, "\n")
cat(" -", required_column_check_path, "\n")
cat(" -", detected_metrics_path, "\n")
cat(" -", variable_map_path, "\n")
cat(" -", bank_summary_path, "\n")
cat(" -", year_summary_path, "\n")
cat(" -", metric_block_summary_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")