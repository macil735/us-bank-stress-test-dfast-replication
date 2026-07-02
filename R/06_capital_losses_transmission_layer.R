 # ============================================================
# USA Bank Stress Test DFAST Replication
# Script 06 — Capital & Losses Transmission Layer
# ============================================================
# Objective:
#   Build the core DFAST-style transmission layer linking:
#     - PPNR
#     - credit losses
#     - provisions
#     - pretax net income
#     - RWA
#     - capital ratios
#     - capital depletion
#
# Input:
#   data/processed/fed/fed_dfast_benchmark_wide.csv
#
# Outputs:
#   data/processed/model/dfast_capital_losses_transmission_panel.csv
#   data/processed/model/dfast_capital_losses_transmission_long.csv
#   outputs/transmission_layer/transmission_variable_map.csv
#   outputs/transmission_layer/transmission_bank_summary.csv
#   outputs/transmission_layer/transmission_scenario_summary.csv
#   outputs/transmission_layer/script06_transmission_layer_report.docx
#   outputs/transmission_layer/script06_execution_log.txt
#
# Methodological note:
#   This script does not estimate hidden Fed supervisory models.
#   It builds an observable accounting/regulatory transmission layer
#   from public DFAST results.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "06"
script_name <- "capital_losses_transmission_layer"
start_time <- Sys.time()

setwd(project_root)

dir.create("data/processed/model", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/transmission_layer", recursive = TRUE, showWarnings = FALSE)


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

first_existing <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

get_col_or_na <- function(df, col) {
  if (is.na(col) || !col %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[col]]))
}

safe_divide <- function(num, den) {
  ifelse(!is.na(den) & den != 0, num / den, NA_real_)
}

classify_transmission_variable <- function(x) {
  case_when(
    str_detect(x, "cet1|tier1|total_capital|leverage|capital_depletion") ~ "Capital and solvency",
    str_detect(x, "rwa") ~ "Risk-weighted assets",
    str_detect(x, "loss|provision") ~ "Losses and provisions",
    str_detect(x, "ppnr|revenue|income|expense|pretax") ~ "Income and PPNR",
    str_detect(x, "coverage|absorption|burden|margin") ~ "Transmission ratios",
    TRUE ~ "Other"
  )
}


# ------------------------------------------------------------
# 3. Input data
# ------------------------------------------------------------

input_path <- "data/processed/fed/fed_dfast_benchmark_wide.csv"

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
# 4. Identify source variables
# ------------------------------------------------------------

source_map <- tibble(
  model_variable = c(
    "cet1_actual_ratio",
    "cet1_end_ratio",
    "cet1_min_ratio",
    "tier1_actual_ratio",
    "tier1_end_ratio",
    "tier1_min_ratio",
    "total_capital_actual_ratio",
    "total_capital_end_ratio",
    "total_capital_min_ratio",
    "tier1_leverage_actual_ratio",
    "tier1_leverage_end_ratio",
    "tier1_leverage_min_ratio",
    "supp_leverage_actual_ratio",
    "supp_leverage_end_ratio",
    "supp_leverage_min_ratio",
    "rwa_actual_amount",
    "rwa_end_amount",
    "total_loan_losses_amount",
    "total_loan_losses_rate",
    "ppnr_amount",
    "ppnr_rate",
    "provision_amount",
    "pretax_net_income_amount",
    "pretax_net_income_rate",
    "net_interest_income_amount",
    "noninterest_income_amount",
    "noninterest_expense_amount",
    "other_revenue_amount",
    "securities_losses_amount",
    "trading_counterparty_losses_amount",
    "other_losses_amount",
    "aoci_actual_amount",
    "aoci_end_amount"
  ),
  source_column = c(
    first_existing(dfast, c("common_equity_tier1_actual_rat", "tier1_common_actual_rat")),
    first_existing(dfast, c("common_equity_tier1_end_rat", "tier1_common_end_rat")),
    first_existing(dfast, c("common_equity_tier1_min_rat", "tier1_common_min_rat")),

    first_existing(dfast, c("tier1_capital_actual_rat")),
    first_existing(dfast, c("tier1_capital_end_rat")),
    first_existing(dfast, c("tier1_capital_min_rat")),

    first_existing(dfast, c("total_capital_actual_rat")),
    first_existing(dfast, c("total_capital_end_rat")),
    first_existing(dfast, c("total_capital_min_rat")),

    first_existing(dfast, c("tier1_leverage_actual_rat")),
    first_existing(dfast, c("tier1_leverage_end_rat")),
    first_existing(dfast, c("tier1_leverage_min_rat")),

    first_existing(dfast, c("supp_leverage_actual_rat")),
    first_existing(dfast, c("supp_leverage_end_rat")),
    first_existing(dfast, c("supp_leverage_min_rat")),

    first_existing(dfast, c("rwa_standardized_appr_actual_amt", "rwa_general_appr_actual_amt")),
    first_existing(dfast, c("rwa_standardized_appr_end_amt", "rwa_general_appr_end_amt")),

    first_existing(dfast, c("loss_total_loan_amt")),
    first_existing(dfast, c("loss_total_loan_rate")),

    first_existing(dfast, c("revenue_preprovision_net_amt")),
    first_existing(dfast, c("revenue_preprovision_net_rate")),

    first_existing(dfast, c("provision_amt")),

    first_existing(dfast, c("income_pretax_net_amt")),
    first_existing(dfast, c("income_pretax_net_rate")),

    first_existing(dfast, c("income_net_interest_amt")),
    first_existing(dfast, c("income_noninterest_amt")),
    first_existing(dfast, c("expense_noninterest_amt")),
    first_existing(dfast, c("revenue_other_amt")),

    first_existing(dfast, c("loss_securities_amt")),
    first_existing(dfast, c("loss_trading_counterparty_amt")),
    first_existing(dfast, c("loss_other_amt")),

    first_existing(dfast, c("aoci_incl_capital_actual_amt")),
    first_existing(dfast, c("aoci_incl_capital_end_amt"))
  )
) |>
  mutate(
    source_available = !is.na(source_column),
    variable_block = classify_transmission_variable(model_variable)
  ) |>
  safe_df()


# ------------------------------------------------------------
# 5. Build transmission panel
# ------------------------------------------------------------

transmission_panel <- dfast |>
  transmute(
    bank_rssd_id = bank_rssd_id,
    bank_name = bank_name,
    exercise_year = exercise_year,
    exercise_quarter = exercise_quarter,
    exercise_name_std = exercise_name_std,
    scenario_code = scenario_code,
    scenario_label = scenario_label,

    cet1_actual_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "cet1_actual_ratio"]),
    cet1_end_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "cet1_end_ratio"]),
    cet1_min_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "cet1_min_ratio"]),

    tier1_actual_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "tier1_actual_ratio"]),
    tier1_end_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "tier1_end_ratio"]),
    tier1_min_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "tier1_min_ratio"]),

    total_capital_actual_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "total_capital_actual_ratio"]),
    total_capital_end_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "total_capital_end_ratio"]),
    total_capital_min_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "total_capital_min_ratio"]),

    tier1_leverage_actual_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "tier1_leverage_actual_ratio"]),
    tier1_leverage_end_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "tier1_leverage_end_ratio"]),
    tier1_leverage_min_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "tier1_leverage_min_ratio"]),

    supp_leverage_actual_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "supp_leverage_actual_ratio"]),
    supp_leverage_end_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "supp_leverage_end_ratio"]),
    supp_leverage_min_ratio = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "supp_leverage_min_ratio"]),

    rwa_actual_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "rwa_actual_amount"]),
    rwa_end_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "rwa_end_amount"]),

    total_loan_losses_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "total_loan_losses_amount"]),
    total_loan_losses_rate = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "total_loan_losses_rate"]),

    ppnr_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "ppnr_amount"]),
    ppnr_rate = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "ppnr_rate"]),

    provision_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "provision_amount"]),

    pretax_net_income_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "pretax_net_income_amount"]),
    pretax_net_income_rate = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "pretax_net_income_rate"]),

    net_interest_income_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "net_interest_income_amount"]),
    noninterest_income_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "noninterest_income_amount"]),
    noninterest_expense_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "noninterest_expense_amount"]),
    other_revenue_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "other_revenue_amount"]),

    securities_losses_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "securities_losses_amount"]),
    trading_counterparty_losses_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "trading_counterparty_losses_amount"]),
    other_losses_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "other_losses_amount"]),

    aoci_actual_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "aoci_actual_amount"]),
    aoci_end_amount = get_col_or_na(dfast, source_map$source_column[source_map$model_variable == "aoci_end_amount"])
  ) |>
  mutate(
    cet1_depletion_end = cet1_actual_ratio - cet1_end_ratio,
    cet1_depletion_min = cet1_actual_ratio - cet1_min_ratio,

    tier1_depletion_end = tier1_actual_ratio - tier1_end_ratio,
    tier1_depletion_min = tier1_actual_ratio - tier1_min_ratio,

    total_capital_depletion_end = total_capital_actual_ratio - total_capital_end_ratio,
    total_capital_depletion_min = total_capital_actual_ratio - total_capital_min_ratio,

    leverage_depletion_end = tier1_leverage_actual_ratio - tier1_leverage_end_ratio,
    leverage_depletion_min = tier1_leverage_actual_ratio - tier1_leverage_min_ratio,

    rwa_change_amount = rwa_end_amount - rwa_actual_amount,
    rwa_growth_rate = safe_divide(rwa_end_amount - rwa_actual_amount, rwa_actual_amount),

    total_market_operational_losses_amount =
      securities_losses_amount +
      trading_counterparty_losses_amount +
      other_losses_amount,

    total_observed_losses_amount =
      total_loan_losses_amount +
      securities_losses_amount +
      trading_counterparty_losses_amount +
      other_losses_amount,

    ppnr_loss_absorption_ratio =
      safe_divide(ppnr_amount, total_observed_losses_amount),

    provision_to_loan_loss_ratio =
      safe_divide(provision_amount, total_loan_losses_amount),

    pretax_income_to_ppnr_ratio =
      safe_divide(pretax_net_income_amount, ppnr_amount),

    loan_loss_burden_on_ppnr =
      safe_divide(total_loan_losses_amount, ppnr_amount),

    observed_loss_burden_on_ppnr =
      safe_divide(total_observed_losses_amount, ppnr_amount),

    aoci_change_amount = aoci_end_amount - aoci_actual_amount
  ) |>
  safe_df()


# ------------------------------------------------------------
# 6. Long format
# ------------------------------------------------------------

id_cols <- c(
  "bank_rssd_id",
  "bank_name",
  "exercise_year",
  "exercise_quarter",
  "exercise_name_std",
  "scenario_code",
  "scenario_label"
)

transmission_long <- transmission_panel |>
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "transmission_variable",
    values_to = "value"
  ) |>
  mutate(
    transmission_block = classify_transmission_variable(transmission_variable)
  ) |>
  arrange(
    exercise_year,
    bank_name,
    scenario_code,
    transmission_block,
    transmission_variable
  ) |>
  safe_df()


# ------------------------------------------------------------
# 7. Variable map
# ------------------------------------------------------------

transmission_variable_map <- transmission_long |>
  group_by(transmission_block, transmission_variable) |>
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
  arrange(transmission_block, transmission_variable) |>
  safe_df()


# ------------------------------------------------------------
# 8. Bank summary
# ------------------------------------------------------------

transmission_bank_summary <- transmission_panel |>
  group_by(bank_rssd_id, bank_name, exercise_year, scenario_code, scenario_label) |>
  summarise(
    cet1_actual_ratio = mean(cet1_actual_ratio, na.rm = TRUE),
    cet1_min_ratio = mean(cet1_min_ratio, na.rm = TRUE),
    cet1_depletion_min = mean(cet1_depletion_min, na.rm = TRUE),
    total_loan_losses_amount = mean(total_loan_losses_amount, na.rm = TRUE),
    ppnr_amount = mean(ppnr_amount, na.rm = TRUE),
    pretax_net_income_amount = mean(pretax_net_income_amount, na.rm = TRUE),
    ppnr_loss_absorption_ratio = mean(ppnr_loss_absorption_ratio, na.rm = TRUE),
    rwa_growth_rate = mean(rwa_growth_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  arrange(exercise_year, bank_name, scenario_code) |>
  safe_df()


# ------------------------------------------------------------
# 9. Scenario summary
# ------------------------------------------------------------

transmission_scenario_summary <- transmission_panel |>
  group_by(exercise_year, scenario_code, scenario_label) |>
  summarise(
    n_banks = n_distinct(bank_name),
    mean_cet1_actual_ratio = mean(cet1_actual_ratio, na.rm = TRUE),
    mean_cet1_min_ratio = mean(cet1_min_ratio, na.rm = TRUE),
    mean_cet1_depletion_min = mean(cet1_depletion_min, na.rm = TRUE),
    total_loan_losses_amount = sum(total_loan_losses_amount, na.rm = TRUE),
    total_ppnr_amount = sum(ppnr_amount, na.rm = TRUE),
    total_pretax_net_income_amount = sum(pretax_net_income_amount, na.rm = TRUE),
    mean_ppnr_loss_absorption_ratio = mean(ppnr_loss_absorption_ratio, na.rm = TRUE),
    mean_rwa_growth_rate = mean(rwa_growth_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  arrange(exercise_year, scenario_code) |>
  safe_df()


# ------------------------------------------------------------
# 10. Transmission identity audit
# ------------------------------------------------------------

identity_audit <- transmission_panel |>
  transmute(
    bank_rssd_id,
    bank_name,
    exercise_year,
    scenario_code,
    scenario_label,

    ppnr_amount,
    total_loan_losses_amount,
    securities_losses_amount,
    trading_counterparty_losses_amount,
    other_losses_amount,
    total_observed_losses_amount,
    provision_amount,
    pretax_net_income_amount,

    implied_pretax_from_components =
      ppnr_amount -
      provision_amount -
      securities_losses_amount -
      trading_counterparty_losses_amount -
      other_losses_amount,

    pretax_identity_gap =
      pretax_net_income_amount - implied_pretax_from_components
  ) |>
  safe_df()

identity_audit_summary <- identity_audit |>
  summarise(
    observations = n(),
    non_missing_identity_gaps = sum(!is.na(pretax_identity_gap)),
    mean_gap = mean(pretax_identity_gap, na.rm = TRUE),
    median_gap = median(pretax_identity_gap, na.rm = TRUE),
    max_abs_gap = max(abs(pretax_identity_gap), na.rm = TRUE)
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.infinite(.x), NA_real_, .x)
    )
  ) |>
  safe_df()


# ------------------------------------------------------------
# 11. Save processed datasets
# ------------------------------------------------------------

panel_path <- "data/processed/model/dfast_capital_losses_transmission_panel.csv"
long_path <- "data/processed/model/dfast_capital_losses_transmission_long.csv"

write_csv(transmission_panel, panel_path)
write_csv(transmission_long, long_path)


# ------------------------------------------------------------
# 12. Save audit outputs
# ------------------------------------------------------------

out_dir <- "outputs/transmission_layer"

source_map_path <- file.path(out_dir, "transmission_source_map.csv")
variable_map_path <- file.path(out_dir, "transmission_variable_map.csv")
bank_summary_path <- file.path(out_dir, "transmission_bank_summary.csv")
scenario_summary_path <- file.path(out_dir, "transmission_scenario_summary.csv")
identity_audit_path <- file.path(out_dir, "transmission_identity_audit.csv")
identity_summary_path <- file.path(out_dir, "transmission_identity_audit_summary.csv")
execution_summary_path <- file.path(out_dir, "script06_execution_summary.csv")
excel_output_path <- file.path(out_dir, "script06_transmission_layer_outputs.xlsx")
report_docx_path <- file.path(out_dir, "script06_transmission_layer_report.docx")
execution_log_path <- file.path(out_dir, "script06_execution_log.txt")

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_rows = nrow(dfast),
  transmission_panel_rows = nrow(transmission_panel),
  transmission_long_rows = nrow(transmission_long),
  banks = n_distinct(transmission_panel$bank_name),
  years = n_distinct(transmission_panel$exercise_year),
  scenarios = n_distinct(transmission_panel$scenario_label),
  source_variables_available = sum(source_map$source_available),
  source_variables_expected = nrow(source_map),
  derived_variables = ncol(transmission_panel) - length(id_cols) - sum(source_map$source_available)
) |>
  safe_df()

write_csv(source_map, source_map_path)
write_csv(transmission_variable_map, variable_map_path)
write_csv(transmission_bank_summary, bank_summary_path)
write_csv(transmission_scenario_summary, scenario_summary_path)
write_csv(identity_audit, identity_audit_path)
write_csv(identity_audit_summary, identity_summary_path)
write_csv(execution_summary, execution_summary_path)


# ------------------------------------------------------------
# 13. Excel workbook
# ------------------------------------------------------------

wb <- createWorkbook()

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", execution_summary)

addWorksheet(wb, "source_map")
writeData(wb, "source_map", source_map)

addWorksheet(wb, "variable_map")
writeData(wb, "variable_map", transmission_variable_map)

addWorksheet(wb, "bank_summary")
writeData(wb, "bank_summary", transmission_bank_summary)

addWorksheet(wb, "scenario_summary")
writeData(wb, "scenario_summary", transmission_scenario_summary)

addWorksheet(wb, "identity_audit_summary")
writeData(wb, "identity_audit_summary", identity_audit_summary)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_output_path, overwrite = TRUE)


# ------------------------------------------------------------
# 14. Word report
# ------------------------------------------------------------

doc <- read_docx()

doc <- doc |>
  body_add_par("Script 06 - Capital and Losses Transmission Layer", style = "heading 1") |>
  body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  body_add_par("1. Objective", style = "heading 2") |>
  body_add_par(
    "This script builds the core observable transmission layer linking PPNR, losses, provisions, pretax income, RWA and capital ratios using public Federal Reserve DFAST benchmark data.",
    style = "Normal"
  ) |>
  body_add_par("2. Execution summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(execution_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("3. Source variable map", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(source_map) |>
    autofit()
)

doc <- doc |>
  body_add_par("4. Scenario summary", style = "heading 2")

scenario_summary_small <- transmission_scenario_summary |>
  head(25)

doc <- body_add_flextable(
  doc,
  flextable(scenario_summary_small) |>
    autofit()
)

doc <- doc |>
  body_add_par("5. Identity audit summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(identity_audit_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("6. Methodological note", style = "heading 2") |>
  body_add_par(
    "The layer is based on observable public DFAST fields. It does not reproduce confidential Federal Reserve models. Some identity gaps may arise because the public data aggregate supervisory concepts that are not fully decomposed in the CSV file.",
    style = "Normal"
  )

print(doc, target = report_docx_path)


# ------------------------------------------------------------
# 15. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 06 - Capital and Losses Transmission Layer completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input rows:", nrow(dfast)),
  paste("Transmission panel rows:", nrow(transmission_panel)),
  paste("Transmission long rows:", nrow(transmission_long)),
  paste("Banks:", n_distinct(transmission_panel$bank_name)),
  paste("Years:", n_distinct(transmission_panel$exercise_year)),
  paste("Scenarios:", n_distinct(transmission_panel$scenario_label)),
  paste("Source variables available:", sum(source_map$source_available)),
  paste("Source variables expected:", nrow(source_map)),
  "",
  "Identity audit summary:",
  capture.output(print(identity_audit_summary)),
  "",
  "Processed outputs:",
  paste(" -", panel_path),
  paste(" -", long_path),
  "",
  "Audit outputs:",
  paste(" -", source_map_path),
  paste(" -", variable_map_path),
  paste(" -", bank_summary_path),
  paste(" -", scenario_summary_path),
  paste(" -", identity_audit_path),
  paste(" -", identity_summary_path),
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
cat("Script 06 - Capital and Losses Transmission Layer completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input rows:\n", nrow(dfast), "\n")
cat("Transmission panel rows:\n", nrow(transmission_panel), "\n")
cat("Transmission long rows:\n", nrow(transmission_long), "\n")
cat("Banks:\n", n_distinct(transmission_panel$bank_name), "\n")
cat("Years:\n", n_distinct(transmission_panel$exercise_year), "\n")
cat("Scenarios:\n", n_distinct(transmission_panel$scenario_label), "\n")
cat("Source variables available:\n", sum(source_map$source_available), "\n")
cat("Source variables expected:\n", nrow(source_map), "\n\n")

cat("Identity audit summary:\n")
print(identity_audit_summary)

cat("\nProcessed outputs:\n")
cat(" -", panel_path, "\n")
cat(" -", long_path, "\n")

cat("\nAudit outputs:\n")
cat(" -", source_map_path, "\n")
cat(" -", variable_map_path, "\n")
cat(" -", bank_summary_path, "\n")
cat(" -", scenario_summary_path, "\n")
cat(" -", identity_audit_path, "\n")
cat(" -", identity_summary_path, "\n")
cat(" -", execution_summary_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")