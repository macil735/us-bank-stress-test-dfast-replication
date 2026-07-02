# ============================================================
# US Bank Stress Test Replication and DFAST Benchmarking
# Script 01 — Data Availability Audit
#
# Real file name:
#   R/01_data_availability_audit.R
#
# Project root:
#   D:/GitHub/us-bank-stress-test-dfast-replication
#
# Purpose:
#   Create the initial project structure, define a preliminary
#   data-source inventory, assess likely data availability, and
#   produce CSV and Word outputs for the first audit stage.
# ============================================================


# ============================================================
# 0. Initial settings
# ============================================================

rm(list = ls())

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  encoding = "UTF-8"
)

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

dirs <- c(
  "R",
  "data/raw",
  "data/raw/dfast",
  "data/raw/fdic",
  "data/raw/ffiec",
  "data/raw/fr_y9c",
  "data/raw/fred",
  "data/processed",
  "outputs/data_audit",
  "outputs/report_ready",
  "outputs/tables",
  "outputs/figures",
  "outputs/diagnostics",
  "docs"
)

dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

for (d in dirs) {
  dir.create(file.path(project_root, d), recursive = TRUE, showWarnings = FALSE)
}

audit_dir <- file.path(project_root, "outputs/data_audit")


# ============================================================
# 1. Package checks
# ============================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "officer",
  "flextable"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  message("Missing packages: ", paste(missing_packages, collapse = ", "))
  message("Install with:")
  message("install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))")
}

core_packages <- c("dplyr", "tidyr", "readr", "stringr")

missing_core <- core_packages[
  !vapply(core_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_core) > 0) {
  stop(
    paste0(
      "Core packages missing: ",
      paste(missing_core, collapse = ", "),
      ". Install them before running Script 01."
    )
  )
}

library(dplyr)
library(tidyr)
library(readr)
library(stringr)


# ============================================================
# 2. Helper functions
# ============================================================

safe_write_csv <- function(x, path) {
  readr::write_csv(x, path, na = "")
}

classify_availability <- function(access_mode, expected_standardization, expected_manual_burden) {
  dplyr::case_when(
    access_mode %in% c("Bulk download", "API", "Direct CSV/XLSX") &
      expected_standardization %in% c("High", "Very high") &
      expected_manual_burden %in% c("Low", "Very low") ~ "High",
    access_mode %in% c("Bulk download", "API", "Direct CSV/XLSX") ~ "Moderate",
    expected_manual_burden == "High" ~ "Low",
    TRUE ~ "Review required"
  )
}


# ============================================================
# 3. Project metadata
# ============================================================

project_metadata <- data.frame(
  field = c(
    "project_title",
    "project_short_name",
    "project_root",
    "country",
    "banking_system",
    "main_objective",
    "script_number",
    "script_name",
    "script_file"
  ),
  value = c(
    "US Bank Stress Test Replication and DFAST Benchmarking",
    "us-bank-stress-test-dfast-replication",
    project_root,
    "United States",
    "Large U.S. banking organizations",
    "Build a replicable data infrastructure for DFAST benchmarking and bank-level stress-test replication.",
    "01",
    "Data Availability Audit",
    "R/01_data_availability_audit.R"
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 4. Initial bank universe
# ============================================================

bank_universe <- data.frame(
  bank_short_name = c(
    "JPMorgan Chase",
    "Bank of America",
    "Citigroup",
    "Wells Fargo",
    "Goldman Sachs",
    "Morgan Stanley",
    "U.S. Bancorp",
    "PNC",
    "Truist",
    "Capital One"
  ),
  holding_company_name = c(
    "JPMorgan Chase & Co.",
    "Bank of America Corporation",
    "Citigroup Inc.",
    "Wells Fargo & Company",
    "The Goldman Sachs Group, Inc.",
    "Morgan Stanley",
    "U.S. Bancorp",
    "The PNC Financial Services Group, Inc.",
    "Truist Financial Corporation",
    "Capital One Financial Corporation"
  ),
  initial_priority = c(
    "Immediate", "Immediate", "Immediate", "Immediate",
    "High", "High", "High", "High", "High", "High"
  ),
  rationale = c(
    "Largest U.S. bank; strong DFAST relevance; already familiar from previous project.",
    "Large diversified bank; strong DFAST and regulatory data availability.",
    "Large global systemically important bank; strong DFAST relevance.",
    "Large retail and commercial bank; relevant for credit-loss stress.",
    "Capital markets and trading exposure; useful for market-risk comparison.",
    "Capital markets and wealth management exposure; useful for PPNR comparison.",
    "Large regional bank; useful benchmark against G-SIBs.",
    "Large regional bank; relevant for commercial credit stress.",
    "Large regional bank; useful post-merger case.",
    "Consumer-credit bank; useful for credit-card and loan-loss stress."
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 5. Source catalog
# ============================================================

source_catalog <- data.frame(
  source_id = c(
    "FED_DFAST",
    "FED_SUPERVISORY_SCENARIOS",
    "FDIC_SDI",
    "FDIC_CALL_REPORTS",
    "FFIEC_CDR",
    "FR_Y9C",
    "FRED_MACRO",
    "SEC_10K",
    "BANK_ANNUAL_REPORTS"
  ),
  source_name = c(
    "Federal Reserve DFAST / Stress Test Results",
    "Federal Reserve Supervisory Scenarios",
    "FDIC Statistics on Depository Institutions",
    "FDIC Call Reports",
    "FFIEC Central Data Repository",
    "Federal Reserve FR Y-9C Reports",
    "Federal Reserve Economic Data",
    "SEC Form 10-K filings",
    "Bank annual reports"
  ),
  source_owner = c(
    "Federal Reserve",
    "Federal Reserve",
    "Federal Deposit Insurance Corporation",
    "Federal Deposit Insurance Corporation",
    "Federal Financial Institutions Examination Council",
    "Federal Reserve",
    "Federal Reserve Bank of St. Louis",
    "U.S. Securities and Exchange Commission",
    "Individual banks"
  ),
  likely_url = c(
    "https://www.federalreserve.gov/supervisionreg/dfa-stress-tests.htm",
    "https://www.federalreserve.gov/supervisionreg/stress-tests-capital-planning.htm",
    "https://www.fdic.gov/bank-data-guide/statistics-depository-institutions",
    "https://www.fdic.gov/bank-data-guide/data-downloads",
    "https://cdr.ffiec.gov/",
    "https://www.federalreserve.gov/apps/reportingforms/Report/Index/FR_Y-9C",
    "https://fred.stlouisfed.org/",
    "https://www.sec.gov/edgar/search/",
    "Bank investor relations websites"
  ),
  access_mode = c(
    "Direct CSV/XLSX",
    "PDF/XLSX",
    "Direct CSV/XLSX",
    "Bulk download",
    "Bulk download",
    "Bulk download",
    "API",
    "HTML/XBRL",
    "PDF"
  ),
  expected_frequency = c(
    "Annual",
    "Annual",
    "Quarterly",
    "Quarterly",
    "Quarterly",
    "Quarterly",
    "Monthly/Quarterly",
    "Annual/Quarterly",
    "Annual/Quarterly"
  ),
  expected_standardization = c(
    "High",
    "High",
    "Very high",
    "Very high",
    "Very high",
    "Very high",
    "Very high",
    "Moderate",
    "Moderate"
  ),
  expected_manual_burden = c(
    "Low",
    "Low",
    "Very low",
    "Low",
    "Low",
    "Low",
    "Very low",
    "Moderate",
    "High"
  ),
  main_use = c(
    "Benchmark stress-test outputs: CET1, losses, PPNR and capital actions.",
    "Macroeconomic and financial scenario variables.",
    "Bank financial ratios, balance-sheet and income-statement indicators.",
    "Detailed bank regulatory data.",
    "Official call-report retrieval and metadata.",
    "Holding-company consolidated regulatory data.",
    "Macroeconomic variables and interest-rate paths.",
    "Validation against bank disclosures.",
    "Narrative validation and supplementary disclosures."
  ),
  stringsAsFactors = FALSE
) %>%
  mutate(
    availability_class = classify_availability(
      access_mode,
      expected_standardization,
      expected_manual_burden
    )
  )


# ============================================================
# 6. Preliminary variable inventory
# ============================================================

variable_inventory <- data.frame(
  variable_name = c(
    "total_assets",
    "total_loans",
    "commercial_loans",
    "consumer_loans",
    "credit_card_loans",
    "residential_mortgages",
    "allowance_credit_losses",
    "nonperforming_loans",
    "net_charge_offs",
    "loan_loss_provisions",
    "net_interest_income",
    "noninterest_income",
    "noninterest_expense",
    "pre_provision_net_revenue",
    "net_income",
    "cet1_capital",
    "tier1_capital",
    "total_risk_based_capital",
    "risk_weighted_assets",
    "cet1_ratio",
    "tier1_ratio",
    "total_capital_ratio",
    "leverage_ratio",
    "deposits",
    "wholesale_funding",
    "liquid_assets",
    "treasury_securities",
    "agency_mbs",
    "trading_assets",
    "dFAST_projected_losses",
    "dFAST_projected_ppnr",
    "dFAST_minimum_cet1_ratio",
    "gdp_growth",
    "unemployment_rate",
    "cpi_inflation",
    "fed_funds_rate",
    "treasury_10y_rate",
    "house_price_index",
    "corporate_bond_spread",
    "stock_market_index"
  ),
  model_block = c(
    rep("Balance sheet", 9),
    rep("Income and losses", 6),
    rep("Capital and solvency", 8),
    rep("Funding and liquidity", 6),
    rep("DFAST benchmark", 3),
    rep("Macro scenario", 8)
  ),
  preferred_source = c(
    rep("FDIC/FFIEC/FR_Y9C", 29),
    rep("FED_DFAST", 3),
    rep("FRED / Federal Reserve scenarios", 8)
  ),
  expected_frequency = c(
    rep("Quarterly", 29),
    rep("Annual", 3),
    rep("Quarterly/Annual scenario path", 8)
  ),
  stress_test_role = c(
    "Scale variable for bank size.",
    "Credit exposure base.",
    "Commercial credit-loss channel.",
    "Consumer credit-loss channel.",
    "Credit-card loss channel.",
    "Mortgage credit-loss channel.",
    "Loss absorption and credit-quality indicator.",
    "Credit-quality deterioration indicator.",
    "Realized credit-loss flow.",
    "Provisioning and expected-loss flow.",
    "Core earnings before provisions.",
    "Revenue diversification and PPNR component.",
    "Operating-cost and efficiency channel.",
    "Pre-provision earnings stress buffer.",
    "Bottom-line profitability.",
    "Core regulatory capital stock.",
    "Tier 1 capital stock.",
    "Total regulatory capital stock.",
    "Capital denominator.",
    "Primary solvency stress metric.",
    "Secondary solvency metric.",
    "Total capital adequacy metric.",
    "Balance-sheet leverage metric.",
    "Deposit funding base.",
    "Market funding sensitivity.",
    "Liquidity buffer.",
    "Sovereign securities exposure.",
    "Agency mortgage-backed securities exposure.",
    "Trading and market-risk exposure.",
    "Supervisory benchmark for losses.",
    "Supervisory benchmark for PPNR.",
    "Supervisory benchmark for solvency.",
    "Macro scenario driver.",
    "Credit-loss and household stress driver.",
    "Inflation and rate-stress driver.",
    "Short-rate scenario driver.",
    "Long-rate and securities valuation driver.",
    "Mortgage and collateral-value driver.",
    "Credit-spread and market-stress driver.",
    "Equity-market and market-risk driver."
  ),
  inclusion_status = c(
    rep("Core", 32),
    rep("Core", 8)
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 7. Source-variable mapping
# ============================================================

source_variable_map <- variable_inventory %>%
  mutate(
    likely_automation = case_when(
      preferred_source %in% c("FDIC/FFIEC/FR_Y9C", "FED_DFAST", "FRED / Federal Reserve scenarios") ~ "Automatable",
      TRUE ~ "Review required"
    ),
    expected_gap_risk = case_when(
      preferred_source == "FDIC/FFIEC/FR_Y9C" ~ "Low",
      preferred_source == "FED_DFAST" ~ "Low to moderate",
      preferred_source == "FRED / Federal Reserve scenarios" ~ "Low",
      TRUE ~ "Moderate"
    ),
    first_action = case_when(
      preferred_source == "FDIC/FFIEC/FR_Y9C" ~
        "Map bank identifiers and download regulatory data.",
      preferred_source == "FED_DFAST" ~
        "Download DFAST result files and build benchmark dictionary.",
      preferred_source == "FRED / Federal Reserve scenarios" ~
        "Download macro variables and scenario paths.",
      TRUE ~
        "Review source manually."
    )
  )


# ============================================================
# 8. Data availability assessment
# ============================================================

availability_assessment <- source_variable_map %>%
  group_by(model_block, preferred_source, likely_automation, expected_gap_risk) %>%
  summarise(
    number_of_variables = n(),
    core_variables = sum(inclusion_status == "Core"),
    .groups = "drop"
  ) %>%
  arrange(model_block, preferred_source)


# ============================================================
# 9. Initial script roadmap
# ============================================================

script_roadmap <- data.frame(
  script_number = sprintf("%02d", 1:13),
  script_name = c(
    "Data Availability Audit",
    "DFAST Results Download and Dictionary",
    "FDIC / FFIEC Bank Data Layer",
    "FR Y-9C Holding Company Data Layer",
    "Bank Identifier Crosswalk",
    "Build Regulatory Panel Dataset",
    "Capital Ratio Module",
    "Credit Loss Module",
    "PPNR Module",
    "Scenario and Shock Construction",
    "DFAST Benchmark Comparison",
    "Multi-Bank Stress Dashboard",
    "Final Technical Report"
  ),
  real_file_name = c(
    "R/01_data_availability_audit.R",
    "R/02_dfast_results_download_dictionary.R",
    "R/03_fdic_ffiec_bank_data_layer.R",
    "R/04_fr_y9c_holding_company_data_layer.R",
    "R/05_bank_identifier_crosswalk.R",
    "R/06_build_regulatory_panel_dataset.R",
    "R/07_capital_ratio_module.R",
    "R/08_credit_loss_module.R",
    "R/09_ppnr_module.R",
    "R/10_scenario_shock_construction.R",
    "R/11_dfast_benchmark_comparison.R",
    "R/12_multi_bank_stress_dashboard.R",
    "R/13_final_technical_report.R"
  ),
  purpose = c(
    "Audit sources, variables and project feasibility.",
    "Download and standardize Federal Reserve DFAST results.",
    "Download institution-level FDIC/FFIEC data.",
    "Download holding-company FR Y-9C data.",
    "Map bank names, RSSD IDs, certificates and holding-company identifiers.",
    "Construct a clean quarterly panel dataset.",
    "Compute capital ratios and capital depletion paths.",
    "Estimate and benchmark credit losses.",
    "Estimate pre-provision net revenue under stress.",
    "Build baseline, adverse and severe scenario paths.",
    "Compare model outputs against DFAST benchmarks.",
    "Produce tables and figures for multi-bank comparison.",
    "Generate final institutional report."
  ),
  expected_dependency = c(
    "None",
    "Script 01",
    "Script 01",
    "Script 01",
    "Scripts 02–04",
    "Scripts 02–05",
    "Script 06",
    "Script 06",
    "Script 06",
    "Scripts 02 and 06",
    "Scripts 07–10",
    "Script 11",
    "Script 12"
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 10. Project structure table
# ============================================================

project_structure <- data.frame(
  folder = dirs,
  role = c(
    "R scripts.",
    "Raw data parent folder.",
    "Federal Reserve DFAST data.",
    "FDIC SDI and related files.",
    "FFIEC Call Report data.",
    "Federal Reserve FR Y-9C files.",
    "FRED macro and financial data.",
    "Processed analytical datasets.",
    "Data audit outputs.",
    "Tables prepared for reports.",
    "Final tables.",
    "Figures and charts.",
    "Diagnostics and logs.",
    "Documentation and final reports."
  ),
  created = file.exists(file.path(project_root, dirs)),
  stringsAsFactors = FALSE
)


# ============================================================
# 11. Write CSV outputs
# ============================================================

metadata_path <- file.path(audit_dir, "us_project_metadata.csv")
bank_universe_path <- file.path(audit_dir, "us_initial_bank_universe.csv")
source_catalog_path <- file.path(audit_dir, "us_source_catalog.csv")
variable_inventory_path <- file.path(audit_dir, "us_variable_inventory.csv")
source_variable_map_path <- file.path(audit_dir, "us_source_variable_map.csv")
availability_assessment_path <- file.path(audit_dir, "us_data_availability_assessment.csv")
script_roadmap_path <- file.path(audit_dir, "us_script_roadmap.csv")
project_structure_path <- file.path(audit_dir, "us_project_structure.csv")

safe_write_csv(project_metadata, metadata_path)
safe_write_csv(bank_universe, bank_universe_path)
safe_write_csv(source_catalog, source_catalog_path)
safe_write_csv(variable_inventory, variable_inventory_path)
safe_write_csv(source_variable_map, source_variable_map_path)
safe_write_csv(availability_assessment, availability_assessment_path)
safe_write_csv(script_roadmap, script_roadmap_path)
safe_write_csv(project_structure, project_structure_path)


# ============================================================
# 12. Word report
# ============================================================

report_path <- file.path(
  audit_dir,
  "script01_data_availability_audit_report.docx"
)

can_make_docx <- requireNamespace("officer", quietly = TRUE) &&
  requireNamespace("flextable", quietly = TRUE)

if (can_make_docx) {
  
  library(officer)
  library(flextable)
  
  source_summary <- source_catalog %>%
    count(availability_class, name = "number_of_sources") %>%
    arrange(availability_class)
  
  variable_summary <- variable_inventory %>%
    count(model_block, inclusion_status, name = "number_of_variables") %>%
    arrange(model_block)
  
  doc <- read_docx()
  
  doc <- body_add_par(doc, "US Bank Stress Test Replication and DFAST Benchmarking", style = "heading 1")
  doc <- body_add_par(doc, "Script 01 — Data Availability Audit", style = "heading 2")
  
  doc <- body_add_par(
    doc,
    paste0(
      "This report documents the initial data availability audit for a U.S. bank stress-test replication project. ",
      "The audit focuses on public regulatory and supervisory sources that are expected to be more standardized ",
      "and more automatable than PDF-based manual extraction."
    ),
    style = "Normal"
  )
  
  doc <- body_add_par(doc, "1. Project metadata", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(project_metadata)))
  
  doc <- body_add_par(doc, "2. Initial bank universe", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(bank_universe)))
  
  doc <- body_add_par(doc, "3. Source catalog", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(source_catalog)))
  
  doc <- body_add_par(doc, "4. Source availability summary", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(source_summary)))
  
  doc <- body_add_par(doc, "5. Preliminary variable inventory", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(variable_inventory)))
  
  doc <- body_add_par(doc, "6. Variable summary by block", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(variable_summary)))
  
  doc <- body_add_par(doc, "7. Source-variable map", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(source_variable_map)))
  
  doc <- body_add_par(doc, "8. Availability assessment", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(availability_assessment)))
  
  doc <- body_add_par(doc, "9. Script roadmap", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(script_roadmap)))
  
  doc <- body_add_par(doc, "10. Project structure", style = "heading 2")
  doc <- body_add_flextable(doc, autofit(flextable(project_structure)))
  
  doc <- body_add_par(doc, "11. Initial decision", style = "heading 2")
  
  decision_table <- data.frame(
    decision_area = c(
      "Project feasibility",
      "Data strategy",
      "Immediate next script",
      "Main advantage over Mozambique project",
      "Main technical risk"
    ),
    decision = c(
      "Feasible for automated and reproducible stress-test replication.",
      "Prioritize DFAST, FDIC/FFIEC, FR Y-9C and FRED sources.",
      "Proceed to Script 02 — DFAST Results Download and Dictionary.",
      "Public U.S. regulatory data are more standardized and suitable for bulk processing.",
      "Bank identifier mapping across FDIC, FFIEC, FR Y-9C and DFAST sources must be handled carefully."
    ),
    stringsAsFactors = FALSE
  )
  
  doc <- body_add_flextable(doc, autofit(flextable(decision_table)))
  
  print(doc, target = report_path)
  
} else {
  
  txt_report_path <- sub("\\.docx$", ".txt", report_path)
  
  report_lines <- c(
    "US Bank Stress Test Replication and DFAST Benchmarking",
    "Script 01 — Data Availability Audit",
    "",
    "The Word report was not created because packages 'officer' and/or 'flextable' are not installed.",
    "The CSV outputs were created successfully.",
    "",
    "Main CSV outputs:",
    metadata_path,
    bank_universe_path,
    source_catalog_path,
    variable_inventory_path,
    source_variable_map_path,
    availability_assessment_path,
    script_roadmap_path,
    project_structure_path
  )
  
  writeLines(report_lines, con = txt_report_path, useBytes = TRUE)
}


# ============================================================
# 13. Execution log
# ============================================================

log_path <- file.path(audit_dir, "script01_execution_log.txt")

log_lines <- c(
  "US Bank Stress Test Replication and DFAST Benchmarking",
  "Script 01 — Data Availability Audit",
  paste0("Execution date: ", Sys.time()),
  paste0("Project root: ", project_root),
  paste0("Sources inventoried: ", nrow(source_catalog)),
  paste0("Banks in initial universe: ", nrow(bank_universe)),
  paste0("Variables inventoried: ", nrow(variable_inventory)),
  paste0("Script roadmap entries: ", nrow(script_roadmap)),
  "",
  "Main outputs:",
  metadata_path,
  bank_universe_path,
  source_catalog_path,
  variable_inventory_path,
  source_variable_map_path,
  availability_assessment_path,
  script_roadmap_path,
  project_structure_path,
  report_path
)

writeLines(log_lines, con = log_path, useBytes = TRUE)


# ============================================================
# 14. Console summary
# ============================================================

cat("\n============================================================\n")
cat("Script 01 — Data Availability Audit completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Sources inventoried:\n", nrow(source_catalog), "\n")
cat("Banks in initial universe:\n", nrow(bank_universe), "\n")
cat("Variables inventoried:\n", nrow(variable_inventory), "\n")
cat("Script roadmap entries:\n", nrow(script_roadmap), "\n\n")

cat("Source availability summary:\n")
print(
  source_catalog %>%
    count(availability_class, name = "number_of_sources") %>%
    arrange(availability_class)
)

cat("\nVariable summary:\n")
print(
  variable_inventory %>%
    count(model_block, name = "number_of_variables") %>%
    arrange(model_block)
)

cat("\nMain outputs:\n")
cat(" - ", metadata_path, "\n", sep = "")
cat(" - ", bank_universe_path, "\n", sep = "")
cat(" - ", source_catalog_path, "\n", sep = "")
cat(" - ", variable_inventory_path, "\n", sep = "")
cat(" - ", source_variable_map_path, "\n", sep = "")
cat(" - ", availability_assessment_path, "\n", sep = "")
cat(" - ", script_roadmap_path, "\n", sep = "")
cat(" - ", project_structure_path, "\n", sep = "")
cat(" - ", report_path, "\n", sep = "")

cat("\nAdditional outputs:\n")
cat(" - ", log_path, "\n", sep = "")
cat("============================================================\n")