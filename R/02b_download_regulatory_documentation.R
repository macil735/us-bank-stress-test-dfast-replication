# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 02b — Download Regulatory Documentation
# ============================================================
# Objective:
#   Download and inventory the regulatory documentation used to
#   guide the public DFAST-style stress test replication.
#
# Project root:
#   D:/GitHub/us-bank-stress-test-dfast-replication
#
# Main documentation folder:
#   docs/regulatory_framework/source_documents/
#
# Main outputs:
#   outputs/regulatory_documentation/us_regulatory_document_targets.csv
#   outputs/regulatory_documentation/us_regulatory_document_download_log.csv
#   outputs/regulatory_documentation/us_regulatory_document_inventory.csv
#   outputs/regulatory_documentation/us_regulatory_document_download_summary.csv
#   outputs/regulatory_documentation/script02b_regulatory_documentation_report.docx
#   outputs/regulatory_documentation/script02b_execution_log.txt
#
# Methodological note:
#   This script only downloads and inventories documentation.
#   It does not change data/raw or data/processed.
# ============================================================


# ------------------------------------------------------------
# 0. Initial setup
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

script_id <- "02b"
script_name <- "download_regulatory_documentation"
start_time <- Sys.time()

dir_list <- c(
  file.path(project_root, "R"),
  file.path(project_root, "docs"),
  file.path(project_root, "docs/regulatory_framework"),
  file.path(project_root, "docs/regulatory_framework/source_documents"),
  file.path(project_root, "docs/regulatory_framework/source_documents/federal_reserve"),
  file.path(project_root, "docs/regulatory_framework/source_documents/ecfr"),
  file.path(project_root, "docs/regulatory_framework/source_documents/sec"),
  file.path(project_root, "docs/regulatory_framework/source_documents/ffiec"),
  file.path(project_root, "outputs"),
  file.path(project_root, "outputs/regulatory_documentation")
)

for (d in dir_list) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

setwd(project_root)


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "purrr",
  "httr2",
  "fs",
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
library(tibble)
library(readr)
library(stringr)
library(purrr)
library(httr2)
library(fs)
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

download_document <- function(document_id,
                              regulator,
                              document_title,
                              url,
                              destination_dir,
                              destination_file,
                              document_type,
                              regulatory_area,
                              timeout_sec = 120) {

  if (!dir.exists(destination_dir)) dir.create(destination_dir, recursive = TRUE)

  destination_path <- file.path(destination_dir, destination_file)

  result <- tryCatch(
    {
      response <- request(url) |>
        req_user_agent(
          "us-bank-stress-test-dfast-replication/0.1 academic contact: gelo.picol@gmail.com"
        ) |>
        req_timeout(timeout_sec) |>
        req_perform(path = destination_path)

      status_code <- resp_status(response)

      file_exists <- file.exists(destination_path)
      file_size <- ifelse(file_exists, file.info(destination_path)$size, NA_real_)

      tibble(
        document_id = document_id,
        regulator = regulator,
        document_title = document_title,
        regulatory_area = regulatory_area,
        document_type = document_type,
        url = url,
        destination_path = destination_path,
        http_status = status_code,
        success = status_code >= 200 & status_code < 400 & file_exists & file_size > 0,
        file_size_bytes = file_size,
        downloaded_at = as.character(Sys.time()),
        error_message = NA_character_
      )
    },
    error = function(e) {
      tibble(
        document_id = document_id,
        regulator = regulator,
        document_title = document_title,
        regulatory_area = regulatory_area,
        document_type = document_type,
        url = url,
        destination_path = destination_path,
        http_status = NA_integer_,
        success = FALSE,
        file_size_bytes = NA_real_,
        downloaded_at = as.character(Sys.time()),
        error_message = safe_chr(conditionMessage(e))
      )
    }
  )

  result
}

make_inventory <- function(base_dir) {
  files <- fs::dir_info(
    path = base_dir,
    recurse = TRUE,
    type = "file"
  )

  if (nrow(files) == 0) {
    return(
      tibble(
        absolute_path = character(),
        relative_path = character(),
        file_name = character(),
        extension = character(),
        size_bytes = numeric(),
        modified_time = character()
      )
    )
  }

  files |>
    transmute(
      absolute_path = as.character(path),
      relative_path = str_replace(as.character(path), fixed(project_root), "") |>
        str_replace("^/", "") |>
        str_replace("^\\\\", ""),
      file_name = fs::path_file(path),
      extension = fs::path_ext(path),
      size_bytes = as.numeric(size),
      modified_time = as.character(modification_time)
    ) |>
    arrange(relative_path)
}

write_text_file <- function(path, lines) {
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}


# ------------------------------------------------------------
# 3. Documentation targets
# ------------------------------------------------------------

docs_root <- file.path(project_root, "docs/regulatory_framework/source_documents")

documentation_targets <- tribble(
  ~document_id, ~regulator, ~document_title, ~year, ~url, ~document_type, ~regulatory_area, ~destination_subdir, ~destination_file, ~replication_role, ~limitations,

  "FED_DFAST_2026_PAGE_HTML",
  "Federal Reserve",
  "Dodd-Frank Act Stress Tests 2026",
  2026,
  "https://www.federalreserve.gov/supervisionreg/dfa-stress-tests-2026.htm",
  "HTML",
  "DFAST central page",
  "federal_reserve",
  "fed_dfast_2026_page.html",
  "Central index for scenarios, methodology, results and model documentation.",
  "HTML snapshot may change if the Fed updates the page.",

  "FED_2026_SCENARIOS_HTML",
  "Federal Reserve",
  "2026 Stress Test Scenarios",
  2026,
  "https://www.federalreserve.gov/publications/2026-stress-test-scenarios.htm",
  "HTML",
  "Scenario design",
  "federal_reserve",
  "fed_2026_stress_test_scenarios.html",
  "Documents hypothetical macroeconomic scenarios used in the stress test.",
  "HTML snapshot.",

  "FED_2026_SCENARIOS_PDF",
  "Federal Reserve",
  "2026 Final Supervisory Stress Test Scenarios",
  2026,
  "https://www.federalreserve.gov/publications/files/2026-final-supervisory-stress-test-scenarios-20260204.pdf",
  "PDF",
  "Scenario design",
  "federal_reserve",
  "fed_2026_final_supervisory_stress_test_scenarios.pdf",
  "Official scenario document.",
  "Scenario is hypothetical, not a forecast.",

  "FED_2026_METHODOLOGY_PDF",
  "Federal Reserve",
  "2026 Supervisory Stress Test Methodology",
  2026,
  "https://www.federalreserve.gov/publications/files/2026-february-supervisory-stress-test-methodology.pdf",
  "PDF",
  "Supervisory methodology",
  "federal_reserve",
  "fed_2026_supervisory_stress_test_methodology.pdf",
  "Core methodological benchmark.",
  "Does not disclose confidential model details.",

  "FED_2026_DFAST_RESULTS_PDF",
  "Federal Reserve",
  "2026 Federal Reserve Stress Test Results",
  2026,
  "https://www.federalreserve.gov/publications/files/2026-dfast-results-20260624.pdf",
  "PDF",
  "DFAST results",
  "federal_reserve",
  "fed_2026_dfast_results.pdf",
  "Official results report.",
  "PDF is documentary; CSV files are preferred for data processing.",

  "FED_PUBLIC_RESULTS_DICTIONARY_PDF",
  "Federal Reserve",
  "Public Results DFAST 2026 Data Dictionary",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/public_results_DFAST_2026_dictionary.pdf",
  "PDF",
  "Data dictionary",
  "federal_reserve",
  "fed_public_results_DFAST_2026_dictionary.pdf",
  "Defines public DFAST results variables.",
  "May fail if the Fed uses a different filename.",

  "FED_9Q_PATHS_DICTIONARY_PDF",
  "Federal Reserve",
  "2026 Detailed Nine Quarter Paths Data Dictionary",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/2026_Detailed_Nine_Quarter_Paths_dictionary.pdf",
  "PDF",
  "Data dictionary",
  "federal_reserve",
  "fed_2026_detailed_nine_quarter_paths_dictionary.pdf",
  "Defines detailed path variables.",
  "May fail if the Fed uses a different filename.",

  "FED_CREDIT_RISK_MODELS_PDF",
  "Federal Reserve",
  "Supervisory Stress Test Documentation Credit Risk Models",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/credit-risk-models.pdf",
  "PDF",
  "Credit risk models",
  "federal_reserve",
  "fed_2026_credit_risk_models.pdf",
  "Documents public approach to supervisory credit loss models.",
  "Public documentation only.",

  "FED_MARKET_RISK_MODELS_PDF",
  "Federal Reserve",
  "Supervisory Stress Test Documentation Market Risk Models",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/market-risk-models.pdf",
  "PDF",
  "Market risk models",
  "federal_reserve",
  "fed_2026_market_risk_models.pdf",
  "Documents public approach to market risk components.",
  "May be used only in later extensions.",

  "FED_GMS_MODEL_PDF",
  "Federal Reserve",
  "Supervisory Stress Test Documentation Global Market Shock Model",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/2026-final-gms-model.pdf",
  "PDF",
  "Global market shock",
  "federal_reserve",
  "fed_2026_global_market_shock_model.pdf",
  "Documents global market shock component.",
  "May be used only in later extensions.",

  "FED_MACRO_MODEL_GUIDE_PDF",
  "Federal Reserve",
  "Supervisory Stress Test Documentation Macroeconomic Model Guide",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/macroeconomic-model-guide.pdf",
  "PDF",
  "Macroeconomic model guide",
  "federal_reserve",
  "fed_2026_macroeconomic_model_guide.pdf",
  "Documents macroeconomic modelling guidance for scenario paths.",
  "Public documentation only.",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_PDF",
  "Federal Reserve",
  "Large Bank Capital Requirements",
  2025,
  "https://www.federalreserve.gov/supervisionreg/files/large-bank-capital-requirements-20250627.pdf",
  "PDF",
  "Capital requirements",
  "federal_reserve",
  "fed_large_bank_capital_requirements_20250627.pdf",
  "Documents capital requirements and stress capital buffer references.",
  "Used for interpretation, not official SCB calculation.",

  "ECFR_REG_YY_12_CFR_252_HTML",
  "eCFR",
  "Regulation YY 12 CFR Part 252",
  2026,
  "https://www.ecfr.gov/current/title-12/chapter-II/subchapter-A/part-252",
  "HTML",
  "Regulatory framework",
  "ecfr",
  "ecfr_12_cfr_part_252_regulation_yy.html",
  "Legal framework for enhanced prudential standards and stress testing.",
  "eCFR is an updated online version.",

  "SEC_EDGAR_API_DOCS_HTML",
  "SEC",
  "EDGAR Application Programming Interfaces",
  2026,
  "https://www.sec.gov/search-filings/edgar-application-programming-interfaces",
  "HTML",
  "SEC filings and XBRL APIs",
  "sec",
  "sec_edgar_application_programming_interfaces.html",
  "Documents submissions and companyfacts endpoints used for reconciliation.",
  "SEC site may block access if user-agent is not adequate.",

  "FFIEC_BULK_CALL_REPORTS_HTML",
  "FFIEC",
  "Bulk Call Reports",
  2026,
  "https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx",
  "HTML",
  "Regulatory reporting data",
  "ffiec",
  "ffiec_bulk_call_reports.html",
  "Documents access point for Call Reports bulk data.",
  "Dynamic page may not fully capture downloadable file options."
) |>
  mutate(
    destination_dir = file.path(docs_root, destination_subdir),
    destination_path = file.path(destination_dir, destination_file)
  ) |>
  safe_df()


# ------------------------------------------------------------
# 4. Download documentation
# ------------------------------------------------------------

message("Downloading regulatory documentation...")

download_log <- pmap_dfr(
  list(
    documentation_targets$document_id,
    documentation_targets$regulator,
    documentation_targets$document_title,
    documentation_targets$url,
    documentation_targets$destination_dir,
    documentation_targets$destination_file,
    documentation_targets$document_type,
    documentation_targets$regulatory_area
  ),
  download_document
) |>
  left_join(
    documentation_targets |>
      select(document_id, year, replication_role, limitations),
    by = "document_id"
  ) |>
  safe_df()


# ------------------------------------------------------------
# 5. Document inventory
# ------------------------------------------------------------

document_inventory <- make_inventory(docs_root) |>
  safe_df()


# ------------------------------------------------------------
# 6. Permanent documentation notes
# ------------------------------------------------------------

write_text_file(
  file.path(project_root, "docs/regulatory_framework/README_regulatory_framework.md"),
  c(
    "# Regulatory Framework Documentation",
    "",
    "This folder stores the regulatory documentation used to guide the USA Bank Stress Test DFAST Replication project.",
    "",
    "The documentation covers Federal Reserve DFAST disclosures, supervisory stress test scenarios, public model documentation, capital requirements, Regulation YY, SEC EDGAR API documentation and FFIEC Call Report access.",
    "",
    "The project is a public, reproducible and approximate replication. It does not reproduce confidential Federal Reserve supervisory models or internal bank capital planning models.",
    "",
    "Source documents are stored in:",
    "",
    "docs/regulatory_framework/source_documents/"
  )
)

write_text_file(
  file.path(project_root, "docs/regulatory_framework/public_replication_disclaimer.md"),
  c(
    "# Public Replication Disclaimer",
    "",
    "This project implements a public, reproducible and approximate DFAST-style stress test replication.",
    "",
    "It uses public Federal Reserve scenarios, published DFAST results, public model documentation, SEC EDGAR filings, FFIEC references and FRED macroeconomic series.",
    "",
    "It does not claim to reproduce confidential Federal Reserve supervisory models, internal bank capital planning models, official Stress Capital Buffer calculations or any legally binding supervisory determination."
  )
)


# ------------------------------------------------------------
# 7. Summaries
# ------------------------------------------------------------

download_summary <- download_log |>
  group_by(regulator, document_type) |>
  summarise(
    number_of_documents = n(),
    successful_downloads = sum(success, na.rm = TRUE),
    failed_downloads = sum(!success, na.rm = TRUE),
    total_size_bytes = sum(file_size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(regulator, document_type) |>
  safe_df()

failed_downloads <- download_log |>
  filter(!success) |>
  select(document_id, regulator, document_title, document_type, url, http_status, error_message) |>
  safe_df()

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  documentation_targets = nrow(documentation_targets),
  successful_downloads = sum(download_log$success, na.rm = TRUE),
  failed_downloads = sum(!download_log$success, na.rm = TRUE),
  documents_inventoried = nrow(document_inventory)
) |>
  safe_df()


# ------------------------------------------------------------
# 8. Save tabular outputs
# ------------------------------------------------------------

out_dir <- file.path(project_root, "outputs/regulatory_documentation")

targets_csv <- file.path(out_dir, "us_regulatory_document_targets.csv")
download_log_csv <- file.path(out_dir, "us_regulatory_document_download_log.csv")
inventory_csv <- file.path(out_dir, "us_regulatory_document_inventory.csv")
summary_csv <- file.path(out_dir, "us_regulatory_document_download_summary.csv")
failed_csv <- file.path(out_dir, "us_regulatory_document_failed_downloads.csv")
execution_summary_csv <- file.path(out_dir, "script02b_execution_summary.csv")
excel_output <- file.path(out_dir, "script02b_regulatory_documentation_outputs.xlsx")
report_docx <- file.path(out_dir, "script02b_regulatory_documentation_report.docx")
execution_log_txt <- file.path(out_dir, "script02b_execution_log.txt")

write_csv(documentation_targets, targets_csv)
write_csv(download_log, download_log_csv)
write_csv(document_inventory, inventory_csv)
write_csv(download_summary, summary_csv)
write_csv(failed_downloads, failed_csv)
write_csv(execution_summary, execution_summary_csv)

wb <- createWorkbook()

addWorksheet(wb, "documentation_targets")
writeData(wb, "documentation_targets", documentation_targets)

addWorksheet(wb, "download_log")
writeData(wb, "download_log", download_log)

addWorksheet(wb, "document_inventory")
writeData(wb, "document_inventory", document_inventory)

addWorksheet(wb, "download_summary")
writeData(wb, "download_summary", download_summary)

addWorksheet(wb, "failed_downloads")
writeData(wb, "failed_downloads", failed_downloads)

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", execution_summary)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_output, overwrite = TRUE)


# ------------------------------------------------------------
# 9. Word report
# ------------------------------------------------------------
# The Word report deliberately excludes long URLs and raw error text
# to avoid XML/Word encoding problems.

doc <- read_docx()

doc <- doc |>
  body_add_par("Script 02b - Download Regulatory Documentation", style = "heading 1") |>
  body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  body_add_par("1. Objective", style = "heading 2") |>
  body_add_par(
    "This script downloads and inventories the regulatory documentation used to guide the DFAST-style public replication project.",
    style = "Normal"
  ) |>
  body_add_par("2. Execution summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(execution_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("3. Download summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(download_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("4. Documentation targets", style = "heading 2")

doc_targets_small <- documentation_targets |>
  select(document_id, regulator, document_title, year, document_type, regulatory_area, destination_file)

doc <- body_add_flextable(
  doc,
  flextable(doc_targets_small) |>
    autofit()
)

doc <- doc |>
  body_add_par("5. Failed downloads", style = "heading 2")

if (nrow(failed_downloads) == 0) {
  doc <- body_add_par(doc, "No failed downloads.", style = "Normal")
} else {
  failed_small <- failed_downloads |>
    select(document_id, regulator, document_title, document_type, http_status)

  doc <- body_add_flextable(
    doc,
    flextable(failed_small) |>
      autofit()
  )
}

doc <- doc |>
  body_add_par("6. Methodological note", style = "heading 2") |>
  body_add_par(
    "The downloaded documents provide the regulatory and methodological basis of the project. They support documentation, interpretation and replication discipline. They do not remove the limitation that confidential supervisory models and internal bank models are not publicly reproducible.",
    style = "Normal"
  )

print(doc, target = report_docx)


# ------------------------------------------------------------
# 10. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 02b - Download Regulatory Documentation completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Documentation targets:", nrow(documentation_targets)),
  paste("Successful downloads:", sum(download_log$success, na.rm = TRUE)),
  paste("Failed downloads:", sum(!download_log$success, na.rm = TRUE)),
  paste("Documents inventoried:", nrow(document_inventory)),
  "",
  "Download summary:",
  capture.output(print(download_summary)),
  "",
  "Failed downloads:",
  capture.output(print(failed_downloads)),
  "",
  "Main documentation folder:",
  paste(" -", docs_root),
  "",
  "Main outputs:",
  paste(" -", targets_csv),
  paste(" -", download_log_csv),
  paste(" -", inventory_csv),
  paste(" -", summary_csv),
  paste(" -", failed_csv),
  paste(" -", execution_summary_csv),
  paste(" -", excel_output),
  paste(" -", report_docx),
  "",
  "Additional outputs:",
  paste(" -", execution_log_txt)
)

writeLines(enc2utf8(log_lines), execution_log_txt, useBytes = TRUE)


# ------------------------------------------------------------
# 11. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 02b - Download Regulatory Documentation completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Documentation targets:\n", nrow(documentation_targets), "\n")
cat("Successful downloads:\n", sum(download_log$success, na.rm = TRUE), "\n")
cat("Failed downloads:\n", sum(!download_log$success, na.rm = TRUE), "\n")
cat("Documents inventoried:\n", nrow(document_inventory), "\n\n")

cat("Download summary:\n")
print(download_summary)

cat("\nFailed downloads:\n")
print(failed_downloads)

cat("\nMain documentation folder:\n")
cat(" -", docs_root, "\n")

cat("\nMain outputs:\n")
cat(" -", targets_csv, "\n")
cat(" -", download_log_csv, "\n")
cat(" -", inventory_csv, "\n")
cat(" -", summary_csv, "\n")
cat(" -", failed_csv, "\n")
cat(" -", execution_summary_csv, "\n")
cat(" -", excel_output, "\n")
cat(" -", report_docx, "\n")

cat("\nAdditional outputs:\n")
cat(" -", execution_log_txt, "\n")