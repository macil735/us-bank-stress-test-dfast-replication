# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 02 — Regulatory Documentation and Raw Data Ingestion
# ============================================================
# Objetivo:
#   1. Criar um catálogo formal da documentação regulatória dos EUA
#      que orienta o projeto.
#   2. Descarregar e guardar os dados brutos públicos prioritários.
#   3. Produzir logs e inventários sem fazer transformação analítica.
#
# Diretório:
#   D:/GitHub/us-bank-stress-test-dfast-replication
#
# Inputs esperados do Script 01:
#   outputs/data_audit/us_source_catalog.csv
#   outputs/data_audit/us_initial_bank_universe.csv
#
# Outputs principais:
#   outputs/data_ingestion/us_regulatory_documentation_catalog.csv
#   outputs/data_ingestion/us_raw_data_download_log.csv
#   outputs/data_ingestion/us_raw_data_file_inventory.csv
#   outputs/data_ingestion/script02_raw_data_ingestion_report.docx
#   outputs/data_ingestion/script02_execution_log.txt
#
# Nota:
#   Este script NÃO estima perdas, capital, PPNR ou CET1.
#   Apenas documenta e descarrega dados brutos.
# ============================================================


# ------------------------------------------------------------
# 0. Configuração inicial
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

script_id <- "02"
script_name <- "regulatory_documentation_and_raw_data_ingestion"

start_time <- Sys.time()

dir_list <- c(
  file.path(project_root, "R"),
  file.path(project_root, "data"),
  file.path(project_root, "data/raw"),
  file.path(project_root, "data/raw/fed"),
  file.path(project_root, "data/raw/fed/scenarios"),
  file.path(project_root, "data/raw/fed/results"),
  file.path(project_root, "data/raw/fed/methodology"),
  file.path(project_root, "data/raw/ffiec"),
  file.path(project_root, "data/raw/sec"),
  file.path(project_root, "data/raw/fred"),
  file.path(project_root, "data/metadata"),
  file.path(project_root, "docs"),
  file.path(project_root, "docs/regulatory_framework"),
  file.path(project_root, "outputs"),
  file.path(project_root, "outputs/data_ingestion")
)

for (d in dir_list) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

setwd(project_root)


# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "purrr",
  "lubridate",
  "httr2",
  "jsonlite",
  "fs",
  "officer",
  "flextable",
  "openxlsx"
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
library(lubridate)
library(httr2)
library(jsonlite)
library(fs)
library(officer)
library(flextable)
library(openxlsx)


# ------------------------------------------------------------
# 2. Funções auxiliares
# ------------------------------------------------------------

safe_read_csv <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    tibble()
  }
}


clean_file_name <- function(x) {
  x |>
    str_replace_all("[^A-Za-z0-9_\\-\\.]", "_") |>
    str_replace_all("_+", "_") |>
    str_replace_all("^_|_$", "")
}


guess_extension <- function(url, fallback = "dat") {
  lower_url <- tolower(url)
  
  case_when(
    str_detect(lower_url, "\\.csv($|\\?)")  ~ "csv",
    str_detect(lower_url, "\\.xlsx($|\\?)") ~ "xlsx",
    str_detect(lower_url, "\\.xls($|\\?)")  ~ "xls",
    str_detect(lower_url, "\\.pdf($|\\?)")  ~ "pdf",
    str_detect(lower_url, "\\.json($|\\?)") ~ "json",
    str_detect(lower_url, "\\.zip($|\\?)")  ~ "zip",
    TRUE ~ fallback
  )
}


download_one_file <- function(file_id,
                              url,
                              destination_dir,
                              destination_file,
                              timeout_sec = 90,
                              user_agent = "us-bank-stress-test-dfast-replication/0.1 academic contact: gelo.picol@gmail.com") {
  
  if (!dir.exists(destination_dir)) dir.create(destination_dir, recursive = TRUE)
  
  destination_path <- file.path(destination_dir, destination_file)
  
  result <- tryCatch(
    {
      response <- request(url) |>
        req_user_agent(user_agent) |>
        req_timeout(timeout_sec) |>
        req_perform(path = destination_path)
      
      file_info <- file.info(destination_path)
      
      tibble(
        file_id = file_id,
        url = url,
        destination_path = destination_path,
        http_status = resp_status(response),
        success = resp_status(response) >= 200 & resp_status(response) < 400 & file.exists(destination_path),
        file_size_bytes = ifelse(file.exists(destination_path), file_info$size, NA_real_),
        downloaded_at = as.character(Sys.time()),
        error_message = NA_character_
      )
    },
    error = function(e) {
      tibble(
        file_id = file_id,
        url = url,
        destination_path = destination_path,
        http_status = NA_integer_,
        success = FALSE,
        file_size_bytes = NA_real_,
        downloaded_at = as.character(Sys.time()),
        error_message = conditionMessage(e)
      )
    }
  )
  
  result
}


write_markdown_doc <- function(path, title, body_lines) {
  lines <- c(
    paste0("# ", title),
    "",
    body_lines
  )
  writeLines(lines, path, useBytes = TRUE)
}


make_docx_report <- function(report_path,
                             regulatory_catalog,
                             download_log,
                             file_inventory,
                             execution_summary) {
  
  doc <- read_docx()
  
  doc <- doc |>
    body_add_par("Script 02 — Regulatory Documentation and Raw Data Ingestion", style = "heading 1") |>
    body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
    body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
    body_add_par("1. Objective", style = "heading 2") |>
    body_add_par(
      "This script documents the regulatory framework and ingests raw public data required for a DFAST-style stress test replication. It preserves source files without analytical transformation.",
      style = "Normal"
    ) |>
    body_add_par("2. Execution summary", style = "heading 2")
  
  doc <- body_add_flextable(doc, flextable(execution_summary) |> autofit())
  
  doc <- doc |>
    body_add_par("3. Regulatory documentation catalog", style = "heading 2")
  
  doc <- body_add_flextable(
    doc,
    regulatory_catalog |>
      select(document_id, regulator, document_title, year, regulatory_area, replication_role) |>
      flextable() |>
      autofit()
  )
  
  doc <- doc |>
    body_add_par("4. Download log", style = "heading 2")
  
  doc <- body_add_flextable(
    doc,
    download_log |>
      select(file_id, success, http_status, file_size_bytes, destination_path) |>
      flextable() |>
      autofit()
  )
  
  doc <- doc |>
    body_add_par("5. Raw file inventory", style = "heading 2")
  
  doc <- body_add_flextable(
    doc,
    file_inventory |>
      select(relative_path, extension, size_bytes, modified_time) |>
      flextable() |>
      autofit()
  )
  
  doc <- doc |>
    body_add_par("6. Methodological note", style = "heading 2") |>
    body_add_par(
      "The project is a public, reproducible and approximate DFAST-style replication. It does not claim to reproduce confidential Federal Reserve supervisory models or internal bank capital planning models.",
      style = "Normal"
    )
  
  print(doc, target = report_path)
}


# ------------------------------------------------------------
# 3. Caminhos dos inputs do Script 01
# ------------------------------------------------------------

source_catalog_path <- file.path(
  project_root,
  "outputs/data_audit/us_source_catalog.csv"
)

bank_universe_path <- file.path(
  project_root,
  "outputs/data_audit/us_initial_bank_universe.csv"
)

script01_source_catalog <- safe_read_csv(source_catalog_path)
script01_bank_universe <- safe_read_csv(bank_universe_path)


# ------------------------------------------------------------
# 4. Catálogo regulatório
# ------------------------------------------------------------

regulatory_catalog <- tribble(
  ~document_id, ~regulator, ~document_title, ~year, ~url, ~document_type, ~regulatory_area, ~used_for, ~script_reference, ~replication_role, ~limitations,
  
  "FED_DFAST_2026_PAGE",
  "Federal Reserve",
  "Dodd-Frank Act Stress Tests 2026",
  2026,
  "https://www.federalreserve.gov/supervisionreg/dfa-stress-tests-2026.htm",
  "HTML",
  "DFAST public exercise",
  "Central index for 2026 scenarios, methodology, results and model documentation.",
  "Scripts 02-13",
  "Primary regulatory reference page.",
  "Page structure may change over time; downloaded files are treated as dated source copies.",
  
  "FED_2026_SCENARIOS",
  "Federal Reserve",
  "2026 Stress Test Scenarios",
  2026,
  "https://www.federalreserve.gov/publications/2026-stress-test-scenarios.htm",
  "HTML/PDF",
  "Macroeconomic scenarios",
  "Defines baseline and severely adverse hypothetical economic paths.",
  "Scripts 02, 04, 07",
  "Primary scenario framework.",
  "Scenarios are hypothetical conditions, not forecasts.",
  
  "FED_2026_METHODOLOGY",
  "Federal Reserve",
  "2026 Supervisory Stress Test Methodology",
  2026,
  "https://www.federalreserve.gov/publications/files/2026-february-supervisory-stress-test-methodology.pdf",
  "PDF",
  "Supervisory methodology",
  "Documents high-level model methodology and changes for 2026.",
  "Scripts 02, 08, 09, 10, 13",
  "Methodological benchmark.",
  "Public methodology does not disclose all confidential supervisory model details.",
  
  "FED_2026_RESULTS",
  "Federal Reserve",
  "2026 Federal Reserve Stress Test Results",
  2026,
  "https://www.federalreserve.gov/publications/files/2026-dfast-results-20260624.pdf",
  "PDF",
  "Stress test results",
  "Official results narrative and validation reference.",
  "Scripts 02, 11, 13",
  "Benchmark for comparing replicated outputs.",
  "PDF is not the preferred machine-readable source.",
  
  "FED_RESULTS_2013_2026_CSV",
  "Federal Reserve",
  "Stress Test Results, 2013-2026",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/public_results_DFAST_2026.csv",
  "CSV",
  "DFAST benchmark data",
  "Bank-level public stress test results across exercises.",
  "Scripts 02, 03, 11",
  "Core benchmark dataset.",
  "Variable definitions must be reconciled with the data dictionary.",
  
  "FED_DETAILED_9Q_PATHS_2026",
  "Federal Reserve",
  "2026 Detailed Nine Quarter Paths",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/2026_Detailed_Nine_Quarter_Paths.csv",
  "CSV",
  "DFAST benchmark paths",
  "Detailed projected bank-level nine-quarter paths.",
  "Scripts 02, 03, 10, 11",
  "Dynamic validation benchmark.",
  "May not contain all internal model inputs.",
  
  "FED_RESULTS_DATA_DICTIONARY",
  "Federal Reserve",
  "Stress Test Results Data Dictionary",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/public_results_DFAST_2026_dictionary.pdf",
  "PDF",
  "Data dictionary",
  "Defines variables in the public DFAST results file.",
  "Scripts 02, 03, 11",
  "Variable interpretation reference.",
  "File naming may change if Fed revises public files.",
  
  "FED_9Q_PATHS_DICTIONARY",
  "Federal Reserve",
  "2026 Detailed Nine Quarter Paths Data Dictionary",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/2026_Detailed_Nine_Quarter_Paths_dictionary.pdf",
  "PDF",
  "Data dictionary",
  "Defines variables in detailed nine-quarter paths.",
  "Scripts 02, 03, 10, 11",
  "Variable interpretation reference.",
  "File naming may change if Fed revises public files.",
  
  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS",
  "Federal Reserve",
  "Large Bank Capital Requirements",
  2026,
  "https://www.federalreserve.gov/supervisionreg/files/large-bank-capital-requirements-20250627.pdf",
  "PDF",
  "Capital requirements and SCB",
  "Documents capital requirements and stress capital buffer references.",
  "Scripts 02, 10, 13",
  "Capital interpretation framework.",
  "Final SCB calculations may not be fully reproducible from public data alone.",
  
  "REG_YY_12_CFR_252",
  "Federal Reserve / eCFR",
  "Regulation YY — 12 CFR Part 252",
  2026,
  "https://www.ecfr.gov/current/title-12/chapter-II/subchapter-A/part-252",
  "HTML",
  "Enhanced prudential standards and stress testing rules",
  "Defines regulatory perimeter for large banking organizations.",
  "Scripts 02, 13",
  "Legal and institutional framework.",
  "Legal interpretation should be treated as documentary, not legal advice.",
  
  "FFIEC_BULK_CALL_REPORTS",
  "FFIEC",
  "Bulk Call Reports",
  2026,
  "https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx",
  "HTML",
  "Regulatory reporting data",
  "Call Reports, balance sheet, income statement and past due data.",
  "Scripts 02, 05, 06",
  "Bank financial data source.",
  "Some downloads may require manual selection or dynamic parameters.",
  
  "SEC_EDGAR_APIS",
  "SEC",
  "EDGAR Application Programming Interfaces",
  2024,
  "https://www.sec.gov/search-filings/edgar-application-programming-interfaces",
  "HTML",
  "SEC filings and XBRL APIs",
  "Defines access to submissions and companyfacts JSON files.",
  "Scripts 02, 05, 06",
  "Public filing reconciliation source.",
  "Requires appropriate user-agent and rate discipline."
)


# ------------------------------------------------------------
# 5. Dados brutos prioritários para download
# ------------------------------------------------------------

download_targets <- tribble(
  ~file_id, ~source_family, ~url, ~destination_subdir, ~destination_file, ~priority, ~notes,
  
  "fed_dfast_results_2013_2026",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/public_results_DFAST_2026.csv",
  "data/raw/fed/results",
  "fed_public_results_DFAST_2026.csv",
  "Core",
  "Public DFAST results, 2013-2026.",
  
  "fed_dfast_results_dictionary_2026",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/public_results_DFAST_2026_dictionary.pdf",
  "data/raw/fed/results",
  "fed_public_results_DFAST_2026_dictionary.pdf",
  "Core",
  "Data dictionary for public DFAST results.",
  
  "fed_detailed_9q_paths_2026",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Detailed_Nine_Quarter_Paths.csv",
  "data/raw/fed/results",
  "fed_2026_Detailed_Nine_Quarter_Paths.csv",
  "Core",
  "Detailed nine-quarter paths.",
  
  "fed_detailed_9q_paths_dictionary_2026",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Detailed_Nine_Quarter_Paths_dictionary.pdf",
  "data/raw/fed/results",
  "fed_2026_Detailed_Nine_Quarter_Paths_dictionary.pdf",
  "Core",
  "Data dictionary for detailed nine-quarter paths.",
  
  "fed_2026_historic_domestic",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Final_Historic_Domestic.csv",
  "data/raw/fed/scenarios",
  "fed_2026_Final_Historic_Domestic.csv",
  "Core",
  "Historic domestic macro data.",
  
  "fed_2026_historic_international",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Final_Historic_International.csv",
  "data/raw/fed/scenarios",
  "fed_2026_Final_Historic_International.csv",
  "Important",
  "Historic international macro data.",
  
  "fed_2026_baseline_domestic",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Final_Supervisory_Baseline_Domestic.csv",
  "data/raw/fed/scenarios",
  "fed_2026_Final_Supervisory_Baseline_Domestic.csv",
  "Core",
  "Baseline domestic scenario.",
  
  "fed_2026_baseline_international",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Final_Supervisory_Baseline_International.csv",
  "data/raw/fed/scenarios",
  "fed_2026_Final_Supervisory_Baseline_International.csv",
  "Important",
  "Baseline international scenario.",
  
  "fed_2026_severely_adverse_domestic",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Final_Supervisory_Severely_Adverse_Domestic.csv",
  "data/raw/fed/scenarios",
  "fed_2026_Final_Supervisory_Severely_Adverse_Domestic.csv",
  "Core",
  "Severely adverse domestic scenario.",
  
  "fed_2026_severely_adverse_international",
  "Federal Reserve",
  "https://www.federalreserve.gov/supervisionreg/files/2026_Final_Supervisory_Severely_Adverse_International.csv",
  "data/raw/fed/scenarios",
  "fed_2026_Final_Supervisory_Severely_Adverse_International.csv",
  "Important",
  "Severely adverse international scenario.",
  
  "fed_2026_stress_test_methodology_pdf",
  "Federal Reserve",
  "https://www.federalreserve.gov/publications/files/2026-february-supervisory-stress-test-methodology.pdf",
  "data/raw/fed/methodology",
  "fed_2026_supervisory_stress_test_methodology.pdf",
  "Core",
  "Official methodology document.",
  
  "fed_2026_dfast_results_pdf",
  "Federal Reserve",
  "https://www.federalreserve.gov/publications/files/2026-dfast-results-20260624.pdf",
  "data/raw/fed/methodology",
  "fed_2026_dfast_results_report.pdf",
  "Core",
  "Official results report.",
  
  "sec_jpm_submissions",
  "SEC EDGAR",
  "https://data.sec.gov/submissions/CIK0000019617.json",
  "data/raw/sec",
  "sec_JPM_CIK0000019617_submissions.json",
  "Important",
  "JPMorgan Chase SEC submissions metadata.",
  
  "sec_jpm_companyfacts",
  "SEC EDGAR",
  "https://data.sec.gov/api/xbrl/companyfacts/CIK0000019617.json",
  "data/raw/sec",
  "sec_JPM_CIK0000019617_companyfacts.json",
  "Important",
  "JPMorgan Chase XBRL companyfacts.",
  
  "fred_unrate",
  "FRED",
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE",
  "data/raw/fred",
  "fred_UNRATE.csv",
  "Support",
  "Unemployment rate validation series.",
  
  "fred_real_gdp",
  "FRED",
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDPC1",
  "data/raw/fred",
  "fred_GDPC1.csv",
  "Support",
  "Real GDP validation series.",
  
  "fred_case_shiller_hpi",
  "FRED",
  "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CSUSHPINSA",
  "data/raw/fred",
  "fred_CSUSHPINSA.csv",
  "Support",
  "House price validation series."
) |>
  mutate(
    destination_dir = file.path(project_root, destination_subdir),
    destination_path = file.path(destination_dir, destination_file)
  )


# ------------------------------------------------------------
# 6. Download dos ficheiros brutos
# ------------------------------------------------------------

message("Starting raw data download...")

download_log <- pmap_dfr(
  list(
    download_targets$file_id,
    download_targets$url,
    download_targets$destination_dir,
    download_targets$destination_file
  ),
  download_one_file
) |>
  left_join(
    download_targets |>
      select(file_id, source_family, priority, notes),
    by = "file_id"
  ) |>
  relocate(source_family, priority, notes, .after = file_id)


# ------------------------------------------------------------
# 7. Inventário dos ficheiros brutos
# ------------------------------------------------------------

raw_files <- fs::dir_info(
  path = file.path(project_root, "data/raw"),
  recurse = TRUE,
  type = "file"
)

file_inventory <- raw_files |>
  transmute(
    absolute_path = as.character(path),
    relative_path = str_replace(as.character(path), fixed(project_root), "") |>
      str_replace("^/", "") |>
      str_replace("^\\\\", ""),
    file_name = path_file(path),
    extension = path_ext(path),
    size_bytes = size,
    modified_time = as.character(modification_time)
  ) |>
  arrange(relative_path)


# ------------------------------------------------------------
# 8. Pequena validação estrutural dos CSV/JSON baixados
# ------------------------------------------------------------

validate_raw_file <- function(path) {
  ext <- tolower(fs::path_ext(path))
  
  out <- tryCatch(
    {
      if (ext == "csv") {
        x <- readr::read_csv(path, n_max = 5, show_col_types = FALSE)
        
        tibble(
          absolute_path = as.character(path),
          readable = TRUE,
          detected_type = "csv",
          n_preview_rows = nrow(x),
          n_preview_cols = ncol(x),
          column_preview = paste(names(x), collapse = " | "),
          validation_error = NA_character_
        )
        
      } else if (ext == "json") {
        x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
        
        tibble(
          absolute_path = as.character(path),
          readable = TRUE,
          detected_type = "json",
          n_preview_rows = NA_integer_,
          n_preview_cols = NA_integer_,
          column_preview = paste(names(x)[1:min(length(names(x)), 20)], collapse = " | "),
          validation_error = NA_character_
        )
        
      } else if (ext == "pdf") {
        tibble(
          absolute_path = as.character(path),
          readable = file.exists(path) && file.info(path)$size > 0,
          detected_type = "pdf",
          n_preview_rows = NA_integer_,
          n_preview_cols = NA_integer_,
          column_preview = NA_character_,
          validation_error = NA_character_
        )
        
      } else {
        tibble(
          absolute_path = as.character(path),
          readable = file.exists(path) && file.info(path)$size > 0,
          detected_type = ext,
          n_preview_rows = NA_integer_,
          n_preview_cols = NA_integer_,
          column_preview = NA_character_,
          validation_error = NA_character_
        )
      }
    },
    error = function(e) {
      tibble(
        absolute_path = as.character(path),
        readable = FALSE,
        detected_type = ext,
        n_preview_rows = NA_integer_,
        n_preview_cols = NA_integer_,
        column_preview = NA_character_,
        validation_error = conditionMessage(e)
      )
    }
  )
  
  out
}

raw_validation <- map_dfr(file_inventory$absolute_path, validate_raw_file)


# ------------------------------------------------------------
# 9. Criar documentos Markdown do enquadramento regulatório
# ------------------------------------------------------------

framework_dir <- file.path(project_root, "docs/regulatory_framework")

write_markdown_doc(
  file.path(framework_dir, "01_fed_dfast_framework.md"),
  "Federal Reserve DFAST Framework",
  c(
    "This project follows the public Federal Reserve DFAST and supervisory stress test documentation.",
    "",
    "The project uses official scenarios, published DFAST results, detailed nine-quarter paths and public model documentation.",
    "",
    "The replication is public and approximate. It does not reproduce confidential supervisory models."
  )
)

write_markdown_doc(
  file.path(framework_dir, "02_regulation_yy_12_cfr_252.md"),
  "Regulation YY — 12 CFR Part 252",
  c(
    "Regulation YY provides the legal and institutional framework for enhanced prudential standards and stress testing requirements for large banking organizations.",
    "",
    "The project uses this regulation as documentary background for the perimeter of covered banking organizations.",
    "",
    "This document is used for academic and methodological documentation only."
  )
)

write_markdown_doc(
  file.path(framework_dir, "03_capital_plan_rule_scb.md"),
  "Capital Planning and Stress Capital Buffer",
  c(
    "The project documents the link between supervisory stress testing, capital planning and stressed capital ratios.",
    "",
    "The project may compute indicative capital depletion and compare stressed ratios with published benchmarks.",
    "",
    "It does not claim to calculate an official supervisory Stress Capital Buffer."
  )
)

write_markdown_doc(
  file.path(framework_dir, "04_data_sources_and_reporting_forms.md"),
  "Data Sources and Reporting Forms",
  c(
    "The project combines Federal Reserve DFAST disclosures, FFIEC regulatory reporting sources, SEC EDGAR filings and FRED macroeconomic validation series.",
    "",
    "Federal Reserve files provide scenarios and DFAST benchmarks.",
    "FFIEC and SEC sources provide bank financial data and reconciliation material.",
    "FRED provides independent macroeconomic validation series."
  )
)

write_markdown_doc(
  file.path(framework_dir, "05_public_replication_limitations.md"),
  "Public Replication Limitations",
  c(
    "This project is a public, reproducible and approximate replication.",
    "",
    "Main limitations:",
    "",
    "- Confidential Federal Reserve supervisory models are not replicated.",
    "- Internal bank capital planning models are not replicated.",
    "- Public accounting data may differ from regulatory reporting definitions.",
    "- Some FFIEC files may require manual download or separate parsing logic.",
    "- Market risk, counterparty default and operational risk may require simplified treatment."
  )
)


# ------------------------------------------------------------
# 10. Tabelas de resumo
# ------------------------------------------------------------

download_summary <- download_log |>
  group_by(source_family, priority) |>
  summarise(
    number_of_files = n(),
    successful_downloads = sum(success, na.rm = TRUE),
    failed_downloads = sum(!success, na.rm = TRUE),
    total_size_bytes = sum(file_size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(source_family, desc(priority))

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  regulatory_documents_catalogued = nrow(regulatory_catalog),
  download_targets = nrow(download_targets),
  successful_downloads = sum(download_log$success, na.rm = TRUE),
  failed_downloads = sum(!download_log$success, na.rm = TRUE),
  raw_files_inventoried = nrow(file_inventory),
  script01_source_catalog_found = file.exists(source_catalog_path),
  script01_bank_universe_found = file.exists(bank_universe_path)
)


# ------------------------------------------------------------
# 11. Guardar outputs
# ------------------------------------------------------------

out_dir <- file.path(project_root, "outputs/data_ingestion")

regulatory_catalog_csv <- file.path(out_dir, "us_regulatory_documentation_catalog.csv")
download_targets_csv <- file.path(out_dir, "us_download_targets.csv")
download_log_csv <- file.path(out_dir, "us_raw_data_download_log.csv")
file_inventory_csv <- file.path(out_dir, "us_raw_data_file_inventory.csv")
raw_validation_csv <- file.path(out_dir, "us_raw_data_validation_preview.csv")
download_summary_csv <- file.path(out_dir, "us_raw_data_download_summary.csv")
execution_summary_csv <- file.path(out_dir, "script02_execution_summary.csv")
excel_output <- file.path(out_dir, "script02_data_ingestion_outputs.xlsx")
report_docx <- file.path(out_dir, "script02_raw_data_ingestion_report.docx")
execution_log_txt <- file.path(out_dir, "script02_execution_log.txt")

write_csv(regulatory_catalog, regulatory_catalog_csv)
write_csv(download_targets, download_targets_csv)
write_csv(download_log, download_log_csv)
write_csv(file_inventory, file_inventory_csv)
write_csv(raw_validation, raw_validation_csv)
write_csv(download_summary, download_summary_csv)
write_csv(execution_summary, execution_summary_csv)

wb <- createWorkbook()

addWorksheet(wb, "regulatory_catalog")
writeData(wb, "regulatory_catalog", regulatory_catalog)

addWorksheet(wb, "download_targets")
writeData(wb, "download_targets", download_targets)

addWorksheet(wb, "download_log")
writeData(wb, "download_log", download_log)

addWorksheet(wb, "file_inventory")
writeData(wb, "file_inventory", file_inventory)

addWorksheet(wb, "raw_validation")
writeData(wb, "raw_validation", raw_validation)

addWorksheet(wb, "download_summary")
writeData(wb, "download_summary", download_summary)

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", execution_summary)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:50, widths = "auto")
}

saveWorkbook(wb, excel_output, overwrite = TRUE)

make_docx_report(
  report_path = report_docx,
  regulatory_catalog = regulatory_catalog,
  download_log = download_log,
  file_inventory = file_inventory,
  execution_summary = execution_summary
)

log_lines <- c(
  "============================================================",
  "Script 02 — Regulatory Documentation and Raw Data Ingestion completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Regulatory documents catalogued:", nrow(regulatory_catalog)),
  paste("Download targets:", nrow(download_targets)),
  paste("Successful downloads:", sum(download_log$success, na.rm = TRUE)),
  paste("Failed downloads:", sum(!download_log$success, na.rm = TRUE)),
  paste("Raw files inventoried:", nrow(file_inventory)),
  "",
  "Download summary:",
  capture.output(print(download_summary)),
  "",
  "Main outputs:",
  paste(" -", regulatory_catalog_csv),
  paste(" -", download_targets_csv),
  paste(" -", download_log_csv),
  paste(" -", file_inventory_csv),
  paste(" -", raw_validation_csv),
  paste(" -", download_summary_csv),
  paste(" -", execution_summary_csv),
  paste(" -", excel_output),
  paste(" -", report_docx),
  "",
  "Additional outputs:",
  paste(" -", execution_log_txt)
)

writeLines(log_lines, execution_log_txt, useBytes = TRUE)


# ------------------------------------------------------------
# 12. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 02 — Regulatory Documentation and Raw Data Ingestion completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")
cat("Regulatory documents catalogued:\n", nrow(regulatory_catalog), "\n")
cat("Download targets:\n", nrow(download_targets), "\n")
cat("Successful downloads:\n", sum(download_log$success, na.rm = TRUE), "\n")
cat("Failed downloads:\n", sum(!download_log$success, na.rm = TRUE), "\n")
cat("Raw files inventoried:\n", nrow(file_inventory), "\n\n")

cat("Download summary:\n")
print(download_summary)

cat("\nMain outputs:\n")
cat(" -", regulatory_catalog_csv, "\n")
cat(" -", download_targets_csv, "\n")
cat(" -", download_log_csv, "\n")
cat(" -", file_inventory_csv, "\n")
cat(" -", raw_validation_csv, "\n")
cat(" -", download_summary_csv, "\n")
cat(" -", execution_summary_csv, "\n")
cat(" -", excel_output, "\n")
cat(" -", report_docx, "\n")

cat("\nAdditional outputs:\n")
cat(" -", execution_log_txt, "\n")