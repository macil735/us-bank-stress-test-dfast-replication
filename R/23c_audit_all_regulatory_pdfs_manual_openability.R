# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 23c — Audit All Regulatory PDFs Against Manual Openability List
# ============================================================
# Objective:
#   Audit all regulatory PDF files across key project folders by
#   combining:
#     1. automatic PDF signature validation;
#     2. manual openability status reported by the analyst;
#     3. publication decision rules.
#
# Key rule:
#   A PDF is publication-ready only if:
#     - it starts with the internal %PDF signature; and
#     - it is not manually classified as "não abre".
#
# This script DOES NOT move or delete files.
# It only creates an audit and recommendation table.
#
# Project root:
#   D:/GitHub/us-bank-stress-test-dfast-replication
#
# Main outputs:
#   outputs/publication_package/script23c_all_regulatory_pdf_audit.csv
#   outputs/publication_package/script23c_all_regulatory_pdf_summary.csv
#   outputs/publication_package/script23c_pdfs_to_keep.csv
#   outputs/publication_package/script23c_pdfs_to_quarantine.csv
#   outputs/publication_package/script23c_manual_unmatched_entries.csv
#   outputs/publication_package/script23c_all_regulatory_pdf_audit.xlsx
#   outputs/publication_package/script23c_execution_log.txt
# ============================================================


# ------------------------------------------------------------
# 0. Initial setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 23c — Audit All Regulatory PDFs Against Manual List\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

out_dir <- file.path(project_root, "outputs/publication_package")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

start_time <- Sys.time()


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "purrr",
  "fs",
  "openxlsx"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
  library(stringr)
  library(purrr)
  library(fs)
  library(openxlsx)
})


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

read_header_raw <- function(path, n_bytes = 16) {
  if (!file.exists(path)) return(raw(0))

  size <- file.info(path)$size
  if (is.na(size) || size <= 0) return(raw(0))

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  readBin(con, what = "raw", n = min(n_bytes, size))
}

raw_to_ascii <- function(raw_bytes) {
  if (length(raw_bytes) == 0) return(NA_character_)

  ints <- as.integer(raw_bytes)

  chars <- vapply(
    ints,
    function(i) {
      if (!is.na(i) && i >= 32 && i <= 126) {
        intToUtf8(i)
      } else {
        "."
      }
    },
    character(1)
  )

  paste0(chars, collapse = "")
}

raw_to_hex <- function(raw_bytes) {
  if (length(raw_bytes) == 0) return(NA_character_)
  paste(sprintf("%02X", as.integer(raw_bytes)), collapse = " ")
}

is_valid_pdf_signature <- function(path) {
  raw_bytes <- read_header_raw(path, n_bytes = 4)

  if (length(raw_bytes) < 4) return(FALSE)

  identical(
    as.integer(raw_bytes[1:4]),
    as.integer(charToRaw("%PDF"))
  )
}

normalize_status <- function(x) {
  x <- safe_chr(x)
  x_lower <- stringr::str_to_lower(x)

  dplyr::case_when(
    stringr::str_detect(x_lower, "não") |
      stringr::str_detect(x_lower, "nao") |
      stringr::str_detect(x_lower, "not") ~ "NAO_ABRE",
    stringr::str_detect(x_lower, "sim") |
      stringr::str_detect(x_lower, "abre") ~ "SIM_ABRE",
    TRUE ~ "UNKNOWN"
  )
}


# ------------------------------------------------------------
# 3. Folders to audit
# ------------------------------------------------------------

folders_to_audit <- tibble::tribble(
  ~folder_role, ~folder_path,

  "REGULATORY_PUBLICATION_FEDERAL_RESERVE",
  file.path(project_root, "docs/regulatory_framework/source_documents/federal_reserve"),

  "RAW_FED_METHODOLOGY",
  file.path(project_root, "data/raw/fed/methodology"),

  "RAW_FED_RESULTS",
  file.path(project_root, "data/raw/fed/results")
) |>
  dplyr::mutate(
    folder_exists = dir.exists(folder_path)
  ) |>
  safe_df()


# ------------------------------------------------------------
# 4. Manual openability list
# ------------------------------------------------------------
# This table reflects the manual status supplied by the analyst.
# "SIM_ABRE" means the PDF opens manually.
# "NAO_ABRE" means the PDF does not open manually.

manual_openability <- tibble::tribble(
  ~folder_role, ~file_name, ~manual_status_raw,

  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_aggregation_models.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_credit_risk_models.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_Detailed_Nine_Quarter_Paths_dictionary.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_detailed_nine_quarter_paths_dictionary_attempt_01.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_dfast_results.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_dfast_results_report.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_final_supervisory_stress_test_scenarios.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_global_market_shock_model.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_macroeconomic_model_guide.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_market_risk_models.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_operational_risk_model.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_ppnr_models.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_supervisory_stress_test_methodology.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_2026_supervisory_stress_test_models_glossary.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_detailed_nine_quarter_path_data_dictionary_attempt_03.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_detailed_nine_quarter_path_data_dictionary_december_2025.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_large_bank_capital_requirements_20250627.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_large_bank_capital_requirements_20250829.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_large_bank_capital_requirements_20250930.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_large_bank_capital_requirements_20260624.pdf", "sim abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_public_results_DFAST_2026_dictionary.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_public_results_DFAST_2026_dictionary_attempt_01.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_public_results_DFAST_2026_dictionary_attempt_02.pdf", "não abre",
  "REGULATORY_PUBLICATION_FEDERAL_RESERVE", "fed_stress_test_results_data_dictionary_attempt_03.pdf", "não abre",

  "RAW_FED_METHODOLOGY", "fed_2026_dfast_results_report.pdf", "sim abre",
  "RAW_FED_METHODOLOGY", "fed_2026_supervisory_stress_test_methodology.pdf", "sim abre",

  "RAW_FED_RESULTS", "fed_2026_Detailed_Nine_Quarter_Paths_dictionary.pdf", "não abre",
  "RAW_FED_RESULTS", "fed_public_results_DFAST_2026_dictionary.pdf", "não abre"
) |>
  dplyr::mutate(
    manual_status = normalize_status(manual_status_raw)
  ) |>
  safe_df()


# ------------------------------------------------------------
# 5. Inventory all PDFs in audited folders
# ------------------------------------------------------------

pdf_inventory <- purrr::pmap_dfr(
  list(folders_to_audit$folder_role, folders_to_audit$folder_path),
  function(folder_role, folder_path) {
    if (!dir.exists(folder_path)) {
      return(
        tibble(
          folder_role = folder_role,
          folder_path = folder_path,
          file_name = character(),
          full_path = character(),
          relative_path = character(),
          extension = character(),
          size_bytes = numeric(),
          size_kb = numeric(),
          modified_time = character()
        )
      )
    }

    files <- fs::dir_info(
      path = folder_path,
      recurse = FALSE,
      type = "file"
    )

    if (nrow(files) == 0) {
      return(
        tibble(
          folder_role = folder_role,
          folder_path = folder_path,
          file_name = character(),
          full_path = character(),
          relative_path = character(),
          extension = character(),
          size_bytes = numeric(),
          size_kb = numeric(),
          modified_time = character()
        )
      )
    }

    files |>
      dplyr::filter(stringr::str_to_lower(fs::path_ext(path)) == "pdf") |>
      dplyr::transmute(
        folder_role = folder_role,
        folder_path = folder_path,
        file_name = fs::path_file(path),
        full_path = as.character(path),
        relative_path = stringr::str_replace(as.character(path), fixed(project_root), "") |>
          stringr::str_replace("^/", "") |>
          stringr::str_replace("^\\\\", ""),
        extension = fs::path_ext(path),
        size_bytes = as.numeric(size),
        size_kb = round(size_bytes / 1024, 1),
        modified_time = as.character(modification_time)
      )
  }
) |>
  safe_df()


# ------------------------------------------------------------
# 6. Automatic PDF validation
# ------------------------------------------------------------

if (nrow(pdf_inventory) == 0) {
  pdf_audit <- tibble(
    folder_role = character(),
    folder_path = character(),
    file_name = character(),
    full_path = character(),
    relative_path = character(),
    extension = character(),
    size_bytes = numeric(),
    size_kb = numeric(),
    modified_time = character(),
    pdf_signature_valid = logical(),
    pdf_signature_status = character(),
    ascii_header = character(),
    hex_header = character(),
    manual_status_raw = character(),
    manual_status = character(),
    publication_decision = character(),
    recommended_action = character(),
    decision_reason = character()
  )
} else {
  pdf_audit <- pdf_inventory |>
    dplyr::mutate(
      pdf_signature_valid = purrr::map_lgl(full_path, is_valid_pdf_signature),
      pdf_signature_status = dplyr::if_else(
        pdf_signature_valid,
        "VALID_PDF_SIGNATURE",
        "INVALID_PDF_SIGNATURE"
      ),
      ascii_header = purrr::map_chr(full_path, ~ raw_to_ascii(read_header_raw(.x, 16))),
      hex_header = purrr::map_chr(full_path, ~ raw_to_hex(read_header_raw(.x, 16)))
    ) |>
    dplyr::left_join(
      manual_openability |>
        dplyr::select(folder_role, file_name, manual_status_raw, manual_status),
      by = c("folder_role", "file_name")
    ) |>
    dplyr::mutate(
      manual_status = dplyr::if_else(
        is.na(manual_status),
        "NOT_MANUALLY_CLASSIFIED",
        manual_status
      ),
      manual_status_raw = dplyr::if_else(
        is.na(manual_status_raw),
        "not classified",
        manual_status_raw
      ),
      publication_decision = dplyr::case_when(
        manual_status == "NAO_ABRE" ~ "QUARANTINE",
        pdf_signature_valid == FALSE ~ "QUARANTINE",
        pdf_signature_valid == TRUE & manual_status %in% c("SIM_ABRE", "NOT_MANUALLY_CLASSIFIED") ~ "KEEP",
        TRUE ~ "REVIEW"
      ),
      recommended_action = dplyr::case_when(
        publication_decision == "KEEP" ~ "Keep in current folder",
        publication_decision == "QUARANTINE" ~ "Move to failed_downloads/invalid_or_unopenable_pdf",
        TRUE ~ "Manual review required"
      ),
      decision_reason = dplyr::case_when(
        manual_status == "NAO_ABRE" & pdf_signature_valid == FALSE ~
          "Manual status says file does not open and PDF signature is invalid.",
        manual_status == "NAO_ABRE" & pdf_signature_valid == TRUE ~
          "Manual status says file does not open despite valid PDF signature.",
        manual_status != "NAO_ABRE" & pdf_signature_valid == FALSE ~
          "PDF signature is invalid.",
        publication_decision == "KEEP" & manual_status == "SIM_ABRE" ~
          "PDF signature is valid and file opens manually.",
        publication_decision == "KEEP" & manual_status == "NOT_MANUALLY_CLASSIFIED" ~
          "PDF signature is valid but file was not manually classified.",
        TRUE ~
          "Requires review."
      )
    ) |>
    dplyr::arrange(publication_decision, folder_role, file_name) |>
    safe_df()
}


# ------------------------------------------------------------
# 7. Manual entries not found in inventory
# ------------------------------------------------------------

manual_unmatched_entries <- manual_openability |>
  dplyr::anti_join(
    pdf_inventory |>
      dplyr::select(folder_role, file_name),
    by = c("folder_role", "file_name")
  ) |>
  dplyr::arrange(folder_role, file_name) |>
  safe_df()


# ------------------------------------------------------------
# 8. Summary tables
# ------------------------------------------------------------

audit_summary <- pdf_audit |>
  dplyr::group_by(folder_role, publication_decision, pdf_signature_status, manual_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    total_size_kb = round(total_size_bytes / 1024, 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(folder_role, publication_decision, pdf_signature_status, manual_status) |>
  safe_df()

global_summary <- tibble::tibble(
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  folders_audited = nrow(folders_to_audit),
  folders_existing = sum(folders_to_audit$folder_exists, na.rm = TRUE),
  pdf_files_found = nrow(pdf_audit),
  pdfs_to_keep = sum(pdf_audit$publication_decision == "KEEP", na.rm = TRUE),
  pdfs_to_quarantine = sum(pdf_audit$publication_decision == "QUARANTINE", na.rm = TRUE),
  pdfs_to_review = sum(pdf_audit$publication_decision == "REVIEW", na.rm = TRUE),
  valid_pdf_signatures = sum(pdf_audit$pdf_signature_valid == TRUE, na.rm = TRUE),
  invalid_pdf_signatures = sum(pdf_audit$pdf_signature_valid == FALSE, na.rm = TRUE),
  manual_entries = nrow(manual_openability),
  manual_entries_unmatched = nrow(manual_unmatched_entries),
  publication_readiness = dplyr::case_when(
    sum(pdf_audit$publication_decision == "QUARANTINE", na.rm = TRUE) == 0 &
      sum(pdf_audit$publication_decision == "REVIEW", na.rm = TRUE) == 0 ~
      "PDF_PUBLICATION_READY",
    TRUE ~
      "PDF_PUBLICATION_CLEANUP_REQUIRED"
  )
) |>
  safe_df()

pdfs_to_keep <- pdf_audit |>
  dplyr::filter(publication_decision == "KEEP") |>
  safe_df()

pdfs_to_quarantine <- pdf_audit |>
  dplyr::filter(publication_decision == "QUARANTINE") |>
  safe_df()

pdfs_to_review <- pdf_audit |>
  dplyr::filter(publication_decision == "REVIEW") |>
  safe_df()


# ------------------------------------------------------------
# 9. Save outputs
# ------------------------------------------------------------

audit_csv <- file.path(out_dir, "script23c_all_regulatory_pdf_audit.csv")
summary_csv <- file.path(out_dir, "script23c_all_regulatory_pdf_summary.csv")
global_summary_csv <- file.path(out_dir, "script23c_global_summary.csv")
keep_csv <- file.path(out_dir, "script23c_pdfs_to_keep.csv")
quarantine_csv <- file.path(out_dir, "script23c_pdfs_to_quarantine.csv")
review_csv <- file.path(out_dir, "script23c_pdfs_to_review.csv")
manual_unmatched_csv <- file.path(out_dir, "script23c_manual_unmatched_entries.csv")
folders_csv <- file.path(out_dir, "script23c_folders_audited.csv")
manual_csv <- file.path(out_dir, "script23c_manual_openability_list.csv")
excel_output <- file.path(out_dir, "script23c_all_regulatory_pdf_audit.xlsx")
execution_log_txt <- file.path(out_dir, "script23c_execution_log.txt")

readr::write_csv(pdf_audit, audit_csv)
readr::write_csv(audit_summary, summary_csv)
readr::write_csv(global_summary, global_summary_csv)
readr::write_csv(pdfs_to_keep, keep_csv)
readr::write_csv(pdfs_to_quarantine, quarantine_csv)
readr::write_csv(pdfs_to_review, review_csv)
readr::write_csv(manual_unmatched_entries, manual_unmatched_csv)
readr::write_csv(folders_to_audit, folders_csv)
readr::write_csv(manual_openability, manual_csv)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  global_summary = global_summary,
  audit_summary = audit_summary,
  pdf_audit = pdf_audit,
  pdfs_to_keep = pdfs_to_keep,
  pdfs_to_quarantine = pdfs_to_quarantine,
  pdfs_to_review = pdfs_to_review,
  manual_unmatched = manual_unmatched_entries,
  folders_audited = folders_to_audit,
  manual_openability = manual_openability
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output, overwrite = TRUE)


# ------------------------------------------------------------
# 10. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 23c — Audit All Regulatory PDFs Against Manual List",
  "============================================================",
  paste("Project root:", project_root),
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Global summary:",
  capture.output(print(global_summary)),
  "",
  "Audit summary:",
  capture.output(print(audit_summary)),
  "",
  "PDFs to quarantine:",
  capture.output(print(pdfs_to_quarantine |> dplyr::select(folder_role, file_name, pdf_signature_status, manual_status, recommended_action))),
  "",
  "Manual unmatched entries:",
  capture.output(print(manual_unmatched_entries)),
  "",
  "Main outputs:",
  paste(" -", audit_csv),
  paste(" -", summary_csv),
  paste(" -", global_summary_csv),
  paste(" -", keep_csv),
  paste(" -", quarantine_csv),
  paste(" -", review_csv),
  paste(" -", manual_unmatched_csv),
  paste(" -", folders_csv),
  paste(" -", manual_csv),
  paste(" -", excel_output),
  paste(" -", execution_log_txt)
)

writeLines(enc2utf8(log_lines), execution_log_txt, useBytes = TRUE)


# ------------------------------------------------------------
# 11. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 23c — Audit All Regulatory PDFs Against Manual List completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Global summary:\n")
print(global_summary)

cat("\nPDFs to quarantine:\n")
print(
  pdfs_to_quarantine |>
    dplyr::select(
      folder_role,
      file_name,
      size_kb,
      pdf_signature_status,
      manual_status,
      recommended_action
    )
)

cat("\nPDFs to keep:\n")
print(
  pdfs_to_keep |>
    dplyr::select(
      folder_role,
      file_name,
      size_kb,
      pdf_signature_status,
      manual_status
    )
)

cat("\nManual entries not found in PDF inventory:\n")
print(manual_unmatched_entries)

cat("\nMain outputs:\n")
cat(" -", audit_csv, "\n")
cat(" -", summary_csv, "\n")
cat(" -", global_summary_csv, "\n")
cat(" -", keep_csv, "\n")
cat(" -", quarantine_csv, "\n")
cat(" -", review_csv, "\n")
cat(" -", manual_unmatched_csv, "\n")
cat(" -", folders_csv, "\n")
cat(" -", manual_csv, "\n")
cat(" -", excel_output, "\n")
cat(" -", execution_log_txt, "\n")
cat("============================================================\n")