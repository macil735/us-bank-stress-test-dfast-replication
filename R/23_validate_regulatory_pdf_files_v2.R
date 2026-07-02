# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 23 v2 — Validate Regulatory PDF Files Excluding Quarantine
# ============================================================
# Objective:
#   Validate all regulatory PDF files that remain in publication
#   and working folders after Script 24.
#
# Key rule:
#   A file with .pdf extension is valid only if its internal
#   binary signature starts with %PDF.
#
# Important:
#   This script excludes:
#     docs/regulatory_framework/failed_downloads/
#
#   Therefore, quarantined files preserved for auditability do
#   not block publication.
#
# This script does not move or delete files.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 23 v2 — Validate Regulatory PDF Files Excluding Quarantine\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

script_id <- "23_v2"
script_name <- "validate_regulatory_pdf_files_excluding_quarantine"
start_time <- Sys.time()

out_dir <- file.path(project_root, "outputs/publication_package")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

note_path <- file.path(
  project_root,
  "docs/regulatory_framework/pdf_validation_note_v2.md"
)


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

write_text_file <- function(path, lines) {
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

normalize_path <- function(path) {
  path |>
    as.character() |>
    stringr::str_replace_all("\\\\", "/")
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

is_inside_excluded_folder <- function(path, excluded_roots) {
  path_norm <- normalize_path(path)
  excluded_norm <- normalize_path(excluded_roots)

  any(
    purrr::map_lgl(
      excluded_norm,
      function(excluded_path) {
        stringr::str_starts(path_norm, excluded_path)
      }
    )
  )
}

empty_pdf_inventory <- function() {
  tibble::tibble(
    source_folder_role = character(),
    source_folder_path = character(),
    full_path = character(),
    relative_path = character(),
    file_name = character(),
    extension = character(),
    size_bytes = numeric(),
    size_kb = numeric(),
    modified_time = character(),
    excluded_from_check = logical(),
    exclusion_reason = character()
  )
}


# ------------------------------------------------------------
# 3. Folders to search and folders to exclude
# ------------------------------------------------------------

folders_to_search <- tibble::tibble(
  source_folder_role = c(
    "REGULATORY_FRAMEWORK",
    "RAW_FED_RESULTS",
    "RAW_FED_METHODOLOGY"
  ),
  source_folder_path = c(
    file.path(project_root, "docs/regulatory_framework"),
    file.path(project_root, "data/raw/fed/results"),
    file.path(project_root, "data/raw/fed/methodology")
  )
) |>
  dplyr::mutate(
    folder_exists = purrr::map_lgl(source_folder_path, dir.exists)
  ) |>
  safe_df()

folders_to_exclude <- tibble::tibble(
  excluded_folder_role = c(
    "FAILED_DOWNLOADS_ALL"
  ),
  excluded_folder_path = c(
    file.path(project_root, "docs/regulatory_framework/failed_downloads")
  )
) |>
  dplyr::mutate(
    folder_exists = purrr::map_lgl(excluded_folder_path, dir.exists)
  ) |>
  safe_df()

excluded_roots <- folders_to_exclude$excluded_folder_path


# ------------------------------------------------------------
# 4. Build candidate PDF inventory
# ------------------------------------------------------------

candidate_pdfs <- purrr::pmap_dfr(
  list(
    folders_to_search$source_folder_role,
    folders_to_search$source_folder_path
  ),
  function(source_folder_role, source_folder_path) {

    if (!dir.exists(source_folder_path)) {
      return(empty_pdf_inventory())
    }

    files <- fs::dir_info(
      path = source_folder_path,
      recurse = TRUE,
      type = "file"
    )

    if (nrow(files) == 0) {
      return(empty_pdf_inventory())
    }

    files |>
      dplyr::filter(stringr::str_to_lower(fs::path_ext(path)) == "pdf") |>
      dplyr::transmute(
        source_folder_role = source_folder_role,
        source_folder_path = source_folder_path,
        full_path = as.character(path),
        relative_path = stringr::str_replace(as.character(path), fixed(project_root), "") |>
          stringr::str_replace("^/", "") |>
          stringr::str_replace("^\\\\", ""),
        file_name = fs::path_file(path),
        extension = fs::path_ext(path),
        size_bytes = as.numeric(size),
        size_kb = round(size_bytes / 1024, 1),
        modified_time = as.character(modification_time),
        excluded_from_check = purrr::map_lgl(
          as.character(path),
          ~ is_inside_excluded_folder(.x, excluded_roots)
        ),
        exclusion_reason = dplyr::if_else(
          excluded_from_check,
          "Inside failed_downloads quarantine/audit folder",
          NA_character_
        )
      )
  }
) |>
  dplyr::distinct(full_path, .keep_all = TRUE) |>
  safe_df()

pdfs_excluded <- candidate_pdfs |>
  dplyr::filter(excluded_from_check == TRUE) |>
  safe_df()

pdfs_to_check <- candidate_pdfs |>
  dplyr::filter(excluded_from_check != TRUE) |>
  safe_df()


# ------------------------------------------------------------
# 5. Validate PDFs outside quarantine
# ------------------------------------------------------------

if (nrow(pdfs_to_check) == 0) {

  pdf_validation_audit <- tibble::tibble(
    source_folder_role = character(),
    source_folder_path = character(),
    full_path = character(),
    relative_path = character(),
    file_name = character(),
    extension = character(),
    size_bytes = numeric(),
    size_kb = numeric(),
    modified_time = character(),
    pdf_signature_valid = logical(),
    validation_status = character(),
    ascii_header = character(),
    hex_header = character(),
    publication_blocking = logical(),
    recommended_action = character()
  )

} else {

  pdf_validation_audit <- pdfs_to_check |>
    dplyr::mutate(
      pdf_signature_valid = purrr::map_lgl(full_path, is_valid_pdf_signature),
      validation_status = dplyr::if_else(
        pdf_signature_valid,
        "VALID_PDF",
        "INVALID_PDF_SIGNATURE"
      ),
      ascii_header = purrr::map_chr(
        full_path,
        ~ raw_to_ascii(read_header_raw(.x, 16))
      ),
      hex_header = purrr::map_chr(
        full_path,
        ~ raw_to_hex(read_header_raw(.x, 16))
      ),
      publication_blocking = validation_status != "VALID_PDF",
      recommended_action = dplyr::case_when(
        validation_status == "VALID_PDF" ~ "Keep file",
        validation_status == "INVALID_PDF_SIGNATURE" ~
          "Move to failed_downloads/invalid_or_unopenable_pdf before publication",
        TRUE ~ "Manual review required"
      )
    ) |>
    dplyr::arrange(validation_status, source_folder_role, file_name) |>
    safe_df()
}


# ------------------------------------------------------------
# 6. Summary tables
# ------------------------------------------------------------

valid_pdf_files <- pdf_validation_audit |>
  dplyr::filter(validation_status == "VALID_PDF") |>
  safe_df()

invalid_pdf_files <- pdf_validation_audit |>
  dplyr::filter(validation_status != "VALID_PDF") |>
  safe_df()

pdf_validation_summary <- pdf_validation_audit |>
  dplyr::group_by(source_folder_role, validation_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    total_size_kb = round(total_size_bytes / 1024, 1),
    publication_blocking_files = sum(publication_blocking == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(source_folder_role, validation_status) |>
  safe_df()

# Defensive scalar counts for publication readiness
folders_searched_n <- nrow(folders_to_search)
folders_searched_existing_n <- sum(folders_to_search$folder_exists == TRUE, na.rm = TRUE)

folders_excluded_n <- nrow(folders_to_exclude)
folders_excluded_existing_n <- sum(folders_to_exclude$folder_exists == TRUE, na.rm = TRUE)

candidate_pdf_files_found_n <- nrow(candidate_pdfs)
pdf_files_excluded_from_check_n <- nrow(pdfs_excluded)
pdf_files_checked_n <- nrow(pdf_validation_audit)
valid_pdf_files_n <- nrow(valid_pdf_files)
invalid_pdf_files_n <- nrow(invalid_pdf_files)

publication_blocking_files_n <- if ("publication_blocking" %in% names(pdf_validation_audit)) {
  sum(pdf_validation_audit$publication_blocking == TRUE, na.rm = TRUE)
} else {
  0
}

publication_pdf_status_value <- dplyr::case_when(
  pdf_files_checked_n == 0 ~ "NO_PDF_FILES_FOUND_FOR_CHECK",
  invalid_pdf_files_n == 0 & publication_blocking_files_n == 0 ~ "PDF_PUBLICATION_READY",
  TRUE ~ "PDF_PUBLICATION_NOT_READY"
)

publication_pdf_readiness <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  folders_searched = folders_searched_n,
  folders_searched_existing = folders_searched_existing_n,
  folders_excluded = folders_excluded_n,
  folders_excluded_existing = folders_excluded_existing_n,
  candidate_pdf_files_found = candidate_pdf_files_found_n,
  pdf_files_excluded_from_check = pdf_files_excluded_from_check_n,
  pdf_files_checked = pdf_files_checked_n,
  valid_pdf_files = valid_pdf_files_n,
  invalid_pdf_files = invalid_pdf_files_n,
  publication_blocking_files = publication_blocking_files_n,
  publication_pdf_status = publication_pdf_status_value,
  validation_rule = "PDF files are valid only if the internal file signature begins with %PDF. Files inside failed_downloads are excluded from publication blocking checks."
) |>
  safe_df()

# ------------------------------------------------------------
# 7. PDF validation note
# ------------------------------------------------------------

note_lines <- c(
  "# PDF Validation Note — Script 23 v2",
  "",
  paste("Generated on", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Purpose",
  "",
  "This note documents the validation of regulatory PDF files after the cleanup performed in Script 24.",
  "",
  "## Validation rule",
  "",
  "A file with `.pdf` extension is considered valid only if its internal file signature begins with `%PDF`.",
  "",
  "## Excluded quarantine folder",
  "",
  "The following folder is excluded from publication blocking checks:",
  "",
  paste0("- `", folders_to_exclude$excluded_folder_path, "`"),
  "",
  "Files in this folder are retained for auditability but should not be cited as valid regulatory documents.",
  "",
  "## Result",
  "",
  paste("- Candidate PDF files found:", nrow(candidate_pdfs)),
  paste("- PDF files excluded from check:", nrow(pdfs_excluded)),
  paste("- PDF files checked:", nrow(pdf_validation_audit)),
  paste("- Valid PDF files:", nrow(valid_pdf_files)),
  paste("- Invalid PDF files:", nrow(invalid_pdf_files)),
  paste("- Publication blocking files:", sum(pdf_validation_audit$publication_blocking == TRUE, na.rm = TRUE)),
  paste("- Publication PDF status:", publication_pdf_readiness$publication_pdf_status),
  "",
  "## Publication implication",
  "",
  "Only PDFs outside quarantine folders are considered part of the publication documentation set."
)

write_text_file(note_path, note_lines)


# ------------------------------------------------------------
# 8. Save outputs
# ------------------------------------------------------------

audit_csv <- file.path(out_dir, "script23_v2_pdf_validation_audit.csv")
summary_csv <- file.path(out_dir, "script23_v2_pdf_validation_summary.csv")
valid_csv <- file.path(out_dir, "script23_v2_valid_pdf_files.csv")
invalid_csv <- file.path(out_dir, "script23_v2_invalid_pdf_files.csv")
excluded_csv <- file.path(out_dir, "script23_v2_excluded_quarantine_pdf_files.csv")
candidate_csv <- file.path(out_dir, "script23_v2_candidate_pdf_files.csv")
readiness_csv <- file.path(out_dir, "script23_v2_publication_pdf_readiness.csv")
search_folders_csv <- file.path(out_dir, "script23_v2_search_folders.csv")
excluded_folders_csv <- file.path(out_dir, "script23_v2_excluded_folders.csv")
excel_output <- file.path(out_dir, "script23_v2_pdf_validation_audit.xlsx")
execution_log_txt <- file.path(out_dir, "script23_v2_execution_log.txt")

readr::write_csv(pdf_validation_audit, audit_csv)
readr::write_csv(pdf_validation_summary, summary_csv)
readr::write_csv(valid_pdf_files, valid_csv)
readr::write_csv(invalid_pdf_files, invalid_csv)
readr::write_csv(pdfs_excluded, excluded_csv)
readr::write_csv(candidate_pdfs, candidate_csv)
readr::write_csv(publication_pdf_readiness, readiness_csv)
readr::write_csv(folders_to_search, search_folders_csv)
readr::write_csv(folders_to_exclude, excluded_folders_csv)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  publication_readiness = publication_pdf_readiness,
  validation_summary = pdf_validation_summary,
  validation_audit = pdf_validation_audit,
  valid_pdf_files = valid_pdf_files,
  invalid_pdf_files = invalid_pdf_files,
  excluded_quarantine = pdfs_excluded,
  candidate_pdf_files = candidate_pdfs,
  search_folders = folders_to_search,
  excluded_folders = folders_to_exclude
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output, overwrite = TRUE)


# ------------------------------------------------------------
# 9. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 23 v2 — Validate Regulatory PDF Files Excluding Quarantine",
  "============================================================",
  paste("Project root:", project_root),
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Publication PDF readiness:",
  capture.output(print(publication_pdf_readiness)),
  "",
  "PDF validation summary:",
  capture.output(print(pdf_validation_summary)),
  "",
  "Invalid PDF files outside quarantine:",
  capture.output(print(invalid_pdf_files)),
  "",
  "Excluded quarantine PDF files:",
  capture.output(print(pdfs_excluded |> dplyr::select(relative_path, size_kb, exclusion_reason))),
  "",
  "Main outputs:",
  paste(" -", audit_csv),
  paste(" -", summary_csv),
  paste(" -", valid_csv),
  paste(" -", invalid_csv),
  paste(" -", excluded_csv),
  paste(" -", candidate_csv),
  paste(" -", readiness_csv),
  paste(" -", search_folders_csv),
  paste(" -", excluded_folders_csv),
  paste(" -", excel_output),
  paste(" -", note_path),
  paste(" -", execution_log_txt)
)

writeLines(enc2utf8(log_lines), execution_log_txt, useBytes = TRUE)


# ------------------------------------------------------------
# 10. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 23 v2 — Validate Regulatory PDF Files Excluding Quarantine completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Publication PDF readiness:\n")
print(publication_pdf_readiness)

cat("\nPDF validation summary:\n")
print(pdf_validation_summary)

cat("\nInvalid PDF files outside quarantine:\n")
print(
  invalid_pdf_files |>
    dplyr::select(
      source_folder_role,
      file_name,
      size_kb,
      validation_status,
      relative_path,
      recommended_action
    )
)

cat("\nExcluded quarantine PDF files:\n")
print(
  pdfs_excluded |>
    dplyr::select(
      file_name,
      size_kb,
      relative_path,
      exclusion_reason
    )
)

cat("\nPDF validation note:\n")
cat(" -", note_path, "\n")

cat("\nMain outputs:\n")
cat(" -", audit_csv, "\n")
cat(" -", summary_csv, "\n")
cat(" -", valid_csv, "\n")
cat(" -", invalid_csv, "\n")
cat(" -", excluded_csv, "\n")
cat(" -", candidate_csv, "\n")
cat(" -", readiness_csv, "\n")
cat(" -", search_folders_csv, "\n")
cat(" -", excluded_folders_csv, "\n")
cat(" -", excel_output, "\n")
cat(" -", execution_log_txt, "\n")
cat("============================================================\n")