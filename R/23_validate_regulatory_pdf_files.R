# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 23 — Validate Regulatory PDF Files
# ============================================================
# Objective:
#   Validate whether files with .pdf extension in regulatory and
#   raw Federal Reserve documentation folders are true PDF files.
#
# Core validation:
#   A valid PDF file should begin with the internal signature "%PDF".
#
# Search locations:
#   docs/regulatory_framework
#   data/raw/fed
#   data/raw/fed/results
#   data/raw/fed/methodology
#
# Main outputs:
#   outputs/publication_package/script23_pdf_validation_audit.csv
#   outputs/publication_package/script23_pdf_validation_summary.csv
#   outputs/publication_package/script23_invalid_pdf_files.csv
#   outputs/publication_package/script23_valid_pdf_files.csv
#   outputs/publication_package/script23_pdf_validation_audit.xlsx
#   outputs/publication_package/script23_execution_log.txt
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 23 - Validate Regulatory PDF Files\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "23"
script_name <- "validate_regulatory_pdf_files"
start_time <- Sys.time()

setwd(project_root)

dir.create("outputs/publication_package", recursive = TRUE, showWarnings = FALSE)

cat("Project root:", project_root, "\n")
cat("Directories checked.\n\n")


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

cat("Loading packages...\n")

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "openxlsx"
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

write_lines_utf8 <- function(lines, path) {
  writeLines(enc2utf8(lines), con = path, useBytes = TRUE)
}

read_pdf_header <- function(path, n_bytes = 8) {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  size <- file.info(path)$size

  if (is.na(size) || size <= 0) {
    return(NA_character_)
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  raw_bytes <- readBin(con, what = "raw", n = min(n_bytes, size))

  if (length(raw_bytes) == 0) {
    return(NA_character_)
  }

  paste0(rawToChar(raw_bytes, multiple = TRUE), collapse = "")
}

validate_pdf_file <- function(path) {
  exists <- file.exists(path)
  size_bytes <- ifelse(exists, file.info(path)$size, NA_real_)
  header <- read_pdf_header(path, n_bytes = 8)

  starts_with_pdf <- !is.na(header) && stringr::str_starts(header, "%PDF")

  validation_status <- dplyr::case_when(
    !exists ~ "MISSING_FILE",
    is.na(size_bytes) ~ "UNKNOWN_SIZE",
    size_bytes == 0 ~ "EMPTY_FILE",
    !starts_with_pdf ~ "INVALID_PDF_SIGNATURE",
    starts_with_pdf & size_bytes < 1024 ~ "VERY_SMALL_PDF_REVIEW",
    starts_with_pdf ~ "VALID_PDF",
    TRUE ~ "UNKNOWN"
  )

  tibble::tibble(
    file_path = path,
    file_name = basename(path),
    relative_directory = dirname(path),
    extension = stringr::str_to_lower(tools::file_ext(path)),
    exists = exists,
    size_bytes = size_bytes,
    modified_at = ifelse(exists, as.character(file.info(path)$mtime), NA_character_),
    pdf_header = ifelse(is.na(header), NA_character_, header),
    starts_with_pdf_signature = starts_with_pdf,
    validation_status = validation_status
  )
}

classify_pdf_location <- function(file_path) {
  file_path_lower <- stringr::str_to_lower(file_path)

  dplyr::case_when(
    stringr::str_starts(file_path_lower, "docs/regulatory_framework") ~
      "REGULATORY_PUBLICATION_FOLDER",

    stringr::str_starts(file_path_lower, "data/raw/fed/results") ~
      "RAW_FED_RESULTS_FOLDER",

    stringr::str_starts(file_path_lower, "data/raw/fed/methodology") ~
      "RAW_FED_METHODOLOGY_FOLDER",

    stringr::str_starts(file_path_lower, "data/raw/fed") ~
      "RAW_FED_FOLDER",

    TRUE ~
      "OTHER_LOCATION"
  )
}

classify_publication_action <- function(location_class, validation_status, file_name) {
  file_name_lower <- stringr::str_to_lower(file_name)

  dplyr::case_when(
    validation_status == "VALID_PDF" &
      location_class == "REGULATORY_PUBLICATION_FOLDER" ~
      "Keep in regulatory publication folder.",

    validation_status == "VALID_PDF" &
      location_class != "REGULATORY_PUBLICATION_FOLDER" ~
      "Valid PDF outside regulatory folder. Keep as raw source or consider copying to docs/regulatory_framework/source_documents if central to documentation.",

    validation_status == "VERY_SMALL_PDF_REVIEW" ~
      "Review manually. Signature is valid but file size is unusually small.",

    validation_status %in% c("INVALID_PDF_SIGNATURE", "EMPTY_FILE", "UNKNOWN_SIZE") &
      stringr::str_detect(file_name_lower, "attempt|download|dictionary|page") ~
      "Move to failed_downloads or remove before publication. Do not present as valid PDF documentation.",

    validation_status %in% c("INVALID_PDF_SIGNATURE", "EMPTY_FILE", "UNKNOWN_SIZE") ~
      "Invalid PDF. Review, replace, move to failed_downloads or remove before publication.",

    validation_status == "MISSING_FILE" ~
      "Missing file. Check path or remove stale reference.",

    TRUE ~
      "Review manually."
  )
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Define search locations
# ------------------------------------------------------------

cat("Defining PDF search locations...\n")

search_locations <- tibble::tibble(
  search_location = c(
    "docs/regulatory_framework",
    "data/raw/fed",
    "data/raw/fed/results",
    "data/raw/fed/methodology"
  ),
  exists = file.exists(search_location)
) |>
  safe_df()

existing_search_locations <- search_locations |>
  dplyr::filter(exists) |>
  dplyr::pull(search_location)

cat("Search locations:\n")
print(search_locations)
cat("\n")


# ------------------------------------------------------------
# 4. Search PDF files
# ------------------------------------------------------------

cat("Searching PDF files...\n")

pdf_files <- character()

for (loc in existing_search_locations) {
  found <- list.files(
    loc,
    pattern = "\\.pdf$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  pdf_files <- c(pdf_files, found)
}

pdf_files <- unique(pdf_files)

cat("PDF files found:", length(pdf_files), "\n\n")


# ------------------------------------------------------------
# 5. Validate PDFs
# ------------------------------------------------------------

cat("Validating PDF signatures...\n")

if (length(pdf_files) == 0) {
  pdf_validation_audit <- tibble::tibble(
    file_path = character(),
    file_name = character(),
    relative_directory = character(),
    extension = character(),
    exists = logical(),
    size_bytes = numeric(),
    modified_at = character(),
    pdf_header = character(),
    starts_with_pdf_signature = logical(),
    validation_status = character()
  )
} else {
  pdf_validation_audit <- dplyr::bind_rows(
    lapply(pdf_files, validate_pdf_file)
  )
}

pdf_validation_audit <- pdf_validation_audit |>
  dplyr::mutate(
    location_class = classify_pdf_location(file_path),
    publication_action = classify_publication_action(
      location_class,
      validation_status,
      file_name
    ),
    publication_blocking_issue = dplyr::case_when(
      location_class == "REGULATORY_PUBLICATION_FOLDER" &
        validation_status %in% c(
          "INVALID_PDF_SIGNATURE",
          "EMPTY_FILE",
          "UNKNOWN_SIZE",
          "MISSING_FILE"
        ) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::arrange(location_class, validation_status, file_name) |>
  safe_df()

cat("PDF validation completed.\n\n")


# ------------------------------------------------------------
# 6. Summaries
# ------------------------------------------------------------

cat("Creating validation summaries...\n")

pdf_validation_summary <- pdf_validation_audit |>
  dplyr::group_by(validation_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(validation_status) |>
  safe_df()

pdf_validation_by_location <- pdf_validation_audit |>
  dplyr::group_by(location_class, validation_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    blocking_issues = sum(publication_blocking_issue, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(location_class, validation_status) |>
  safe_df()

valid_pdf_files <- pdf_validation_audit |>
  dplyr::filter(validation_status == "VALID_PDF") |>
  safe_df()

invalid_pdf_files <- pdf_validation_audit |>
  dplyr::filter(
    validation_status %in% c(
      "INVALID_PDF_SIGNATURE",
      "EMPTY_FILE",
      "UNKNOWN_SIZE",
      "MISSING_FILE"
    )
  ) |>
  safe_df()

small_pdf_review_files <- pdf_validation_audit |>
  dplyr::filter(validation_status == "VERY_SMALL_PDF_REVIEW") |>
  safe_df()

blocking_invalid_pdfs <- pdf_validation_audit |>
  dplyr::filter(publication_blocking_issue) |>
  safe_df()

publication_pdf_readiness <- tibble::tibble(
  item = c(
    "PDF files searched",
    "Valid PDF files",
    "Invalid PDF files",
    "Very small PDFs requiring review",
    "Invalid PDFs in regulatory publication folder",
    "Publication PDF status",
    "Required action"
  ),
  value = c(
    as.character(nrow(pdf_validation_audit)),
    as.character(nrow(valid_pdf_files)),
    as.character(nrow(invalid_pdf_files)),
    as.character(nrow(small_pdf_review_files)),
    as.character(nrow(blocking_invalid_pdfs)),
    ifelse(
      nrow(blocking_invalid_pdfs) == 0,
      "PDF_PUBLICATION_READY",
      "PDF_PUBLICATION_NOT_READY"
    ),
    ifelse(
      nrow(blocking_invalid_pdfs) == 0,
      "No blocking invalid PDFs were found in the regulatory publication folder.",
      "Move, remove or replace invalid PDFs in the regulatory publication folder before GitHub publication."
    )
  )
) |>
  safe_df()

cat("Validation summaries created.\n\n")


# ------------------------------------------------------------
# 7. Recommended cleanup commands
# ------------------------------------------------------------

cat("Creating recommended cleanup table...\n")

recommended_cleanup <- invalid_pdf_files |>
  dplyr::mutate(
    recommended_failed_folder = dplyr::case_when(
      location_class == "REGULATORY_PUBLICATION_FOLDER" ~
        "docs/regulatory_framework/failed_downloads",
      stringr::str_starts(file_path, "data/raw/fed") ~
        "data/raw/fed/failed_downloads",
      TRUE ~
        "failed_downloads"
    ),
    recommended_target_path = file.path(
      recommended_failed_folder,
      file_name
    ),
    recommended_action_type = dplyr::case_when(
      validation_status == "INVALID_PDF_SIGNATURE" ~ "MOVE_INVALID_SIGNATURE",
      validation_status == "EMPTY_FILE" ~ "MOVE_EMPTY_FILE",
      validation_status == "UNKNOWN_SIZE" ~ "MOVE_UNKNOWN_SIZE",
      validation_status == "MISSING_FILE" ~ "REMOVE_STALE_REFERENCE",
      TRUE ~ "REVIEW"
    )
  ) |>
  dplyr::select(
    file_path,
    file_name,
    location_class,
    validation_status,
    size_bytes,
    pdf_header,
    recommended_action_type,
    recommended_failed_folder,
    recommended_target_path,
    publication_action
  ) |>
  safe_df()

cat("Recommended cleanup table created.\n\n")


# ------------------------------------------------------------
# 8. Create PDF validation note
# ------------------------------------------------------------

cat("Creating PDF validation note...\n")

pdf_validation_note_path <- "docs/regulatory_framework/pdf_validation_note.md"

pdf_validation_note_lines <- c(
  "# PDF Validation Note",
  "",
  "This note records the final validation of PDF files before GitHub publication.",
  "",
  "## Validation rule",
  "",
  "A valid PDF file should begin internally with the `%PDF` file signature.",
  "",
  "Files with `.pdf` extension but without the `%PDF` signature should not be presented as valid PDF documentation.",
  "",
  "## Search locations",
  "",
  paste0("- ", search_locations$search_location, " — exists: ", search_locations$exists),
  "",
  "## Validation result",
  "",
  paste0("- PDF files searched: ", nrow(pdf_validation_audit)),
  paste0("- Valid PDF files: ", nrow(valid_pdf_files)),
  paste0("- Invalid PDF files: ", nrow(invalid_pdf_files)),
  paste0("- Very small PDFs requiring review: ", nrow(small_pdf_review_files)),
  paste0("- Invalid PDFs in regulatory publication folder: ", nrow(blocking_invalid_pdfs)),
  "",
  "## Publication status",
  "",
  publication_pdf_readiness$value[publication_pdf_readiness$item == "Publication PDF status"],
  "",
  "## Required action",
  "",
  publication_pdf_readiness$value[publication_pdf_readiness$item == "Required action"],
  "",
  "## Interpretation",
  "",
  "The Windows file association shown in File Explorer does not prove that a file is a valid PDF. The internal file signature is the relevant validation test.",
  "",
  "## Disclaimer",
  "",
  "This validation only checks the technical PDF signature and basic file size. It does not verify the legal completeness, currentness or substantive accuracy of each regulatory document."
)

write_lines_utf8(pdf_validation_note_lines, pdf_validation_note_path)

cat("PDF validation note created.\n\n")


# ------------------------------------------------------------
# 9. Save outputs
# ------------------------------------------------------------

cat("Saving Script 23 outputs...\n")

out_dir <- "outputs/publication_package"

paths_out <- list(
  pdf_validation_audit = file.path(out_dir, "script23_pdf_validation_audit.csv"),
  pdf_validation_summary = file.path(out_dir, "script23_pdf_validation_summary.csv"),
  pdf_validation_by_location = file.path(out_dir, "script23_pdf_validation_by_location.csv"),
  valid_pdf_files = file.path(out_dir, "script23_valid_pdf_files.csv"),
  invalid_pdf_files = file.path(out_dir, "script23_invalid_pdf_files.csv"),
  small_pdf_review_files = file.path(out_dir, "script23_small_pdf_review_files.csv"),
  blocking_invalid_pdfs = file.path(out_dir, "script23_blocking_invalid_pdfs.csv"),
  publication_pdf_readiness = file.path(out_dir, "script23_publication_pdf_readiness.csv"),
  recommended_cleanup = file.path(out_dir, "script23_recommended_pdf_cleanup.csv"),
  excel = file.path(out_dir, "script23_pdf_validation_audit.xlsx"),
  execution_summary = file.path(out_dir, "script23_execution_summary.csv"),
  execution_log = file.path(out_dir, "script23_execution_log.txt")
)

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  pdf_files_searched = nrow(pdf_validation_audit),
  valid_pdf_files = nrow(valid_pdf_files),
  invalid_pdf_files = nrow(invalid_pdf_files),
  very_small_pdf_review_files = nrow(small_pdf_review_files),
  invalid_pdfs_in_regulatory_publication_folder = nrow(blocking_invalid_pdfs),
  publication_pdf_status = publication_pdf_readiness$value[
    publication_pdf_readiness$item == "Publication PDF status"
  ],
  pdf_validation_note_created = file.exists(pdf_validation_note_path)
) |>
  safe_df()

readr::write_csv(pdf_validation_audit, paths_out$pdf_validation_audit)
readr::write_csv(pdf_validation_summary, paths_out$pdf_validation_summary)
readr::write_csv(pdf_validation_by_location, paths_out$pdf_validation_by_location)
readr::write_csv(valid_pdf_files, paths_out$valid_pdf_files)
readr::write_csv(invalid_pdf_files, paths_out$invalid_pdf_files)
readr::write_csv(small_pdf_review_files, paths_out$small_pdf_review_files)
readr::write_csv(blocking_invalid_pdfs, paths_out$blocking_invalid_pdfs)
readr::write_csv(publication_pdf_readiness, paths_out$publication_pdf_readiness)
readr::write_csv(recommended_cleanup, paths_out$recommended_cleanup)
readr::write_csv(execution_summary, paths_out$execution_summary)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  execution_summary = execution_summary,
  publication_pdf_readiness = publication_pdf_readiness,
  pdf_validation_summary = pdf_validation_summary,
  pdf_validation_by_location = pdf_validation_by_location,
  pdf_validation_audit = pdf_validation_audit,
  valid_pdf_files = valid_pdf_files,
  invalid_pdf_files = invalid_pdf_files,
  small_pdf_review = small_pdf_review_files,
  blocking_invalid_pdfs = blocking_invalid_pdfs,
  recommended_cleanup = recommended_cleanup,
  search_locations = search_locations
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, paths_out$excel, overwrite = TRUE)

cat("Script 23 outputs saved.\n\n")


# ------------------------------------------------------------
# 10. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 23 - Validate Regulatory PDF Files completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Search locations:",
  capture.output(print(search_locations)),
  "",
  paste("PDF files searched:", nrow(pdf_validation_audit)),
  paste("Valid PDF files:", nrow(valid_pdf_files)),
  paste("Invalid PDF files:", nrow(invalid_pdf_files)),
  paste("Very small PDFs requiring review:", nrow(small_pdf_review_files)),
  paste("Invalid PDFs in regulatory publication folder:", nrow(blocking_invalid_pdfs)),
  paste(
    "Publication PDF status:",
    publication_pdf_readiness$value[
      publication_pdf_readiness$item == "Publication PDF status"
    ]
  ),
  "",
  "Publication PDF readiness:",
  capture.output(print(publication_pdf_readiness)),
  "",
  "PDF validation summary:",
  capture.output(print(pdf_validation_summary)),
  "",
  "PDF validation by location:",
  capture.output(print(pdf_validation_by_location)),
  "",
  "Invalid PDF files:",
  capture.output(print(invalid_pdf_files)),
  "",
  "Recommended cleanup:",
  capture.output(print(recommended_cleanup)),
  "",
  "Main outputs:",
  paste(" -", paths_out$pdf_validation_audit),
  paste(" -", paths_out$pdf_validation_summary),
  paste(" -", paths_out$pdf_validation_by_location),
  paste(" -", paths_out$valid_pdf_files),
  paste(" -", paths_out$invalid_pdf_files),
  paste(" -", paths_out$blocking_invalid_pdfs),
  paste(" -", paths_out$publication_pdf_readiness),
  paste(" -", paths_out$recommended_cleanup),
  paste(" -", paths_out$excel),
  paste(" -", pdf_validation_note_path),
  paste(" -", paths_out$execution_log)
)

write_lines_utf8(log_lines, paths_out$execution_log)


# ------------------------------------------------------------
# 11. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 23 - Validate Regulatory PDF Files completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("PDF files searched:\n", nrow(pdf_validation_audit), "\n")
cat("Valid PDF files:\n", nrow(valid_pdf_files), "\n")
cat("Invalid PDF files:\n", nrow(invalid_pdf_files), "\n")
cat("Very small PDFs requiring review:\n", nrow(small_pdf_review_files), "\n")
cat("Invalid PDFs in regulatory publication folder:\n", nrow(blocking_invalid_pdfs), "\n")
cat(
  "Publication PDF status:\n",
  publication_pdf_readiness$value[
    publication_pdf_readiness$item == "Publication PDF status"
  ],
  "\n\n"
)

cat("Publication PDF readiness:\n")
print(publication_pdf_readiness)

cat("\nPDF validation summary:\n")
print(pdf_validation_summary)

cat("\nPDF validation by location:\n")
print(pdf_validation_by_location)

cat("\nInvalid PDF files:\n")
print(invalid_pdf_files)

cat("\nRecommended cleanup:\n")
print(recommended_cleanup)

cat("\nMain outputs:\n")
cat(" -", paths_out$pdf_validation_audit, "\n")
cat(" -", paths_out$pdf_validation_summary, "\n")
cat(" -", paths_out$pdf_validation_by_location, "\n")
cat(" -", paths_out$valid_pdf_files, "\n")
cat(" -", paths_out$invalid_pdf_files, "\n")
cat(" -", paths_out$blocking_invalid_pdfs, "\n")
cat(" -", paths_out$publication_pdf_readiness, "\n")
cat(" -", paths_out$recommended_cleanup, "\n")
cat(" -", paths_out$excel, "\n")
cat(" -", pdf_validation_note_path, "\n")
cat(" -", paths_out$execution_log, "\n")
cat("============================================================\n")