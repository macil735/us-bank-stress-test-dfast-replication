# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 24 — Clean Regulatory Documentation After PDF Audit
# ============================================================
# Objective:
#   Move invalid or manually unopenable PDF files to quarantine
#   after Script 23c.
#
# This script DOES NOT delete files.
# It only moves files classified as QUARANTINE by:
#
#   outputs/publication_package/script23c_pdfs_to_quarantine.csv
#
# It preserves:
#   - valid PDFs;
#   - legitimate HTML files;
#   - legitimate CSV files;
#   - all files not explicitly listed for quarantine.
#
# Quarantine folder:
#   docs/regulatory_framework/failed_downloads/invalid_or_unopenable_pdf/
#
# Main outputs:
#   outputs/publication_package/script24_cleaning_summary.csv
#   outputs/publication_package/script24_quarantine_move_log.csv
#   outputs/publication_package/script24_post_cleaning_pdf_inventory.csv
#   outputs/publication_package/script24_post_cleaning_pdf_summary.csv
#   outputs/publication_package/script24_cleaning_outputs.xlsx
#   docs/regulatory_framework/regulatory_pdf_cleanup_note.md
#   outputs/publication_package/script24_execution_log.txt
# ============================================================


# ------------------------------------------------------------
# 0. Initial setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 24 — Clean Regulatory Documentation After PDF Audit\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

start_time <- Sys.time()

out_dir <- file.path(project_root, "outputs/publication_package")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

quarantine_dir <- file.path(
  project_root,
  "docs/regulatory_framework/failed_downloads/invalid_or_unopenable_pdf"
)

dir.create(quarantine_dir, recursive = TRUE, showWarnings = FALSE)

input_quarantine_csv <- file.path(
  out_dir,
  "script23c_pdfs_to_quarantine.csv"
)

regulatory_note_path <- file.path(
  project_root,
  "docs/regulatory_framework/regulatory_pdf_cleanup_note.md"
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

make_unique_quarantine_path <- function(original_path, folder_role, file_name) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  safe_folder_role <- stringr::str_replace_all(folder_role, "[^A-Za-z0-9_\\-]", "_")
  safe_file_name <- stringr::str_replace_all(file_name, "[^A-Za-z0-9_\\.\\-]", "_")

  target_name <- paste0(
    safe_folder_role,
    "__",
    tools::file_path_sans_ext(safe_file_name),
    "__quarantined_",
    timestamp,
    ".",
    tools::file_ext(safe_file_name)
  )

  target_path <- file.path(quarantine_dir, target_name)

  counter <- 1

  while (file.exists(target_path)) {
    target_name <- paste0(
      safe_folder_role,
      "__",
      tools::file_path_sans_ext(safe_file_name),
      "__quarantined_",
      timestamp,
      "_",
      counter,
      ".",
      tools::file_ext(safe_file_name)
    )

    target_path <- file.path(quarantine_dir, target_name)
    counter <- counter + 1
  }

  target_path
}

move_one_file_to_quarantine <- function(full_path,
                                        folder_role,
                                        file_name,
                                        pdf_signature_status,
                                        manual_status,
                                        decision_reason) {

  full_path <- safe_chr(full_path)
  folder_role <- safe_chr(folder_role)
  file_name <- safe_chr(file_name)

  file_exists_before <- file.exists(full_path)
  size_before <- ifelse(
    file_exists_before,
    file.info(full_path)$size,
    NA_real_
  )

  signature_before <- ifelse(
    file_exists_before,
    ifelse(is_valid_pdf_signature(full_path), "VALID_PDF_SIGNATURE", "INVALID_PDF_SIGNATURE"),
    "MISSING_FILE"
  )

  ascii_header_before <- ifelse(
    file_exists_before,
    raw_to_ascii(read_header_raw(full_path, 16)),
    NA_character_
  )

  hex_header_before <- ifelse(
    file_exists_before,
    raw_to_hex(read_header_raw(full_path, 16)),
    NA_character_
  )

  target_path <- make_unique_quarantine_path(
    original_path = full_path,
    folder_role = folder_role,
    file_name = file_name
  )

  moved <- FALSE
  copied_then_deleted <- FALSE
  error_message <- NA_character_

  if (!file_exists_before) {
    error_message <- "Source file did not exist at cleaning time."
  } else {
    moved <- suppressWarnings(file.rename(full_path, target_path))

    if (!moved) {
      copy_ok <- suppressWarnings(file.copy(full_path, target_path, overwrite = TRUE))

      if (copy_ok) {
        unlink(full_path)
        moved <- !file.exists(full_path) && file.exists(target_path)
        copied_then_deleted <- moved
      } else {
        error_message <- "file.rename and file.copy both failed."
      }
    }
  }

  file_exists_after_source <- file.exists(full_path)
  file_exists_after_target <- file.exists(target_path)

  tibble::tibble(
    folder_role = folder_role,
    file_name = file_name,
    source_path = full_path,
    quarantine_path = target_path,
    file_exists_before = file_exists_before,
    file_exists_after_source = file_exists_after_source,
    file_exists_after_target = file_exists_after_target,
    size_bytes_before = size_before,
    size_kb_before = round(size_before / 1024, 1),
    pdf_signature_status_script23c = pdf_signature_status,
    pdf_signature_status_before_move = signature_before,
    manual_status = manual_status,
    decision_reason = decision_reason,
    ascii_header_before = ascii_header_before,
    hex_header_before = hex_header_before,
    moved_to_quarantine = moved,
    copied_then_deleted = copied_then_deleted,
    error_message = error_message,
    moved_at = as.character(Sys.time())
  )
}

inventory_pdfs <- function(folders) {
  purrr::pmap_dfr(
    list(folders$folder_role, folders$folder_path),
    function(folder_role, folder_path) {
      if (!dir.exists(folder_path)) {
        return(
          tibble::tibble(
            folder_role = folder_role,
            folder_path = folder_path,
            file_name = character(),
            full_path = character(),
            relative_path = character(),
            extension = character(),
            size_bytes = numeric(),
            size_kb = numeric(),
            pdf_signature_valid = logical(),
            pdf_signature_status = character(),
            ascii_header = character(),
            hex_header = character()
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
          tibble::tibble(
            folder_role = folder_role,
            folder_path = folder_path,
            file_name = character(),
            full_path = character(),
            relative_path = character(),
            extension = character(),
            size_bytes = numeric(),
            size_kb = numeric(),
            pdf_signature_valid = logical(),
            pdf_signature_status = character(),
            ascii_header = character(),
            hex_header = character()
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
          pdf_signature_valid = purrr::map_lgl(as.character(path), is_valid_pdf_signature),
          pdf_signature_status = dplyr::if_else(
            pdf_signature_valid,
            "VALID_PDF_SIGNATURE",
            "INVALID_PDF_SIGNATURE"
          ),
          ascii_header = purrr::map_chr(as.character(path), ~ raw_to_ascii(read_header_raw(.x, 16))),
          hex_header = purrr::map_chr(as.character(path), ~ raw_to_hex(read_header_raw(.x, 16)))
        )
    }
  ) |>
    safe_df()
}


# ------------------------------------------------------------
# 3. Folders audited by Script 23c
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
  dplyr::mutate(folder_exists = dir.exists(folder_path)) |>
  safe_df()


# ------------------------------------------------------------
# 4. Load quarantine list from Script 23c
# ------------------------------------------------------------

if (!file.exists(input_quarantine_csv)) {
  stop(
    paste0(
      "Required input not found: ",
      input_quarantine_csv,
      "\nRun Script 23c before Script 24."
    )
  )
}

quarantine_input <- readr::read_csv(
  input_quarantine_csv,
  show_col_types = FALSE
) |>
  safe_df()

required_input_cols <- c(
  "folder_role",
  "file_name",
  "full_path",
  "pdf_signature_status",
  "manual_status",
  "decision_reason",
  "publication_decision"
)

missing_cols <- setdiff(required_input_cols, names(quarantine_input))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "Script 23c quarantine input is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

quarantine_input <- quarantine_input |>
  dplyr::filter(publication_decision == "QUARANTINE") |>
  dplyr::distinct(folder_role, file_name, full_path, .keep_all = TRUE) |>
  safe_df()

quarantine_input_n <- nrow(quarantine_input)


# ------------------------------------------------------------
# 5. Pre-cleaning inventory
# ------------------------------------------------------------

pre_cleaning_inventory <- inventory_pdfs(folders_to_audit)

pre_cleaning_summary <- pre_cleaning_inventory |>
  dplyr::group_by(folder_role, pdf_signature_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    total_size_kb = round(total_size_bytes / 1024, 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(folder_role, pdf_signature_status) |>
  safe_df()


# ------------------------------------------------------------
# 6. Move quarantine files
# ------------------------------------------------------------

cat("\nFiles listed for quarantine by Script 23c:", quarantine_input_n, "\n")

if (quarantine_input_n == 0) {
  move_log <- tibble::tibble(
    folder_role = character(),
    file_name = character(),
    source_path = character(),
    quarantine_path = character(),
    file_exists_before = logical(),
    file_exists_after_source = logical(),
    file_exists_after_target = logical(),
    size_bytes_before = numeric(),
    size_kb_before = numeric(),
    pdf_signature_status_script23c = character(),
    pdf_signature_status_before_move = character(),
    manual_status = character(),
    decision_reason = character(),
    ascii_header_before = character(),
    hex_header_before = character(),
    moved_to_quarantine = logical(),
    copied_then_deleted = logical(),
    error_message = character(),
    moved_at = character()
  )
} else {
  move_log <- purrr::pmap_dfr(
    list(
      quarantine_input$full_path,
      quarantine_input$folder_role,
      quarantine_input$file_name,
      quarantine_input$pdf_signature_status,
      quarantine_input$manual_status,
      quarantine_input$decision_reason
    ),
    move_one_file_to_quarantine
  ) |>
    safe_df()
}


# ------------------------------------------------------------
# 7. Post-cleaning inventory
# ------------------------------------------------------------

post_cleaning_inventory <- inventory_pdfs(folders_to_audit)

post_cleaning_summary <- post_cleaning_inventory |>
  dplyr::group_by(folder_role, pdf_signature_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    total_size_kb = round(total_size_bytes / 1024, 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(folder_role, pdf_signature_status) |>
  safe_df()

remaining_invalid_pdfs <- post_cleaning_inventory |>
  dplyr::filter(pdf_signature_status == "INVALID_PDF_SIGNATURE") |>
  safe_df()

quarantine_inventory <- if (dir.exists(quarantine_dir)) {
  fs::dir_info(
    path = quarantine_dir,
    recurse = FALSE,
    type = "file"
  ) |>
    dplyr::filter(stringr::str_to_lower(fs::path_ext(path)) == "pdf") |>
    dplyr::transmute(
      quarantine_file_name = fs::path_file(path),
      quarantine_path = as.character(path),
      size_bytes = as.numeric(size),
      size_kb = round(size_bytes / 1024, 1),
      modified_time = as.character(modification_time),
      pdf_signature_valid = purrr::map_lgl(as.character(path), is_valid_pdf_signature),
      pdf_signature_status = dplyr::if_else(
        pdf_signature_valid,
        "VALID_PDF_SIGNATURE",
        "INVALID_PDF_SIGNATURE"
      )
    ) |>
    safe_df()
} else {
  tibble::tibble(
    quarantine_file_name = character(),
    quarantine_path = character(),
    size_bytes = numeric(),
    size_kb = numeric(),
    modified_time = character(),
    pdf_signature_valid = logical(),
    pdf_signature_status = character()
  )
}


# ------------------------------------------------------------
# 8. Cleaning summary
# ------------------------------------------------------------

cleaning_summary <- tibble::tibble(
  script_id = "24",
  script_name = "clean_regulatory_documentation_after_pdf_audit",
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  script23c_quarantine_input = input_quarantine_csv,
  files_listed_for_quarantine = quarantine_input_n,
  files_moved_to_quarantine = sum(move_log$moved_to_quarantine == TRUE, na.rm = TRUE),
  files_not_moved = sum(move_log$moved_to_quarantine != TRUE, na.rm = TRUE),
  source_files_remaining_after_move = sum(move_log$file_exists_after_source == TRUE, na.rm = TRUE),
  quarantine_files_created = sum(move_log$file_exists_after_target == TRUE, na.rm = TRUE),
  pre_cleaning_pdf_files = nrow(pre_cleaning_inventory),
  post_cleaning_pdf_files = nrow(post_cleaning_inventory),
  pre_cleaning_invalid_pdf_signatures = sum(pre_cleaning_inventory$pdf_signature_status == "INVALID_PDF_SIGNATURE", na.rm = TRUE),
  post_cleaning_invalid_pdf_signatures = sum(post_cleaning_inventory$pdf_signature_status == "INVALID_PDF_SIGNATURE", na.rm = TRUE),
  remaining_invalid_pdfs = nrow(remaining_invalid_pdfs),
  quarantine_folder = quarantine_dir,
  cleaning_status = dplyr::case_when(
    quarantine_input_n == 0 ~ "NO_FILES_LISTED_FOR_QUARANTINE",
    nrow(remaining_invalid_pdfs) == 0 &
      sum(move_log$moved_to_quarantine == TRUE, na.rm = TRUE) == quarantine_input_n ~
      "CLEANING_COMPLETED",
    nrow(remaining_invalid_pdfs) == 0 ~
      "CLEANING_COMPLETED_WITH_MOVE_WARNINGS",
    TRUE ~
      "CLEANING_INCOMPLETE_REVIEW_REQUIRED"
  )
) |>
  safe_df()


# ------------------------------------------------------------
# 9. Methodological cleanup note
# ------------------------------------------------------------

note_lines <- c(
  "# Regulatory PDF Cleanup Note",
  "",
  paste("Generated by Script 24 on", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## Purpose",
  "",
  "This note documents the cleanup of regulatory PDF files after the combined automatic and manual audit performed in Script 23c.",
  "",
  "The cleanup was necessary because some files had a `.pdf` extension but did not contain valid PDF content. These files were typically HTML pages or failed download responses saved with a PDF extension.",
  "",
  "## Cleaning rule",
  "",
  "A PDF file was moved to quarantine if it was classified as `QUARANTINE` by Script 23c.",
  "",
  "The Script 23c decision combined two checks:",
  "",
  "1. automatic PDF signature validation using the internal `%PDF` signature;",
  "2. manual openability status, based on whether the file opened correctly in a PDF reader.",
  "",
  "## What was preserved",
  "",
  "- Valid PDF files were preserved in their original folders.",
  "- Legitimate `.html` files were preserved.",
  "- Legitimate `.csv` files were preserved.",
  "- No files were deleted.",
  "",
  "## Quarantine folder",
  "",
  paste0("Files moved out of publication folders were placed in: `", quarantine_dir, "`"),
  "",
  "## Summary",
  "",
  paste("- Files listed for quarantine:", quarantine_input_n),
  paste("- Files moved to quarantine:", sum(move_log$moved_to_quarantine == TRUE, na.rm = TRUE)),
  paste("- Remaining invalid PDFs after cleanup:", nrow(remaining_invalid_pdfs)),
  paste("- Cleaning status:", cleaning_summary$cleaning_status),
  "",
  "## Publication implication",
  "",
  "After this cleanup, the regulatory documentation folders should contain only valid PDFs, legitimate HTML files and legitimate CSV files. Quarantined files are retained for auditability but should not be cited as valid regulatory documentation."
)

write_text_file(regulatory_note_path, note_lines)


# ------------------------------------------------------------
# 10. Save outputs
# ------------------------------------------------------------

cleaning_summary_csv <- file.path(out_dir, "script24_cleaning_summary.csv")
move_log_csv <- file.path(out_dir, "script24_quarantine_move_log.csv")
pre_inventory_csv <- file.path(out_dir, "script24_pre_cleaning_pdf_inventory.csv")
pre_summary_csv <- file.path(out_dir, "script24_pre_cleaning_pdf_summary.csv")
post_inventory_csv <- file.path(out_dir, "script24_post_cleaning_pdf_inventory.csv")
post_summary_csv <- file.path(out_dir, "script24_post_cleaning_pdf_summary.csv")
remaining_invalid_csv <- file.path(out_dir, "script24_remaining_invalid_pdfs.csv")
quarantine_inventory_csv <- file.path(out_dir, "script24_quarantine_inventory.csv")
excel_output <- file.path(out_dir, "script24_cleaning_outputs.xlsx")
execution_log_txt <- file.path(out_dir, "script24_execution_log.txt")

readr::write_csv(cleaning_summary, cleaning_summary_csv)
readr::write_csv(move_log, move_log_csv)
readr::write_csv(pre_cleaning_inventory, pre_inventory_csv)
readr::write_csv(pre_cleaning_summary, pre_summary_csv)
readr::write_csv(post_cleaning_inventory, post_inventory_csv)
readr::write_csv(post_cleaning_summary, post_summary_csv)
readr::write_csv(remaining_invalid_pdfs, remaining_invalid_csv)
readr::write_csv(quarantine_inventory, quarantine_inventory_csv)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  cleaning_summary = cleaning_summary,
  move_log = move_log,
  pre_cleaning_inventory = pre_cleaning_inventory,
  pre_cleaning_summary = pre_cleaning_summary,
  post_cleaning_inventory = post_cleaning_inventory,
  post_cleaning_summary = post_cleaning_summary,
  remaining_invalid_pdfs = remaining_invalid_pdfs,
  quarantine_inventory = quarantine_inventory
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output, overwrite = TRUE)


# ------------------------------------------------------------
# 11. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 24 — Clean Regulatory Documentation After PDF Audit",
  "============================================================",
  paste("Project root:", project_root),
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Cleaning summary:",
  capture.output(print(cleaning_summary)),
  "",
  "Pre-cleaning summary:",
  capture.output(print(pre_cleaning_summary)),
  "",
  "Post-cleaning summary:",
  capture.output(print(post_cleaning_summary)),
  "",
  "Move log:",
  capture.output(print(move_log |> dplyr::select(folder_role, file_name, moved_to_quarantine, source_path, quarantine_path, error_message))),
  "",
  "Remaining invalid PDFs:",
  capture.output(print(remaining_invalid_pdfs)),
  "",
  "Quarantine inventory:",
  capture.output(print(quarantine_inventory)),
  "",
  "Main outputs:",
  paste(" -", cleaning_summary_csv),
  paste(" -", move_log_csv),
  paste(" -", pre_inventory_csv),
  paste(" -", pre_summary_csv),
  paste(" -", post_inventory_csv),
  paste(" -", post_summary_csv),
  paste(" -", remaining_invalid_csv),
  paste(" -", quarantine_inventory_csv),
  paste(" -", excel_output),
  paste(" -", regulatory_note_path),
  paste(" -", execution_log_txt)
)

writeLines(enc2utf8(log_lines), execution_log_txt, useBytes = TRUE)


# ------------------------------------------------------------
# 12. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 24 — Clean Regulatory Documentation After PDF Audit completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Cleaning summary:\n")
print(cleaning_summary)

cat("\nPre-cleaning summary:\n")
print(pre_cleaning_summary)

cat("\nPost-cleaning summary:\n")
print(post_cleaning_summary)

cat("\nFiles moved to quarantine:\n")
print(
  move_log |>
    dplyr::select(
      folder_role,
      file_name,
      size_kb_before,
      moved_to_quarantine,
      quarantine_path,
      error_message
    )
)

cat("\nRemaining invalid PDFs:\n")
print(
  remaining_invalid_pdfs |>
    dplyr::select(
      folder_role,
      file_name,
      size_kb,
      pdf_signature_status,
      full_path
    )
)

cat("\nQuarantine folder:\n")
cat(" -", quarantine_dir, "\n")

cat("\nMethodological note:\n")
cat(" -", regulatory_note_path, "\n")

cat("\nMain outputs:\n")
cat(" -", cleaning_summary_csv, "\n")
cat(" -", move_log_csv, "\n")
cat(" -", pre_inventory_csv, "\n")
cat(" -", pre_summary_csv, "\n")
cat(" -", post_inventory_csv, "\n")
cat(" -", post_summary_csv, "\n")
cat(" -", remaining_invalid_csv, "\n")
cat(" -", quarantine_inventory_csv, "\n")
cat(" -", excel_output, "\n")
cat(" -", execution_log_txt, "\n")
cat("============================================================\n")