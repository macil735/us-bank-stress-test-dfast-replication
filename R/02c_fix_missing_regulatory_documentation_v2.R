# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 02c v2 — Fix Missing Regulatory Documentation Downloads
# with File Signature Validation
# ============================================================
# Objective:
#   Correct missing or failed regulatory documentation downloads
#   after Script 02b v2.
#
# Improvement over Script 02c v1:
#   v1 treated a download as successful when:
#      HTTP status was 2xx,
#      the file existed,
#      and the file had positive size.
#
#   v2 validates the actual downloaded content:
#      PDF  -> must start with internal signature %PDF
#      HTML -> must contain <!doctype or <html in the file header
#      CSV  -> must be readable by readr::read_csv
#
# This prevents HTML pages from being stored and classified as
# valid PDF documentation.
#
# Project root:
#   D:/GitHub/us-bank-stress-test-dfast-replication
#
# Main documentation folder:
#   docs/regulatory_framework/source_documents/
#
# Failed download folder:
#   docs/regulatory_framework/failed_downloads/
#
# Main outputs:
#   outputs/regulatory_documentation/script02c_v2_fix_targets.csv
#   outputs/regulatory_documentation/script02c_v2_download_log.csv
#   outputs/regulatory_documentation/script02c_v2_best_available_documents.csv
#   outputs/regulatory_documentation/script02c_v2_group_status.csv
#   outputs/regulatory_documentation/script02c_v2_document_inventory.csv
#   outputs/regulatory_documentation/script02c_v2_download_summary.csv
#   outputs/regulatory_documentation/script02c_v2_execution_summary.csv
#   outputs/regulatory_documentation/script02c_v2_fix_missing_regulatory_documentation_outputs.xlsx
#   outputs/regulatory_documentation/script02c_v2_fix_missing_regulatory_documentation_report.docx
#   outputs/regulatory_documentation/script02c_v2_execution_log.txt
# ============================================================


# ------------------------------------------------------------
# 0. Initial setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 02c v2 - Fix Missing Regulatory Documentation\n")
cat("with File Signature Validation\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

script_id <- "02c_v2"
script_name <- "fix_missing_regulatory_documentation_with_signature_validation"
start_time <- Sys.time()

dir_list <- c(
  file.path(project_root, "R"),
  file.path(project_root, "docs"),
  file.path(project_root, "docs/regulatory_framework"),
  file.path(project_root, "docs/regulatory_framework/source_documents"),
  file.path(project_root, "docs/regulatory_framework/source_documents/federal_reserve"),
  file.path(project_root, "docs/regulatory_framework/failed_downloads"),
  file.path(project_root, "outputs"),
  file.path(project_root, "outputs/regulatory_documentation")
)

for (d in dir_list) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
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

suppressPackageStartupMessages({
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
})


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
    dplyr::mutate(dplyr::across(where(is.character), safe_chr))
}

write_text_file <- function(path, lines) {
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
}

read_binary_header_raw <- function(path, n_bytes = 1024) {
  if (!file.exists(path)) {
    return(raw(0))
  }

  size <- file.info(path)$size

  if (is.na(size) || size <= 0) {
    return(raw(0))
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  readBin(con, what = "raw", n = min(n_bytes, size))
}

raw_to_ascii_preview <- function(raw_bytes) {
  if (length(raw_bytes) == 0) {
    return(NA_character_)
  }

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

raw_to_hex_preview <- function(raw_bytes) {
  if (length(raw_bytes) == 0) {
    return(NA_character_)
  }

  paste(sprintf("%02X", as.integer(raw_bytes)), collapse = " ")
}

starts_with_pdf_signature <- function(path) {
  raw_bytes <- read_binary_header_raw(path, n_bytes = 4)

  if (length(raw_bytes) < 4) {
    return(FALSE)
  }

  identical(
    as.integer(raw_bytes[1:4]),
    as.integer(charToRaw("%PDF"))
  )
}

looks_like_html <- function(path) {
  raw_bytes <- read_binary_header_raw(path, n_bytes = 4096)

  if (length(raw_bytes) == 0) {
    return(FALSE)
  }

  ascii_preview <- raw_to_ascii_preview(raw_bytes)
  ascii_lower <- stringr::str_to_lower(ascii_preview)

  stringr::str_detect(ascii_lower, "<!doctype") |
    stringr::str_detect(ascii_lower, "<html")
}

is_readable_csv <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  tryCatch(
    {
      x <- readr::read_csv(path, n_max = 5, show_col_types = FALSE)
      ncol(x) > 0
    },
    error = function(e) FALSE
  )
}

detect_file_signature <- function(path) {
  raw_bytes <- read_binary_header_raw(path, n_bytes = 16)

  if (length(raw_bytes) == 0) {
    return(NA_character_)
  }

  paste0(
    "ASCII: ",
    raw_to_ascii_preview(raw_bytes),
    " | HEX: ",
    raw_to_hex_preview(raw_bytes)
  )
}

validate_downloaded_file <- function(path, document_type, destination_file) {
  ext <- stringr::str_to_lower(fs::path_ext(destination_file))
  doc_type <- stringr::str_to_upper(as.character(document_type))

  exists <- file.exists(path)
  size <- ifelse(exists, file.info(path)$size, NA_real_)
  signature <- detect_file_signature(path)

  if (!exists) {
    return(
      tibble(
        detected_signature = NA_character_,
        validation_status = "MISSING_FILE",
        validation_passed = FALSE,
        validation_message = "Downloaded file does not exist."
      )
    )
  }

  if (is.na(size) || size <= 0) {
    return(
      tibble(
        detected_signature = signature,
        validation_status = "EMPTY_FILE",
        validation_passed = FALSE,
        validation_message = "Downloaded file is empty or has unknown size."
      )
    )
  }

  if (doc_type == "PDF" || ext == "pdf") {
    valid_pdf <- starts_with_pdf_signature(path)

    return(
      tibble(
        detected_signature = signature,
        validation_status = ifelse(valid_pdf, "VALID_PDF", "INVALID_PDF_SIGNATURE"),
        validation_passed = valid_pdf,
        validation_message = ifelse(
          valid_pdf,
          "PDF signature validated.",
          "File has .pdf extension or PDF document type but does not start with %PDF."
        )
      )
    )
  }

  if (doc_type == "HTML" || ext %in% c("html", "htm")) {
    valid_html <- looks_like_html(path)

    return(
      tibble(
        detected_signature = signature,
        validation_status = ifelse(valid_html, "VALID_HTML", "HTML_REVIEW"),
        validation_passed = valid_html,
        validation_message = ifelse(
          valid_html,
          "HTML content validated.",
          "File is declared as HTML but does not clearly contain <!doctype or <html in the header."
        )
      )
    )
  }

  if (doc_type == "CSV" || ext == "csv") {
    valid_csv <- is_readable_csv(path)

    return(
      tibble(
        detected_signature = signature,
        validation_status = ifelse(valid_csv, "VALID_CSV", "INVALID_CSV"),
        validation_passed = valid_csv,
        validation_message = ifelse(
          valid_csv,
          "CSV preview read successfully.",
          "CSV could not be read by readr::read_csv."
        )
      )
    )
  }

  tibble(
    detected_signature = signature,
    validation_status = "VALIDATION_NOT_REQUIRED_FOR_TYPE",
    validation_passed = TRUE,
    validation_message = "No strict validation rule was required for this document type."
  )
}

move_to_failed_downloads <- function(path, document_id, reason = "failed") {
  if (!file.exists(path)) {
    return(NA_character_)
  }

  failed_dir <- file.path(project_root, "docs/regulatory_framework/failed_downloads")

  if (!dir.exists(failed_dir)) {
    dir.create(failed_dir, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  ext <- tools::file_ext(path)
  stem <- tools::file_path_sans_ext(basename(path))

  safe_document_id <- stringr::str_replace_all(document_id, "[^A-Za-z0-9_\\-]", "_")
  safe_reason <- stringr::str_replace_all(reason, "[^A-Za-z0-9_\\-]", "_")

  if (is.na(ext) || ext == "") {
    target_name <- paste0(stem, "_", safe_document_id, "_", safe_reason, "_", timestamp)
  } else {
    target_name <- paste0(stem, "_", safe_document_id, "_", safe_reason, "_", timestamp, ".", ext)
  }

  target_path <- file.path(failed_dir, target_name)

  ok <- suppressWarnings(file.rename(path, target_path))

  if (ok) {
    return(target_path)
  }

  copy_ok <- suppressWarnings(file.copy(path, target_path, overwrite = TRUE))

  if (copy_ok) {
    unlink(path)
    return(target_path)
  }

  NA_character_
}

clean_existing_invalid_destination <- function(destination_path, document_type, destination_file, document_id) {
  if (!file.exists(destination_path)) {
    return(NA_character_)
  }

  validation <- validate_downloaded_file(
    path = destination_path,
    document_type = document_type,
    destination_file = destination_file
  )

  if (!isTRUE(validation$validation_passed)) {
    return(
      move_to_failed_downloads(
        path = destination_path,
        document_id = document_id,
        reason = "legacy_invalid"
      )
    )
  }

  NA_character_
}

download_one_v2 <- function(document_id,
                            document_group,
                            document_title,
                            url,
                            destination_path,
                            document_type,
                            attempt_priority,
                            timeout_sec = 120) {

  destination_dir <- dirname(destination_path)
  destination_file <- basename(destination_path)

  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }

  legacy_invalid_moved_to <- clean_existing_invalid_destination(
    destination_path = destination_path,
    document_type = document_type,
    destination_file = destination_file,
    document_id = document_id
  )

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  temp_file <- file.path(
    destination_dir,
    paste0(".download_tmp_", document_id, "_", timestamp, "_", destination_file)
  )

  result <- tryCatch(
    {
      response <- request(url) |>
        req_user_agent(
          "us-bank-stress-test-dfast-replication/0.1 academic contact: gelo.picol@gmail.com"
        ) |>
        req_timeout(timeout_sec) |>
        req_perform(path = temp_file)

      status_code <- resp_status(response)

      content_type <- tryCatch(
        resp_header(response, "content-type"),
        error = function(e) NA_character_
      )

      temp_exists <- file.exists(temp_file)
      temp_size <- ifelse(temp_exists, file.info(temp_file)$size, NA_real_)

      validation <- validate_downloaded_file(
        path = temp_file,
        document_type = document_type,
        destination_file = destination_file
      )

      http_success <- status_code >= 200 & status_code < 400

      success_validated <- http_success &&
        temp_exists &&
        !is.na(temp_size) &&
        temp_size > 0 &&
        isTRUE(validation$validation_passed)

      failed_moved_to <- NA_character_
      final_written <- FALSE

      if (success_validated) {
        final_written <- file.copy(
          from = temp_file,
          to = destination_path,
          overwrite = TRUE
        )

        if (file.exists(temp_file)) {
          unlink(temp_file)
        }
      } else {
        if (file.exists(temp_file)) {
          failed_moved_to <- move_to_failed_downloads(
            path = temp_file,
            document_id = document_id,
            reason = "invalid_download"
          )
        }
      }

      tibble(
        document_id = document_id,
        document_group = document_group,
        document_title = document_title,
        attempt_priority = attempt_priority,
        document_type = document_type,
        url = url,
        destination_path = destination_path,
        temp_download_path = temp_file,
        http_status = status_code,
        content_type = content_type,
        file_exists = file.exists(destination_path),
        file_size_bytes = ifelse(file.exists(destination_path), file.info(destination_path)$size, NA_real_),
        temp_file_size_bytes = temp_size,
        detected_signature = validation$detected_signature,
        validation_status = validation$validation_status,
        validation_passed = validation$validation_passed,
        success = success_validated && final_written,
        final_written = final_written,
        legacy_invalid_moved_to = legacy_invalid_moved_to,
        failed_moved_to = failed_moved_to,
        downloaded_at = as.character(Sys.time()),
        error_message = ifelse(
          success_validated && final_written,
          NA_character_,
          validation$validation_message
        )
      )
    },
    error = function(e) {
      failed_moved_to <- NA_character_

      if (file.exists(temp_file)) {
        failed_moved_to <- move_to_failed_downloads(
          path = temp_file,
          document_id = document_id,
          reason = "download_error"
        )
      }

      tibble(
        document_id = document_id,
        document_group = document_group,
        document_title = document_title,
        attempt_priority = attempt_priority,
        document_type = document_type,
        url = url,
        destination_path = destination_path,
        temp_download_path = temp_file,
        http_status = NA_integer_,
        content_type = NA_character_,
        file_exists = file.exists(destination_path),
        file_size_bytes = ifelse(file.exists(destination_path), file.info(destination_path)$size, NA_real_),
        temp_file_size_bytes = NA_real_,
        detected_signature = ifelse(file.exists(destination_path), detect_file_signature(destination_path), NA_character_),
        validation_status = "DOWNLOAD_ERROR",
        validation_passed = FALSE,
        success = FALSE,
        final_written = FALSE,
        legacy_invalid_moved_to = legacy_invalid_moved_to,
        failed_moved_to = failed_moved_to,
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
        modified_time = character(),
        pdf_signature_valid = logical()
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
      modified_time = as.character(modification_time),
      pdf_signature_valid = ifelse(
        stringr::str_to_lower(extension) == "pdf",
        purrr::map_lgl(as.character(path), starts_with_pdf_signature),
        NA
      )
    ) |>
    arrange(relative_path)
}


# ------------------------------------------------------------
# 3. Corrective targets
# ------------------------------------------------------------

docs_root <- file.path(project_root, "docs/regulatory_framework/source_documents")
fed_dir <- file.path(docs_root, "federal_reserve")

fix_targets <- tibble::tribble(
  ~document_id, ~document_group, ~document_title, ~attempt_priority, ~url, ~document_type, ~destination_file,

  "FED_PUBLIC_RESULTS_DICTIONARY_ATTEMPT_01",
  "Public Results Data Dictionary",
  "Public Results DFAST 2026 Data Dictionary",
  1,
  "https://www.federalreserve.gov/supervisionreg/files/public_results_DFAST_2026_dictionary.pdf",
  "PDF",
  "fed_public_results_DFAST_2026_dictionary_attempt_01.pdf",

  "FED_PUBLIC_RESULTS_DICTIONARY_ATTEMPT_02",
  "Public Results Data Dictionary",
  "Public Results DFAST 2026 Data Dictionary",
  2,
  "https://www.federalreserve.gov/supervisionreg/files/Public_Results_DFAST_2026_Data_Dictionary.pdf",
  "PDF",
  "fed_public_results_DFAST_2026_dictionary_attempt_02.pdf",

  "FED_PUBLIC_RESULTS_DICTIONARY_ATTEMPT_03",
  "Public Results Data Dictionary",
  "Stress Test Results Data Dictionary",
  3,
  "https://www.federalreserve.gov/supervisionreg/files/Stress_Test_Results_Data_Dictionary.pdf",
  "PDF",
  "fed_stress_test_results_data_dictionary_attempt_03.pdf",

  "FED_PUBLIC_RESULTS_DICTIONARY_PAGE_ATTEMPT_04",
  "Public Results Data Dictionary",
  "Federal Reserve DFAST 2026 Page as Fallback Reference",
  4,
  "https://www.federalreserve.gov/supervisionreg/dfa-stress-tests-2026.htm",
  "HTML",
  "fed_public_results_dictionary_fallback_dfast_2026_page.html",

  "FED_9Q_PATHS_DICTIONARY_ATTEMPT_01",
  "Detailed Nine Quarter Paths Dictionary",
  "2026 Detailed Nine Quarter Paths Data Dictionary",
  1,
  "https://www.federalreserve.gov/supervisionreg/files/2026_Detailed_Nine_Quarter_Paths_dictionary.pdf",
  "PDF",
  "fed_2026_detailed_nine_quarter_paths_dictionary_attempt_01.pdf",

  "FED_9Q_PATHS_DICTIONARY_ATTEMPT_02",
  "Detailed Nine Quarter Paths Dictionary",
  "Detailed Nine Quarter Path Data Dictionary December 2025",
  2,
  "https://www.federalreserve.gov/supervisionreg/files/Detailed_Nine_Quarter_Path_Data_Dictionary_December_2025.pdf",
  "PDF",
  "fed_detailed_nine_quarter_path_data_dictionary_december_2025.pdf",

  "FED_9Q_PATHS_DICTIONARY_ATTEMPT_03",
  "Detailed Nine Quarter Paths Dictionary",
  "Detailed Nine Quarter Path Data Dictionary",
  3,
  "https://www.federalreserve.gov/supervisionreg/files/Detailed_Nine_Quarter_Path_Data_Dictionary.pdf",
  "PDF",
  "fed_detailed_nine_quarter_path_data_dictionary_attempt_03.pdf",

  "FED_9Q_PATHS_DICTIONARY_PAGE_ATTEMPT_04",
  "Detailed Nine Quarter Paths Dictionary",
  "Federal Reserve DFAST 2026 Page as Fallback Reference",
  4,
  "https://www.federalreserve.gov/supervisionreg/dfa-stress-tests-2026.htm",
  "HTML",
  "fed_9q_paths_dictionary_fallback_dfast_2026_page.html",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_01",
  "Large Bank Capital Requirements",
  "Annual Large Bank Capital Requirements Page",
  1,
  "https://www.federalreserve.gov/supervisionreg/large-bank-capital-requirements.htm",
  "HTML",
  "fed_annual_large_bank_capital_requirements_page.html",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_02",
  "Large Bank Capital Requirements",
  "Large Bank Capital Requirements June 2025",
  2,
  "https://www.federalreserve.gov/publications/files/large-bank-capital-requirements-20250627.pdf",
  "PDF",
  "fed_large_bank_capital_requirements_20250627_attempt_02.pdf",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_03",
  "Large Bank Capital Requirements",
  "Large Bank Capital Requirements August 2025",
  3,
  "https://www.federalreserve.gov/publications/files/large-bank-capital-requirements-20250829.pdf",
  "PDF",
  "fed_large_bank_capital_requirements_20250829.pdf",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_04",
  "Large Bank Capital Requirements",
  "Large Bank Capital Requirements September 2025",
  4,
  "https://www.federalreserve.gov/publications/files/large-bank-capital-requirements-20250930.pdf",
  "PDF",
  "fed_large_bank_capital_requirements_20250930.pdf",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_05",
  "Large Bank Capital Requirements",
  "Large Bank Capital Requirements June 2026",
  5,
  "https://www.federalreserve.gov/publications/files/large-bank-capital-requirements-20260624.pdf",
  "PDF",
  "fed_large_bank_capital_requirements_20260624.pdf"
) |>
  dplyr::mutate(
    destination_path = file.path(fed_dir, destination_file)
  ) |>
  safe_df()

fix_targets_n <- nrow(fix_targets)


# ------------------------------------------------------------
# 4. Download corrective targets with validation
# ------------------------------------------------------------

message("Fixing missing regulatory documentation downloads with signature validation...")

download_log_raw <- purrr::pmap_dfr(
  list(
    fix_targets$document_id,
    fix_targets$document_group,
    fix_targets$document_title,
    fix_targets$url,
    fix_targets$destination_path,
    fix_targets$document_type,
    fix_targets$attempt_priority
  ),
  download_one_v2
)

# Defensive normalization: never rely on bare variable `success`
if (!is.data.frame(download_log_raw)) {
  download_log_raw <- tibble::tibble()
}

required_cols <- c(
  "document_id",
  "document_group",
  "document_title",
  "attempt_priority",
  "document_type",
  "url",
  "destination_path",
  "temp_download_path",
  "http_status",
  "content_type",
  "file_exists",
  "file_size_bytes",
  "temp_file_size_bytes",
  "detected_signature",
  "validation_status",
  "validation_passed",
  "success",
  "final_written",
  "legacy_invalid_moved_to",
  "failed_moved_to",
  "downloaded_at",
  "error_message"
)

for (col in required_cols) {
  if (!col %in% names(download_log_raw)) {
    if (col %in% c("file_exists", "validation_passed", "success", "final_written")) {
      download_log_raw[[col]] <- FALSE
    } else if (col %in% c("attempt_priority", "http_status", "file_size_bytes", "temp_file_size_bytes")) {
      download_log_raw[[col]] <- NA_real_
    } else {
      download_log_raw[[col]] <- NA_character_
    }
  }
}

download_log <- download_log_raw |>
  dplyr::mutate(
    download_success = dplyr::case_when(
      isTRUE(success) ~ TRUE,
      success == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    validation_passed = dplyr::case_when(
      isTRUE(validation_passed) ~ TRUE,
      validation_passed == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    final_written = dplyr::case_when(
      isTRUE(final_written) ~ TRUE,
      final_written == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    file_exists = dplyr::case_when(
      isTRUE(file_exists) ~ TRUE,
      file_exists == TRUE ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::select(-success) |>
  safe_df()


# ------------------------------------------------------------
# 5. Select best available document per group
# ------------------------------------------------------------

best_available <- download_log |>
  dplyr::filter(.data$download_success == TRUE) |>
  dplyr::arrange(.data$document_group, .data$attempt_priority) |>
  dplyr::group_by(.data$document_group) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(
    document_group,
    selected_document_id = document_id,
    selected_title = document_title,
    selected_document_type = document_type,
    selected_url = url,
    selected_destination_path = destination_path,
    validation_status,
    file_size_bytes
  ) |>
  safe_df()

group_status <- fix_targets |>
  dplyr::distinct(.data$document_group) |>
  dplyr::left_join(
    best_available |>
      dplyr::select(
        document_group,
        selected_document_id,
        selected_document_type,
        validation_status
      ),
    by = "document_group"
  ) |>
  dplyr::mutate(
    fixed = !is.na(.data$selected_document_id),
    status = dplyr::case_when(
      .data$fixed == TRUE & .data$selected_document_type == "PDF" ~
        "Fixed with validated PDF",
      .data$fixed == TRUE & .data$selected_document_type == "HTML" ~
        "Fixed with HTML fallback reference",
      TRUE ~
        "Still missing"
    )
  ) |>
  safe_df()


# ------------------------------------------------------------
# 6. Updated documentation inventory
# ------------------------------------------------------------

document_inventory <- make_inventory(docs_root) |>
  safe_df()


# ------------------------------------------------------------
# 7. Summaries
# ------------------------------------------------------------

pdf_attempts_n <- sum(download_log$document_type == "PDF", na.rm = TRUE)
html_attempts_n <- sum(download_log$document_type == "HTML", na.rm = TRUE)

valid_pdf_attempts_n <- sum(
  download_log$validation_status == "VALID_PDF" &
    download_log$download_success == TRUE,
  na.rm = TRUE
)

valid_html_attempts_n <- sum(
  download_log$validation_status == "VALID_HTML" &
    download_log$download_success == TRUE,
  na.rm = TRUE
)

invalid_pdf_signature_attempts_n <- sum(
  download_log$validation_status == "INVALID_PDF_SIGNATURE",
  na.rm = TRUE
)

download_error_attempts_n <- sum(
  download_log$validation_status == "DOWNLOAD_ERROR",
  na.rm = TRUE
)

failed_downloads_moved_n <- sum(
  !is.na(download_log$failed_moved_to),
  na.rm = TRUE
)

legacy_invalid_moved_n <- sum(
  !is.na(download_log$legacy_invalid_moved_to),
  na.rm = TRUE
)

successful_validated_attempts_n <- sum(
  download_log$download_success == TRUE,
  na.rm = TRUE
)

failed_or_invalid_attempts_n <- sum(
  download_log$download_success != TRUE,
  na.rm = TRUE
)

groups_fixed_n <- sum(group_status$fixed == TRUE, na.rm = TRUE)
groups_still_missing_n <- sum(group_status$fixed != TRUE, na.rm = TRUE)

download_summary <- download_log |>
  dplyr::group_by(
    .data$document_group,
    .data$document_type,
    .data$validation_status
  ) |>
  dplyr::summarise(
    attempts = dplyr::n(),
    successful_validated_attempts = sum(.data$download_success == TRUE, na.rm = TRUE),
    failed_or_invalid_attempts = sum(.data$download_success != TRUE, na.rm = TRUE),
    total_size_bytes = sum(.data$file_size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    .data$document_group,
    .data$document_type,
    .data$validation_status
  ) |>
  safe_df()

failed_or_invalid_downloads <- download_log |>
  dplyr::filter(.data$download_success != TRUE) |>
  dplyr::select(
    document_id,
    document_group,
    document_title,
    attempt_priority,
    document_type,
    url,
    http_status,
    content_type,
    file_size_bytes,
    temp_file_size_bytes,
    detected_signature,
    validation_status,
    validation_passed,
    legacy_invalid_moved_to,
    failed_moved_to,
    error_message
  ) |>
  safe_df()

validated_downloads <- download_log |>
  dplyr::filter(.data$download_success == TRUE) |>
  dplyr::select(
    document_id,
    document_group,
    document_title,
    attempt_priority,
    document_type,
    destination_path,
    http_status,
    content_type,
    file_size_bytes,
    detected_signature,
    validation_status,
    validation_passed
  ) |>
  safe_df()

pdf_validation_summary <- download_log |>
  dplyr::filter(
    .data$document_type == "PDF" |
      stringr::str_detect(stringr::str_to_lower(.data$destination_path), "\\.pdf$")
  ) |>
  dplyr::group_by(.data$validation_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    successful_validated_attempts = sum(.data$download_success == TRUE, na.rm = TRUE),
    failed_or_invalid_attempts = sum(.data$download_success != TRUE, na.rm = TRUE),
    total_size_bytes = sum(.data$file_size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$validation_status) |>
  safe_df()

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  corrective_targets = fix_targets_n,
  successful_validated_attempts = successful_validated_attempts_n,
  failed_or_invalid_attempts = failed_or_invalid_attempts_n,
  pdf_attempts = pdf_attempts_n,
  html_attempts = html_attempts_n,
  valid_pdf_attempts = valid_pdf_attempts_n,
  valid_html_attempts = valid_html_attempts_n,
  invalid_pdf_signature_attempts = invalid_pdf_signature_attempts_n,
  download_error_attempts = download_error_attempts_n,
  groups_fixed = groups_fixed_n,
  groups_still_missing = groups_still_missing_n,
  documents_inventoried = nrow(document_inventory),
  failed_downloads_moved = failed_downloads_moved_n,
  legacy_invalid_files_moved = legacy_invalid_moved_n,
  publication_pdf_rule = "PDF success requires internal %PDF signature."
) |>
  safe_df()

# ------------------------------------------------------------
# 8. Save outputs
# ------------------------------------------------------------

out_dir <- file.path(project_root, "outputs/regulatory_documentation")

targets_csv <- file.path(out_dir, "script02c_v2_fix_targets.csv")
download_log_csv <- file.path(out_dir, "script02c_v2_download_log.csv")
best_available_csv <- file.path(out_dir, "script02c_v2_best_available_documents.csv")
group_status_csv <- file.path(out_dir, "script02c_v2_group_status.csv")
inventory_csv <- file.path(out_dir, "script02c_v2_document_inventory.csv")
summary_csv <- file.path(out_dir, "script02c_v2_download_summary.csv")
failed_csv <- file.path(out_dir, "script02c_v2_failed_or_invalid_downloads.csv")
validated_csv <- file.path(out_dir, "script02c_v2_validated_downloads.csv")
pdf_validation_summary_csv <- file.path(out_dir, "script02c_v2_pdf_validation_summary.csv")
execution_summary_csv <- file.path(out_dir, "script02c_v2_execution_summary.csv")
excel_output <- file.path(out_dir, "script02c_v2_fix_missing_regulatory_documentation_outputs.xlsx")
report_docx <- file.path(out_dir, "script02c_v2_fix_missing_regulatory_documentation_report.docx")
execution_log_txt <- file.path(out_dir, "script02c_v2_execution_log.txt")

readr::write_csv(fix_targets, targets_csv)
readr::write_csv(download_log, download_log_csv)
readr::write_csv(best_available, best_available_csv)
readr::write_csv(group_status, group_status_csv)
readr::write_csv(document_inventory, inventory_csv)
readr::write_csv(download_summary, summary_csv)
readr::write_csv(failed_or_invalid_downloads, failed_csv)
readr::write_csv(validated_downloads, validated_csv)
readr::write_csv(pdf_validation_summary, pdf_validation_summary_csv)
readr::write_csv(execution_summary, execution_summary_csv)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  fix_targets = fix_targets,
  execution_summary = execution_summary,
  group_status = group_status,
  best_available = best_available,
  download_log = download_log,
  validated_downloads = validated_downloads,
  failed_or_invalid = failed_or_invalid_downloads,
  document_inventory = document_inventory,
  download_summary = download_summary,
  pdf_validation_summary = pdf_validation_summary
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output, overwrite = TRUE)


# ------------------------------------------------------------
# 9. Word report
# ------------------------------------------------------------

doc <- officer::read_docx()

doc <- doc |>
  officer::body_add_par("Script 02c v2 - Fix Missing Regulatory Documentation with File Signature Validation", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This corrective script attempts to recover regulatory documentation that failed or remained unavailable after Script 02b v2. It validates PDF, HTML and other files before classifying an attempt as successful.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Execution summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(execution_summary) |> flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Group status", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(group_status) |> flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. Best available documents", style = "heading 2")

if (nrow(best_available) == 0) {
  doc <- officer::body_add_par(doc, "No missing document group was fixed.", style = "Normal")
} else {
  best_available_small <- best_available |>
    dplyr::select(
      document_group,
      selected_document_id,
      selected_document_type,
      validation_status,
      selected_destination_path,
      file_size_bytes
    )

  doc <- flextable::body_add_flextable(
    x = doc,
    value = flextable::flextable(best_available_small) |> flextable::autofit()
  )
}

doc <- doc |>
  officer::body_add_par("5. PDF validation summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(pdf_validation_summary) |> flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("6. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "If some documents remain missing, the project may continue as long as the core Federal Reserve scenario and results datasets are available. Missing documentation should be recorded as a reproducibility limitation. A file with .pdf extension is not treated as valid unless its internal signature begins with %PDF.",
    style = "Normal"
  )

print(doc, target = report_docx)


# ------------------------------------------------------------
# 10. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 02c v2 - Fix Missing Regulatory Documentation Downloads completed",
  "with File Signature Validation",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Corrective targets:", fix_targets_n),
  paste("Successful validated attempts:", sum(download_log$success, na.rm = TRUE)),
  paste("Failed or invalid attempts:", sum(!download_log$success, na.rm = TRUE)),
  paste("PDF attempts:", pdf_attempts_n),
  paste("HTML attempts:", html_attempts_n),
  paste("Valid PDF attempts:", valid_pdf_attempts_n),
  paste("Valid HTML attempts:", valid_html_attempts_n),
  paste("Invalid PDF signature attempts:", invalid_pdf_signature_attempts_n),
  paste("Download error attempts:", download_error_attempts_n),
  paste("Groups fixed:", sum(group_status$fixed, na.rm = TRUE)),
  paste("Groups still missing:", sum(!group_status$fixed, na.rm = TRUE)),
  paste("Documents inventoried:", nrow(document_inventory)),
  paste("Failed downloads moved:", failed_downloads_moved_n),
  paste("Legacy invalid files moved:", legacy_invalid_moved_n),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Group status:",
  capture.output(print(group_status)),
  "",
  "Best available documents:",
  capture.output(print(best_available)),
  "",
  "PDF validation summary:",
  capture.output(print(pdf_validation_summary)),
  "",
  "Failed or invalid downloads:",
  capture.output(print(failed_or_invalid_downloads)),
  "",
  "Main outputs:",
  paste(" -", targets_csv),
  paste(" -", download_log_csv),
  paste(" -", best_available_csv),
  paste(" -", group_status_csv),
  paste(" -", inventory_csv),
  paste(" -", summary_csv),
  paste(" -", failed_csv),
  paste(" -", validated_csv),
  paste(" -", pdf_validation_summary_csv),
  paste(" -", execution_summary_csv),
  paste(" -", excel_output),
  paste(" -", report_docx),
  paste(" -", execution_log_txt)
)

writeLines(enc2utf8(log_lines), execution_log_txt, useBytes = TRUE)


# ------------------------------------------------------------
# 11. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 02c v2 - Fix Missing Regulatory Documentation Downloads completed\n")
cat("with File Signature Validation\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Corrective targets:\n", fix_targets_n, "\n")
cat("Successful validated attempts:\n", sum(download_log$success, na.rm = TRUE), "\n")
cat("Failed or invalid attempts:\n", sum(!download_log$success, na.rm = TRUE), "\n")
cat("PDF attempts:\n", pdf_attempts_n, "\n")
cat("HTML attempts:\n", html_attempts_n, "\n")
cat("Valid PDF attempts:\n", valid_pdf_attempts_n, "\n")
cat("Valid HTML attempts:\n", valid_html_attempts_n, "\n")
cat("Invalid PDF signature attempts:\n", invalid_pdf_signature_attempts_n, "\n")
cat("Download error attempts:\n", download_error_attempts_n, "\n")
cat("Groups fixed:\n", sum(group_status$fixed, na.rm = TRUE), "\n")
cat("Groups still missing:\n", sum(!group_status$fixed, na.rm = TRUE), "\n")
cat("Documents inventoried:\n", nrow(document_inventory), "\n")
cat("Failed downloads moved:\n", failed_downloads_moved_n, "\n")
cat("Legacy invalid files moved:\n", legacy_invalid_moved_n, "\n\n")

cat("Execution summary:\n")
print(execution_summary)

cat("\nGroup status:\n")
print(group_status)

cat("\nBest available documents:\n")
print(best_available)

cat("\nPDF validation summary:\n")
print(pdf_validation_summary)

cat("\nFailed or invalid downloads:\n")
print(failed_or_invalid_downloads)

cat("\nMain outputs:\n")
cat(" -", targets_csv, "\n")
cat(" -", download_log_csv, "\n")
cat(" -", best_available_csv, "\n")
cat(" -", group_status_csv, "\n")
cat(" -", inventory_csv, "\n")
cat(" -", summary_csv, "\n")
cat(" -", failed_csv, "\n")
cat(" -", validated_csv, "\n")
cat(" -", pdf_validation_summary_csv, "\n")
cat(" -", execution_summary_csv, "\n")
cat(" -", excel_output, "\n")
cat(" -", report_docx, "\n")
cat(" -", execution_log_txt, "\n")
cat("============================================================\n")