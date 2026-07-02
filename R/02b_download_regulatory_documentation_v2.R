# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 02b v2 — Download Regulatory Documentation
# with File Signature Validation
# ============================================================
# Objective:
#   Download and inventory the regulatory documentation used to
#   guide the public DFAST-style stress test replication.
#
# Main improvement over Script 02b v1:
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
# This prevents HTML error pages from being stored as valid PDF files.
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
#   outputs/regulatory_documentation/us_regulatory_document_targets_v2.csv
#   outputs/regulatory_documentation/us_regulatory_document_download_log_v2.csv
#   outputs/regulatory_documentation/us_regulatory_document_inventory_v2.csv
#   outputs/regulatory_documentation/us_regulatory_document_download_summary_v2.csv
#   outputs/regulatory_documentation/us_regulatory_document_failed_downloads_v2.csv
#   outputs/regulatory_documentation/us_regulatory_document_validated_downloads_v2.csv
#   outputs/regulatory_documentation/us_regulatory_document_pdf_validation_summary_v2.csv
#   outputs/regulatory_documentation/script02b_v2_regulatory_documentation_outputs.xlsx
#   outputs/regulatory_documentation/script02b_v2_regulatory_documentation_report.docx
#   outputs/regulatory_documentation/script02b_v2_execution_log.txt
# ============================================================


# ------------------------------------------------------------
# 0. Initial setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 02b v2 - Download Regulatory Documentation\n")
cat("with File Signature Validation\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

script_id <- "02b_v2"
script_name <- "download_regulatory_documentation_with_signature_validation"
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

  if (is.na(ext) || ext == "") {
    target_name <- paste0(stem, "_", document_id, "_", reason, "_", timestamp)
  } else {
    target_name <- paste0(stem, "_", document_id, "_", reason, "_", timestamp, ".", ext)
  }

  target_path <- file.path(failed_dir, target_name)

  ok <- file.rename(path, target_path)

  if (ok) {
    target_path
  } else {
    NA_character_
  }
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

download_document_v2 <- function(document_id,
                                 regulator,
                                 document_title,
                                 url,
                                 destination_dir,
                                 destination_file,
                                 document_type,
                                 regulatory_area,
                                 timeout_sec = 120) {

  if (!dir.exists(destination_dir)) {
    dir.create(destination_dir, recursive = TRUE)
  }

  destination_path <- file.path(destination_dir, destination_file)

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
        regulator = regulator,
        document_title = document_title,
        regulatory_area = regulatory_area,
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
        regulator = regulator,
        document_title = document_title,
        regulatory_area = regulatory_area,
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
# 3. Documentation targets
# ------------------------------------------------------------

docs_root <- file.path(project_root, "docs/regulatory_framework/source_documents")

documentation_targets <- tibble::tribble(
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
  dplyr::mutate(
    destination_dir = file.path(docs_root, destination_subdir),
    destination_path = file.path(destination_dir, destination_file)
  ) |>
  safe_df()

documentation_targets_n <- nrow(documentation_targets)


# ------------------------------------------------------------
# 4. Download documentation with validation
# ------------------------------------------------------------

message("Downloading regulatory documentation with file signature validation...")

download_log <- purrr::pmap_dfr(
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
  download_document_v2
) |>
  dplyr::left_join(
    documentation_targets |>
      dplyr::select(document_id, year, replication_role, limitations),
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
    "Important validation note:",
    "",
    "PDF files are only treated as valid downloaded PDF documents if their internal file signature begins with `%PDF`.",
    "",
    "Source documents are stored in:",
    "",
    "docs/regulatory_framework/source_documents/",
    "",
    "Failed or invalid downloads are moved to:",
    "",
    "docs/regulatory_framework/failed_downloads/"
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
# 7. Defensive counts and summaries
# ------------------------------------------------------------

pdf_targets_n <- if ("document_type" %in% names(download_log)) {
  sum(download_log$document_type == "PDF", na.rm = TRUE)
} else {
  NA_integer_
}

valid_pdf_downloads_n <- if ("validation_status" %in% names(download_log)) {
  sum(download_log$validation_status == "VALID_PDF" & download_log$success, na.rm = TRUE)
} else {
  NA_integer_
}

invalid_pdf_signature_downloads_n <- if ("validation_status" %in% names(download_log)) {
  sum(download_log$validation_status == "INVALID_PDF_SIGNATURE", na.rm = TRUE)
} else {
  NA_integer_
}

failed_downloads_moved_n <- if ("failed_moved_to" %in% names(download_log)) {
  sum(!is.na(download_log$failed_moved_to), na.rm = TRUE)
} else {
  NA_integer_
}

legacy_invalid_moved_n <- if ("legacy_invalid_moved_to" %in% names(download_log)) {
  sum(!is.na(download_log$legacy_invalid_moved_to), na.rm = TRUE)
} else {
  NA_integer_
}

download_summary <- download_log |>
  dplyr::group_by(regulator, document_type, validation_status) |>
  dplyr::summarise(
    number_of_documents = dplyr::n(),
    successful_validated_downloads = sum(success, na.rm = TRUE),
    failed_or_invalid_downloads = sum(!success, na.rm = TRUE),
    total_size_bytes = sum(file_size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(regulator, document_type, validation_status) |>
  safe_df()

failed_downloads <- download_log |>
  dplyr::filter(!success) |>
  dplyr::select(
    document_id,
    regulator,
    document_title,
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
  dplyr::filter(success) |>
  dplyr::select(
    document_id,
    regulator,
    document_title,
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
  dplyr::filter(document_type == "PDF" | stringr::str_detect(stringr::str_to_lower(destination_path), "\\.pdf$")) |>
  dplyr::group_by(validation_status) |>
  dplyr::summarise(
    files = dplyr::n(),
    successful_validated_downloads = sum(success, na.rm = TRUE),
    failed_or_invalid_downloads = sum(!success, na.rm = TRUE),
    total_size_bytes = sum(file_size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(validation_status) |>
  safe_df()

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  documentation_targets = documentation_targets_n,
  successful_validated_downloads = sum(download_log$success, na.rm = TRUE),
  failed_or_invalid_downloads = sum(!download_log$success, na.rm = TRUE),
  pdf_targets = pdf_targets_n,
  valid_pdf_downloads = valid_pdf_downloads_n,
  invalid_pdf_signature_downloads = invalid_pdf_signature_downloads_n,
  documents_inventoried = nrow(document_inventory),
  failed_downloads_moved = failed_downloads_moved_n,
  legacy_invalid_files_moved = legacy_invalid_moved_n,
  publication_pdf_rule = "PDF success requires internal %PDF signature."
) |>
  safe_df()


# ------------------------------------------------------------
# 8. Save tabular outputs
# ------------------------------------------------------------

out_dir <- file.path(project_root, "outputs/regulatory_documentation")

targets_csv <- file.path(out_dir, "us_regulatory_document_targets_v2.csv")
download_log_csv <- file.path(out_dir, "us_regulatory_document_download_log_v2.csv")
inventory_csv <- file.path(out_dir, "us_regulatory_document_inventory_v2.csv")
summary_csv <- file.path(out_dir, "us_regulatory_document_download_summary_v2.csv")
failed_csv <- file.path(out_dir, "us_regulatory_document_failed_downloads_v2.csv")
validated_csv <- file.path(out_dir, "us_regulatory_document_validated_downloads_v2.csv")
pdf_validation_summary_csv <- file.path(out_dir, "us_regulatory_document_pdf_validation_summary_v2.csv")
execution_summary_csv <- file.path(out_dir, "script02b_v2_execution_summary.csv")
excel_output <- file.path(out_dir, "script02b_v2_regulatory_documentation_outputs.xlsx")
report_docx <- file.path(out_dir, "script02b_v2_regulatory_documentation_report.docx")
execution_log_txt <- file.path(out_dir, "script02b_v2_execution_log.txt")

readr::write_csv(documentation_targets, targets_csv)
readr::write_csv(download_log, download_log_csv)
readr::write_csv(document_inventory, inventory_csv)
readr::write_csv(download_summary, summary_csv)
readr::write_csv(failed_downloads, failed_csv)
readr::write_csv(validated_downloads, validated_csv)
readr::write_csv(pdf_validation_summary, pdf_validation_summary_csv)
readr::write_csv(execution_summary, execution_summary_csv)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  documentation_targets = documentation_targets,
  execution_summary = execution_summary,
  download_log = download_log,
  validated_downloads = validated_downloads,
  failed_downloads = failed_downloads,
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
  officer::body_add_par("Script 02b v2 - Download Regulatory Documentation with File Signature Validation", style = "heading 1") |>
  officer::body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  officer::body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  officer::body_add_par("1. Objective", style = "heading 2") |>
  officer::body_add_par(
    "This script downloads and inventories the regulatory documentation used to guide the DFAST-style public replication project. Version 2 validates downloaded files by declared type and prevents HTML pages from being treated as valid PDF documents.",
    style = "Normal"
  ) |>
  officer::body_add_par("2. Execution summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(execution_summary) |> flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("3. Download summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(download_summary) |> flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("4. PDF validation summary", style = "heading 2")

doc <- flextable::body_add_flextable(
  x = doc,
  value = flextable::flextable(pdf_validation_summary) |> flextable::autofit()
)

doc <- doc |>
  officer::body_add_par("5. Failed or invalid downloads", style = "heading 2")

if (nrow(failed_downloads) == 0) {
  doc <- officer::body_add_par(doc, "No failed or invalid downloads.", style = "Normal")
} else {
  failed_small <- failed_downloads |>
    dplyr::select(
      document_id,
      regulator,
      document_type,
      http_status,
      file_size_bytes,
      temp_file_size_bytes,
      detected_signature,
      validation_status,
      legacy_invalid_moved_to,
      failed_moved_to
    )

  doc <- flextable::body_add_flextable(
    x = doc,
    value = flextable::flextable(failed_small) |> flextable::autofit()
  )
}

doc <- doc |>
  officer::body_add_par("6. Methodological note", style = "heading 2") |>
  officer::body_add_par(
    "A file with .pdf extension is not automatically a valid PDF. In this project, a PDF download is considered valid only if the internal file signature begins with %PDF. Invalid downloads are moved to a failed_downloads folder for traceability and are not presented as valid regulatory documentation.",
    style = "Normal"
  )

print(doc, target = report_docx)


# ------------------------------------------------------------
# 10. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 02b v2 - Download Regulatory Documentation completed",
  "with File Signature Validation",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Documentation targets:", documentation_targets_n),
  paste("Successful validated downloads:", sum(download_log$success, na.rm = TRUE)),
  paste("Failed or invalid downloads:", sum(!download_log$success, na.rm = TRUE)),
  paste("PDF targets:", pdf_targets_n),
  paste("Valid PDF downloads:", valid_pdf_downloads_n),
  paste("Invalid PDF signature downloads:", invalid_pdf_signature_downloads_n),
  paste("Failed downloads moved:", failed_downloads_moved_n),
  paste("Legacy invalid files moved:", legacy_invalid_moved_n),
  paste("Documents inventoried:", nrow(document_inventory)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Download summary:",
  capture.output(print(download_summary)),
  "",
  "PDF validation summary:",
  capture.output(print(pdf_validation_summary)),
  "",
  "Failed or invalid downloads:",
  capture.output(print(failed_downloads)),
  "",
  "Main documentation folder:",
  paste(" -", docs_root),
  "",
  "Failed download folder:",
  paste(" -", file.path(project_root, "docs/regulatory_framework/failed_downloads")),
  "",
  "Main outputs:",
  paste(" -", targets_csv),
  paste(" -", download_log_csv),
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
cat("Script 02b v2 - Download Regulatory Documentation completed\n")
cat("with File Signature Validation\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Documentation targets:\n", documentation_targets_n, "\n")
cat("Successful validated downloads:\n", sum(download_log$success, na.rm = TRUE), "\n")
cat("Failed or invalid downloads:\n", sum(!download_log$success, na.rm = TRUE), "\n")
cat("PDF targets:\n", pdf_targets_n, "\n")
cat("Valid PDF downloads:\n", valid_pdf_downloads_n, "\n")
cat("Invalid PDF signature downloads:\n", invalid_pdf_signature_downloads_n, "\n")
cat("Failed downloads moved:\n", failed_downloads_moved_n, "\n")
cat("Legacy invalid files moved:\n", legacy_invalid_moved_n, "\n")
cat("Documents inventoried:\n", nrow(document_inventory), "\n\n")

cat("Execution summary:\n")
print(execution_summary)

cat("\nPDF validation summary:\n")
print(pdf_validation_summary)

cat("\nFailed or invalid downloads:\n")
print(failed_downloads)

cat("\nMain outputs:\n")
cat(" -", targets_csv, "\n")
cat(" -", download_log_csv, "\n")
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