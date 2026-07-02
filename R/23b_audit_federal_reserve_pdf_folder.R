# ============================================================
# Script 23b — Audit Federal Reserve PDF Folder Only
# ============================================================

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

target_dir <- file.path(
  project_root,
  "docs/regulatory_framework/source_documents/federal_reserve"
)

out_dir <- file.path(project_root, "outputs/publication_package")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

required_packages <- c("dplyr", "tibble", "readr", "stringr", "fs", "openxlsx")

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
  library(fs)
  library(openxlsx)
})

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

is_valid_pdf <- function(path) {
  raw_bytes <- read_header_raw(path, n_bytes = 4)

  if (length(raw_bytes) < 4) return(FALSE)

  identical(
    as.integer(raw_bytes[1:4]),
    as.integer(charToRaw("%PDF"))
  )
}

pdf_files <- fs::dir_info(
  path = target_dir,
  recurse = FALSE,
  type = "file"
) |>
  filter(str_to_lower(fs::path_ext(path)) == "pdf")

if (nrow(pdf_files) == 0) {
  audit <- tibble(
    file_name = character(),
    path = character(),
    size_bytes = numeric(),
    size_kb = numeric(),
    pdf_signature_valid = logical(),
    validation_status = character(),
    ascii_header = character(),
    hex_header = character()
  )
} else {
  audit <- pdf_files |>
    transmute(
      file_name = fs::path_file(path),
      path = as.character(path),
      size_bytes = as.numeric(size),
      size_kb = round(size_bytes / 1024, 1),
      pdf_signature_valid = purrr::map_lgl(as.character(path), is_valid_pdf),
      validation_status = ifelse(pdf_signature_valid, "VALID_PDF", "INVALID_PDF"),
      ascii_header = purrr::map_chr(as.character(path), ~ raw_to_ascii(read_header_raw(.x, 16))),
      hex_header = purrr::map_chr(as.character(path), ~ raw_to_hex(read_header_raw(.x, 16)))
    ) |>
    arrange(validation_status, file_name)
}

summary <- audit |>
  group_by(validation_status) |>
  summarise(
    files = n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    total_size_kb = round(total_size_bytes / 1024, 1),
    .groups = "drop"
  )

invalid_pdfs <- audit |>
  filter(validation_status == "INVALID_PDF")

valid_pdfs <- audit |>
  filter(validation_status == "VALID_PDF")

audit_csv <- file.path(out_dir, "script23b_federal_reserve_pdf_folder_audit.csv")
summary_csv <- file.path(out_dir, "script23b_federal_reserve_pdf_folder_summary.csv")
invalid_csv <- file.path(out_dir, "script23b_invalid_federal_reserve_pdfs.csv")
valid_csv <- file.path(out_dir, "script23b_valid_federal_reserve_pdfs.csv")
excel_output <- file.path(out_dir, "script23b_federal_reserve_pdf_folder_audit.xlsx")

readr::write_csv(audit, audit_csv)
readr::write_csv(summary, summary_csv)
readr::write_csv(invalid_pdfs, invalid_csv)
readr::write_csv(valid_pdfs, valid_csv)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "summary")
openxlsx::writeData(wb, "summary", summary)

openxlsx::addWorksheet(wb, "audit")
openxlsx::writeData(wb, "audit", audit)

openxlsx::addWorksheet(wb, "valid_pdfs")
openxlsx::writeData(wb, "valid_pdfs", valid_pdfs)

openxlsx::addWorksheet(wb, "invalid_pdfs")
openxlsx::writeData(wb, "invalid_pdfs", invalid_pdfs)

for (sheet in names(wb)) {
  openxlsx::setColWidths(wb, sheet, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, excel_output, overwrite = TRUE)

cat("\n")
cat("============================================================\n")
cat("Script 23b — Federal Reserve PDF Folder Audit completed\n")
cat("============================================================\n")
cat("Folder audited:\n", target_dir, "\n\n")

cat("PDF files found:\n", nrow(audit), "\n")
cat("Valid PDFs:\n", nrow(valid_pdfs), "\n")
cat("Invalid PDFs:\n", nrow(invalid_pdfs), "\n\n")

cat("Summary:\n")
print(summary)

cat("\nInvalid PDFs:\n")
print(invalid_pdfs |> select(file_name, size_kb, ascii_header, validation_status))

cat("\nOutputs:\n")
cat(" -", audit_csv, "\n")
cat(" -", summary_csv, "\n")
cat(" -", invalid_csv, "\n")
cat(" -", valid_csv, "\n")
cat(" -", excel_output, "\n")
cat("============================================================\n")