# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 02c — Fix Missing Regulatory Documentation Downloads
# ============================================================
# Objective:
#   Correct the missing regulatory documentation downloads from
#   Script 02b without changing data/raw or data/processed.
#
# Project root:
#   D:/GitHub/us-bank-stress-test-dfast-replication
#
# Main documentation folder:
#   docs/regulatory_framework/source_documents/
#
# Main outputs:
#   outputs/regulatory_documentation/script02c_fix_targets.csv
#   outputs/regulatory_documentation/script02c_download_log.csv
#   outputs/regulatory_documentation/script02c_document_inventory.csv
#   outputs/regulatory_documentation/script02c_execution_summary.csv
#   outputs/regulatory_documentation/script02c_execution_log.txt
#
# Methodological note:
#   This script is corrective. It only tries to recover documents
#   that failed in Script 02b.
# ============================================================


# ------------------------------------------------------------
# 0. Initial setup
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

script_id <- "02c"
script_name <- "fix_missing_regulatory_documentation"
start_time <- Sys.time()

dir_list <- c(
  file.path(project_root, "R"),
  file.path(project_root, "docs"),
  file.path(project_root, "docs/regulatory_framework"),
  file.path(project_root, "docs/regulatory_framework/source_documents"),
  file.path(project_root, "docs/regulatory_framework/source_documents/federal_reserve"),
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

download_one <- function(document_id,
                         document_group,
                         document_title,
                         url,
                         destination_path,
                         attempt_priority,
                         timeout_sec = 120) {

  destination_dir <- dirname(destination_path)

  if (!dir.exists(destination_dir)) dir.create(destination_dir, recursive = TRUE)

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
        document_group = document_group,
        document_title = document_title,
        attempt_priority = attempt_priority,
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
        document_group = document_group,
        document_title = document_title,
        attempt_priority = attempt_priority,
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


# ------------------------------------------------------------
# 3. Corrective targets
# ------------------------------------------------------------
# Strategy:
#   - For the two data dictionaries, try likely Fed file names.
#   - For large bank capital requirements, use the Fed annual page
#     as the stable reference and also try known PDF patterns.
#   - The script does not fail if some attempts remain unavailable.

docs_root <- file.path(project_root, "docs/regulatory_framework/source_documents")
fed_dir <- file.path(docs_root, "federal_reserve")

fix_targets <- tribble(
  ~document_id, ~document_group, ~document_title, ~attempt_priority, ~url, ~destination_file,

  "FED_PUBLIC_RESULTS_DICTIONARY_ATTEMPT_01",
  "Public Results Data Dictionary",
  "Public Results DFAST 2026 Data Dictionary",
  1,
  "https://www.federalreserve.gov/supervisionreg/files/public_results_DFAST_2026_dictionary.pdf",
  "fed_public_results_DFAST_2026_dictionary_attempt_01.pdf",

  "FED_PUBLIC_RESULTS_DICTIONARY_ATTEMPT_02",
  "Public Results Data Dictionary",
  "Public Results DFAST 2026 Data Dictionary",
  2,
  "https://www.federalreserve.gov/supervisionreg/files/Public_Results_DFAST_2026_Data_Dictionary.pdf",
  "fed_public_results_DFAST_2026_dictionary_attempt_02.pdf",

  "FED_PUBLIC_RESULTS_DICTIONARY_ATTEMPT_03",
  "Public Results Data Dictionary",
  "Stress Test Results Data Dictionary",
  3,
  "https://www.federalreserve.gov/supervisionreg/files/Stress_Test_Results_Data_Dictionary.pdf",
  "fed_stress_test_results_data_dictionary_attempt_03.pdf",

  "FED_9Q_PATHS_DICTIONARY_ATTEMPT_01",
  "Detailed Nine Quarter Paths Dictionary",
  "2026 Detailed Nine Quarter Paths Data Dictionary",
  1,
  "https://www.federalreserve.gov/supervisionreg/files/2026_Detailed_Nine_Quarter_Paths_dictionary.pdf",
  "fed_2026_detailed_nine_quarter_paths_dictionary_attempt_01.pdf",

  "FED_9Q_PATHS_DICTIONARY_ATTEMPT_02",
  "Detailed Nine Quarter Paths Dictionary",
  "Detailed Nine Quarter Path Data Dictionary December 2025",
  2,
  "https://www.federalreserve.gov/supervisionreg/files/Detailed_Nine_Quarter_Path_Data_Dictionary_December_2025.pdf",
  "fed_detailed_nine_quarter_path_data_dictionary_december_2025.pdf",

  "FED_9Q_PATHS_DICTIONARY_ATTEMPT_03",
  "Detailed Nine Quarter Paths Dictionary",
  "Detailed Nine Quarter Path Data Dictionary",
  3,
  "https://www.federalreserve.gov/supervisionreg/files/Detailed_Nine_Quarter_Path_Data_Dictionary.pdf",
  "fed_detailed_nine_quarter_path_data_dictionary_attempt_03.pdf",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_01",
  "Large Bank Capital Requirements",
  "Annual Large Bank Capital Requirements Page",
  1,
  "https://www.federalreserve.gov/supervisionreg/large-bank-capital-requirements.htm",
  "fed_annual_large_bank_capital_requirements_page.html",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_02",
  "Large Bank Capital Requirements",
  "Large Bank Capital Requirements August 2025",
  2,
  "https://www.federalreserve.gov/publications/files/large-bank-capital-requirements-20250829.pdf",
  "fed_large_bank_capital_requirements_20250829.pdf",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_03",
  "Large Bank Capital Requirements",
  "Large Bank Capital Requirements June 2026",
  3,
  "https://www.federalreserve.gov/publications/files/large-bank-capital-requirements-20260624.pdf",
  "fed_large_bank_capital_requirements_20260624.pdf",

  "FED_LARGE_BANK_CAPITAL_REQUIREMENTS_ATTEMPT_04",
  "Large Bank Capital Requirements",
  "Large Bank Capital Requirements September 2025",
  4,
  "https://www.federalreserve.gov/publications/files/large-bank-capital-requirements-20250930.pdf",
  "fed_large_bank_capital_requirements_20250930.pdf"
) |>
  mutate(
    destination_path = file.path(fed_dir, destination_file)
  ) |>
  safe_df()


# ------------------------------------------------------------
# 4. Download corrective targets
# ------------------------------------------------------------

message("Fixing missing regulatory documentation downloads...")

download_log <- pmap_dfr(
  list(
    fix_targets$document_id,
    fix_targets$document_group,
    fix_targets$document_title,
    fix_targets$url,
    fix_targets$destination_path,
    fix_targets$attempt_priority
  ),
  download_one
) |>
  safe_df()


# ------------------------------------------------------------
# 5. Select best available document per group
# ------------------------------------------------------------

best_available <- download_log |>
  filter(success) |>
  arrange(document_group, attempt_priority) |>
  group_by(document_group) |>
  slice(1) |>
  ungroup() |>
  select(
    document_group,
    selected_document_id = document_id,
    selected_title = document_title,
    selected_url = url,
    selected_destination_path = destination_path,
    file_size_bytes
  ) |>
  safe_df()

group_status <- fix_targets |>
  distinct(document_group) |>
  left_join(
    best_available |> select(document_group, selected_document_id),
    by = "document_group"
  ) |>
  mutate(
    fixed = !is.na(selected_document_id),
    status = ifelse(fixed, "Fixed", "Still missing")
  ) |>
  safe_df()


# ------------------------------------------------------------
# 6. Updated documentation inventory
# ------------------------------------------------------------

document_inventory <- make_inventory(docs_root) |>
  safe_df()


# ------------------------------------------------------------
# 7. Execution summary
# ------------------------------------------------------------

download_summary <- download_log |>
  group_by(document_group) |>
  summarise(
    attempts = n(),
    successful_attempts = sum(success, na.rm = TRUE),
    failed_attempts = sum(!success, na.rm = TRUE),
    total_size_bytes = sum(file_size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(document_group) |>
  safe_df()

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  corrective_targets = nrow(fix_targets),
  successful_attempts = sum(download_log$success, na.rm = TRUE),
  failed_attempts = sum(!download_log$success, na.rm = TRUE),
  groups_fixed = sum(group_status$fixed, na.rm = TRUE),
  groups_still_missing = sum(!group_status$fixed, na.rm = TRUE),
  documents_inventoried = nrow(document_inventory)
) |>
  safe_df()


# ------------------------------------------------------------
# 8. Save outputs
# ------------------------------------------------------------

out_dir <- file.path(project_root, "outputs/regulatory_documentation")

targets_csv <- file.path(out_dir, "script02c_fix_targets.csv")
download_log_csv <- file.path(out_dir, "script02c_download_log.csv")
best_available_csv <- file.path(out_dir, "script02c_best_available_documents.csv")
group_status_csv <- file.path(out_dir, "script02c_group_status.csv")
inventory_csv <- file.path(out_dir, "script02c_document_inventory.csv")
summary_csv <- file.path(out_dir, "script02c_download_summary.csv")
execution_summary_csv <- file.path(out_dir, "script02c_execution_summary.csv")
excel_output <- file.path(out_dir, "script02c_fix_missing_regulatory_documentation_outputs.xlsx")
report_docx <- file.path(out_dir, "script02c_fix_missing_regulatory_documentation_report.docx")
execution_log_txt <- file.path(out_dir, "script02c_execution_log.txt")

write_csv(fix_targets, targets_csv)
write_csv(download_log, download_log_csv)
write_csv(best_available, best_available_csv)
write_csv(group_status, group_status_csv)
write_csv(document_inventory, inventory_csv)
write_csv(download_summary, summary_csv)
write_csv(execution_summary, execution_summary_csv)

wb <- createWorkbook()

addWorksheet(wb, "fix_targets")
writeData(wb, "fix_targets", fix_targets)

addWorksheet(wb, "download_log")
writeData(wb, "download_log", download_log)

addWorksheet(wb, "best_available")
writeData(wb, "best_available", best_available)

addWorksheet(wb, "group_status")
writeData(wb, "group_status", group_status)

addWorksheet(wb, "document_inventory")
writeData(wb, "document_inventory", document_inventory)

addWorksheet(wb, "download_summary")
writeData(wb, "download_summary", download_summary)

addWorksheet(wb, "execution_summary")
writeData(wb, "execution_summary", execution_summary)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_output, overwrite = TRUE)


# ------------------------------------------------------------
# 9. Word report
# ------------------------------------------------------------

doc <- read_docx()

doc <- doc |>
  body_add_par("Script 02c - Fix Missing Regulatory Documentation Downloads", style = "heading 1") |>
  body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  body_add_par("1. Objective", style = "heading 2") |>
  body_add_par(
    "This corrective script attempts to recover regulatory documentation that failed in Script 02b. It does not alter raw or processed data.",
    style = "Normal"
  ) |>
  body_add_par("2. Execution summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(execution_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("3. Group status", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(group_status) |>
    autofit()
)

doc <- doc |>
  body_add_par("4. Best available documents", style = "heading 2")

if (nrow(best_available) == 0) {
  doc <- body_add_par(doc, "No missing document group was fixed.", style = "Normal")
} else {
  best_available_small <- best_available |>
    select(document_group, selected_document_id, selected_title, selected_destination_path, file_size_bytes)

  doc <- body_add_flextable(
    doc,
    flextable(best_available_small) |>
      autofit()
  )
}

doc <- doc |>
  body_add_par("5. Download summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(download_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("6. Methodological note", style = "heading 2") |>
  body_add_par(
    "If some documents remain missing, the project may continue as long as the core Federal Reserve scenario and results datasets are available. Missing documentation should be recorded as a reproducibility limitation.",
    style = "Normal"
  )

print(doc, target = report_docx)


# ------------------------------------------------------------
# 10. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 02c - Fix Missing Regulatory Documentation Downloads completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Corrective targets:", nrow(fix_targets)),
  paste("Successful attempts:", sum(download_log$success, na.rm = TRUE)),
  paste("Failed attempts:", sum(!download_log$success, na.rm = TRUE)),
  paste("Groups fixed:", sum(group_status$fixed, na.rm = TRUE)),
  paste("Groups still missing:", sum(!group_status$fixed, na.rm = TRUE)),
  paste("Documents inventoried:", nrow(document_inventory)),
  "",
  "Group status:",
  capture.output(print(group_status)),
  "",
  "Best available documents:",
  capture.output(print(best_available)),
  "",
  "Download summary:",
  capture.output(print(download_summary)),
  "",
  "Main outputs:",
  paste(" -", targets_csv),
  paste(" -", download_log_csv),
  paste(" -", best_available_csv),
  paste(" -", group_status_csv),
  paste(" -", inventory_csv),
  paste(" -", summary_csv),
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
cat("Script 02c - Fix Missing Regulatory Documentation Downloads completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Corrective targets:\n", nrow(fix_targets), "\n")
cat("Successful attempts:\n", sum(download_log$success, na.rm = TRUE), "\n")
cat("Failed attempts:\n", sum(!download_log$success, na.rm = TRUE), "\n")
cat("Groups fixed:\n", sum(group_status$fixed, na.rm = TRUE), "\n")
cat("Groups still missing:\n", sum(!group_status$fixed, na.rm = TRUE), "\n")
cat("Documents inventoried:\n", nrow(document_inventory), "\n\n")

cat("Group status:\n")
print(group_status)

cat("\nBest available documents:\n")
print(best_available)

cat("\nDownload summary:\n")
print(download_summary)

cat("\nMain outputs:\n")
cat(" -", targets_csv, "\n")
cat(" -", download_log_csv, "\n")
cat(" -", best_available_csv, "\n")
cat(" -", group_status_csv, "\n")
cat(" -", inventory_csv, "\n")
cat(" -", summary_csv, "\n")
cat(" -", execution_summary_csv, "\n")
cat(" -", excel_output, "\n")
cat(" -", report_docx, "\n")

cat("\nAdditional outputs:\n")
cat(" -", execution_log_txt, "\n")