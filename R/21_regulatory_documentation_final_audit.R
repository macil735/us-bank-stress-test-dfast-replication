# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 21 — Regulatory Documentation Final Audit, version 2
# ============================================================
# Objective:
#   Perform a final broad audit of regulatory documentation before
#   GitHub publication.
#
# This version searches not only docs/regulatory_framework, but also
# other likely project locations where regulatory documents, source
# inventories, downloaded PDFs, Markdown notes, logs or audit files
# may have been stored.
#
# Search locations:
#   docs/regulatory_framework
#   docs
#   data/raw
#   outputs/regulatory_documentation
#   outputs/data_audit
#   outputs/publication_package
#
# Main outputs:
#   outputs/publication_package/script21_regulatory_documentation_audit.csv
#   outputs/publication_package/script21_regulatory_documentation_summary.csv
#   outputs/publication_package/script21_regulatory_documentation_audit.xlsx
#   docs/regulatory_framework/regulatory_documentation_note.md
#   outputs/publication_package/script21_readme_regulatory_documentation_block.md
#   outputs/publication_package/script21_execution_log.txt
#
# Methodological note:
#   This script does not alter model outputs. It audits documentation
#   coverage, file types and publication wording.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 21 - Regulatory Documentation Final Audit, version 2\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "21"
script_name <- "regulatory_documentation_final_audit_v2"
start_time <- Sys.time()

setwd(project_root)

dir.create("docs/regulatory_framework", recursive = TRUE, showWarnings = FALSE)
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

classify_document_role <- function(file_path, file_name, extension) {
  file_path_lower <- stringr::str_to_lower(file_path)
  file_name_lower <- stringr::str_to_lower(file_name)

  dplyr::case_when(
    extension == "pdf" ~
      "Official or downloaded PDF document",

    extension %in% c("html", "htm") ~
      "Downloaded web page or HTML source",

    extension == "md" &
      stringr::str_detect(file_name_lower, "note|readme|disclaimer|scope|regulatory") ~
      "Markdown note or explanatory regulatory documentation",

    extension == "md" ~
      "Markdown reference or internal note",

    extension == "csv" &
      stringr::str_detect(file_path_lower, "regulatory|documentation|source|audit|catalog|inventory") ~
      "CSV regulatory inventory or audit table",

    extension %in% c("xlsx", "xls") &
      stringr::str_detect(file_path_lower, "regulatory|documentation|source|audit|catalog|inventory") ~
      "Excel regulatory inventory or audit workbook",

    extension == "csv" ~
      "CSV data or audit table",

    extension %in% c("xlsx", "xls") ~
      "Excel workbook",

    extension == "txt" ~
      "Text note or execution log",

    extension == "" ~
      "File without extension",

    TRUE ~
      "Other file type"
  )
}

classify_search_zone <- function(file_path) {
  file_path_lower <- stringr::str_to_lower(file_path)

  dplyr::case_when(
    stringr::str_starts(file_path_lower, "docs/regulatory_framework") ~
      "Primary regulatory framework folder",

    stringr::str_starts(file_path_lower, "outputs/regulatory_documentation") ~
      "Regulatory documentation outputs",

    stringr::str_starts(file_path_lower, "data/raw") ~
      "Raw data folder",

    stringr::str_starts(file_path_lower, "outputs/data_audit") ~
      "Data audit outputs",

    stringr::str_starts(file_path_lower, "outputs/publication_package") ~
      "Publication package outputs",

    stringr::str_starts(file_path_lower, "docs") ~
      "General docs folder",

    TRUE ~
      "Other searched location"
  )
}

is_regulatory_relevant <- function(file_path, file_name, extension) {
  file_path_lower <- stringr::str_to_lower(file_path)
  file_name_lower <- stringr::str_to_lower(file_name)

  keyword_hit <- stringr::str_detect(
    file_path_lower,
    paste(
      c(
        "regulatory",
        "regulation",
        "dodd",
        "dodd-frank",
        "dfast",
        "stress",
        "capital",
        "supervisory",
        "federal",
        "reserve",
        "fed",
        "ccar",
        "scb",
        "basel",
        "capital_plan",
        "public_results",
        "nine_quarter",
        "macro_scenario",
        "documentation",
        "source",
        "dictionary"
      ),
      collapse = "|"
    )
  ) |
    stringr::str_detect(
      file_name_lower,
      paste(
        c(
          "regulatory",
          "regulation",
          "dodd",
          "dfast",
          "stress",
          "capital",
          "supervisory",
          "federal",
          "reserve",
          "fed",
          "ccar",
          "scb",
          "basel",
          "public_results",
          "nine_quarter",
          "macro",
          "documentation",
          "dictionary"
        ),
        collapse = "|"
      )
    )

  extension %in% c("pdf", "md", "csv", "xlsx", "xls", "html", "htm", "txt") & keyword_hit
}

safe_file_info <- function(paths) {
  tibble::tibble(
    file_path = paths,
    file_name = basename(paths),
    relative_directory = dirname(paths),
    extension = stringr::str_to_lower(tools::file_ext(paths)),
    exists = file.exists(paths),
    size_bytes = ifelse(file.exists(paths), file.info(paths)$size, NA_real_),
    modified_at = ifelse(file.exists(paths), as.character(file.info(paths)$mtime), NA_character_)
  )
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Define search locations
# ------------------------------------------------------------

cat("Defining documentation search locations...\n")

search_locations <- tibble::tibble(
  search_location = c(
    "docs/regulatory_framework",
    "docs",
    "data/raw",
    "outputs/regulatory_documentation",
    "outputs/data_audit",
    "outputs/publication_package"
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
# 4. Broad file search
# ------------------------------------------------------------

cat("Searching regulatory documentation across project locations...\n")

all_candidate_files <- character()

for (loc in existing_search_locations) {
  loc_files <- list.files(
    loc,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  all_candidate_files <- c(all_candidate_files, loc_files)
}

all_candidate_files <- unique(all_candidate_files)

all_file_audit <- if (length(all_candidate_files) == 0) {
  tibble::tibble(
    file_path = character(),
    file_name = character(),
    relative_directory = character(),
    extension = character(),
    exists = logical(),
    size_bytes = numeric(),
    modified_at = character()
  )
} else {
  safe_file_info(all_candidate_files)
}

all_file_audit <- all_file_audit |>
  dplyr::mutate(
    search_zone = classify_search_zone(file_path),
    document_role = classify_document_role(file_path, file_name, extension),
    regulatory_relevant = is_regulatory_relevant(file_path, file_name, extension),
    non_empty = exists & !is.na(size_bytes) & size_bytes > 0,
    likely_official_download = extension %in% c("pdf", "html", "htm"),
    likely_internal_note = extension %in% c("md", "txt"),
    likely_inventory = extension %in% c("csv", "xlsx", "xls")
  ) |>
  dplyr::arrange(search_zone, relative_directory, file_name) |>
  safe_df()

regulatory_file_audit <- all_file_audit |>
  dplyr::filter(regulatory_relevant) |>
  safe_df()

cat("Candidate files searched:", nrow(all_file_audit), "\n")
cat("Regulatory-relevant files found:", nrow(regulatory_file_audit), "\n\n")


# ------------------------------------------------------------
# 5. Primary folder audit
# ------------------------------------------------------------

cat("Auditing primary regulatory framework folder separately...\n")

primary_dir <- "docs/regulatory_framework"

primary_files <- list.files(
  primary_dir,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)

primary_file_audit <- if (length(primary_files) == 0) {
  tibble::tibble(
    file_path = character(),
    file_name = character(),
    relative_directory = character(),
    extension = character(),
    exists = logical(),
    size_bytes = numeric(),
    modified_at = character()
  )
} else {
  safe_file_info(primary_files)
}

primary_file_audit <- primary_file_audit |>
  dplyr::mutate(
    search_zone = classify_search_zone(file_path),
    document_role = classify_document_role(file_path, file_name, extension),
    regulatory_relevant = is_regulatory_relevant(file_path, file_name, extension),
    non_empty = exists & !is.na(size_bytes) & size_bytes > 0,
    likely_official_download = extension %in% c("pdf", "html", "htm"),
    likely_internal_note = extension %in% c("md", "txt"),
    likely_inventory = extension %in% c("csv", "xlsx", "xls")
  ) |>
  dplyr::arrange(relative_directory, file_name) |>
  safe_df()

cat("Primary folder files found:", nrow(primary_file_audit), "\n\n")


# ------------------------------------------------------------
# 6. Summaries
# ------------------------------------------------------------

cat("Creating documentation summaries...\n")

extension_summary_all <- regulatory_file_audit |>
  dplyr::group_by(extension) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    non_empty_files = sum(non_empty, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(files)) |>
  safe_df()

extension_summary_primary <- primary_file_audit |>
  dplyr::group_by(extension) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    non_empty_files = sum(non_empty, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(files)) |>
  safe_df()

role_summary_all <- regulatory_file_audit |>
  dplyr::group_by(document_role) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    non_empty_files = sum(non_empty, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(files)) |>
  safe_df()

search_zone_summary <- regulatory_file_audit |>
  dplyr::group_by(search_zone) |>
  dplyr::summarise(
    files = dplyr::n(),
    pdf_files = sum(extension == "pdf", na.rm = TRUE),
    markdown_files = sum(extension == "md", na.rm = TRUE),
    inventory_files = sum(extension %in% c("csv", "xlsx", "xls"), na.rm = TRUE),
    html_files = sum(extension %in% c("html", "htm"), na.rm = TRUE),
    non_empty_files = sum(non_empty, na.rm = TRUE),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(search_zone) |>
  safe_df()

documentation_type_summary <- tibble::tibble(
  item = c(
    "Total candidate files searched",
    "Regulatory-relevant files found",
    "Primary folder files",
    "PDF files found across searched locations",
    "PDF files in primary regulatory folder",
    "Markdown files across searched locations",
    "Markdown files in primary regulatory folder",
    "Inventory files across searched locations",
    "HTML files across searched locations",
    "Non-empty regulatory-relevant files",
    "Regulatory-relevant files outside primary folder"
  ),
  value = c(
    nrow(all_file_audit),
    nrow(regulatory_file_audit),
    nrow(primary_file_audit),
    sum(regulatory_file_audit$extension == "pdf", na.rm = TRUE),
    sum(primary_file_audit$extension == "pdf", na.rm = TRUE),
    sum(regulatory_file_audit$extension == "md", na.rm = TRUE),
    sum(primary_file_audit$extension == "md", na.rm = TRUE),
    sum(regulatory_file_audit$extension %in% c("csv", "xlsx", "xls"), na.rm = TRUE),
    sum(regulatory_file_audit$extension %in% c("html", "htm"), na.rm = TRUE),
    sum(regulatory_file_audit$non_empty, na.rm = TRUE),
    sum(!stringr::str_starts(regulatory_file_audit$file_path, "docs/regulatory_framework"), na.rm = TRUE)
  )
) |>
  safe_df()

pdf_files_all <- regulatory_file_audit |>
  dplyr::filter(extension == "pdf") |>
  safe_df()

pdf_files_primary <- primary_file_audit |>
  dplyr::filter(extension == "pdf") |>
  safe_df()

markdown_files_all <- regulatory_file_audit |>
  dplyr::filter(extension == "md") |>
  safe_df()

inventory_files_all <- regulatory_file_audit |>
  dplyr::filter(extension %in% c("csv", "xlsx", "xls")) |>
  safe_df()

regulatory_files_outside_primary <- regulatory_file_audit |>
  dplyr::filter(!stringr::str_starts(file_path, "docs/regulatory_framework")) |>
  safe_df()

cat("Documentation summaries created.\n\n")


# ------------------------------------------------------------
# 7. Publication interpretation
# ------------------------------------------------------------

cat("Creating publication interpretation...\n")

pdf_count_all <- sum(regulatory_file_audit$extension == "pdf", na.rm = TRUE)
pdf_count_primary <- sum(primary_file_audit$extension == "pdf", na.rm = TRUE)
md_count_all <- sum(regulatory_file_audit$extension == "md", na.rm = TRUE)
md_count_primary <- sum(primary_file_audit$extension == "md", na.rm = TRUE)
inventory_count_all <- sum(regulatory_file_audit$extension %in% c("csv", "xlsx", "xls"), na.rm = TRUE)
outside_primary_count <- nrow(regulatory_files_outside_primary)

documentation_publication_status <- dplyr::case_when(
  nrow(regulatory_file_audit) == 0 ~
    "NO_REGULATORY_RELEVANT_FILES_FOUND",

  pdf_count_primary > 0 & md_count_primary > 0 ~
    "PRIMARY_FOLDER_MIXED_PDF_AND_MARKDOWN",

  pdf_count_primary > 0 & md_count_primary == 0 ~
    "PRIMARY_FOLDER_HAS_PDFS",

  pdf_count_primary == 0 & pdf_count_all > 0 ~
    "PDFS_FOUND_OUTSIDE_PRIMARY_REGULATORY_FOLDER",

  pdf_count_all == 0 & md_count_all > 0 ~
    "MARKDOWN_NOTES_ONLY_OR_MARKDOWN_DOMINANT",

  TRUE ~
    "REGULATORY_SUPPORT_FILES_PRESENT_BUT_NO_CLEAR_PDF_ARCHIVE"
)

recommended_repository_language <- dplyr::case_when(
  documentation_publication_status == "PRIMARY_FOLDER_MIXED_PDF_AND_MARKDOWN" ~
    "The repository contains a mixed regulatory documentation folder, including downloaded public PDF documents where available, Markdown notes, source references and audit inventories.",

  documentation_publication_status == "PRIMARY_FOLDER_HAS_PDFS" ~
    "The repository contains downloaded public regulatory PDF documents in the regulatory framework folder, together with supporting notes and inventories where applicable.",

  documentation_publication_status == "PDFS_FOUND_OUTSIDE_PRIMARY_REGULATORY_FOLDER" ~
    "The repository contains regulatory-relevant PDF files, but they are located outside the primary docs/regulatory_framework folder. The documentation layer should be described as distributed across regulatory notes, inventories and downloaded files.",

  documentation_publication_status == "MARKDOWN_NOTES_ONLY_OR_MARKDOWN_DOMINANT" ~
    "The repository contains regulatory documentation notes, source references and audit inventories mainly in Markdown and tabular formats. It should not be described as containing a complete local PDF archive of regulatory documents.",

  documentation_publication_status == "NO_REGULATORY_RELEVANT_FILES_FOUND" ~
    "No regulatory-relevant documentation files were found in the searched locations. The repository should not claim that regulatory documentation is included until corrected.",

  TRUE ~
    "The repository contains regulatory support files, but file types and locations should be described carefully."
)

publication_risk <- dplyr::case_when(
  documentation_publication_status == "NO_REGULATORY_RELEVANT_FILES_FOUND" ~
    "High: documentation claims should be corrected before publication.",

  documentation_publication_status == "PDFS_FOUND_OUTSIDE_PRIMARY_REGULATORY_FOLDER" ~
    "Moderate: PDFs exist, but location should be clarified or files should be moved/copied to docs/regulatory_framework/source_documents.",

  documentation_publication_status == "MARKDOWN_NOTES_ONLY_OR_MARKDOWN_DOMINANT" ~
    "Low to moderate: publication is acceptable if the repository does not claim a complete PDF archive.",

  TRUE ~
    "Low: publication is acceptable if documentation wording is accurate."
)

required_action <- dplyr::case_when(
  documentation_publication_status == "PDFS_FOUND_OUTSIDE_PRIMARY_REGULATORY_FOLDER" ~
    "Either clarify in README that regulatory documents are distributed across folders or copy the PDF files to docs/regulatory_framework/source_documents before publication.",

  documentation_publication_status == "MARKDOWN_NOTES_ONLY_OR_MARKDOWN_DOMINANT" ~
    "Update README wording to state that regulatory documentation is represented by Markdown notes, source references and inventories, not a complete local PDF archive.",

  documentation_publication_status == "NO_REGULATORY_RELEVANT_FILES_FOUND" ~
    "Add regulatory documentation notes or source inventories before publication.",

  TRUE ~
    "No blocking action required; retain accurate repository wording."
)

publication_interpretation <- tibble::tibble(
  item = c(
    "Documentation publication status",
    "PDF count across searched locations",
    "PDF count in primary regulatory folder",
    "Markdown count across searched locations",
    "Markdown count in primary regulatory folder",
    "Inventory count across searched locations",
    "Regulatory-relevant files outside primary folder",
    "Recommended repository language",
    "Publication risk",
    "Required action before GitHub publication"
  ),
  value = c(
    documentation_publication_status,
    as.character(pdf_count_all),
    as.character(pdf_count_primary),
    as.character(md_count_all),
    as.character(md_count_primary),
    as.character(inventory_count_all),
    as.character(outside_primary_count),
    recommended_repository_language,
    publication_risk,
    required_action
  )
) |>
  safe_df()

cat("Publication interpretation created.\n\n")


# ------------------------------------------------------------
# 8. Create regulatory documentation note
# ------------------------------------------------------------

cat("Creating docs/regulatory_framework/regulatory_documentation_note.md...\n")

note_path <- "docs/regulatory_framework/regulatory_documentation_note.md"

note_lines <- c(
  "# Regulatory Documentation Note",
  "",
  "This note records the final audit of regulatory documentation used in the USA Bank Stress Test DFAST Replication project.",
  "",
  "## Important clarification",
  "",
  "The regulatory documentation layer should not automatically be interpreted as a complete local archive of official PDF regulatory documents.",
  "",
  "Depending on public source availability, download stability and repository organization, the project may contain a combination of:",
  "",
  "- Markdown notes;",
  "- source references;",
  "- documentation inventories;",
  "- audit tables;",
  "- downloaded public PDF documents where available;",
  "- downloaded HTML source material;",
  "- supporting files generated by the reproducibility pipeline.",
  "",
  "## Search locations audited",
  "",
  paste0("- ", search_locations$search_location, " — exists: ", search_locations$exists),
  "",
  "## Current audit result",
  "",
  paste0("- Candidate files searched: ", nrow(all_file_audit)),
  paste0("- Regulatory-relevant files found: ", nrow(regulatory_file_audit)),
  paste0("- Files in primary regulatory folder: ", nrow(primary_file_audit)),
  paste0("- PDF files across searched locations: ", pdf_count_all),
  paste0("- PDF files in primary regulatory folder: ", pdf_count_primary),
  paste0("- Markdown files across searched locations: ", md_count_all),
  paste0("- Markdown files in primary regulatory folder: ", md_count_primary),
  paste0("- Inventory files across searched locations: ", inventory_count_all),
  paste0("- Regulatory-relevant files outside primary folder: ", outside_primary_count),
  "",
  "## Publication interpretation",
  "",
  paste0("Documentation publication status: ", documentation_publication_status),
  "",
  recommended_repository_language,
  "",
  "## Required action",
  "",
  required_action,
  "",
  "## Teaching note",
  "",
  "Students should distinguish between four different objects:",
  "",
  "1. Public data files used directly in the empirical pipeline;",
  "2. Official public regulatory documents downloaded as local files;",
  "3. Markdown notes that summarize or reference regulatory sources;",
  "4. Audit tables that record source availability and download status.",
  "",
  "## Disclaimer",
  "",
  "This project is an independent educational and analytical project based on public data. It does not use confidential supervisory information, confidential Federal Reserve models, confidential bank submissions or non-public bank data."
)

write_lines_utf8(note_lines, note_path)

cat("Regulatory documentation note created.\n\n")


# ------------------------------------------------------------
# 9. README wording recommendation
# ------------------------------------------------------------

cat("Creating README wording recommendation...\n")

readme_regulatory_block_path <- "outputs/publication_package/script21_readme_regulatory_documentation_block.md"

readme_regulatory_block <- c(
  "## Regulatory documentation note",
  "",
  "The repository includes a regulatory documentation layer under `docs/regulatory_framework/` and supporting audit outputs.",
  "",
  recommended_repository_language,
  "",
  "The folder may include Markdown notes, source references, documentation inventories, audit tables and downloaded public documents where available. It should not be interpreted as a complete local archive of official regulatory PDFs unless PDF files are explicitly present.",
  "",
  "The empirical pipeline relies on public Federal Reserve DFAST data and public scenario files. Regulatory documentation files support interpretation, transparency and teaching, but the project does not use confidential supervisory information."
)

write_lines_utf8(readme_regulatory_block, readme_regulatory_block_path)

cat("README wording recommendation created.\n\n")


# ------------------------------------------------------------
# 10. Optional recommendation to copy PDFs if found outside primary
# ------------------------------------------------------------

cat("Creating optional PDF relocation recommendation...\n")

pdf_relocation_recommendation <- if (pdf_count_all > 0 & pdf_count_primary == 0) {
  pdf_files_all |>
    dplyr::mutate(
      recommended_target_folder = "docs/regulatory_framework/source_documents",
      recommended_action = paste0(
        "Consider copying this PDF to docs/regulatory_framework/source_documents/",
        file_name
      )
    ) |>
    safe_df()
} else {
  tibble::tibble(
    file_path = character(),
    file_name = character(),
    recommended_target_folder = character(),
    recommended_action = character()
  ) |>
    safe_df()
}

cat("Optional PDF relocation recommendation created.\n\n")


# ------------------------------------------------------------
# 11. Save outputs
# ------------------------------------------------------------

cat("Saving Script 21 v2 outputs...\n")

out_dir <- "outputs/publication_package"

paths_out <- list(
  all_file_audit = file.path(out_dir, "script21_all_candidate_file_audit.csv"),
  regulatory_file_audit = file.path(out_dir, "script21_regulatory_documentation_audit.csv"),
  primary_file_audit = file.path(out_dir, "script21_primary_regulatory_folder_audit.csv"),
  extension_summary_all = file.path(out_dir, "script21_extension_summary_all_regulatory_relevant.csv"),
  extension_summary_primary = file.path(out_dir, "script21_extension_summary_primary_folder.csv"),
  role_summary_all = file.path(out_dir, "script21_role_summary_all_regulatory_relevant.csv"),
  search_zone_summary = file.path(out_dir, "script21_search_zone_summary.csv"),
  documentation_type_summary = file.path(out_dir, "script21_regulatory_documentation_type_summary.csv"),
  publication_interpretation = file.path(out_dir, "script21_regulatory_documentation_publication_interpretation.csv"),
  pdf_files_all = file.path(out_dir, "script21_pdf_files_all_locations.csv"),
  pdf_files_primary = file.path(out_dir, "script21_pdf_files_primary_folder.csv"),
  markdown_files_all = file.path(out_dir, "script21_markdown_files_all_locations.csv"),
  inventory_files_all = file.path(out_dir, "script21_inventory_files_all_locations.csv"),
  regulatory_files_outside_primary = file.path(out_dir, "script21_regulatory_files_outside_primary_folder.csv"),
  pdf_relocation_recommendation = file.path(out_dir, "script21_pdf_relocation_recommendation.csv"),
  excel = file.path(out_dir, "script21_regulatory_documentation_audit.xlsx"),
  execution_summary = file.path(out_dir, "script21_execution_summary.csv"),
  execution_log = file.path(out_dir, "script21_execution_log.txt")
)

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  candidate_files_searched = nrow(all_file_audit),
  regulatory_relevant_files_found = nrow(regulatory_file_audit),
  primary_folder_files = nrow(primary_file_audit),
  pdf_files_all_locations = pdf_count_all,
  pdf_files_primary_folder = pdf_count_primary,
  markdown_files_all_locations = md_count_all,
  markdown_files_primary_folder = md_count_primary,
  inventory_files_all_locations = inventory_count_all,
  regulatory_files_outside_primary_folder = outside_primary_count,
  documentation_publication_status = documentation_publication_status,
  publication_risk = publication_risk,
  required_action = required_action,
  regulatory_note_created = file.exists(note_path),
  readme_regulatory_block_created = file.exists(readme_regulatory_block_path)
) |>
  safe_df()

readr::write_csv(all_file_audit, paths_out$all_file_audit)
readr::write_csv(regulatory_file_audit, paths_out$regulatory_file_audit)
readr::write_csv(primary_file_audit, paths_out$primary_file_audit)
readr::write_csv(extension_summary_all, paths_out$extension_summary_all)
readr::write_csv(extension_summary_primary, paths_out$extension_summary_primary)
readr::write_csv(role_summary_all, paths_out$role_summary_all)
readr::write_csv(search_zone_summary, paths_out$search_zone_summary)
readr::write_csv(documentation_type_summary, paths_out$documentation_type_summary)
readr::write_csv(publication_interpretation, paths_out$publication_interpretation)
readr::write_csv(pdf_files_all, paths_out$pdf_files_all)
readr::write_csv(pdf_files_primary, paths_out$pdf_files_primary)
readr::write_csv(markdown_files_all, paths_out$markdown_files_all)
readr::write_csv(inventory_files_all, paths_out$inventory_files_all)
readr::write_csv(regulatory_files_outside_primary, paths_out$regulatory_files_outside_primary)
readr::write_csv(pdf_relocation_recommendation, paths_out$pdf_relocation_recommendation)
readr::write_csv(execution_summary, paths_out$execution_summary)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  execution_summary = execution_summary,
  publication_interpretation = publication_interpretation,
  type_summary = documentation_type_summary,
  search_zone_summary = search_zone_summary,
  extension_summary_all = extension_summary_all,
  extension_summary_primary = extension_summary_primary,
  role_summary_all = role_summary_all,
  primary_folder_audit = primary_file_audit,
  regulatory_file_audit = regulatory_file_audit,
  pdf_files_all = pdf_files_all,
  pdf_files_primary = pdf_files_primary,
  markdown_files_all = markdown_files_all,
  inventory_files_all = inventory_files_all,
  outside_primary = regulatory_files_outside_primary,
  pdf_relocation = pdf_relocation_recommendation,
  search_locations = search_locations
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, paths_out$excel, overwrite = TRUE)

cat("Script 21 v2 outputs saved.\n\n")


# ------------------------------------------------------------
# 12. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 21 - Regulatory Documentation Final Audit, version 2 completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Search locations:",
  capture.output(print(search_locations)),
  "",
  paste("Candidate files searched:", nrow(all_file_audit)),
  paste("Regulatory-relevant files found:", nrow(regulatory_file_audit)),
  paste("Primary folder files:", nrow(primary_file_audit)),
  paste("PDF files across searched locations:", pdf_count_all),
  paste("PDF files in primary regulatory folder:", pdf_count_primary),
  paste("Markdown files across searched locations:", md_count_all),
  paste("Markdown files in primary regulatory folder:", md_count_primary),
  paste("Inventory files across searched locations:", inventory_count_all),
  paste("Regulatory-relevant files outside primary folder:", outside_primary_count),
  paste("Documentation publication status:", documentation_publication_status),
  paste("Publication risk:", publication_risk),
  paste("Required action:", required_action),
  "",
  "Publication interpretation:",
  capture.output(print(publication_interpretation)),
  "",
  "Documentation type summary:",
  capture.output(print(documentation_type_summary)),
  "",
  "Search zone summary:",
  capture.output(print(search_zone_summary)),
  "",
  "Extension summary all:",
  capture.output(print(extension_summary_all)),
  "",
  "Extension summary primary:",
  capture.output(print(extension_summary_primary)),
  "",
  "PDF files all locations:",
  capture.output(print(pdf_files_all)),
  "",
  "Main outputs:",
  paste(" -", paths_out$all_file_audit),
  paste(" -", paths_out$regulatory_file_audit),
  paste(" -", paths_out$primary_file_audit),
  paste(" -", paths_out$extension_summary_all),
  paste(" -", paths_out$extension_summary_primary),
  paste(" -", paths_out$role_summary_all),
  paste(" -", paths_out$search_zone_summary),
  paste(" -", paths_out$documentation_type_summary),
  paste(" -", paths_out$publication_interpretation),
  paste(" -", paths_out$pdf_files_all),
  paste(" -", paths_out$pdf_files_primary),
  paste(" -", paths_out$markdown_files_all),
  paste(" -", paths_out$inventory_files_all),
  paste(" -", paths_out$regulatory_files_outside_primary),
  paste(" -", paths_out$pdf_relocation_recommendation),
  paste(" -", paths_out$excel),
  paste(" -", note_path),
  paste(" -", readme_regulatory_block_path),
  paste(" -", paths_out$execution_log)
)

write_lines_utf8(log_lines, paths_out$execution_log)


# ------------------------------------------------------------
# 13. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 21 - Regulatory Documentation Final Audit, version 2 completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Candidate files searched:\n", nrow(all_file_audit), "\n")
cat("Regulatory-relevant files found:\n", nrow(regulatory_file_audit), "\n")
cat("Primary folder files:\n", nrow(primary_file_audit), "\n")
cat("PDF files across searched locations:\n", pdf_count_all, "\n")
cat("PDF files in primary regulatory folder:\n", pdf_count_primary, "\n")
cat("Markdown files across searched locations:\n", md_count_all, "\n")
cat("Markdown files in primary regulatory folder:\n", md_count_primary, "\n")
cat("Inventory files across searched locations:\n", inventory_count_all, "\n")
cat("Regulatory-relevant files outside primary folder:\n", outside_primary_count, "\n")
cat("Documentation publication status:\n", documentation_publication_status, "\n")
cat("Publication risk:\n", publication_risk, "\n")
cat("Required action:\n", required_action, "\n\n")

cat("Publication interpretation:\n")
print(publication_interpretation)

cat("\nSearch zone summary:\n")
print(search_zone_summary)

cat("\nExtension summary across all regulatory-relevant files:\n")
print(extension_summary_all)

cat("\nExtension summary in primary regulatory folder:\n")
print(extension_summary_primary)

cat("\nPDF files found across all searched locations:\n")
print(pdf_files_all)

cat("\nMain outputs:\n")
cat(" -", paths_out$all_file_audit, "\n")
cat(" -", paths_out$regulatory_file_audit, "\n")
cat(" -", paths_out$primary_file_audit, "\n")
cat(" -", paths_out$search_zone_summary, "\n")
cat(" -", paths_out$documentation_type_summary, "\n")
cat(" -", paths_out$publication_interpretation, "\n")
cat(" -", paths_out$pdf_files_all, "\n")
cat(" -", paths_out$pdf_files_primary, "\n")
cat(" -", paths_out$excel, "\n")
cat(" -", note_path, "\n")
cat(" -", readme_regulatory_block_path, "\n")
cat(" -", paths_out$execution_log, "\n")
cat("============================================================\n")