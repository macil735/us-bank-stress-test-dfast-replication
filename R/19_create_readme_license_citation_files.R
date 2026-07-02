# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 19 — Create README, LICENSE and CITATION Files
# ============================================================
# Objective:
#   Create the formal repository files required for publication:
#   README.md, LICENSE, CITATION.cff, public disclaimer,
#   reproducibility notes and project scope documentation.
#
# Main outputs:
#   README.md
#   LICENSE
#   CITATION.cff
#   docs/public_disclaimer.md
#   docs/reproducibility_notes.md
#   docs/project_scope.md
#   outputs/publication_package/script19_publication_metadata_check.xlsx
#   outputs/publication_package/script19_execution_log.txt
#
# Methodological note:
#   This script does not alter data, models or results.
#   It only creates repository metadata and publication documents.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 19 - Create README, LICENSE and CITATION Files\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "19"
script_name <- "create_readme_license_citation_files"
start_time <- Sys.time()

setwd(project_root)

dir.create("docs", recursive = TRUE, showWarnings = FALSE)
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
  "janitor",
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

file_status <- function(path, required = TRUE, description = NA_character_) {
  exists <- file.exists(path)

  tibble::tibble(
    description = description,
    file_path = path,
    required = required,
    exists = exists,
    size_bytes = ifelse(exists, file.info(path)$size, NA_real_),
    modified_at = ifelse(exists, as.character(file.info(path)$mtime), NA_character_),
    status = dplyr::case_when(
      required & exists & file.info(path)$size > 0 ~ "OK",
      required & (!exists | file.info(path)$size == 0) ~ "MISSING_OR_EMPTY",
      !required & exists ~ "OPTIONAL_PRESENT",
      !required & !exists ~ "OPTIONAL_MISSING",
      TRUE ~ "UNKNOWN"
    )
  )
}

safe_read_csv_optional <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }

  readr::read_csv(
    path,
    show_col_types = FALSE,
    guess_max = 100000
  ) |>
    janitor::clean_names() |>
    safe_df()
}

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Load Script 18 drafts if available
# ------------------------------------------------------------

cat("Loading Script 18 draft blocks if available...\n")

readme_results_block_path <- "outputs/publication_package/script18_readme_results_block.md"
release_notes_draft_path <- "outputs/publication_package/script18_release_notes_draft.md"

readme_results_block <- if (file.exists(readme_results_block_path)) {
  readLines(readme_results_block_path, warn = FALSE, encoding = "UTF-8")
} else {
  c(
    "## Final Results",
    "",
    "- Final bank-year-scenario observations: 497",
    "- Banks: 56",
    "- Exercise years: 11",
    "- Scenario categories: 3",
    "- Latest exercise year: 2025",
    "- CET1 minimum ratio RMSE: 0.846",
    "- CET1 minimum ratio MAE: 0.5873",
    "- CET1 minimum ratio R-squared: 0.9688",
    "- CET1 minimum ratio correlation: 0.9843"
  )
}

cat("Draft blocks loaded.\n\n")


# ------------------------------------------------------------
# 4. Repository metadata
# ------------------------------------------------------------

cat("Defining repository metadata...\n")

project_title <- "USA Bank Stress Test DFAST Replication"
project_subtitle <- "Public reduced-form replication of DFAST-style bank stress testing"
author_name <- "Gelo Picol"
github_user <- "macil735"
repository_url <- "https://github.com/macil735/us-bank-stress-test-dfast-replication"
release_version <- "v1.0.0"
release_year <- format(Sys.Date(), "%Y")

metadata_table <- tibble::tibble(
  field = c(
    "Project title",
    "Subtitle",
    "Author",
    "GitHub user",
    "Repository URL",
    "Release version",
    "Release year",
    "License"
  ),
  value = c(
    project_title,
    project_subtitle,
    author_name,
    github_user,
    repository_url,
    release_version,
    release_year,
    "MIT License"
  )
) |>
  safe_df()

cat("Repository metadata defined.\n\n")


# ------------------------------------------------------------
# 5. README.md
# ------------------------------------------------------------

cat("Creating README.md...\n")

readme_lines <- c(
  paste0("# ", project_title),
  "",
  paste0("**", project_subtitle, "**"),
  "",
  "This repository contains a public-data, reproducible stress testing project for large U.S. banking organizations. The project builds a reduced-form DFAST-style stress test engine using public Federal Reserve stress test results, public macroeconomic scenario data, reproducible R scripts, validation diagnostics, final reports and teaching material.",
  "",
  "## Purpose",
  "",
  "The purpose of the project is to provide a transparent and auditable framework for understanding how public stress testing outputs can be organized, modelled, validated and communicated.",
  "",
  "The project is designed for:",
  "",
  "- applied banking and financial regulation courses;",
  "- macro-financial stress testing instruction;",
  "- financial econometrics teaching;",
  "- reproducible research demonstrations;",
  "- public benchmarking of stress test outputs.",
  "",
  "## Important disclaimer",
  "",
  "This repository is an independent educational and analytical project based entirely on public data. It is not affiliated with, endorsed by or approved by the Federal Reserve, any banking organization or any supervisory authority.",
  "",
  "No confidential supervisory information, confidential Federal Reserve models, confidential bank submissions, internal bank models or non-public bank data are used.",
  "",
  "The results must not be interpreted as an official regulatory assessment, investment recommendation, credit opinion, bank rating or statement on the safety and soundness of any institution.",
  "",
  "## Repository structure",
  "",
  "```text",
  "R/                         R scripts for the complete reproducible pipeline",
  "data/raw/                  Raw public data downloaded or ingested by the pipeline",
  "data/processed/            Cleaned and processed analytical datasets",
  "docs/                      Scope notes, disclaimers and reproducibility documentation",
  "report/                    Final institutional report",
  "manual/                    Final technical and pedagogical manual",
  "outputs/                   Analytical outputs, logs, figures and workbooks",
  "outputs/final_results/     Final stress test results and vulnerability rankings",
  "outputs/benchmark_validation/  Validation against public Federal Reserve results",
  "outputs/model_risk/        Robustness, sensitivity and model risk assessment",
  "outputs/publication_package/ Publication checklist and release material",
  "```",
  "",
  "## Analytical pipeline",
  "",
  "The project follows a staged R pipeline:",
  "",
  "| Script | Role |",
  "|---|---|",
  "| Script 01 | Data availability audit |",
  "| Script 02 / 02b / 02c | Regulatory documentation and public source ingestion |",
  "| Script 03 | Federal Reserve DFAST data cleaning |",
  "| Script 04 | Macro scenario structuring |",
  "| Script 05 | DFAST benchmark dataset construction |",
  "| Script 06 | Capital and losses transmission layer |",
  "| Script 07 | Exploratory analysis |",
  "| Script 08 | Modelling sample and treatment rules |",
  "| Script 09 | Credit loss model |",
  "| Script 10 | PPNR model |",
  "| Script 11 | Capital depletion model |",
  "| Script 12 / 12b | Integrated stress test projection engine and join-key correction |",
  "| Script 13 | Final results and bank vulnerability ranking |",
  "| Script 14 | Benchmark validation against Federal Reserve public results |",
  "| Script 15 | Robustness, sensitivity and model risk assessment |",
  "| Script 16 | Final institutional report |",
  "| Script 17 | Final technical and pedagogical manual |",
  "| Script 18 | Publication checklist and repository packaging |",
  "| Script 19 | README, license and citation files |",
  "",
  readme_results_block,
  "",
  "## Main final documents",
  "",
  "- `report/final_institutional_report_dfast_replication.docx`",
  "- `manual/final_technical_pedagogical_manual_dfast_replication.docx`",
  "- `outputs/final_results/script13_final_results_outputs.xlsx`",
  "- `outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx`",
  "- `outputs/model_risk/script15_model_risk_assessment_outputs.xlsx`",
  "- `outputs/publication_package/script18_publication_checklist.xlsx`",
  "",
  "## How to reproduce",
  "",
  "Open RStudio and run the scripts sequentially from the `R/` folder.",
  "",
  "Example:",
  "",
  "```r",
  "source(\"D:/GitHub/us-bank-stress-test-dfast-replication/R/01_data_availability_audit.R\")",
  "```",
  "",
  "The full project was designed for local execution on Windows. Paths inside the scripts use the project root:",
  "",
  "```text",
  "D:/GitHub/us-bank-stress-test-dfast-replication",
  "```",
  "",
  "If the repository is cloned to another location, update the `project_root` variable at the top of each script or create a shared configuration file.",
  "",
  "## Teaching use",
  "",
  "The manual in `manual/` explains the project for students. It includes stress testing concepts, data structure, variable definitions, modelling blocks, validation metrics, model risk, exercises and glossary.",
  "",
  "Suggested teaching sequence:",
  "",
  "1. Introduce DFAST, CET1, PPNR, RWAs and stress testing.",
  "2. Reproduce data audit, cleaning and sample construction.",
  "3. Estimate the credit loss, PPNR and capital depletion models.",
  "4. Build and validate the integrated stress test engine.",
  "5. Discuss model risk, limitations and responsible interpretation.",
  "",
  "## Citation",
  "",
  "If you use this repository, cite it as:",
  "",
  paste0(author_name, ". (", release_year, "). *", project_title, "* (", release_version, "). GitHub. ", repository_url),
  "",
  "A machine-readable citation file is provided in `CITATION.cff`.",
  "",
  "## License",
  "",
  "This project is released under the MIT License. See `LICENSE` for details."
)

write_lines_utf8(readme_lines, "README.md")

cat("README.md created.\n\n")


# ------------------------------------------------------------
# 6. LICENSE
# ------------------------------------------------------------

cat("Creating LICENSE...\n")

license_lines <- c(
  "MIT License",
  "",
  paste0("Copyright (c) ", release_year, " ", author_name),
  "",
  "Permission is hereby granted, free of charge, to any person obtaining a copy",
  "of this software and associated documentation files (the \"Software\"), to deal",
  "in the Software without restriction, including without limitation the rights",
  "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
  "copies of the Software, and to permit persons to whom the Software is",
  "furnished to do so, subject to the following conditions:",
  "",
  "The above copyright notice and this permission notice shall be included in all",
  "copies or substantial portions of the Software.",
  "",
  "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR",
  "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
  "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
  "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
  "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
  "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE",
  "SOFTWARE.",
  "",
  "Additional project-specific disclaimer:",
  "",
  "This repository is an independent educational and analytical project based",
  "entirely on public data. It is not affiliated with, endorsed by or approved",
  "by the Federal Reserve, any banking organization or any supervisory authority.",
  "The results are not official supervisory assessments, investment advice,",
  "credit opinions or bank ratings."
)

write_lines_utf8(license_lines, "LICENSE")

cat("LICENSE created.\n\n")


# ------------------------------------------------------------
# 7. CITATION.cff
# ------------------------------------------------------------

cat("Creating CITATION.cff...\n")

citation_lines <- c(
  "cff-version: 1.2.0",
  paste0("title: \"", project_title, "\""),
  "message: \"If you use this project, please cite it using the metadata from this file.\"",
  "type: software",
  "authors:",
  "  - family-names: \"Picol\"",
  "    given-names: \"Gelo\"",
  paste0("repository-code: \"", repository_url, "\""),
  paste0("url: \"", repository_url, "\""),
  "license: MIT",
  paste0("version: \"", release_version, "\""),
  paste0("date-released: \"", Sys.Date(), "\""),
  "abstract: \"A public-data, reproducible DFAST-style stress testing project for large U.S. banking organizations, including data processing, reduced-form modelling, benchmark validation, model risk assessment, final institutional reporting and a technical pedagogical manual.\"",
  "keywords:",
  "  - stress testing",
  "  - DFAST",
  "  - banking",
  "  - capital adequacy",
  "  - CET1",
  "  - PPNR",
  "  - Federal Reserve",
  "  - reproducible research",
  "  - financial econometrics"
)

write_lines_utf8(citation_lines, "CITATION.cff")

cat("CITATION.cff created.\n\n")


# ------------------------------------------------------------
# 8. docs/public_disclaimer.md
# ------------------------------------------------------------

cat("Creating docs/public_disclaimer.md...\n")

public_disclaimer_lines <- c(
  "# Public Disclaimer",
  "",
  "This project is an independent educational and analytical exercise based entirely on public data.",
  "",
  "It is not affiliated with, endorsed by or approved by the Federal Reserve, any banking organization or any supervisory authority.",
  "",
  "The project does not use:",
  "",
  "- confidential supervisory information;",
  "- confidential Federal Reserve models;",
  "- confidential bank submissions;",
  "- internal bank capital planning models;",
  "- non-public bank data;",
  "- loan-level supervisory data;",
  "- confidential management action assumptions.",
  "",
  "The results are intended only for:",
  "",
  "- teaching;",
  "- public benchmarking;",
  "- reproducible research;",
  "- methodological illustration;",
  "- stress testing education.",
  "",
  "The outputs must not be interpreted as:",
  "",
  "- official regulatory assessments;",
  "- bank ratings;",
  "- investment recommendations;",
  "- credit opinions;",
  "- capital adequacy opinions;",
  "- statements on the safety and soundness of any institution.",
  "",
  "All vulnerability rankings are model-based, relative and analytical. They are not official supervisory findings."
)

write_lines_utf8(public_disclaimer_lines, "docs/public_disclaimer.md")

cat("docs/public_disclaimer.md created.\n\n")


# ------------------------------------------------------------
# 9. docs/reproducibility_notes.md
# ------------------------------------------------------------

cat("Creating docs/reproducibility_notes.md...\n")

reproducibility_lines <- c(
  "# Reproducibility Notes",
  "",
  "This project was designed as a reproducible R pipeline.",
  "",
  "## Execution environment",
  "",
  "The project was developed for local execution on Windows using RStudio.",
  "",
  "The default project root used in the scripts is:",
  "",
  "```text",
  "D:/GitHub/us-bank-stress-test-dfast-replication",
  "```",
  "",
  "If the project is cloned to a different directory, update the `project_root` object at the beginning of each script.",
  "",
  "## Execution order",
  "",
  "Run the scripts sequentially from the `R/` folder.",
  "",
  "The final analytical chain is:",
  "",
  "```text",
  "01 -> 02 -> 02b -> 02c -> 03 -> 04 -> 05 -> 06 -> 07 -> 08 ->",
  "09 -> 10 -> 11 -> 12 -> 12b -> 13 -> 14 -> 15 -> 16 -> 17 -> 18 -> 19",
  "```",
  "",
  "## Key reproducibility controls",
  "",
  "- Every major script creates an execution log.",
  "- Final panels are audited for duplicated bank-year-scenario keys.",
  "- Final reports and manuals are generated from processed outputs.",
  "- Excel workbooks are created for auditability.",
  "- The final publication checklist is produced by Script 18.",
  "",
  "## Final reproducibility status",
  "",
  "The final analytical project contains:",
  "",
  "- final stress test results;",
  "- benchmark validation outputs;",
  "- robustness and model risk outputs;",
  "- final institutional report;",
  "- final technical and pedagogical manual;",
  "- publication checklist;",
  "- README, LICENSE and CITATION files.",
  "",
  "## Important limitation",
  "",
  "The project is reproducible only with respect to public data and public outputs. It cannot reproduce confidential supervisory models or non-public bank data."
)

write_lines_utf8(reproducibility_lines, "docs/reproducibility_notes.md")

cat("docs/reproducibility_notes.md created.\n\n")


# ------------------------------------------------------------
# 10. docs/project_scope.md
# ------------------------------------------------------------

cat("Creating docs/project_scope.md...\n")

project_scope_lines <- c(
  "# Project Scope",
  "",
  "## Objective",
  "",
  "The objective of this project is to build a public-data, reduced-form DFAST-style stress testing framework for large U.S. banking organizations.",
  "",
  "The project links public stress test outcomes, macro-financial scenarios, credit losses, PPNR, capital depletion and CET1 minimum ratios into a reproducible analytical pipeline.",
  "",
  "## In scope",
  "",
  "- Public Federal Reserve DFAST results;",
  "- public macroeconomic scenario data;",
  "- public-data benchmark construction;",
  "- reduced-form modelling of credit losses, PPNR and CET1 depletion;",
  "- integrated stress test projection;",
  "- benchmark validation against public Federal Reserve outcomes;",
  "- model risk assessment;",
  "- final reporting;",
  "- technical and pedagogical documentation.",
  "",
  "## Out of scope",
  "",
  "- confidential Federal Reserve supervisory models;",
  "- confidential supervisory information;",
  "- confidential bank submissions;",
  "- internal bank stress testing models;",
  "- official capital planning decisions;",
  "- investment recommendations;",
  "- bank ratings;",
  "- statements on institutional safety and soundness.",
  "",
  "## Valid interpretation",
  "",
  "The project should be interpreted as a public benchmarking, teaching and reproducible research framework.",
  "",
  "It is suitable for explaining the mechanics of stress testing and for demonstrating how public supervisory data can be transformed into an auditable analytical pipeline.",
  "",
  "It is not suitable for official regulatory conclusions or investment decisions."
)

write_lines_utf8(project_scope_lines, "docs/project_scope.md")

cat("docs/project_scope.md created.\n\n")


# ------------------------------------------------------------
# 11. Metadata checks
# ------------------------------------------------------------

cat("Checking created metadata files...\n")

metadata_files <- dplyr::bind_rows(
  file_status("README.md", TRUE, "Repository README"),
  file_status("LICENSE", TRUE, "Repository license"),
  file_status("CITATION.cff", TRUE, "Citation metadata"),
  file_status("docs/public_disclaimer.md", TRUE, "Public disclaimer"),
  file_status("docs/reproducibility_notes.md", TRUE, "Reproducibility notes"),
  file_status("docs/project_scope.md", TRUE, "Project scope")
) |>
  safe_df()

metadata_summary <- tibble::tibble(
  metadata_files_checked = nrow(metadata_files),
  metadata_files_ok = sum(metadata_files$status == "OK", na.rm = TRUE),
  metadata_files_missing_or_empty = sum(metadata_files$status == "MISSING_OR_EMPTY", na.rm = TRUE),
  metadata_status = ifelse(
    metadata_files_missing_or_empty == 0,
    "METADATA_READY",
    "METADATA_INCOMPLETE"
  )
) |>
  safe_df()

cat("Metadata checks completed.\n\n")


# ------------------------------------------------------------
# 12. Save Script 19 outputs
# ------------------------------------------------------------

cat("Saving Script 19 outputs...\n")

out_dir <- "outputs/publication_package"

paths_out <- list(
  metadata_table = file.path(out_dir, "script19_repository_metadata.csv"),
  metadata_files = file.path(out_dir, "script19_metadata_files_check.csv"),
  metadata_summary = file.path(out_dir, "script19_metadata_summary.csv"),
  excel = file.path(out_dir, "script19_publication_metadata_check.xlsx"),
  execution_summary = file.path(out_dir, "script19_execution_summary.csv"),
  execution_log = file.path(out_dir, "script19_execution_log.txt")
)

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  readme_created = file.exists("README.md"),
  license_created = file.exists("LICENSE"),
  citation_created = file.exists("CITATION.cff"),
  public_disclaimer_created = file.exists("docs/public_disclaimer.md"),
  reproducibility_notes_created = file.exists("docs/reproducibility_notes.md"),
  project_scope_created = file.exists("docs/project_scope.md"),
  metadata_status = metadata_summary$metadata_status
) |>
  safe_df()

readr::write_csv(metadata_table, paths_out$metadata_table)
readr::write_csv(metadata_files, paths_out$metadata_files)
readr::write_csv(metadata_summary, paths_out$metadata_summary)
readr::write_csv(execution_summary, paths_out$execution_summary)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  execution_summary = execution_summary,
  repository_metadata = metadata_table,
  metadata_files = metadata_files,
  metadata_summary = metadata_summary
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, paths_out$excel, overwrite = TRUE)

cat("Script 19 outputs saved.\n\n")


# ------------------------------------------------------------
# 13. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 19 - Create README, LICENSE and CITATION Files completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Repository metadata:",
  capture.output(print(metadata_table)),
  "",
  "Metadata files check:",
  capture.output(print(metadata_files)),
  "",
  "Metadata summary:",
  capture.output(print(metadata_summary)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", "README.md"),
  paste(" -", "LICENSE"),
  paste(" -", "CITATION.cff"),
  paste(" -", "docs/public_disclaimer.md"),
  paste(" -", "docs/reproducibility_notes.md"),
  paste(" -", "docs/project_scope.md"),
  paste(" -", paths_out$metadata_table),
  paste(" -", paths_out$metadata_files),
  paste(" -", paths_out$metadata_summary),
  paste(" -", paths_out$excel),
  paste(" -", paths_out$execution_log)
)

write_lines_utf8(log_lines, paths_out$execution_log)


# ------------------------------------------------------------
# 14. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 19 - Create README, LICENSE and CITATION Files completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Metadata files checked:\n", metadata_summary$metadata_files_checked, "\n")
cat("Metadata files OK:\n", metadata_summary$metadata_files_ok, "\n")
cat("Metadata files missing or empty:\n", metadata_summary$metadata_files_missing_or_empty, "\n")
cat("Metadata status:\n", metadata_summary$metadata_status, "\n\n")

cat("Metadata files check:\n")
print(metadata_files)

cat("\nMain outputs:\n")
cat(" - README.md\n")
cat(" - LICENSE\n")
cat(" - CITATION.cff\n")
cat(" - docs/public_disclaimer.md\n")
cat(" - docs/reproducibility_notes.md\n")
cat(" - docs/project_scope.md\n")
cat(" -", paths_out$excel, "\n")
cat(" -", paths_out$execution_log, "\n")
cat("============================================================\n")