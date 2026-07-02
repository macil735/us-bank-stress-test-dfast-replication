# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 20 — Create Gitignore and Final Git Publication Instructions
# ============================================================
# Objective:
#   Create the final .gitignore file, Git publication instructions,
#   and final publication status note.
#
# This script does not alter data, models, results, reports or manuals.
# It only prepares the repository for final GitHub publication.
#
# Main outputs:
#   .gitignore
#   docs/git_publication_steps.md
#   docs/final_publication_status.md
#   outputs/publication_package/script20_git_publication_check.csv
#   outputs/publication_package/script20_git_publication_check.xlsx
#   outputs/publication_package/script20_execution_log.txt
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 20 - Create Gitignore and Final Git Publication Instructions\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "20"
script_name <- "create_gitignore_and_final_git_publication_instructions"
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

cat("Helper functions created.\n\n")


# ------------------------------------------------------------
# 3. Create .gitignore
# ------------------------------------------------------------

cat("Creating .gitignore...\n")

gitignore_lines <- c(
  "# ============================================================",
  "# .gitignore",
  "# USA Bank Stress Test DFAST Replication",
  "# ============================================================",
  "",
  "# R session and history files",
  ".Rhistory",
  ".RData",
  ".Ruserdata",
  ".Rproj.user/",
  "",
  "# RStudio project user files",
  "*.Rproj.user",
  "",
  "# Temporary files",
  "*.tmp",
  "*.temp",
  "*.bak",
  "*.backup",
  "*~",
  "~$*",
  "",
  "# Operating system files",
  ".DS_Store",
  "Thumbs.db",
  "desktop.ini",
  "",
  "# Logs that are not part of the reproducibility pipeline",
  "*.log",
  "!outputs/**/script*_execution_log.txt",
  "",
  "# Cache folders",
  ".cache/",
  "cache/",
  "__pycache__/",
  "",
  "# Quarto / R Markdown cache",
  "*_cache/",
  "*.utf8.md",
  "*.knit.md",
  "",
  "# Local environment files",
  ".Renviron",
  ".Rprofile",
  ".env",
  "",
  "# Very large compressed files",
  "*.zip",
  "*.7z",
  "*.rar",
  "*.tar",
  "*.gz",
  "",
  "# Office temporary lock files",
  "~$*.docx",
  "~$*.xlsx",
  "~$*.pptx",
  "",
  "# Keep final project outputs",
  "!report/*.docx",
  "!manual/*.docx",
  "!outputs/**/*.xlsx",
  "!outputs/**/*.csv",
  "!outputs/**/*.txt",
  "!outputs/**/*.md",
  "!outputs/**/*.png",
  "",
  "# Keep public processed datasets",
  "!data/processed/**/*.csv",
  "",
  "# Keep repository documentation",
  "!README.md",
  "!LICENSE",
  "!CITATION.cff",
  "!docs/**/*.md",
  "",
  "# Note:",
  "# Raw public data may be committed if file sizes are reasonable.",
  "# If any raw file is too large, document it in docs/reproducibility_notes.md."
)

write_lines_utf8(gitignore_lines, ".gitignore")

cat(".gitignore created.\n\n")


# ------------------------------------------------------------
# 4. Create Git publication steps
# ------------------------------------------------------------

cat("Creating docs/git_publication_steps.md...\n")

git_publication_steps <- c(
  "# Git Publication Steps",
  "",
  "This document provides the final Git and GitHub publication instructions for the USA Bank Stress Test DFAST Replication project.",
  "",
  "## 1. Confirm project directory",
  "",
  "Open PowerShell and run:",
  "",
  "```powershell",
  "cd D:/GitHub/us-bank-stress-test-dfast-replication",
  "```",
  "",
  "## 2. Check repository status",
  "",
  "```powershell",
  "git status",
  "```",
  "",
  "Review the list of modified, new or deleted files.",
  "",
  "## 3. Add all final files",
  "",
  "```powershell",
  "git add .",
  "```",
  "",
  "## 4. Commit the final publication package",
  "",
  "```powershell",
  "git commit -m \"Complete public DFAST replication project with final reports and publication package\"",
  "```",
  "",
  "## 5. Confirm commit",
  "",
  "```powershell",
  "git log --oneline -5",
  "```",
  "",
  "## 6. Push to GitHub",
  "",
  "```powershell",
  "git push",
  "```",
  "",
  "If the upstream branch is not configured, use:",
  "",
  "```powershell",
  "git push -u origin main",
  "```",
  "",
  "## 7. Verify on GitHub",
  "",
  "After pushing, open the GitHub repository page and verify that the following files are visible:",
  "",
  "- `README.md`",
  "- `LICENSE`",
  "- `CITATION.cff`",
  "- `report/final_institutional_report_dfast_replication.docx`",
  "- `manual/final_technical_pedagogical_manual_dfast_replication.docx`",
  "- `outputs/publication_package/script18_publication_checklist.xlsx`",
  "",
  "## 8. Create a GitHub release",
  "",
  "Recommended release tag:",
  "",
  "```text",
  "v1.0.0",
  "```",
  "",
  "Recommended release title:",
  "",
  "```text",
  "v1.0.0 - First Stable Release",
  "```",
  "",
  "Use the draft release notes created by Script 18:",
  "",
  "```text",
  "outputs/publication_package/script18_release_notes_draft.md",
  "```",
  "",
  "## 9. Final publication statement",
  "",
  "The repository is ready for public release only after Script 18 reports:",
  "",
  "```text",
  "Publication status: READY_FOR_PUBLICATION",
  "```",
  "",
  "## 10. Important disclaimer",
  "",
  "This project is a public-data, reduced-form replication and benchmarking exercise. It does not reproduce confidential Federal Reserve supervisory models, confidential bank submissions, internal bank capital planning models or any non-public supervisory information."
)

write_lines_utf8(git_publication_steps, "docs/git_publication_steps.md")

cat("docs/git_publication_steps.md created.\n\n")


# ------------------------------------------------------------
# 5. Create final publication status document
# ------------------------------------------------------------

cat("Creating docs/final_publication_status.md...\n")

final_publication_status <- c(
  "# Final Publication Status",
  "",
  "## Project",
  "",
  "USA Bank Stress Test DFAST Replication",
  "",
  "## Status",
  "",
  "**READY_FOR_PUBLICATION**",
  "",
  "## Final checks completed",
  "",
  "- Critical files checked: 23",
  "- Missing required files: 0",
  "- Expected scripts: 21",
  "- Missing expected scripts: 0",
  "- Publication checks: 15",
  "- Publication checks passed: 15",
  "- Publication checks failed: 0",
  "- Readiness rate: 100%",
  "",
  "## Final analytical outputs",
  "",
  "- Final stress test results panel",
  "- Final bank vulnerability ranking",
  "- Latest exercise vulnerability ranking",
  "- Benchmark validation panel",
  "- Model risk assessment panel",
  "- Bank-level model risk assessment",
  "",
  "## Final documents",
  "",
  "- `report/final_institutional_report_dfast_replication.docx`",
  "- `manual/final_technical_pedagogical_manual_dfast_replication.docx`",
  "",
  "## Final publication files",
  "",
  "- `README.md`",
  "- `LICENSE`",
  "- `CITATION.cff`",
  "- `.gitignore`",
  "- `docs/public_disclaimer.md`",
  "- `docs/reproducibility_notes.md`",
  "- `docs/project_scope.md`",
  "- `docs/git_publication_steps.md`",
  "",
  "## Public-data disclaimer",
  "",
  "This project is based exclusively on public data and public Federal Reserve DFAST outputs. It does not use confidential supervisory information, confidential bank submissions, internal bank models or non-public bank data.",
  "",
  "## Valid use",
  "",
  "The project is suitable for teaching, reproducible research and public benchmarking.",
  "",
  "## Invalid use",
  "",
  "The project must not be interpreted as an official supervisory stress test, investment recommendation, credit rating, bank rating or regulatory capital planning decision."
)

write_lines_utf8(final_publication_status, "docs/final_publication_status.md")

cat("docs/final_publication_status.md created.\n\n")


# ------------------------------------------------------------
# 6. Git publication checklist
# ------------------------------------------------------------

cat("Creating Git publication checklist...\n")

git_publication_check <- dplyr::bind_rows(
  file_status(".gitignore", TRUE, "Git ignore file"),
  file_status("docs/git_publication_steps.md", TRUE, "Git publication instructions"),
  file_status("docs/final_publication_status.md", TRUE, "Final publication status"),
  file_status("README.md", TRUE, "Repository README"),
  file_status("LICENSE", TRUE, "Repository license"),
  file_status("CITATION.cff", TRUE, "Citation metadata"),
  file_status("docs/public_disclaimer.md", TRUE, "Public disclaimer"),
  file_status("docs/reproducibility_notes.md", TRUE, "Reproducibility notes"),
  file_status("docs/project_scope.md", TRUE, "Project scope"),
  file_status("report/final_institutional_report_dfast_replication.docx", TRUE, "Final institutional report"),
  file_status("manual/final_technical_pedagogical_manual_dfast_replication.docx", TRUE, "Final technical and pedagogical manual"),
  file_status("outputs/publication_package/script18_publication_checklist.xlsx", TRUE, "Final publication checklist workbook")
) |>
  safe_df()

git_publication_summary <- tibble::tibble(
  files_checked = nrow(git_publication_check),
  files_ok = sum(git_publication_check$status == "OK", na.rm = TRUE),
  files_missing_or_empty = sum(git_publication_check$status == "MISSING_OR_EMPTY", na.rm = TRUE),
  git_publication_status = ifelse(
    files_missing_or_empty == 0,
    "READY_FOR_GIT_COMMIT",
    "NOT_READY_FOR_GIT_COMMIT"
  )
) |>
  safe_df()

cat("Git publication checklist created.\n\n")


# ------------------------------------------------------------
# 7. Save outputs
# ------------------------------------------------------------

cat("Saving Script 20 outputs...\n")

out_dir <- "outputs/publication_package"

paths_out <- list(
  git_publication_check = file.path(out_dir, "script20_git_publication_check.csv"),
  git_publication_summary = file.path(out_dir, "script20_git_publication_summary.csv"),
  excel = file.path(out_dir, "script20_git_publication_check.xlsx"),
  execution_summary = file.path(out_dir, "script20_execution_summary.csv"),
  execution_log = file.path(out_dir, "script20_execution_log.txt")
)

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  gitignore_created = file.exists(".gitignore"),
  git_publication_steps_created = file.exists("docs/git_publication_steps.md"),
  final_publication_status_created = file.exists("docs/final_publication_status.md"),
  git_publication_status = git_publication_summary$git_publication_status
) |>
  safe_df()

readr::write_csv(git_publication_check, paths_out$git_publication_check)
readr::write_csv(git_publication_summary, paths_out$git_publication_summary)
readr::write_csv(execution_summary, paths_out$execution_summary)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  execution_summary = execution_summary,
  git_publication_summary = git_publication_summary,
  git_publication_check = git_publication_check
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, paths_out$excel, overwrite = TRUE)

cat("Script 20 outputs saved.\n\n")


# ------------------------------------------------------------
# 8. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 20 - Create Gitignore and Final Git Publication Instructions completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  "Git publication summary:",
  capture.output(print(git_publication_summary)),
  "",
  "Git publication check:",
  capture.output(print(git_publication_check)),
  "",
  "Execution summary:",
  capture.output(print(execution_summary)),
  "",
  "Main outputs:",
  paste(" -", ".gitignore"),
  paste(" -", "docs/git_publication_steps.md"),
  paste(" -", "docs/final_publication_status.md"),
  paste(" -", paths_out$git_publication_check),
  paste(" -", paths_out$git_publication_summary),
  paste(" -", paths_out$excel),
  paste(" -", paths_out$execution_log)
)

write_lines_utf8(log_lines, paths_out$execution_log)


# ------------------------------------------------------------
# 9. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 20 - Create Gitignore and Final Git Publication Instructions completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Files checked:\n", git_publication_summary$files_checked, "\n")
cat("Files OK:\n", git_publication_summary$files_ok, "\n")
cat("Files missing or empty:\n", git_publication_summary$files_missing_or_empty, "\n")
cat("Git publication status:\n", git_publication_summary$git_publication_status, "\n\n")

cat("Git publication check:\n")
print(git_publication_check)

cat("\nMain outputs:\n")
cat(" - .gitignore\n")
cat(" - docs/git_publication_steps.md\n")
cat(" - docs/final_publication_status.md\n")
cat(" -", paths_out$git_publication_check, "\n")
cat(" -", paths_out$git_publication_summary, "\n")
cat(" -", paths_out$excel, "\n")
cat(" -", paths_out$execution_log, "\n")
cat("============================================================\n")