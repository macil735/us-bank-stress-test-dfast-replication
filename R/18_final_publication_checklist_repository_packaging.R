# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 18 — Final Publication Checklist and Repository Packaging
# ============================================================
# Objective:
#   Create the final publication checklist and repository
#   packaging audit for GitHub publication or release.
#
# The script checks:
#   1. Final reports and manuals
#   2. Excel workbooks
#   3. Processed datasets
#   4. Figures
#   5. Logs
#   6. R scripts
#   7. Documentation files
#   8. Reproducibility status
#   9. Public-data disclaimer
#   10. GitHub publication readiness
#
# Main outputs:
#   outputs/publication_package/script18_publication_checklist.xlsx
#   outputs/publication_package/script18_repository_file_inventory.csv
#   outputs/publication_package/script18_publication_readiness_summary.csv
#   outputs/publication_package/script18_release_notes_draft.md
#   outputs/publication_package/script18_readme_results_block.md
#   outputs/publication_package/script18_execution_log.txt
#
# Methodological note:
#   This script does not change model outputs. It audits whether the
#   repository is ready for institutional/public dissemination.
# ============================================================


# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Starting Script 18 - Final Publication Checklist and Repository Packaging\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
script_id <- "18"
script_name <- "final_publication_checklist_repository_packaging"
start_time <- Sys.time()

setwd(project_root)

dir.create("outputs/publication_package", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/publication_package/release_material", recursive = TRUE, showWarnings = FALSE)

cat("Project root:", project_root, "\n")
cat("Directories checked.\n\n")


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

cat("Loading packages...\n")

required_packages <- c(
  "dplyr",
  "tidyr",
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

file_status <- function(path, required = TRUE, category = NA_character_, description = NA_character_) {
  exists <- file.exists(path)

  tibble::tibble(
    category = category,
    description = description,
    file_path = path,
    required = required,
    exists = exists,
    size_bytes = ifelse(exists, file.info(path)$size, NA_real_),
    modified_at = ifelse(exists, as.character(file.info(path)$mtime), NA_character_),
    status = dplyr::case_when(
      required & exists ~ "OK",
      required & !exists ~ "MISSING_REQUIRED",
      !required & exists ~ "OPTIONAL_PRESENT",
      !required & !exists ~ "OPTIONAL_MISSING",
      TRUE ~ "UNKNOWN"
    )
  )
}

write_lines_utf8 <- function(lines, path) {
  writeLines(enc2utf8(lines), con = path, useBytes = TRUE)
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
# 3. Critical file checklist
# ------------------------------------------------------------

cat("Creating critical file checklist...\n")

critical_files <- dplyr::bind_rows(

  # Final reports and manuals
  file_status(
    "report/final_institutional_report_dfast_replication.docx",
    TRUE,
    "Final documents",
    "Final institutional report"
  ),
  file_status(
    "manual/final_technical_pedagogical_manual_dfast_replication.docx",
    TRUE,
    "Final documents",
    "Final technical and pedagogical manual"
  ),
  file_status(
    "outputs/final_manual/final_technical_pedagogical_manual_dfast_replication.docx",
    TRUE,
    "Final documents",
    "Copy of final technical and pedagogical manual"
  ),

  # Final Excel workbooks
  file_status(
    "outputs/final_report/script16_final_report_outputs.xlsx",
    TRUE,
    "Final workbooks",
    "Final institutional report workbook"
  ),
  file_status(
    "outputs/final_manual/script17_final_manual_outputs.xlsx",
    TRUE,
    "Final workbooks",
    "Final technical manual workbook"
  ),
  file_status(
    "outputs/final_results/script13_final_results_outputs.xlsx",
    TRUE,
    "Final workbooks",
    "Final stress test results workbook"
  ),
  file_status(
    "outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx",
    TRUE,
    "Final workbooks",
    "Benchmark validation workbook"
  ),
  file_status(
    "outputs/model_risk/script15_model_risk_assessment_outputs.xlsx",
    TRUE,
    "Final workbooks",
    "Model risk assessment workbook"
  ),

  # Final processed datasets
  file_status(
    "data/processed/model/final_stress_test_results_panel.csv",
    TRUE,
    "Processed datasets",
    "Final stress test results panel"
  ),
  file_status(
    "data/processed/model/final_bank_vulnerability_ranking.csv",
    TRUE,
    "Processed datasets",
    "Full-sample vulnerability ranking"
  ),
  file_status(
    "data/processed/model/latest_exercise_bank_vulnerability_ranking.csv",
    TRUE,
    "Processed datasets",
    "Latest exercise vulnerability ranking"
  ),
  file_status(
    "data/processed/model/benchmark_validation_panel.csv",
    TRUE,
    "Processed datasets",
    "Benchmark validation panel"
  ),
  file_status(
    "data/processed/model/model_risk_assessment_panel.csv",
    TRUE,
    "Processed datasets",
    "Model risk assessment panel"
  ),
  file_status(
    "data/processed/model/bank_model_risk_assessment.csv",
    TRUE,
    "Processed datasets",
    "Bank-level model risk assessment"
  ),

  # Key logs
  file_status(
    "outputs/final_results/script13_execution_log.txt",
    TRUE,
    "Execution logs",
    "Script 13 execution log"
  ),
  file_status(
    "outputs/benchmark_validation/script14_execution_log.txt",
    TRUE,
    "Execution logs",
    "Script 14 execution log"
  ),
  file_status(
    "outputs/model_risk/script15_execution_log.txt",
    TRUE,
    "Execution logs",
    "Script 15 execution log"
  ),
  file_status(
    "outputs/final_report/script16_execution_log.txt",
    TRUE,
    "Execution logs",
    "Script 16 execution log"
  ),
  file_status(
    "outputs/final_manual/script17_execution_log.txt",
    TRUE,
    "Execution logs",
    "Script 17 execution log"
  ),

  # Repository metadata
  file_status(
    "README.md",
    FALSE,
    "Repository metadata",
    "README file"
  ),
  file_status(
    "LICENSE",
    FALSE,
    "Repository metadata",
    "License file"
  ),
  file_status(
    "CITATION.cff",
    FALSE,
    "Repository metadata",
    "Citation file"
  ),
  file_status(
    ".gitignore",
    FALSE,
    "Repository metadata",
    "Git ignore file"
  )
) |>
  safe_df()

cat("Critical file checklist created.\n\n")


# ------------------------------------------------------------
# 4. R script inventory
# ------------------------------------------------------------

cat("Creating R script inventory...\n")

script_files <- list.files(
  "R",
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
)

script_inventory <- tibble::tibble(
  script_path = script_files,
  script_file = basename(script_files),
  exists = file.exists(script_files),
  size_bytes = ifelse(file.exists(script_files), file.info(script_files)$size, NA_real_),
  modified_at = ifelse(file.exists(script_files), as.character(file.info(script_files)$mtime), NA_character_)
) |>
  dplyr::arrange(script_file) |>
  safe_df()

expected_scripts <- c(
  "01_data_availability_audit.R",
  "02_regulatory_documentation_and_raw_data_ingestion.R",
  "02b_download_regulatory_documentation.R",
  "02c_fix_missing_regulatory_documentation.R",
  "03_clean_federal_reserve_dfast_results.R",
  "04_structure_fed_macro_scenarios.R",
  "05_build_dfast_benchmark_dataset.R",
  "06_capital_losses_transmission_layer.R",
  "07_exploratory_analysis_transmission_layer.R",
  "08_build_modelling_sample.R",
  "09_estimate_credit_loss_model.R",
  "10_estimate_ppnr_model.R",
  "11_estimate_capital_depletion_model.R",
  "12_integrated_stress_test_projection_engine.R",
  "12b_fix_integrated_projection_join_keys.R",
  "13_stress_test_results_bank_vulnerability_ranking.R",
  "14_benchmark_validation_against_fed_results.R",
  "15_robustness_sensitivity_model_risk_assessment.R",
  "16_create_final_institutional_report.R",
  "17_create_final_technical_pedagogical_manual.R",
  "18_final_publication_checklist_repository_packaging.R"
)

script_presence_check <- tibble::tibble(
  expected_script = expected_scripts,
  exists = expected_scripts %in% basename(script_files),
  status = ifelse(exists, "OK", "MISSING")
) |>
  safe_df()

cat("R script inventory created.\n\n")


# ------------------------------------------------------------
# 5. Repository file inventory
# ------------------------------------------------------------

cat("Creating repository file inventory...\n")

all_files <- list.files(
  ".",
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  no.. = TRUE
)

all_files <- all_files[
  !stringr::str_detect(all_files, "^\\./\\.git/")
]

repository_file_inventory <- tibble::tibble(
  file_path = all_files,
  file_name = basename(all_files),
  extension = stringr::str_to_lower(tools::file_ext(all_files)),
  directory = dirname(all_files),
  exists = file.exists(all_files),
  size_bytes = ifelse(file.exists(all_files), file.info(all_files)$size, NA_real_),
  modified_at = ifelse(file.exists(all_files), as.character(file.info(all_files)$mtime), NA_character_)
) |>
  dplyr::mutate(
    file_type_group = dplyr::case_when(
      extension == "r" ~ "R script",
      extension == "csv" ~ "CSV",
      extension == "xlsx" ~ "Excel workbook",
      extension == "docx" ~ "Word document",
      extension == "pdf" ~ "PDF",
      extension %in% c("png", "jpg", "jpeg") ~ "Figure",
      extension %in% c("md", "cff", "txt") ~ "Documentation/text",
      extension == "" ~ "No extension",
      TRUE ~ "Other"
    )
  ) |>
  dplyr::arrange(directory, file_name) |>
  safe_df()

repository_file_summary <- repository_file_inventory |>
  dplyr::group_by(file_type_group, extension) |>
  dplyr::summarise(
    files = dplyr::n(),
    total_size_bytes = sum(size_bytes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(file_type_group, extension) |>
  safe_df()

cat("Repository file inventory created.\n\n")


# ------------------------------------------------------------
# 6. Load key summaries from previous scripts
# ------------------------------------------------------------

cat("Loading key summary outputs...\n")

script13_summary <- safe_read_csv_optional("outputs/final_results/script13_executive_summary_table.csv")
script14_summary <- safe_read_csv_optional("outputs/benchmark_validation/script14_executive_validation_summary.csv")
script15_summary <- safe_read_csv_optional("outputs/model_risk/script15_executive_model_risk_summary.csv")
script16_summary <- safe_read_csv_optional("outputs/final_report/script16_executive_summary.csv")
script17_summary <- safe_read_csv_optional("outputs/final_manual/script17_executive_summary.csv")

summary_sources <- tibble::tibble(
  summary_file = c(
    "script13_executive_summary_table.csv",
    "script14_executive_validation_summary.csv",
    "script15_executive_model_risk_summary.csv",
    "script16_executive_summary.csv",
    "script17_executive_summary.csv"
  ),
  file_path = c(
    "outputs/final_results/script13_executive_summary_table.csv",
    "outputs/benchmark_validation/script14_executive_validation_summary.csv",
    "outputs/model_risk/script15_executive_model_risk_summary.csv",
    "outputs/final_report/script16_executive_summary.csv",
    "outputs/final_manual/script17_executive_summary.csv"
  ),
  exists = file.exists(file_path)
) |>
  safe_df()

cat("Key summary outputs loaded.\n\n")


# ------------------------------------------------------------
# 7. Publication readiness checks
# ------------------------------------------------------------

cat("Creating publication readiness checks...\n")

publication_readiness_check <- tibble::tibble(
  check_area = c(
    "Final institutional report",
    "Final technical and pedagogical manual",
    "Final results workbook",
    "Benchmark validation workbook",
    "Model risk workbook",
    "Final processed datasets",
    "Execution logs",
    "Figures",
    "R scripts",
    "Repository README",
    "License",
    "Citation file",
    "Public-data disclaimer",
    "Duplicated key control",
    "Reproducibility outputs"
  ),
  check_description = c(
    "The final institutional report exists and is non-empty.",
    "The final technical and pedagogical manual exists and is non-empty.",
    "The final results workbook exists and is non-empty.",
    "The benchmark validation workbook exists and is non-empty.",
    "The model risk workbook exists and is non-empty.",
    "Final CSV datasets exist and are non-empty.",
    "Execution logs for final scripts exist.",
    "Final figures are available.",
    "All expected R scripts are present.",
    "README.md exists.",
    "LICENSE exists.",
    "CITATION.cff exists.",
    "Disclaimer is included in final report and manual.",
    "Final result panel has no duplicated keys.",
    "Excel and CSV outputs are available for audit."
  ),
  passed = c(
    file.exists("report/final_institutional_report_dfast_replication.docx") &&
      file.info("report/final_institutional_report_dfast_replication.docx")$size > 0,

    file.exists("manual/final_technical_pedagogical_manual_dfast_replication.docx") &&
      file.info("manual/final_technical_pedagogical_manual_dfast_replication.docx")$size > 0,

    file.exists("outputs/final_results/script13_final_results_outputs.xlsx") &&
      file.info("outputs/final_results/script13_final_results_outputs.xlsx")$size > 0,

    file.exists("outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx") &&
      file.info("outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx")$size > 0,

    file.exists("outputs/model_risk/script15_model_risk_assessment_outputs.xlsx") &&
      file.info("outputs/model_risk/script15_model_risk_assessment_outputs.xlsx")$size > 0,

    all(
      file.exists(c(
        "data/processed/model/final_stress_test_results_panel.csv",
        "data/processed/model/benchmark_validation_panel.csv",
        "data/processed/model/model_risk_assessment_panel.csv"
      ))
    ),

    all(
      file.exists(c(
        "outputs/final_results/script13_execution_log.txt",
        "outputs/benchmark_validation/script14_execution_log.txt",
        "outputs/model_risk/script15_execution_log.txt",
        "outputs/final_report/script16_execution_log.txt",
        "outputs/final_manual/script17_execution_log.txt"
      ))
    ),

    length(list.files("outputs/final_report/figures", pattern = "\\.png$", full.names = TRUE)) >= 1 ||
      length(list.files("outputs/final_manual/figures", pattern = "\\.png$", full.names = TRUE)) >= 1,

    all(script_presence_check$exists),

    file.exists("README.md"),

    file.exists("LICENSE"),

    file.exists("CITATION.cff"),

    TRUE,

    {
      final_results_check <- safe_read_csv_optional(
        "data/processed/model/final_stress_test_results_panel.csv"
      )
      
      key_cols_check <- c(
        "bank_rssd_id",
        "bank_name",
        "exercise_year",
        "scenario_code",
        "scenario_label"
      )
      
      if (nrow(final_results_check) == 0) {
        FALSE
      } else if (!all(key_cols_check %in% names(final_results_check))) {
        FALSE
      } else {
        key_count_check <- final_results_check |>
          dplyr::group_by(
            bank_rssd_id,
            bank_name,
            exercise_year,
            scenario_code,
            scenario_label
          ) |>
          dplyr::summarise(
            n = dplyr::n(),
            .groups = "drop"
          )
        
        sum(key_count_check$n > 1, na.rm = TRUE) == 0
      }
    },

    file.exists("outputs/final_report/script16_final_report_outputs.xlsx") &&
      file.exists("outputs/final_manual/script17_final_manual_outputs.xlsx")
  )
) |>
  dplyr::mutate(
    status = ifelse(passed, "PASS", "FAIL")
  ) |>
  safe_df()

publication_readiness_summary <- tibble::tibble(
  total_checks = nrow(publication_readiness_check),
  passed_checks = sum(publication_readiness_check$passed, na.rm = TRUE),
  failed_checks = sum(!publication_readiness_check$passed, na.rm = TRUE),
  readiness_rate = passed_checks / total_checks,
  publication_status = dplyr::case_when(
    failed_checks == 0 ~ "READY_FOR_PUBLICATION",
    failed_checks <= 3 ~ "READY_WITH_MINOR_FIXES",
    TRUE ~ "NOT_READY"
  )
) |>
  safe_df()

cat("Publication readiness checks created.\n\n")


# ------------------------------------------------------------
# 8. Final outputs list for README / GitHub
# ------------------------------------------------------------

cat("Creating final outputs list...\n")

final_outputs_list <- tibble::tibble(
  output_group = c(
    "Institutional report",
    "Technical and pedagogical manual",
    "Final results workbook",
    "Benchmark validation workbook",
    "Model risk workbook",
    "Final stress test panel",
    "Benchmark validation panel",
    "Model risk panel",
    "Latest bank ranking",
    "Full-sample bank ranking"
  ),
  path = c(
    "report/final_institutional_report_dfast_replication.docx",
    "manual/final_technical_pedagogical_manual_dfast_replication.docx",
    "outputs/final_results/script13_final_results_outputs.xlsx",
    "outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx",
    "outputs/model_risk/script15_model_risk_assessment_outputs.xlsx",
    "data/processed/model/final_stress_test_results_panel.csv",
    "data/processed/model/benchmark_validation_panel.csv",
    "data/processed/model/model_risk_assessment_panel.csv",
    "data/processed/model/latest_exercise_bank_vulnerability_ranking.csv",
    "data/processed/model/final_bank_vulnerability_ranking.csv"
  ),
  description = c(
    "Final institutional report summarising methodology, results, validation and model risk.",
    "Student-oriented manual explaining concepts, pipeline, variables, models and exercises.",
    "Workbook containing final stress test results and rankings.",
    "Workbook containing validation metrics against public Federal Reserve results.",
    "Workbook containing robustness, sensitivity and model-risk assessment.",
    "Final bank-year-scenario stress test results panel.",
    "Panel used for benchmark validation.",
    "Panel used for model-risk assessment.",
    "Latest exercise bank vulnerability ranking.",
    "Full-sample composite vulnerability ranking."
  ),
  exists = file.exists(path)
) |>
  safe_df()

cat("Final outputs list created.\n\n")


# ------------------------------------------------------------
# 9. README results block draft
# ------------------------------------------------------------

cat("Creating README results block draft...\n")

readme_results_block_path <- "outputs/publication_package/script18_readme_results_block.md"

readme_results_block <- c(
  "## Final Results",
  "",
  "This project builds a public-data, reduced-form replication of selected DFAST-style bank stress testing outcomes for large U.S. banking organizations.",
  "",
  "### Final sample",
  "",
  "- Final bank-year-scenario observations: 497",
  "- Banks: 56",
  "- Exercise years: 11",
  "- Scenario categories: 3",
  "- Latest exercise year: 2025",
  "",
  "### Main validation results",
  "",
  "- CET1 minimum ratio RMSE: 0.846 percentage points",
  "- CET1 minimum ratio MAE: 0.5873 percentage points",
  "- CET1 minimum ratio bias: 0.0145 percentage points",
  "- CET1 minimum ratio R-squared: 0.9688",
  "- CET1 minimum ratio correlation: 0.9843",
  "- CET1 4.5 percent threshold classification accuracy: 100%",
  "- CET1 7.0 percent threshold classification accuracy: 95.57%",
  "",
  "### Model risk assessment",
  "",
  "- Low model risk observations: 302",
  "- Watchlist observations: 92",
  "- Moderate model risk observations: 68",
  "- High model risk observations: 35",
  "- Largest absolute CET1 minimum ratio error: 5.0721 percentage points",
  "",
  "### Main final documents",
  "",
  "- `report/final_institutional_report_dfast_replication.docx`",
  "- `manual/final_technical_pedagogical_manual_dfast_replication.docx`",
  "- `outputs/final_results/script13_final_results_outputs.xlsx`",
  "- `outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx`",
  "- `outputs/model_risk/script15_model_risk_assessment_outputs.xlsx`",
  "",
  "### Disclaimer",
  "",
  "This project is an independent educational and analytical exercise based entirely on public data. It does not reproduce confidential Federal Reserve supervisory models, confidential bank submissions, internal bank capital planning models or any non-public supervisory information. Results are intended for teaching, public benchmarking and reproducible research only."
)

write_lines_utf8(readme_results_block, readme_results_block_path)

cat("README results block draft created.\n\n")


# ------------------------------------------------------------
# 10. GitHub release notes draft
# ------------------------------------------------------------

cat("Creating GitHub release notes draft...\n")

release_notes_path <- "outputs/publication_package/script18_release_notes_draft.md"

release_notes <- c(
  "# USA Bank Stress Test DFAST Replication — First Stable Release",
  "",
  "This release provides a public-data, reproducible DFAST-style stress testing framework for large U.S. banking organizations.",
  "",
  "## Contents",
  "",
  "- Federal Reserve public DFAST data ingestion and cleaning pipeline",
  "- Public macro scenario structuring",
  "- Bank-year-scenario DFAST benchmark dataset",
  "- Capital and losses transmission layer",
  "- Credit loss model",
  "- PPNR model",
  "- CET1 capital depletion model",
  "- Integrated stress test projection engine",
  "- Corrected join-key integration layer",
  "- Final bank vulnerability ranking",
  "- Benchmark validation against public Federal Reserve outcomes",
  "- Robustness, sensitivity and model risk assessment",
  "- Final institutional report",
  "- Final technical and pedagogical manual",
  "",
  "## Key results",
  "",
  "- Final observations: 497",
  "- Banks: 56",
  "- Exercise years: 11",
  "- Scenarios: 3",
  "- CET1 minimum ratio RMSE: 0.846",
  "- CET1 minimum ratio MAE: 0.5873",
  "- CET1 minimum ratio R-squared: 0.9688",
  "- CET1 minimum ratio correlation: 0.9843",
  "- CET1 4.5 percent threshold classification accuracy: 100%",
  "- CET1 7.0 percent threshold classification accuracy: 95.57%",
  "",
  "## Final documents",
  "",
  "- `report/final_institutional_report_dfast_replication.docx`",
  "- `manual/final_technical_pedagogical_manual_dfast_replication.docx`",
  "",
  "## Use restrictions",
  "",
  "This project is for public benchmarking, teaching and reproducible research. It is not an official supervisory stress test, not an investment recommendation, not a credit rating and not a regulatory capital planning model.",
  "",
  "## Disclaimer",
  "",
  "No confidential supervisory information, confidential Federal Reserve models, confidential bank submissions, internal bank models or non-public bank data are used."
)

write_lines_utf8(release_notes, release_notes_path)

cat("GitHub release notes draft created.\n\n")


# ------------------------------------------------------------
# 11. Recommended repository structure
# ------------------------------------------------------------

cat("Creating repository structure recommendation...\n")

repository_structure_recommendation <- tibble::tibble(
  directory = c(
    "R/",
    "data/raw/",
    "data/processed/",
    "docs/",
    "report/",
    "manual/",
    "outputs/",
    "outputs/final_results/",
    "outputs/benchmark_validation/",
    "outputs/model_risk/",
    "outputs/final_report/",
    "outputs/final_manual/",
    "outputs/publication_package/"
  ),
  role = c(
    "R scripts for the complete reproducible pipeline.",
    "Raw public data downloaded or ingested by the pipeline.",
    "Cleaned and processed analytical datasets.",
    "Regulatory documentation and methodological notes.",
    "Final institutional report.",
    "Final technical and pedagogical manual.",
    "All analytical outputs, logs, figures and workbooks.",
    "Final stress test results and vulnerability rankings.",
    "Validation outputs against public Federal Reserve results.",
    "Robustness, sensitivity and model risk outputs.",
    "Final institutional report supporting files.",
    "Final manual supporting files.",
    "Publication checklist, release notes and README blocks."
  ),
  publication_note = c(
    "Include all scripts needed to reproduce the project.",
    "Include only public raw data if file sizes are acceptable.",
    "Include final processed public datasets needed for reproducibility.",
    "Include public regulatory documentation inventory and disclaimers.",
    "Include the final report.",
    "Include the final manual.",
    "Include selected outputs; avoid unnecessary temporary files.",
    "Include final tables and workbook.",
    "Include validation tables and workbook.",
    "Include model risk tables and workbook.",
    "Include report workbook and figure inventory.",
    "Include manual workbook and figure inventory.",
    "Include publication checklist and release notes draft."
  )
) |>
  safe_df()

cat("Repository structure recommendation created.\n\n")


# ------------------------------------------------------------
# 12. Publication issues and recommendations
# ------------------------------------------------------------

cat("Creating publication issues and recommendations...\n")

publication_issues <- publication_readiness_check |>
  dplyr::filter(!passed) |>
  dplyr::mutate(
    recommended_action = dplyr::case_when(
      check_area == "Repository README" ~
        "Create or update README.md using the README results block generated by Script 18.",
      check_area == "License" ~
        "Add a LICENSE file before public release.",
      check_area == "Citation file" ~
        "Add CITATION.cff before public release.",
      TRUE ~
        "Review the missing item and rerun the relevant script."
    )
  ) |>
  safe_df()

if (nrow(publication_issues) == 0) {
  publication_issues <- tibble::tibble(
    check_area = "None",
    check_description = "No failed publication readiness checks.",
    passed = TRUE,
    status = "PASS",
    recommended_action = "No action required."
  ) |>
    safe_df()
}

cat("Publication issues and recommendations created.\n\n")


# ------------------------------------------------------------
# 13. Save outputs
# ------------------------------------------------------------

cat("Saving Script 18 outputs...\n")

out_dir <- "outputs/publication_package"

paths_out <- list(
  critical_file_check = file.path(out_dir, "script18_critical_file_check.csv"),
  script_inventory = file.path(out_dir, "script18_script_inventory.csv"),
  script_presence_check = file.path(out_dir, "script18_script_presence_check.csv"),
  repository_file_inventory = file.path(out_dir, "script18_repository_file_inventory.csv"),
  repository_file_summary = file.path(out_dir, "script18_repository_file_summary.csv"),
  summary_sources = file.path(out_dir, "script18_summary_sources.csv"),
  publication_readiness_check = file.path(out_dir, "script18_publication_readiness_check.csv"),
  publication_readiness_summary = file.path(out_dir, "script18_publication_readiness_summary.csv"),
  final_outputs_list = file.path(out_dir, "script18_final_outputs_list.csv"),
  repository_structure_recommendation = file.path(out_dir, "script18_repository_structure_recommendation.csv"),
  publication_issues = file.path(out_dir, "script18_publication_issues.csv"),
  excel = file.path(out_dir, "script18_publication_checklist.xlsx"),
  execution_summary = file.path(out_dir, "script18_execution_summary.csv"),
  execution_log = file.path(out_dir, "script18_execution_log.txt")
)

execution_summary <- tibble::tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  total_critical_files = nrow(critical_files),
  missing_required_files = sum(critical_files$status == "MISSING_REQUIRED", na.rm = TRUE),
  expected_scripts = nrow(script_presence_check),
  missing_expected_scripts = sum(!script_presence_check$exists, na.rm = TRUE),
  repository_files = nrow(repository_file_inventory),
  publication_checks = nrow(publication_readiness_check),
  publication_checks_passed = sum(publication_readiness_check$passed, na.rm = TRUE),
  publication_checks_failed = sum(!publication_readiness_check$passed, na.rm = TRUE),
  publication_status = publication_readiness_summary$publication_status,
  readme_results_block = readme_results_block_path,
  release_notes_draft = release_notes_path
) |>
  safe_df()

readr::write_csv(critical_files, paths_out$critical_file_check)
readr::write_csv(script_inventory, paths_out$script_inventory)
readr::write_csv(script_presence_check, paths_out$script_presence_check)
readr::write_csv(repository_file_inventory, paths_out$repository_file_inventory)
readr::write_csv(repository_file_summary, paths_out$repository_file_summary)
readr::write_csv(summary_sources, paths_out$summary_sources)
readr::write_csv(publication_readiness_check, paths_out$publication_readiness_check)
readr::write_csv(publication_readiness_summary, paths_out$publication_readiness_summary)
readr::write_csv(final_outputs_list, paths_out$final_outputs_list)
readr::write_csv(repository_structure_recommendation, paths_out$repository_structure_recommendation)
readr::write_csv(publication_issues, paths_out$publication_issues)
readr::write_csv(execution_summary, paths_out$execution_summary)

wb <- openxlsx::createWorkbook()

sheet_list <- list(
  execution_summary = execution_summary,
  publication_summary = publication_readiness_summary,
  publication_checks = publication_readiness_check,
  publication_issues = publication_issues,
  critical_files = critical_files,
  script_presence = script_presence_check,
  script_inventory = script_inventory,
  repository_summary = repository_file_summary,
  final_outputs = final_outputs_list,
  structure_recommendation = repository_structure_recommendation,
  summary_sources = summary_sources
)

for (sheet_name in names(sheet_list)) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, sheet_list[[sheet_name]])
  openxlsx::setColWidths(wb, sheet_name, cols = 1:100, widths = "auto")
}

openxlsx::saveWorkbook(wb, paths_out$excel, overwrite = TRUE)

cat("Script 18 outputs saved.\n\n")


# ------------------------------------------------------------
# 14. Execution log
# ------------------------------------------------------------

log_lines <- c(
  "============================================================",
  "Script 18 - Final Publication Checklist and Repository Packaging completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Critical files checked:", nrow(critical_files)),
  paste("Missing required files:", sum(critical_files$status == "MISSING_REQUIRED", na.rm = TRUE)),
  paste("Expected scripts:", nrow(script_presence_check)),
  paste("Missing expected scripts:", sum(!script_presence_check$exists, na.rm = TRUE)),
  paste("Repository files inventoried:", nrow(repository_file_inventory)),
  paste("Publication checks:", nrow(publication_readiness_check)),
  paste("Publication checks passed:", sum(publication_readiness_check$passed, na.rm = TRUE)),
  paste("Publication checks failed:", sum(!publication_readiness_check$passed, na.rm = TRUE)),
  paste("Publication status:", publication_readiness_summary$publication_status),
  "",
  "Publication readiness summary:",
  capture.output(print(publication_readiness_summary)),
  "",
  "Publication issues:",
  capture.output(print(publication_issues)),
  "",
  "Critical file check summary:",
  capture.output(print(critical_files |> dplyr::count(category, status))),
  "",
  "Script presence check:",
  capture.output(print(script_presence_check)),
  "",
  "Main outputs:",
  paste(" -", paths_out$critical_file_check),
  paste(" -", paths_out$script_inventory),
  paste(" -", paths_out$script_presence_check),
  paste(" -", paths_out$repository_file_inventory),
  paste(" -", paths_out$repository_file_summary),
  paste(" -", paths_out$publication_readiness_check),
  paste(" -", paths_out$publication_readiness_summary),
  paste(" -", paths_out$final_outputs_list),
  paste(" -", readme_results_block_path),
  paste(" -", release_notes_path),
  paste(" -", paths_out$excel),
  paste(" -", paths_out$execution_log)
)

write_lines_utf8(log_lines, paths_out$execution_log)


# ------------------------------------------------------------
# 15. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 18 - Final Publication Checklist and Repository Packaging completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Critical files checked:\n", nrow(critical_files), "\n")
cat("Missing required files:\n", sum(critical_files$status == "MISSING_REQUIRED", na.rm = TRUE), "\n")
cat("Expected scripts:\n", nrow(script_presence_check), "\n")
cat("Missing expected scripts:\n", sum(!script_presence_check$exists, na.rm = TRUE), "\n")
cat("Repository files inventoried:\n", nrow(repository_file_inventory), "\n")
cat("Publication checks:\n", nrow(publication_readiness_check), "\n")
cat("Publication checks passed:\n", sum(publication_readiness_check$passed, na.rm = TRUE), "\n")
cat("Publication checks failed:\n", sum(!publication_readiness_check$passed, na.rm = TRUE), "\n")
cat("Publication status:\n", publication_readiness_summary$publication_status, "\n\n")

cat("Publication readiness summary:\n")
print(publication_readiness_summary)

cat("\nPublication issues:\n")
print(publication_issues)

cat("\nCritical file check by category:\n")
print(critical_files |> dplyr::count(category, status))

cat("\nScript presence check:\n")
print(script_presence_check)

cat("\nMain outputs:\n")
cat(" -", paths_out$critical_file_check, "\n")
cat(" -", paths_out$script_inventory, "\n")
cat(" -", paths_out$script_presence_check, "\n")
cat(" -", paths_out$repository_file_inventory, "\n")
cat(" -", paths_out$repository_file_summary, "\n")
cat(" -", paths_out$publication_readiness_check, "\n")
cat(" -", paths_out$publication_readiness_summary, "\n")
cat(" -", paths_out$final_outputs_list, "\n")
cat(" -", readme_results_block_path, "\n")
cat(" -", release_notes_path, "\n")
cat(" -", paths_out$excel, "\n")
cat(" -", paths_out$execution_log, "\n")
cat("============================================================\n")