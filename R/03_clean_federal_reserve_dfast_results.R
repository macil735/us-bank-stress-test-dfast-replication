# ============================================================
# USA Bank Stress Test DFAST Replication
# Script 03 — Clean Federal Reserve DFAST Results
# ============================================================
# Objetivo:
#   Limpar, auditar e preparar os ficheiros públicos da Federal
#   Reserve relativos ao DFAST / Supervisory Stress Test.
#
# Inputs principais:
#   data/raw/fed/results/fed_public_results_DFAST_2026.csv
#   data/raw/fed/results/fed_2026_Detailed_Nine_Quarter_Paths.csv
#   data/raw/fed/scenarios/fed_2026_Final_Historic_Domestic.csv
#   data/raw/fed/scenarios/fed_2026_Final_Supervisory_Baseline_Domestic.csv
#   data/raw/fed/scenarios/fed_2026_Final_Supervisory_Severely_Adverse_Domestic.csv
#
# Outputs principais:
#   data/processed/fed/fed_dfast_results_clean.csv
#   data/processed/fed/fed_dfast_nine_quarter_paths_clean.csv
#   data/processed/fed/fed_macro_historic_domestic_clean.csv
#   data/processed/fed/fed_macro_baseline_domestic_clean.csv
#   data/processed/fed/fed_macro_severely_adverse_domestic_clean.csv
#   outputs/fed_cleaning/script03_fed_dfast_cleaning_report.docx
#   outputs/fed_cleaning/script03_execution_log.txt
#
# Nota:
#   Este script ainda NÃO estima modelos de perdas, PPNR ou capital.
#   Apenas cria bases limpas e auditadas da Federal Reserve.
# ============================================================


# ------------------------------------------------------------
# 0. Configuração inicial
# ------------------------------------------------------------

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"

script_id <- "03"
script_name <- "clean_federal_reserve_dfast_results"
start_time <- Sys.time()

dir_list <- c(
  file.path(project_root, "data"),
  file.path(project_root, "data/raw"),
  file.path(project_root, "data/raw/fed"),
  file.path(project_root, "data/raw/fed/results"),
  file.path(project_root, "data/raw/fed/scenarios"),
  file.path(project_root, "data/processed"),
  file.path(project_root, "data/processed/fed"),
  file.path(project_root, "outputs"),
  file.path(project_root, "outputs/fed_cleaning")
)

for (d in dir_list) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

setwd(project_root)


# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "tibble",
  "readr",
  "stringr",
  "purrr",
  "tidyr",
  "janitor",
  "lubridate",
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
library(tidyr)
library(janitor)
library(lubridate)
library(openxlsx)
library(officer)
library(flextable)


# ------------------------------------------------------------
# 2. Caminhos dos ficheiros
# ------------------------------------------------------------

raw_results_path <- file.path(
  project_root,
  "data/raw/fed/results/fed_public_results_DFAST_2026.csv"
)

raw_9q_path <- file.path(
  project_root,
  "data/raw/fed/results/fed_2026_Detailed_Nine_Quarter_Paths.csv"
)

raw_historic_domestic_path <- file.path(
  project_root,
  "data/raw/fed/scenarios/fed_2026_Final_Historic_Domestic.csv"
)

raw_baseline_domestic_path <- file.path(
  project_root,
  "data/raw/fed/scenarios/fed_2026_Final_Supervisory_Baseline_Domestic.csv"
)

raw_severely_adverse_domestic_path <- file.path(
  project_root,
  "data/raw/fed/scenarios/fed_2026_Final_Supervisory_Severely_Adverse_Domestic.csv"
)

processed_dir <- file.path(project_root, "data/processed/fed")
output_dir <- file.path(project_root, "outputs/fed_cleaning")


# ------------------------------------------------------------
# 3. Funções auxiliares
# ------------------------------------------------------------

file_check <- function(path, file_role) {
  tibble(
    file_role = file_role,
    path = path,
    exists = file.exists(path),
    size_bytes = ifelse(file.exists(path), file.info(path)$size, NA_real_),
    modified_time = ifelse(
      file.exists(path),
      as.character(file.info(path)$mtime),
      NA_character_
    )
  )
}


safe_read_csv_full <- function(path) {
  if (!file.exists(path)) {
    warning(paste("File does not exist:", path))
    return(tibble())
  }

  tryCatch(
    {
      readr::read_csv(path, show_col_types = FALSE, guess_max = 100000) |>
        janitor::clean_names()
    },
    error = function(e) {
      warning(paste("Could not read file:", path, "|", conditionMessage(e)))
      tibble()
    }
  )
}


safe_read_csv_preview <- function(path, n_max = 10) {
  if (!file.exists(path)) {
    return(tibble())
  }

  tryCatch(
    {
      readr::read_csv(path, show_col_types = FALSE, n_max = n_max) |>
        janitor::clean_names()
    },
    error = function(e) {
      tibble(read_error = conditionMessage(e))
    }
  )
}


standardize_character_columns <- function(df) {
  df |>
    mutate(
      across(
        where(is.character),
        ~ str_squish(.x)
      )
    )
}


convert_possible_numeric <- function(x) {
  if (is.numeric(x)) return(x)

  if (is.character(x)) {
    x_clean <- x |>
      str_replace_all(",", "") |>
      str_replace_all("%", "") |>
      str_replace_all("\\$", "") |>
      str_squish()

    suppressWarnings(x_num <- as.numeric(x_clean))

    share_numeric <- mean(!is.na(x_num) | is.na(x_clean) | x_clean == "")

    if (!is.nan(share_numeric) && share_numeric > 0.80) {
      return(x_num)
    }
  }

  x
}


convert_numeric_like_columns <- function(df) {
  df |>
    mutate(across(everything(), convert_possible_numeric))
}


create_variable_audit <- function(df, dataset_name) {
  if (nrow(df) == 0 && ncol(df) == 0) {
    return(
      tibble(
        dataset_name = dataset_name,
        variable = NA_character_,
        class = NA_character_,
        n_obs = 0,
        n_missing = NA_integer_,
        missing_share = NA_real_,
        n_unique = NA_integer_
      )
    )
  }

  tibble(
    dataset_name = dataset_name,
    variable = names(df),
    class = map_chr(df, ~ paste(class(.x), collapse = "|")),
    n_obs = nrow(df),
    n_missing = map_int(df, ~ sum(is.na(.x))),
    missing_share = map_dbl(df, ~ mean(is.na(.x))),
    n_unique = map_int(df, ~ dplyr::n_distinct(.x, na.rm = TRUE))
  )
}


detect_id_columns <- function(df) {
  nms <- names(df)

  possible_bank_cols <- nms[
    str_detect(
      nms,
      "bank|firm|company|institution|bhc|rssd|lei|name"
    )
  ]

  possible_year_cols <- nms[
    str_detect(
      nms,
      "year|date|quarter|period|as_of|scenario"
    )
  ]

  tibble(
    possible_bank_or_entity_columns = paste(possible_bank_cols, collapse = " | "),
    possible_time_or_scenario_columns = paste(possible_year_cols, collapse = " | ")
  )
}


make_bank_year_audit <- function(df, dataset_name) {
  if (nrow(df) == 0) {
    return(
      tibble(
        dataset_name = dataset_name,
        audit_note = "Dataset is empty or unreadable"
      )
    )
  }

  nms <- names(df)

  bank_col <- nms[str_detect(nms, "bank|firm|company|institution|bhc|name")][1]
  year_col <- nms[str_detect(nms, "year")][1]
  scenario_col <- nms[str_detect(nms, "scenario")][1]

  out <- tibble(
    dataset_name = dataset_name,
    rows = nrow(df),
    columns = ncol(df),
    detected_bank_column = ifelse(is.na(bank_col), NA_character_, bank_col),
    detected_year_column = ifelse(is.na(year_col), NA_character_, year_col),
    detected_scenario_column = ifelse(is.na(scenario_col), NA_character_, scenario_col)
  )

  if (!is.na(bank_col)) {
    out <- out |>
      mutate(number_of_banks_or_entities = n_distinct(df[[bank_col]], na.rm = TRUE))
  } else {
    out <- out |>
      mutate(number_of_banks_or_entities = NA_integer_)
  }

  if (!is.na(year_col)) {
    out <- out |>
      mutate(
        min_year = suppressWarnings(min(as.numeric(df[[year_col]]), na.rm = TRUE)),
        max_year = suppressWarnings(max(as.numeric(df[[year_col]]), na.rm = TRUE))
      )
  } else {
    out <- out |>
      mutate(min_year = NA_real_, max_year = NA_real_)
  }

  out
}


safe_write_csv <- function(df, path) {
  readr::write_csv(df, path)
  tibble(
    output_path = path,
    exists = file.exists(path),
    size_bytes = ifelse(file.exists(path), file.info(path)$size, NA_real_)
  )
}


# ------------------------------------------------------------
# 4. Verificação dos inputs
# ------------------------------------------------------------

input_file_audit <- bind_rows(
  file_check(raw_results_path, "Fed public DFAST results"),
  file_check(raw_9q_path, "Fed detailed nine-quarter paths"),
  file_check(raw_historic_domestic_path, "Fed historic domestic macro scenario"),
  file_check(raw_baseline_domestic_path, "Fed supervisory baseline domestic scenario"),
  file_check(raw_severely_adverse_domestic_path, "Fed supervisory severely adverse domestic scenario")
)


# ------------------------------------------------------------
# 5. Leitura dos dados brutos
# ------------------------------------------------------------

fed_results_raw <- safe_read_csv_full(raw_results_path)
fed_9q_raw <- safe_read_csv_full(raw_9q_path)
fed_historic_domestic_raw <- safe_read_csv_full(raw_historic_domestic_path)
fed_baseline_domestic_raw <- safe_read_csv_full(raw_baseline_domestic_path)
fed_severely_adverse_domestic_raw <- safe_read_csv_full(raw_severely_adverse_domestic_path)


# ------------------------------------------------------------
# 6. Limpeza leve dos dados
# ------------------------------------------------------------
# Princípio:
#   - Limpar nomes de colunas;
#   - Remover espaços desnecessários;
#   - Converter colunas claramente numéricas;
#   - Não alterar definições económicas;
#   - Não agregar nem estimar nada nesta fase.

fed_results_clean <- fed_results_raw |>
  standardize_character_columns() |>
  convert_numeric_like_columns()

fed_9q_clean <- fed_9q_raw |>
  standardize_character_columns() |>
  convert_numeric_like_columns()

fed_historic_domestic_clean <- fed_historic_domestic_raw |>
  standardize_character_columns() |>
  convert_numeric_like_columns()

fed_baseline_domestic_clean <- fed_baseline_domestic_raw |>
  standardize_character_columns() |>
  convert_numeric_like_columns()

fed_severely_adverse_domestic_clean <- fed_severely_adverse_domestic_raw |>
  standardize_character_columns() |>
  convert_numeric_like_columns()


# ------------------------------------------------------------
# 7. Criação de colunas de metadados
# ------------------------------------------------------------

add_dataset_metadata <- function(df, source_file, dataset_role, scenario_type = NA_character_) {
  if (nrow(df) == 0 && ncol(df) == 0) return(df)

  df |>
    mutate(
      source_file = source_file,
      dataset_role = dataset_role,
      scenario_type = scenario_type,
      cleaned_at = as.character(Sys.time()),
      .before = 1
    )
}

fed_results_clean <- fed_results_clean |>
  add_dataset_metadata(
    source_file = "fed_public_results_DFAST_2026.csv",
    dataset_role = "DFAST public results benchmark",
    scenario_type = "supervisory stress test results"
  )

fed_9q_clean <- fed_9q_clean |>
  add_dataset_metadata(
    source_file = "fed_2026_Detailed_Nine_Quarter_Paths.csv",
    dataset_role = "DFAST detailed nine-quarter paths",
    scenario_type = "supervisory stress test paths"
  )

fed_historic_domestic_clean <- fed_historic_domestic_clean |>
  add_dataset_metadata(
    source_file = "fed_2026_Final_Historic_Domestic.csv",
    dataset_role = "Historic domestic macro data",
    scenario_type = "historic domestic"
  )

fed_baseline_domestic_clean <- fed_baseline_domestic_clean |>
  add_dataset_metadata(
    source_file = "fed_2026_Final_Supervisory_Baseline_Domestic.csv",
    dataset_role = "Supervisory baseline domestic scenario",
    scenario_type = "baseline domestic"
  )

fed_severely_adverse_domestic_clean <- fed_severely_adverse_domestic_clean |>
  add_dataset_metadata(
    source_file = "fed_2026_Final_Supervisory_Severely_Adverse_Domestic.csv",
    dataset_role = "Supervisory severely adverse domestic scenario",
    scenario_type = "severely adverse domestic"
  )


# ------------------------------------------------------------
# 8. Auditoria de variáveis
# ------------------------------------------------------------

variable_audit <- bind_rows(
  create_variable_audit(fed_results_clean, "fed_dfast_results_clean"),
  create_variable_audit(fed_9q_clean, "fed_dfast_nine_quarter_paths_clean"),
  create_variable_audit(fed_historic_domestic_clean, "fed_macro_historic_domestic_clean"),
  create_variable_audit(fed_baseline_domestic_clean, "fed_macro_baseline_domestic_clean"),
  create_variable_audit(fed_severely_adverse_domestic_clean, "fed_macro_severely_adverse_domestic_clean")
)


# ------------------------------------------------------------
# 9. Auditoria de bancos, anos e cenários
# ------------------------------------------------------------

bank_year_audit <- bind_rows(
  make_bank_year_audit(fed_results_clean, "fed_dfast_results_clean"),
  make_bank_year_audit(fed_9q_clean, "fed_dfast_nine_quarter_paths_clean"),
  make_bank_year_audit(fed_historic_domestic_clean, "fed_macro_historic_domestic_clean"),
  make_bank_year_audit(fed_baseline_domestic_clean, "fed_macro_baseline_domestic_clean"),
  make_bank_year_audit(fed_severely_adverse_domestic_clean, "fed_macro_severely_adverse_domestic_clean")
)


id_column_audit <- bind_rows(
  detect_id_columns(fed_results_clean) |>
    mutate(dataset_name = "fed_dfast_results_clean", .before = 1),
  detect_id_columns(fed_9q_clean) |>
    mutate(dataset_name = "fed_dfast_nine_quarter_paths_clean", .before = 1),
  detect_id_columns(fed_historic_domestic_clean) |>
    mutate(dataset_name = "fed_macro_historic_domestic_clean", .before = 1),
  detect_id_columns(fed_baseline_domestic_clean) |>
    mutate(dataset_name = "fed_macro_baseline_domestic_clean", .before = 1),
  detect_id_columns(fed_severely_adverse_domestic_clean) |>
    mutate(dataset_name = "fed_macro_severely_adverse_domestic_clean", .before = 1)
)


# ------------------------------------------------------------
# 10. Pré-visualização das bases
# ------------------------------------------------------------

dataset_preview <- bind_rows(
  fed_results_clean |>
    head(5) |>
    mutate(dataset_name = "fed_dfast_results_clean", .before = 1),
  fed_9q_clean |>
    head(5) |>
    mutate(dataset_name = "fed_dfast_nine_quarter_paths_clean", .before = 1),
  fed_historic_domestic_clean |>
    head(5) |>
    mutate(dataset_name = "fed_macro_historic_domestic_clean", .before = 1),
  fed_baseline_domestic_clean |>
    head(5) |>
    mutate(dataset_name = "fed_macro_baseline_domestic_clean", .before = 1),
  fed_severely_adverse_domestic_clean |>
    head(5) |>
    mutate(dataset_name = "fed_macro_severely_adverse_domestic_clean", .before = 1)
)


# ------------------------------------------------------------
# 11. Estatísticas estruturais
# ------------------------------------------------------------

dataset_structure_summary <- tibble(
  dataset_name = c(
    "fed_dfast_results_clean",
    "fed_dfast_nine_quarter_paths_clean",
    "fed_macro_historic_domestic_clean",
    "fed_macro_baseline_domestic_clean",
    "fed_macro_severely_adverse_domestic_clean"
  ),
  rows = c(
    nrow(fed_results_clean),
    nrow(fed_9q_clean),
    nrow(fed_historic_domestic_clean),
    nrow(fed_baseline_domestic_clean),
    nrow(fed_severely_adverse_domestic_clean)
  ),
  columns = c(
    ncol(fed_results_clean),
    ncol(fed_9q_clean),
    ncol(fed_historic_domestic_clean),
    ncol(fed_baseline_domestic_clean),
    ncol(fed_severely_adverse_domestic_clean)
  ),
  source_file = c(
    "fed_public_results_DFAST_2026.csv",
    "fed_2026_Detailed_Nine_Quarter_Paths.csv",
    "fed_2026_Final_Historic_Domestic.csv",
    "fed_2026_Final_Supervisory_Baseline_Domestic.csv",
    "fed_2026_Final_Supervisory_Severely_Adverse_Domestic.csv"
  )
) |>
  mutate(
    empty_dataset = rows == 0 | columns == 0,
    cleaning_status = ifelse(empty_dataset, "Review required", "Cleaned")
  )


# ------------------------------------------------------------
# 12. Guardar bases processadas
# ------------------------------------------------------------

processed_outputs <- bind_rows(
  safe_write_csv(
    fed_results_clean,
    file.path(processed_dir, "fed_dfast_results_clean.csv")
  ),
  safe_write_csv(
    fed_9q_clean,
    file.path(processed_dir, "fed_dfast_nine_quarter_paths_clean.csv")
  ),
  safe_write_csv(
    fed_historic_domestic_clean,
    file.path(processed_dir, "fed_macro_historic_domestic_clean.csv")
  ),
  safe_write_csv(
    fed_baseline_domestic_clean,
    file.path(processed_dir, "fed_macro_baseline_domestic_clean.csv")
  ),
  safe_write_csv(
    fed_severely_adverse_domestic_clean,
    file.path(processed_dir, "fed_macro_severely_adverse_domestic_clean.csv")
  )
)


# ------------------------------------------------------------
# 13. Guardar auditorias
# ------------------------------------------------------------

input_file_audit_path <- file.path(output_dir, "script03_input_file_audit.csv")
variable_audit_path <- file.path(output_dir, "fed_dfast_results_variable_audit.csv")
bank_year_audit_path <- file.path(output_dir, "fed_dfast_results_bank_year_audit.csv")
id_column_audit_path <- file.path(output_dir, "fed_dfast_id_column_audit.csv")
dataset_structure_summary_path <- file.path(output_dir, "fed_dfast_dataset_structure_summary.csv")
processed_outputs_path <- file.path(output_dir, "script03_processed_outputs.csv")
dataset_preview_path <- file.path(output_dir, "script03_dataset_preview.csv")

write_csv(input_file_audit, input_file_audit_path)
write_csv(variable_audit, variable_audit_path)
write_csv(bank_year_audit, bank_year_audit_path)
write_csv(id_column_audit, id_column_audit_path)
write_csv(dataset_structure_summary, dataset_structure_summary_path)
write_csv(processed_outputs, processed_outputs_path)
write_csv(dataset_preview, dataset_preview_path)


# ------------------------------------------------------------
# 14. Excel consolidado
# ------------------------------------------------------------

excel_output_path <- file.path(output_dir, "script03_fed_dfast_cleaning_outputs.xlsx")

wb <- createWorkbook()

addWorksheet(wb, "input_file_audit")
writeData(wb, "input_file_audit", input_file_audit)

addWorksheet(wb, "dataset_structure")
writeData(wb, "dataset_structure", dataset_structure_summary)

addWorksheet(wb, "variable_audit")
writeData(wb, "variable_audit", variable_audit)

addWorksheet(wb, "bank_year_audit")
writeData(wb, "bank_year_audit", bank_year_audit)

addWorksheet(wb, "id_column_audit")
writeData(wb, "id_column_audit", id_column_audit)

addWorksheet(wb, "processed_outputs")
writeData(wb, "processed_outputs", processed_outputs)

addWorksheet(wb, "dataset_preview")
writeData(wb, "dataset_preview", dataset_preview)

for (sheet in names(wb)) {
  setColWidths(wb, sheet, cols = 1:80, widths = "auto")
}

saveWorkbook(wb, excel_output_path, overwrite = TRUE)


# ------------------------------------------------------------
# 15. Relatório Word
# ------------------------------------------------------------

report_docx_path <- file.path(output_dir, "script03_fed_dfast_cleaning_report.docx")

execution_summary <- tibble(
  script_id = script_id,
  script_name = script_name,
  project_root = project_root,
  started_at = as.character(start_time),
  completed_at = as.character(Sys.time()),
  input_files_checked = nrow(input_file_audit),
  input_files_available = sum(input_file_audit$exists, na.rm = TRUE),
  datasets_cleaned = nrow(dataset_structure_summary),
  processed_files_created = sum(processed_outputs$exists, na.rm = TRUE),
  total_variables_audited = nrow(variable_audit)
)

doc <- read_docx()

doc <- doc |>
  body_add_par("Script 03 — Clean Federal Reserve DFAST Results", style = "heading 1") |>
  body_add_par("USA Bank Stress Test DFAST Replication", style = "Normal") |>
  body_add_par(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), style = "Normal") |>
  body_add_par("1. Objective", style = "heading 2") |>
  body_add_par(
    "This script cleans and audits the public Federal Reserve DFAST datasets required for a public and approximate stress test replication. The script preserves the regulatory meaning of the original variables and avoids analytical transformation at this stage.",
    style = "Normal"
  ) |>
  body_add_par("2. Execution summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(execution_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("3. Input file audit", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(input_file_audit) |>
    autofit()
)

doc <- doc |>
  body_add_par("4. Dataset structure summary", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(dataset_structure_summary) |>
    autofit()
)

doc <- doc |>
  body_add_par("5. Bank, year and scenario audit", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(bank_year_audit) |>
    autofit()
)

doc <- doc |>
  body_add_par("6. Identification column audit", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(id_column_audit) |>
    autofit()
)

doc <- doc |>
  body_add_par("7. Processed outputs", style = "heading 2")

doc <- body_add_flextable(
  doc,
  flextable(processed_outputs) |>
    autofit()
)

doc <- doc |>
  body_add_par("8. Methodological note", style = "heading 2") |>
  body_add_par(
    "This script does not estimate credit losses, pre-provision net revenue, capital depletion or stressed capital ratios. It only creates cleaned and auditable Federal Reserve source datasets for subsequent scripts.",
    style = "Normal"
  )

print(doc, target = report_docx_path)


# ------------------------------------------------------------
# 16. Log de execução
# ------------------------------------------------------------

execution_log_path <- file.path(output_dir, "script03_execution_log.txt")

log_lines <- c(
  "============================================================",
  "Script 03 — Clean Federal Reserve DFAST Results completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("Started at:", as.character(start_time)),
  paste("Completed at:", as.character(Sys.time())),
  "",
  paste("Input files checked:", nrow(input_file_audit)),
  paste("Input files available:", sum(input_file_audit$exists, na.rm = TRUE)),
  paste("Datasets cleaned:", nrow(dataset_structure_summary)),
  paste("Processed files created:", sum(processed_outputs$exists, na.rm = TRUE)),
  paste("Total variables audited:", nrow(variable_audit)),
  "",
  "Dataset structure summary:",
  capture.output(print(dataset_structure_summary)),
  "",
  "Processed outputs:",
  capture.output(print(processed_outputs)),
  "",
  "Main outputs:",
  paste(" -", file.path(processed_dir, "fed_dfast_results_clean.csv")),
  paste(" -", file.path(processed_dir, "fed_dfast_nine_quarter_paths_clean.csv")),
  paste(" -", file.path(processed_dir, "fed_macro_historic_domestic_clean.csv")),
  paste(" -", file.path(processed_dir, "fed_macro_baseline_domestic_clean.csv")),
  paste(" -", file.path(processed_dir, "fed_macro_severely_adverse_domestic_clean.csv")),
  "",
  "Audit outputs:",
  paste(" -", input_file_audit_path),
  paste(" -", variable_audit_path),
  paste(" -", bank_year_audit_path),
  paste(" -", id_column_audit_path),
  paste(" -", dataset_structure_summary_path),
  paste(" -", processed_outputs_path),
  paste(" -", dataset_preview_path),
  paste(" -", excel_output_path),
  paste(" -", report_docx_path),
  paste(" -", execution_log_path)
)

writeLines(log_lines, execution_log_path, useBytes = TRUE)


# ------------------------------------------------------------
# 17. Console output
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("Script 03 — Clean Federal Reserve DFAST Results completed\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")

cat("Input files checked:\n", nrow(input_file_audit), "\n")
cat("Input files available:\n", sum(input_file_audit$exists, na.rm = TRUE), "\n")
cat("Datasets cleaned:\n", nrow(dataset_structure_summary), "\n")
cat("Processed files created:\n", sum(processed_outputs$exists, na.rm = TRUE), "\n")
cat("Total variables audited:\n", nrow(variable_audit), "\n\n")

cat("Dataset structure summary:\n")
print(dataset_structure_summary)

cat("\nProcessed outputs:\n")
print(processed_outputs)

cat("\nMain outputs:\n")
cat(" -", file.path(processed_dir, "fed_dfast_results_clean.csv"), "\n")
cat(" -", file.path(processed_dir, "fed_dfast_nine_quarter_paths_clean.csv"), "\n")
cat(" -", file.path(processed_dir, "fed_macro_historic_domestic_clean.csv"), "\n")
cat(" -", file.path(processed_dir, "fed_macro_baseline_domestic_clean.csv"), "\n")
cat(" -", file.path(processed_dir, "fed_macro_severely_adverse_domestic_clean.csv"), "\n")

cat("\nAudit outputs:\n")
cat(" -", input_file_audit_path, "\n")
cat(" -", variable_audit_path, "\n")
cat(" -", bank_year_audit_path, "\n")
cat(" -", id_column_audit_path, "\n")
cat(" -", dataset_structure_summary_path, "\n")
cat(" -", processed_outputs_path, "\n")
cat(" -", dataset_preview_path, "\n")
cat(" -", excel_output_path, "\n")
cat(" -", report_docx_path, "\n")
cat(" -", execution_log_path, "\n")