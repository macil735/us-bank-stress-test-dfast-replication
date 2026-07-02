# USA Bank Stress Test DFAST Replication

**Public reduced-form replication of selected DFAST-style bank stress testing outcomes**

This repository contains a public-data, reproducible stress testing project for large U.S. banking organizations. The project builds a reduced-form DFAST-style stress testing engine using public Federal Reserve stress test results, public macroeconomic scenario data, reproducible R scripts, validation diagnostics, final reports and teaching material.

The project is designed as an educational, analytical and benchmarking exercise. It does not reproduce confidential supervisory models, confidential bank submissions or internal bank capital planning models.

## Purpose

The purpose of the project is to provide a transparent and auditable framework for understanding how public stress testing outputs can be organized, modelled, validated and communicated.

The project is designed for:

- applied banking and financial regulation courses;
- macro-financial stress testing instruction;
- financial econometrics teaching;
- reproducible research demonstrations;
- public benchmarking of stress test outputs;
- responsible communication of model risk and validation limitations.

## Important disclaimer

This repository is an independent educational and analytical project based entirely on public data. It is not affiliated with, endorsed by or approved by the Federal Reserve, any banking organization or any supervisory authority.

No confidential supervisory information, confidential Federal Reserve models, confidential bank submissions, internal bank models or non-public bank data are used.

The results must not be interpreted as an official regulatory assessment, investment recommendation, credit opinion, bank rating or statement on the safety and soundness of any institution.

## Repository structure

```text
R/                              R scripts for the reproducible pipeline
data/raw/                       Raw public data downloaded or ingested by the pipeline
data/processed/                 Cleaned and processed analytical datasets
docs/                           Scope notes, disclaimers and reproducibility documentation
docs/regulatory_framework/      Public regulatory documentation layer and validation notes
report/                         Final institutional report
manual/                         Final technical and pedagogical manual
outputs/                        Analytical outputs, logs, figures and workbooks
outputs/final_results/          Final stress test results and vulnerability rankings
outputs/benchmark_validation/   Validation against public Federal Reserve results
outputs/model_risk/             Robustness, sensitivity and model risk assessment
outputs/publication_package/    Publication checklist, audit logs and release material

Regulatory documentation note

The repository includes a regulatory documentation layer under docs/regulatory_framework/ and supporting audit outputs under outputs/publication_package/.

The regulatory documentation layer contains public Federal Reserve documents, source references, Markdown notes, audit inventories and validation notes. It is not intended to be a complete local archive of all official regulatory documents.

PDF files were validated before publication. Files with a .pdf extension were treated as valid only when their internal binary signature began with %PDF. Files that failed validation or did not open correctly were moved to a quarantine folder under:

docs/regulatory_framework/failed_downloads/

Quarantined files are retained for auditability but should not be cited as valid regulatory documentation.

The empirical pipeline relies on public Federal Reserve DFAST data and public scenario files. Regulatory documentation files support interpretation, transparency and teaching, but the project does not use confidential supervisory information.

Analytical pipeline

The project follows a staged R pipeline:

Script	Role
Script 01	Data availability audit
Script 02 / 02b / 02c	Regulatory documentation and public source ingestion
Script 03	Federal Reserve DFAST data cleaning
Script 04	Macro scenario structuring
Script 05	DFAST benchmark dataset construction
Script 06	Capital and losses transmission layer
Script 07	Exploratory analysis
Script 08	Modelling sample and treatment rules
Script 09	Credit loss model
Script 10	PPNR model
Script 11	Capital depletion model
Script 12 / 12b	Integrated stress test projection engine and join-key correction
Script 13	Final results and bank vulnerability ranking
Script 14	Benchmark validation against Federal Reserve public results
Script 15	Robustness, sensitivity and model risk assessment
Script 16	Final institutional report
Script 17	Final technical and pedagogical manual
Script 18	Final publication checklist and repository packaging
Script 19	README, license and citation files
Script 20	Gitignore and Git publication instructions
Script 23 v2	Regulatory PDF validation excluding quarantine
Script 24	Regulatory documentation cleanup after PDF audit

Some auxiliary audit scripts are included to document the validation and cleanup of regulatory documentation before publication.

Final results

This project builds a public-data, reduced-form replication of selected DFAST-style bank stress testing outcomes for large U.S. banking organizations.

Final sample
Final bank-year-scenario observations: 497
Banks: 56
Exercise years: 11
Scenario categories: 3
Latest exercise year: 2025
Main validation results
CET1 minimum ratio RMSE: 0.846 percentage points
CET1 minimum ratio MAE: 0.5873 percentage points
CET1 minimum ratio bias: 0.0145 percentage points
CET1 minimum ratio R-squared: 0.9688
CET1 minimum ratio correlation: 0.9843
CET1 4.5 percent threshold classification accuracy: 100%
CET1 7.0 percent threshold classification accuracy: 95.57%
Model risk assessment
Low model risk observations: 302
Watchlist observations: 92
Moderate model risk observations: 68
High model risk observations: 35
Largest absolute CET1 minimum ratio error: 5.0721 percentage points

The validation results indicate that the reduced-form public replication tracks the public Federal Reserve benchmark closely for the final modelling sample. Remaining deviations should be interpreted as model risk and are not evidence of supervisory model replication.

Main final documents
report/final_institutional_report_dfast_replication.docx
manual/final_technical_pedagogical_manual_dfast_replication.docx
outputs/final_results/script13_final_results_outputs.xlsx
outputs/benchmark_validation/script14_benchmark_validation_outputs.xlsx
outputs/model_risk/script15_model_risk_assessment_outputs.xlsx
outputs/publication_package/script18_publication_checklist.xlsx
outputs/publication_package/script23_v2_publication_pdf_readiness.csv
outputs/publication_package/script24_cleaning_summary.csv
How to reproduce

Open RStudio and run the scripts sequentially from the R/ folder.

Example:

source("D:/GitHub/us-bank-stress-test-dfast-replication/R/01_data_availability_audit.R")

The full project was designed for local execution on Windows. Paths inside the scripts use the project root:

D:/GitHub/us-bank-stress-test-dfast-replication

If the repository is cloned to another location, update the project_root variable at the top of each script or create a shared configuration file.

Teaching use

The manual in manual/ explains the project for students. It includes stress testing concepts, data structure, variable definitions, modelling blocks, validation metrics, model risk, exercises and glossary.

Suggested teaching sequence:

Introduce DFAST, CET1, PPNR, RWAs and stress testing.
Reproduce the data audit, cleaning and sample construction steps.
Estimate the credit loss, PPNR and capital depletion models.
Build and validate the integrated stress test engine.
Discuss benchmark validation, model risk, limitations and responsible interpretation.
Responsible interpretation

This project should be read as a public benchmarking and teaching exercise. It does not claim to estimate official regulatory capital requirements, official Stress Capital Buffer requirements or confidential supervisory outcomes.

The reduced-form models use public observed stress test outputs and public scenario information. They are useful for understanding empirical relationships in published DFAST-style data, but they do not replace supervisory models, bank internal models or regulatory judgment.

Citation

If you use this repository, cite it as:

Gelo Picol. (2026). USA Bank Stress Test DFAST Replication (v1.0.0). GitHub. https://github.com/macil735/us-bank-stress-test-dfast-replication

A machine-readable citation file is provided in CITATION.cff.

License

This project is released under the MIT License. See LICENSE for details.