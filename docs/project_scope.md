\# Project Scope



\## Project title



USA Bank Stress Test DFAST Replication



\## Project objective



The objective of this project is to build a transparent, reproducible and public-data stress testing framework for large U.S. banking organizations.



The project develops a reduced-form DFAST-style stress test engine using public Federal Reserve stress test results, public macroeconomic scenarios, reproducible R scripts, validation diagnostics, final reports and teaching material.



The project is designed to show how public stress testing outputs can be organized, modelled, validated and communicated responsibly.



\## Intended use



The project is intended for:



\- applied banking and financial regulation courses;

\- macro-financial stress testing instruction;

\- financial econometrics teaching;

\- reproducible research demonstrations;

\- public benchmarking of published stress test outcomes;

\- discussion of validation, robustness and model risk.



\## What the project does



The project:



\- audits public data availability;

\- ingests public Federal Reserve DFAST and scenario files;

\- cleans and structures public stress testing data;

\- builds a bank-year-scenario benchmark dataset;

\- constructs a capital and losses transmission layer;

\- estimates reduced-form credit loss, PPNR and capital depletion models;

\- builds an integrated public stress test projection engine;

\- validates predictions against public Federal Reserve results;

\- ranks bank vulnerability using public reduced-form outputs;

\- assesses robustness, sensitivity and model risk;

\- produces final institutional and pedagogical documentation;

\- prepares a publication package for reproducible release.



\## What the project does not do



The project does not:



\- reproduce confidential Federal Reserve supervisory models;

\- use confidential supervisory information;

\- use confidential bank submissions;

\- use internal bank capital planning models;

\- calculate official Stress Capital Buffer requirements;

\- produce legally binding regulatory assessments;

\- provide investment, credit or rating advice;

\- assess the official safety and soundness of any institution.



\## Data scope



The project is based on public information only.



The core empirical inputs include public Federal Reserve DFAST results and public supervisory macroeconomic scenario data. Regulatory documentation and public references are used to support interpretation, transparency and teaching.



The project does not rely on non-public supervisory data or confidential bank-level submissions.



\## Methodological scope



The modelling approach is reduced-form and empirical.



The project estimates relationships between published stress test outcomes, public scenario information, bank-year-scenario observations and stress testing metrics such as losses, PPNR, capital depletion and minimum CET1 ratios.



The modelling strategy is designed for transparency, reproducibility and educational use. It is not intended to replicate the internal architecture of supervisory stress testing models.



\## Validation scope



The validation exercise compares model-implied outcomes with public Federal Reserve stress test results.



Validation metrics include RMSE, MAE, bias, R-squared, correlation, threshold classification accuracy and model risk flags.



Strong validation performance should be interpreted as evidence that the reduced-form public model tracks selected published outcomes in the final sample. It should not be interpreted as evidence that confidential supervisory models have been reproduced.



\## Regulatory documentation scope



The repository includes a regulatory documentation layer under `docs/regulatory\_framework/`.



This layer contains public documents, source references, Markdown notes, audit inventories and validation notes. It supports interpretation and reproducibility but is not a complete official archive of regulatory documents.



PDF files were validated before publication. Files with invalid PDF signatures or files that did not open correctly were moved to quarantine under `docs/regulatory\_framework/failed\_downloads/`.



Quarantined files are retained for auditability and should not be cited as valid regulatory documentation.



\## Computational scope



The project was developed and tested locally on Windows using R scripts.



The default project root used in the scripts is:



```text

D:/GitHub/us-bank-stress-test-dfast-replication



Users cloning the repository to another location should update the project\_root variable in the scripts or implement a shared configuration file.



Outputs



The main outputs include:



cleaned and processed datasets;

modelling samples;

reduced-form model estimates;

integrated stress test projections;

final vulnerability rankings;

benchmark validation workbooks;

robustness and model risk diagnostics;

final institutional report;

final technical and pedagogical manual;

publication checklist and audit logs.

Limitations



The main limitations are:



reliance on public stress test outputs rather than confidential supervisory data;

reduced-form modelling rather than structural supervisory model replication;

possible data revisions, naming changes and source availability changes;

sensitivity to modelling specification, sample treatment and join-key structure;

remaining tail errors and threshold-sensitive observations;

incomplete local archiving of some public regulatory documentation.



These limitations are documented to support responsible interpretation and reproducibility.



Publication status



The project includes a final publication checklist. At the time of final packaging, the repository passed the internal publication readiness checks, including required files, expected scripts, final outputs, documentation, metadata and PDF validation outside quarantine folders.





