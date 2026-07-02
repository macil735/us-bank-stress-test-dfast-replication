# Reproducibility Notes

This project was designed as a reproducible R pipeline for a public-data, reduced-form DFAST-style bank stress testing replication.

## Execution environment

The project was developed and tested for local execution on Windows using RStudio.

The default project root used in the scripts is:

```text
D:/GitHub/us-bank-stress-test-dfast-replication

If the project is cloned to a different directory, update the project_root object at the beginning of each script or implement a shared configuration file.

Execution order

Run the scripts sequentially from the R/ folder.

The core analytical chain is:

01 -> 02 -> 02b -> 02c -> 03 -> 04 -> 05 -> 06 -> 07 -> 08 ->
09 -> 10 -> 11 -> 12 -> 12b -> 13 -> 14 -> 15 -> 16 -> 17 -> 18 -> 19 -> 20

Additional publication and documentation validation scripts were used before final release:

23 v2 -> 24 -> 23 v2 -> 18

The purpose of these additional scripts is to validate regulatory PDF files, move invalid or unopenable PDFs to quarantine, exclude quarantine folders from publication-blocking checks and confirm final publication readiness.

Key reproducibility controls
Every major script creates an execution log.
Raw, processed and final datasets are stored in structured folders.
Final panels are audited for duplicated bank-year-scenario keys.
The integrated projection engine includes a join-key correction step.
Model outputs are saved in CSV and Excel formats for auditability.
Final reports and manuals are generated from processed outputs.
Benchmark validation is performed against public Federal Reserve stress test results.
Robustness, sensitivity and model risk diagnostics are produced.
Regulatory PDF files are validated before publication.
Invalid or unopenable PDFs are moved to quarantine and excluded from publication checks.
The final publication checklist is produced by Script 18.
Regulatory documentation reproducibility

The repository includes a regulatory documentation layer under:

docs/regulatory_framework/

That quarantine folder is excluded from GitHub publication through .gitignore. Audit logs, validation tables and cleanup summaries are preserved under:

outputs/publication_package/

Final reproducibility status

The final analytical project contains:

final stress test results;
benchmark validation outputs;
robustness and model risk outputs;
final institutional report;
final technical and pedagogical manual;
regulatory PDF validation logs;
regulatory documentation cleanup logs;
publication checklist;
README, LICENSE and CITATION files.

At the time of final packaging, the project passed the internal publication readiness checks:

Publication status: READY_FOR_PUBLICATION
Publication checks passed: 15
Publication checks failed: 0

Important limitations

The project is reproducible only with respect to public data, public documentation and public stress test outputs.

It cannot reproduce confidential supervisory models, confidential Federal Reserve calculations, confidential bank submissions, internal bank capital planning models or non-public bank data.

Strong benchmark validation performance should be interpreted as evidence that the reduced-form public model tracks selected published outcomes in the final modelling sample. It should not be interpreted as evidence that confidential supervisory models have been reproduced.
