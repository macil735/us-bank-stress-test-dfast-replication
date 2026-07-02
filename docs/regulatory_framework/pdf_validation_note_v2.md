# PDF Validation Note — Script 23 v2

Generated on 2026-07-02 05:16:52

## Purpose

This note documents the validation of regulatory PDF files after the cleanup performed in Script 24.

## Validation rule

A file with `.pdf` extension is considered valid only if its internal file signature begins with `%PDF`.

## Excluded quarantine folder

The following folder is excluded from publication blocking checks:

- `D:/GitHub/us-bank-stress-test-dfast-replication/docs/regulatory_framework/failed_downloads`

Files in this folder are retained for auditability but should not be cited as valid regulatory documents.

## Result

- Candidate PDF files found: 37
- PDF files excluded from check: 24
- PDF files checked: 13
- Valid PDF files: 13
- Invalid PDF files: 0
- Publication blocking files: 0
- Publication PDF status: PDF_PUBLICATION_READY

## Publication implication

Only PDFs outside quarantine folders are considered part of the publication documentation set.
