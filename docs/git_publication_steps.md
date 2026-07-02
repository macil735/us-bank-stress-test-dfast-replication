# Git Publication Steps

This document provides the final Git and GitHub publication instructions for the USA Bank Stress Test DFAST Replication project.

## 1. Confirm project directory

Open PowerShell and run:

```powershell
cd D:/GitHub/us-bank-stress-test-dfast-replication
```

## 2. Check repository status

```powershell
git status
```

Review the list of modified, new or deleted files.

## 3. Add all final files

```powershell
git add .
```

## 4. Commit the final publication package

```powershell
git commit -m "Complete public DFAST replication project with final reports and publication package"
```

## 5. Confirm commit

```powershell
git log --oneline -5
```

## 6. Push to GitHub

```powershell
git push
```

If the upstream branch is not configured, use:

```powershell
git push -u origin main
```

## 7. Verify on GitHub

After pushing, open the GitHub repository page and verify that the following files are visible:

- `README.md`
- `LICENSE`
- `CITATION.cff`
- `report/final_institutional_report_dfast_replication.docx`
- `manual/final_technical_pedagogical_manual_dfast_replication.docx`
- `outputs/publication_package/script18_publication_checklist.xlsx`

## 8. Create a GitHub release

Recommended release tag:

```text
v1.0.0
```

Recommended release title:

```text
v1.0.0 - First Stable Release
```

Use the draft release notes created by Script 18:

```text
outputs/publication_package/script18_release_notes_draft.md
```

## 9. Final publication statement

The repository is ready for public release only after Script 18 reports:

```text
Publication status: READY_FOR_PUBLICATION
```

## 10. Important disclaimer

This project is a public-data, reduced-form replication and benchmarking exercise. It does not reproduce confidential Federal Reserve supervisory models, confidential bank submissions, internal bank capital planning models or any non-public supervisory information.
