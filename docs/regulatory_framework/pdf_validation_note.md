# PDF Validation Note

This note records the final validation of PDF files before GitHub publication.

## Validation rule

A valid PDF file should begin internally with the `%PDF` file signature.

Files with `.pdf` extension but without the `%PDF` signature should not be presented as valid PDF documentation.

## Search locations

- docs/regulatory_framework — exists: TRUE
- data/raw/fed — exists: TRUE
- data/raw/fed/results — exists: TRUE
- data/raw/fed/methodology — exists: TRUE

## Validation result

- PDF files searched: 27
- Valid PDF files: 12
- Invalid PDF files: 15
- Very small PDFs requiring review: 0
- Invalid PDFs in regulatory publication folder: 13

## Publication status

PDF_PUBLICATION_NOT_READY

## Required action

Move, remove or replace invalid PDFs in the regulatory publication folder before GitHub publication.

## Interpretation

The Windows file association shown in File Explorer does not prove that a file is a valid PDF. The internal file signature is the relevant validation test.

## Disclaimer

This validation only checks the technical PDF signature and basic file size. It does not verify the legal completeness, currentness or substantive accuracy of each regulatory document.
