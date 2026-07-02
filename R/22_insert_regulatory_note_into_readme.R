# ============================================================
# Script 22 — Insert Regulatory Documentation Note into README
# ============================================================

cat("\n")
cat("============================================================\n")
cat("Starting Script 22 - Insert Regulatory Documentation Note into README\n")
cat("============================================================\n")

project_root <- "D:/GitHub/us-bank-stress-test-dfast-replication"
setwd(project_root)

readme_path <- "README.md"
block_path <- "outputs/publication_package/script21_readme_regulatory_documentation_block.md"
log_path <- "outputs/publication_package/script22_execution_log.txt"

if (!file.exists(readme_path)) {
  stop("README.md not found.")
}

if (!file.exists(block_path)) {
  stop("Regulatory documentation block not found.")
}

readme_lines <- readLines(readme_path, warn = FALSE, encoding = "UTF-8")
block_lines <- readLines(block_path, warn = FALSE, encoding = "UTF-8")

# Remove old regulatory documentation note if it already exists
start_marker <- "<!-- BEGIN REGULATORY DOCUMENTATION NOTE -->"
end_marker <- "<!-- END REGULATORY DOCUMENTATION NOTE -->"

has_existing_block <- any(readme_lines == start_marker) && any(readme_lines == end_marker)

if (has_existing_block) {
  start_pos <- which(readme_lines == start_marker)[1]
  end_pos <- which(readme_lines == end_marker)[1]

  if (start_pos < end_pos) {
    readme_lines <- readme_lines[-seq(start_pos, end_pos)]
  }
}

new_block <- c(
  "",
  start_marker,
  "",
  block_lines,
  "",
  end_marker,
  ""
)

# Insert after "Repository structure" section if possible;
# otherwise insert before "Analytical pipeline";
# otherwise append near the end.
repo_structure_pos <- grep("^## Repository structure", readme_lines)
analytical_pipeline_pos <- grep("^## Analytical pipeline", readme_lines)

if (length(analytical_pipeline_pos) > 0) {
  insert_pos <- analytical_pipeline_pos[1] - 1
  updated_readme <- c(
    readme_lines[seq_len(insert_pos)],
    new_block,
    readme_lines[(insert_pos + 1):length(readme_lines)]
  )
} else if (length(repo_structure_pos) > 0) {
  insert_pos <- repo_structure_pos[1]
  updated_readme <- c(
    readme_lines[seq_len(insert_pos)],
    new_block,
    readme_lines[(insert_pos + 1):length(readme_lines)]
  )
} else {
  updated_readme <- c(readme_lines, new_block)
}

writeLines(enc2utf8(updated_readme), readme_path, useBytes = TRUE)

check <- tibble::tibble(
  file = c(readme_path, block_path),
  exists = file.exists(c(readme_path, block_path)),
  size_bytes = file.info(c(readme_path, block_path))$size
)

log_lines <- c(
  "============================================================",
  "Script 22 - Insert Regulatory Documentation Note into README completed",
  "============================================================",
  paste("Project root:", project_root),
  "",
  paste("README path:", readme_path),
  paste("Block path:", block_path),
  paste("Existing block replaced:", has_existing_block),
  "",
  "File check:",
  capture.output(print(check)),
  "",
  "Inserted section markers:",
  start_marker,
  end_marker
)

writeLines(enc2utf8(log_lines), log_path, useBytes = TRUE)

cat("\n")
cat("============================================================\n")
cat("Script 22 - Insert Regulatory Documentation Note into README completed\n")
cat("============================================================\n")
cat("README updated:\n", readme_path, "\n")
cat("Block inserted from:\n", block_path, "\n")
cat("Execution log:\n", log_path, "\n\n")
print(check)
cat("============================================================\n")