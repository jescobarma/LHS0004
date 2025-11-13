# ============================================================
# File: generate_md_from_yaml.R
#
# Purpose: Automatically create a Markdown (.md) file from a YAML project file,
#          including a brief "How to Run LHS000401.R" instructions section.
#
# Author:
#
# Date: 2025-10-17
#
# ============================================================

# Load packages
library(yaml)
library(glue)

# Input and Output Paths
yaml_file <- "project.yaml"     # Input YAML file
output_md <- "README.md"        # Output Markdown file

# Read YAML
project_data <- yaml::read_yaml(yaml_file)

# Extract main project info
proj <- project_data$project
scripts <- project_data$scripts

# ------------------------------------------------------------
# Create Markdown header
# ------------------------------------------------------------
header_section <- glue("
# {proj$title}

**Project ID:** {proj$id}  
**Lead:** {proj$lead}  
**Collaborators:** {paste(proj$collaborators, collapse = ', ')}  
**Status:** {proj$status}

---

## Description
{proj$description}

---
")

# ------------------------------------------------------------
# Scripts Overview Table
# ------------------------------------------------------------
scripts_header <- "\n## Scripts Overview\n\n| Code | Task | Software | Created | Status |\n|------|------|-----------|----------|---------|\n" "
## Scripts Overview
| Code | Task | Software | Created | Status |
|------|------|-----------|----------|---------|
"

script_rows <- vapply(scripts, function(s) {
  glue("| {s$code} | {s$task} | {s$software} | {s$created} | {s$status} |")
}, FUN.VALUE = character(1))


# ------------------------------------------------------------
# Add a brief “How to Run LHS000401.R” section
# ------------------------------------------------------------
run_401_section <- "
---

## How to Run LHS000401.R

**Requirements:**  
R ≥ 4.3.0 with packages `haven` and `dplyr`.

**Directory structure expected:**
```
project/
 ├── code/
 │    ├── LHS000401/LHS000401.R
 │    └── LHS000490/LHS000490.R
 ├── data/
 │    ├── raw/COKR72FL.SAS7BDAT
 │    └── derived/
```

**To run from RStudio:**
```r
setwd(\"code/LHS000401\")
source(\"LHS000401.R\")
```

**To run from terminal:**
```
cd code/LHS000401
Rscript LHS000401.R
```

The script outputs:
```
data/derived/LHS000401.rda
```
"

# ------------------------------------------------------------
# Combine full markdown contents
# ------------------------------------------------------------
md_content <- c(
  header_section,
  scripts_header,
  paste(script_rows, collapse = "\n"),
  run_401_section,
  "\n---\n*Automatically generated from project.yaml*"
)

# Write the README
writeLines(md_content, output_md)

cat(glue("Markdown file '{output_md}' successfully created from '{yaml_file}'.\n"))
