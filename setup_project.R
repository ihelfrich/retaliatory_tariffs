args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check-only" %in% args

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!file.exists("README.md") || !dir.exists("data/data_cleaning_code")) {
  stop("Run this from the project root (open retaliatory-tariff.Rproj first).")
}

cran_packages <- c(
  "readr",
  "dplyr",
  "purrr",
  "tidyr",
  "stringr",
  "jsonlite",
  "httr2",
  "ggplot2",
  "scales",
  "forcats",
  "shiny",
  "DT",
  "sf",
  "tigris",
  "viridis",
  "broom",
  "sandwich",
  "lmtest",
  "remotes"
)

installed <- rownames(installed.packages())
missing_cran <- setdiff(cran_packages, installed)

if (length(missing_cran) > 0) {
  if (check_only) {
    message("Missing CRAN packages: ", paste(missing_cran, collapse = ", "))
  } else {
    install.packages(missing_cran)
  }
}

if (!requireNamespace("concordance", quietly = TRUE)) {
  if (check_only) {
    message("Missing GitHub package: concordance (insongkim/concordance)")
  } else {
    remotes::install_github("insongkim/concordance")
  }
}

if (check_only) {
  message("Dependency check complete.")
} else {
  message("Setup complete. You can now run:")
  message("  source('data/data_cleaning_code/build_county_exposure.R')")
  message("  source('data/data_cleaning_code/clean_tariff_retaliation.R')")
  message("  source('code/comprehensive_results_geospatial.R')")
  message("  source('code/overhaul_open_data_panel.R')")
  message("  source('code/validate_core_outputs.R')")
  message("  source('code/build_analysis_outputs.R')")
  message("  source('code/build_site_data.R')")
  message("  source('code/add_campaign_finance_county_measures.R')  # requires FEC_API_KEY")
}
