if (!file.exists("README.md") || !file.exists("setup_project.R")) {
  stop("Run this script from the project root (open retaliatory-tariff.Rproj first).")
}

source("setup_project.R")
source("data/data_cleaning_code/build_county_exposure.R")
source("data/data_cleaning_code/clean_tariff_retaliation.R")
source("code/comprehensive_results_geospatial.R")
source("code/overhaul_open_data_panel.R")
source("code/validate_core_outputs.R")
source("code/build_analysis_outputs.R")
source("code/build_site_data.R")

donations_cache <- "./data/public/raw/fec_donations_by_zip_focus_committees.csv"
spending_cache <- "./data/public/raw/fec_spending_by_zip_focus_committees.csv"
has_fec_key <- nzchar(Sys.getenv("FEC_API_KEY"))
has_fec_cache <- file.exists(donations_cache) && file.exists(spending_cache)

if (has_fec_key || has_fec_cache) {
  source("code/add_campaign_finance_county_measures.R")
} else {
  message("Skipping campaign-finance extension (no FEC_API_KEY and no cached FEC ZIP files).")
}

message("Pipeline finished.")
