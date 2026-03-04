suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(jsonlite)
})

panel_path <- "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv"
year_summary_path <- "./output/analysis/tables/year_summary.csv"
reg_main_path <- "./output/analysis/tables/regression_specs_main_terms.csv"
validation_path <- "./output/overhaul/qa/validation_checks.csv"

if (!file.exists(panel_path)) {
  stop("Missing panel file. Run code/overhaul_open_data_panel.R first.")
}

dir.create("./site/data", recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    county_name = as.character(county_name),
    state_name = as.character(state_name),
    year = as.integer(year),
    county_tariff_exposure_mean = as.numeric(county_tariff_exposure_mean),
    county_tariff_exposure_per_worker = as.numeric(county_tariff_exposure_per_worker),
    tariffed_emp_share = as.numeric(tariffed_emp_share),
    gop_share_20 = as.numeric(gop_share_20),
    gop_share_24 = as.numeric(gop_share_24),
    swingness_20 = as.numeric(swingness_20),
    swingness_24 = as.numeric(swingness_24),
    political_salience_weighted_exposure = as.numeric(political_salience_weighted_exposure)
  ) %>%
  select(
    county_fips, county_name, state_name, year,
    county_tariff_exposure_mean, county_tariff_exposure_per_worker,
    tariffed_emp_share, gop_share_20, gop_share_24, swingness_20, swingness_24,
    political_salience_weighted_exposure
  )

latest_year <- max(panel$year, na.rm = TRUE)

year_summary <- if (file.exists(year_summary_path)) {
  read_csv(year_summary_path, show_col_types = FALSE) %>%
    mutate(across(everything(), ~ifelse(is.nan(.x), NA, .x)))
} else {
  panel %>%
    group_by(year) %>%
    summarise(
      counties = n_distinct(county_fips),
      counties_with_vote20 = sum(!is.na(gop_share_20)),
      nonzero_exposure_share = mean(county_tariff_exposure_mean > 0, na.rm = TRUE),
      exposure_mean = mean(county_tariff_exposure_mean, na.rm = TRUE),
      exposure_median = median(county_tariff_exposure_mean, na.rm = TRUE),
      exposure_p90 = quantile(county_tariff_exposure_mean, 0.9, na.rm = TRUE),
      .groups = "drop"
    )
}

reg_main <- if (file.exists(reg_main_path)) {
  read_csv(reg_main_path, show_col_types = FALSE)
} else {
  fallback_latest <- panel %>%
    filter(year == latest_year, !is.na(gop_share_20), !is.na(swingness_20), county_tariff_exposure_mean > 0) %>%
    mutate(log_exposure = log10(county_tariff_exposure_mean + 1))

  if (nrow(fallback_latest) > 100) {
    mod <- lm(log_exposure ~ gop_share_20 + swingness_20 + gop_share_20:swingness_20, data = fallback_latest)
    mat <- as.matrix(summary(mod)$coefficients)
    tibble::tibble(
      specification = "Latest year baseline (fallback OLS)",
      se_type = "OLS",
      term = rownames(mat),
      estimate = as.numeric(mat[, 1]),
      std_error = as.numeric(mat[, 2]),
      statistic = as.numeric(mat[, 3]),
      p_value = as.numeric(mat[, 4])
    )
  } else {
    tibble::tibble(
      specification = character(),
      se_type = character(),
      term = character(),
      estimate = double(),
      std_error = double(),
      statistic = double(),
      p_value = double()
    )
  }
}

validation <- if (file.exists(validation_path)) {
  read_csv(
    validation_path,
    show_col_types = FALSE,
    col_types = cols(
      check = col_character(),
      status = col_character(),
      detail = col_character()
    )
  )
} else {
  tibble::tibble(check = character(), status = character(), detail = character())
}

top_latest <- panel %>%
  filter(year == latest_year) %>%
  arrange(desc(county_tariff_exposure_mean)) %>%
  select(
    county_fips, county_name, state_name,
    county_tariff_exposure_mean, county_tariff_exposure_per_worker,
    tariffed_emp_share, gop_share_20, swingness_20
  ) %>%
  slice_head(n = 100)

meta <- list(
  generated_at = as.character(Sys.time()),
  latest_year = latest_year,
  years = sort(unique(panel$year)),
  n_panel_rows = nrow(panel),
  n_latest_counties = n_distinct(panel$county_fips[panel$year == latest_year])
)

write_json(panel, "./site/data/panel.json", auto_unbox = TRUE, pretty = FALSE, na = "null")
write_json(year_summary, "./site/data/year_summary.json", auto_unbox = TRUE, pretty = FALSE, na = "null")
write_json(reg_main, "./site/data/regression_main_terms.json", auto_unbox = TRUE, pretty = FALSE, na = "null")
write_json(validation, "./site/data/validation_checks.json", auto_unbox = TRUE, pretty = FALSE, na = "null")
write_json(top_latest, "./site/data/top_latest.json", auto_unbox = TRUE, pretty = FALSE, na = "null")
write_json(meta, "./site/data/meta.json", auto_unbox = TRUE, pretty = FALSE, na = "null")

message("Site data build complete.")
message("Output directory: ./site/data")
