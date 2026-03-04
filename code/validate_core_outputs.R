suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

panel_path <- "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv"
qa_path <- "./output/overhaul/qa/pipeline_qa.csv"
alloc_path <- "./output/overhaul/tables/allocation_check_county_vs_us_exports.csv"

if (!file.exists(panel_path)) {
  stop("Missing panel file. Run code/overhaul_open_data_panel.R first.")
}

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year),
    county_tariff_exposure_mean = as.numeric(county_tariff_exposure_mean),
    county_tariff_exposure_per_worker = as.numeric(county_tariff_exposure_per_worker),
    gop_share_20 = as.numeric(gop_share_20),
    swingness_20 = as.numeric(swingness_20)
  )

years_expected <- 2017:2023
latest_year <- max(panel$year, na.rm = TRUE)
latest <- panel %>% filter(year == latest_year)

checks <- list(
  list(
    check = "years_complete_2017_2023",
    pass = identical(sort(unique(panel$year)), years_expected),
    detail = paste(sort(unique(panel$year)), collapse = ",")
  ),
  list(
    check = "no_duplicate_county_year_rows",
    pass = nrow(panel) == nrow(distinct(panel, county_fips, year)),
    detail = as.character(nrow(panel) - nrow(distinct(panel, county_fips, year)))
  ),
  list(
    check = "county_fips_are_five_digits",
    pass = all(str_detect(panel$county_fips, "^\\d{5}$")),
    detail = as.character(sum(!str_detect(panel$county_fips, "^\\d{5}$")))
  ),
  list(
    check = "exposure_nonnegative",
    pass = all(panel$county_tariff_exposure_mean >= 0, na.rm = TRUE),
    detail = as.character(sum(panel$county_tariff_exposure_mean < 0, na.rm = TRUE))
  ),
  list(
    check = "latest_year_county_coverage_ge_3200",
    pass = n_distinct(latest$county_fips) >= 3200,
    detail = as.character(n_distinct(latest$county_fips))
  ),
  list(
    check = "latest_year_vote20_coverage_ge_3000",
    pass = sum(!is.na(latest$gop_share_20)) >= 3000,
    detail = as.character(sum(!is.na(latest$gop_share_20)))
  ),
  list(
    check = "latest_year_nonzero_exposure_share_ge_0_90",
    pass = mean(latest$county_tariff_exposure_mean > 0, na.rm = TRUE) >= 0.9,
    detail = as.character(round(mean(latest$county_tariff_exposure_mean > 0, na.rm = TRUE), 4))
  )
)

if (file.exists(alloc_path)) {
  alloc <- read_csv(alloc_path, show_col_types = FALSE) %>%
    mutate(
      us_exports = as.numeric(us_exports),
      allocation_ratio = as.numeric(allocation_ratio)
    ) %>%
    filter(us_exports > 0, is.finite(allocation_ratio))

  checks <- append(checks, list(
    list(
      check = "allocation_ratio_median_ge_0_85",
      pass = median(alloc$allocation_ratio, na.rm = TRUE) >= 0.85,
      detail = as.character(round(median(alloc$allocation_ratio, na.rm = TRUE), 4))
    ),
    list(
      check = "allocation_ratio_p10_ge_0_70",
      pass = quantile(alloc$allocation_ratio, 0.10, na.rm = TRUE) >= 0.70,
      detail = as.character(round(quantile(alloc$allocation_ratio, 0.10, na.rm = TRUE), 4))
    )
  ))
}

check_tbl <- bind_rows(lapply(checks, as_tibble)) %>%
  mutate(status = if_else(pass, "pass", "fail")) %>%
  select(check, status, detail)

dir.create("./output/overhaul/qa", recursive = TRUE, showWarnings = FALSE)
write_csv(check_tbl, "./output/overhaul/qa/validation_checks.csv")

if (file.exists(qa_path)) {
  qa <- read_csv(qa_path, show_col_types = FALSE)
  write_csv(qa, "./output/overhaul/qa/pipeline_qa_snapshot.csv")
}

if (any(check_tbl$status == "fail")) {
  stop(
    "Validation failed. See output/overhaul/qa/validation_checks.csv for details."
  )
}

message("Validation checks passed.")
message("Validation file: ./output/overhaul/qa/validation_checks.csv")
