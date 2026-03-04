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
    year = as.integer(year)
  )

id_cols <- c("county_fips", "county_name", "state_name", "year")
numeric_candidates <- setdiff(names(panel), id_cols)

panel <- panel %>%
  mutate(
    across(
      all_of(numeric_candidates),
      ~ suppressWarnings(as.numeric(.x))
    )
  )

numeric_candidates <- numeric_candidates[
  vapply(panel[numeric_candidates], function(x) any(is.finite(x), na.rm = TRUE), logical(1))
]

latest_year <- max(panel$year, na.rm = TRUE)

year_summary <- if (file.exists(year_summary_path)) {
  read_csv(year_summary_path, show_col_types = FALSE) %>%
    mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x)))
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

label_lookup <- c(
  county_tariff_exposure_mean = "Tariff exposure (mean)",
  county_tariff_exposure_p90 = "Tariff exposure (p90)",
  county_tariff_exposure_per_worker = "Tariff exposure per worker",
  county_allocated_exports_total = "Allocated exports",
  tariffed_emp_share = "Tariffed employment share",
  county_emp_total = "County employment",
  sectors_count = "Sector count",
  tariffed_sectors_count = "Tariffed sector count",
  gop_share_16 = "GOP share 2016",
  gop_share_20 = "GOP share 2020",
  gop_share_24 = "GOP share 2024",
  dem_share_16 = "Dem share 2016",
  dem_share_20 = "Dem share 2020",
  dem_share_24 = "Dem share 2024",
  margin_16 = "Margin 2016",
  margin_20 = "Margin 2020",
  margin_24 = "Margin 2024",
  swingness_16 = "Swingness 2016",
  swingness_20 = "Swingness 2020",
  swingness_24 = "Swingness 2024",
  gop_shift_16_20 = "GOP shift 2016-2020",
  gop_shift_20_24 = "GOP shift 2020-2024",
  exposure_rank_within_year = "Exposure rank within year",
  political_salience_weighted_exposure = "Political salience weighted exposure"
)

variable_group <- function(name) {
  if (str_detect(name, "^(gop|dem|margin|swingness|gop_shift)")) return("politics")
  if (str_detect(name, "exposure|tariff|exports")) return("exposure")
  if (str_detect(name, "emp|sector|county_allocated")) return("structure")
  "other"
}

allowed_transforms <- function(name) {
  if (str_detect(name, "share|margin|swing|rank|shift")) return(c("none", "zscore", "rank"))
  if (str_detect(name, "exposure|exports|emp|count|tariff")) return(c("none", "log", "zscore", "rank"))
  c("none", "zscore", "rank")
}

variables <- tibble::tibble(
  name = sort(numeric_candidates)
) %>%
  mutate(
    label = dplyr::coalesce(label_lookup[name], str_to_title(str_replace_all(name, "_", " "))),
    group = vapply(name, variable_group, character(1)),
    allowed_transforms = lapply(name, allowed_transforms),
    is_custom_allowed = TRUE
  )

regression_presets <- list(
  list(
    id = "baseline_latest_hc1",
    label = "Latest year baseline (HC1)",
    sample_scope = "latest_year",
    dependent = "county_tariff_exposure_mean",
    predictors = c("gop_share_20", "swingness_20", "tariffed_emp_share"),
    se_mode = "hc1",
    include_intercept = TRUE
  ),
  list(
    id = "baseline_selected_cluster",
    label = "Selected year baseline (cluster state)",
    sample_scope = "selected_year",
    dependent = "county_tariff_exposure_mean",
    predictors = c("gop_share_20", "swingness_20", "tariffed_emp_share"),
    se_mode = "cluster_state",
    include_intercept = TRUE
  ),
  list(
    id = "shift_2020_hc1",
    label = "2020 GOP shift model (HC1)",
    sample_scope = "latest_year",
    dependent = "gop_shift_16_20",
    predictors = c("county_tariff_exposure_per_worker", "tariffed_emp_share", "gop_share_16"),
    se_mode = "hc1",
    include_intercept = TRUE
  )
)

build_commit <- tryCatch(
  {
    out <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)
    if (length(out) > 0) out[[1]] else NA_character_
  },
  error = function(e) NA_character_
)

meta <- list(
  schema_version = "2026-03-04-v2",
  generated_at = as.character(Sys.time()),
  build_timestamp_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  build_commit = build_commit,
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
write_json(variables, "./site/data/variables.json", auto_unbox = TRUE, pretty = FALSE, na = "null")
write_json(regression_presets, "./site/data/regression_presets.json", auto_unbox = TRUE, pretty = FALSE, na = "null")
write_json(meta, "./site/data/meta.json", auto_unbox = TRUE, pretty = FALSE, na = "null")

message("Site data build complete.")
message("Output directory: ./site/data")
