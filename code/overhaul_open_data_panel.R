suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(jsonlite)
  library(httr2)
  library(ggplot2)
  library(scales)
  library(sf)
  library(tigris)
  library(viridis)
})

if (!file.exists("./data/county_exposure/fixed/derived/tariff_lines_mapped_fixed.csv")) {
  stop("Run from project root and build fixed tariff mapping first.")
}

collapse_naics2 <- function(x) {
  x <- as.character(x)
  x <- str_replace(x, "\\.0$", "")
  x <- if_else(str_detect(x, "^[0-9]{2}$"), x, NA_character_)
  case_when(
    x %in% c("31", "32", "33") ~ "31-33",
    x %in% c("44", "45") ~ "44-45",
    x %in% c("48", "49") ~ "48-49",
    TRUE ~ x
  )
}

make_fips5 <- function(x) {
  x <- as.character(x)
  x <- str_replace(x, "\\.0$", "")
  x <- str_extract(x, "\\d+")
  if_else(!is.na(x), str_pad(x, width = 5, side = "left", pad = "0"), NA_character_)
}

safe_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

fetch_api_matrix <- function(url, tries = 3, sleep_sec = 1.5) {
  last_err <- NULL
  for (i in seq_len(tries)) {
    out <- tryCatch({
      resp <- request(url) %>%
        req_timeout(90) %>%
        req_perform()
      txt <- resp_body_string(resp)
      fromJSON(txt)
    }, error = identity)
    if (!inherits(out, "error")) {
      return(out)
    }
    last_err <- out
    Sys.sleep(sleep_sec * i)
  }
  stop("API request failed: ", url, "\n", conditionMessage(last_err))
}

api_matrix_to_tibble <- function(mat) {
  if (length(mat) < 2) {
    return(tibble::tibble())
  }
  headers <- make.unique(mat[1, ], sep = "_dup")
  body <- mat[-1, , drop = FALSE]
  out <- as_tibble(body, .name_repair = "minimal")
  names(out) <- headers
  out
}

dir.create("./data/public/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/public/derived", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/overhaul/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/overhaul/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/overhaul/qa", recursive = TRUE, showWarnings = FALSE)

target_naics <- c(
  "00", "11", "21", "22", "23", "31-33", "42", "44-45", "48-49",
  "51", "52", "53", "54", "55", "56", "61", "62", "71", "72", "81", "99"
)

# -------------------------------------------------
# 1) Pull county industry employment panel (CBP API)
# -------------------------------------------------
years <- 2017:2023
cbp_raw_path <- "./data/public/raw/cbp_county_2017_2023_raw.csv"

fetch_cbp_year_naics <- function(y, naics_code) {
  message("Pulling CBP county data for ", y, " / NAICS ", naics_code, "...")
  code_url <- utils::URLencode(naics_code, reserved = TRUE)
  url <- paste0(
    "https://api.census.gov/data/", y,
    "/cbp?get=GEO_ID,NAME,NAICS2017,NAICS2017_LABEL,LFO,EMPSZES,YEAR,ESTAB,PAYANN,EMP",
    "&for=county:*&LFO=001&EMPSZES=001",
    "&NAICS2017=", code_url
  )
  api_matrix_to_tibble(fetch_api_matrix(url)) %>%
    mutate(year = as.integer(YEAR))
}

fetch_cbp_us_year_naics <- function(y, naics_code) {
  message("Pulling CBP U.S. totals for ", y, " / NAICS ", naics_code, "...")
  code_url <- utils::URLencode(naics_code, reserved = TRUE)
  url <- paste0(
    "https://api.census.gov/data/", y,
    "/cbp?get=NAICS2017,NAICS2017_LABEL,LFO,EMPSZES,YEAR,ESTAB,PAYANN,EMP",
    "&for=us:1&LFO=001&EMPSZES=001",
    "&NAICS2017=", code_url
  )
  api_matrix_to_tibble(fetch_api_matrix(url)) %>%
    mutate(year = as.integer(YEAR))
}

if (file.exists(cbp_raw_path)) {
  message("Using cached CBP raw file: ", cbp_raw_path)
  cbp_raw <- read_csv(cbp_raw_path, show_col_types = FALSE)
} else {
  cbp_raw <- map_dfr(years, function(y) {
    map_dfr(target_naics, function(code) fetch_cbp_year_naics(y, code))
  })
  write_csv(cbp_raw, cbp_raw_path)
}

cbp_us_raw_path <- "./data/public/raw/cbp_us_sector_2017_2023_raw.csv"
if (file.exists(cbp_us_raw_path)) {
  message("Using cached CBP U.S. file: ", cbp_us_raw_path)
  cbp_us_raw <- read_csv(cbp_us_raw_path, show_col_types = FALSE)
} else {
  cbp_us_raw <- map_dfr(years, function(y) {
    map_dfr(target_naics, function(code) fetch_cbp_us_year_naics(y, code))
  })
  write_csv(cbp_us_raw, cbp_us_raw_path)
}

cbp_clean <- cbp_raw %>%
  transmute(
    GEO_ID,
    county_fips = str_extract(GEO_ID, "\\d{5}$"),
    county_name = as.character(NAME),
    naics2c = as.character(NAICS2017),
    naics_label = as.character(NAICS2017_LABEL),
    year = as.integer(YEAR),
    estab = parse_number(as.character(ESTAB)),
    payann = parse_number(as.character(PAYANN)),
    emp = parse_number(as.character(EMP))
  ) %>%
  filter(!is.na(county_fips))

county_totals <- cbp_clean %>%
  filter(naics2c == "00") %>%
  transmute(county_fips, year, county_emp_total = emp)

county_industry_emp <- cbp_clean %>%
  filter(naics2c != "00") %>%
  left_join(county_totals, by = c("county_fips", "year")) %>%
  mutate(
    emp_share = if_else(!is.na(county_emp_total) & county_emp_total > 0, emp / county_emp_total, NA_real_),
    naics2 = if_else(str_detect(naics2c, "^\\d{2}$"), naics2c, NA_character_)
  )

national_sector_emp_fallback <- county_industry_emp %>%
  group_by(naics2c, year) %>%
  summarise(us_sector_emp_fallback = sum(emp, na.rm = TRUE), .groups = "drop")

national_sector_emp <- cbp_us_raw %>%
  transmute(
    naics2c = as.character(NAICS2017),
    year = as.integer(YEAR),
    us_sector_emp = parse_number(as.character(EMP))
  ) %>%
  filter(naics2c != "00") %>%
  left_join(national_sector_emp_fallback, by = c("naics2c", "year")) %>%
  mutate(
    us_sector_emp = coalesce(us_sector_emp, us_sector_emp_fallback)
  ) %>%
  select(naics2c, year, us_sector_emp)

write_csv(county_industry_emp, "./data/public/derived/county_industry_emp_share_2017_2023.csv")
write_csv(national_sector_emp, "./data/public/derived/us_sector_emp_2017_2023.csv")

# -------------------------------------------------
# 2) Pull annual U.S. exports by NAICS2 (Intl Trade API)
# -------------------------------------------------
export_naics_codes <- c(
  "11", "21", "22", "23", "31", "32", "33", "42", "44", "45",
  "48", "49", "51", "52", "53", "54", "55", "56", "61", "62",
  "71", "72", "81", "99"
)

fetch_exports_code <- function(code, start_year, end_year) {
  message("Pulling exports series for NAICS ", code, "...")
  url <- paste0(
    "https://api.census.gov/data/timeseries/intltrade/exports/naics",
    "?get=ALL_VAL_YR,NAICS&time=from+", start_year, "+to+", end_year, "&NAICS=", code
  )
  dat <- tryCatch(
    api_matrix_to_tibble(fetch_api_matrix(url)),
    error = function(e) {
      message("No exports returned for NAICS ", code, ". Skipping.")
      tibble::tibble()
    }
  )
  if (nrow(dat) == 0) {
    return(
      tibble::tibble(
        naics2 = character(),
        time = character(),
        year = integer(),
        month = integer(),
        all_val_yr = double()
      )
    )
  }
  dat %>%
    transmute(
      naics2 = as.character(NAICS),
      time = as.character(time),
      year = as.integer(str_sub(time, 1, 4)),
      month = as.integer(str_sub(time, 6, 7)),
      all_val_yr = as.numeric(ALL_VAL_YR)
    )
}

exports_raw_path <- "./data/public/raw/us_exports_naics_monthly_2017_2023.csv"
if (file.exists(exports_raw_path)) {
  message("Using cached exports raw file: ", exports_raw_path)
  exports_monthly <- read_csv(exports_raw_path, show_col_types = FALSE)
} else {
  exports_monthly <- map_dfr(export_naics_codes, fetch_exports_code, start_year = min(years), end_year = max(years))
  write_csv(exports_monthly, exports_raw_path)
}

# ALL_VAL_YR is year-to-date cumulative; take the maximum month value per year.
exports_annual <- exports_monthly %>%
  group_by(naics2, year) %>%
  summarise(export_value_usd = max(all_val_yr, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    naics2c = collapse_naics2(naics2)
  ) %>%
  group_by(naics2c, year) %>%
  summarise(export_value_usd = sum(export_value_usd, na.rm = TRUE), .groups = "drop")

write_csv(exports_annual, "./data/public/derived/us_exports_naics2_annual_2017_2023.csv")

# -------------------------------------------------
# 3) Build tariff severity by NAICS2 from mapped tariff lines
# -------------------------------------------------
tariff_lines <- read_csv(
  "./data/county_exposure/fixed/derived/tariff_lines_mapped_fixed.csv",
  show_col_types = FALSE
) %>%
  mutate(
    naics2c = as.character(naics2c),
    tariff_rate = as.numeric(tariff_rate)
  ) %>%
  filter(!is.na(naics2c), !is.na(tariff_rate))

tariff_summary <- tariff_lines %>%
  group_by(naics2c) %>%
  summarise(
    tariff_rate_mean = mean(tariff_rate, na.rm = TRUE),
    tariff_rate_median = median(tariff_rate, na.rm = TRUE),
    tariff_rate_p90 = quantile(tariff_rate, probs = 0.9, na.rm = TRUE),
    n_tariff_lines = n(),
    .groups = "drop"
  )

write_csv(tariff_summary, "./data/public/derived/tariff_severity_by_naics2_from_hs.csv")

# -------------------------------------------------
# 4) County-year exposure panel
# -------------------------------------------------
county_industry_panel <- county_industry_emp %>%
  left_join(national_sector_emp, by = c("naics2c", "year")) %>%
  left_join(exports_annual, by = c("naics2c", "year")) %>%
  left_join(tariff_summary, by = "naics2c") %>%
  mutate(
    export_value_usd = coalesce(export_value_usd, 0),
    tariff_rate_mean = coalesce(tariff_rate_mean, 0),
    tariff_rate_p90 = coalesce(tariff_rate_p90, 0),
    n_tariff_lines = coalesce(n_tariff_lines, 0),
    is_tariffed_sector = n_tariff_lines > 0,
    county_us_sector_emp_share = if_else(!is.na(us_sector_emp) & us_sector_emp > 0, emp / us_sector_emp, NA_real_),
    county_allocated_exports = county_us_sector_emp_share * export_value_usd,
    county_tariff_component_mean = county_allocated_exports * tariff_rate_mean,
    county_tariff_component_p90 = county_allocated_exports * tariff_rate_p90
  )

county_panel <- county_industry_panel %>%
  group_by(county_fips, county_name, year) %>%
  summarise(
    county_tariff_exposure_mean = sum(county_tariff_component_mean, na.rm = TRUE),
    county_tariff_exposure_p90 = sum(county_tariff_component_p90, na.rm = TRUE),
    county_allocated_exports_total = sum(county_allocated_exports, na.rm = TRUE),
    tariffed_emp_share = sum(if_else(is_tariffed_sector, emp_share, 0), na.rm = TRUE),
    county_emp_total = max(county_emp_total, na.rm = TRUE),
    sectors_count = n(),
    tariffed_sectors_count = sum(is_tariffed_sector, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    county_tariff_exposure_per_worker = if_else(
      !is.na(county_emp_total) & county_emp_total > 0,
      county_tariff_exposure_mean / county_emp_total,
      NA_real_
    )
  )

write_csv(county_industry_panel, "./data/public/derived/county_industry_tariff_panel_2017_2023.csv")
write_csv(county_panel, "./data/public/derived/county_tariff_exposure_panel_2017_2023.csv")

# -------------------------------------------------
# 5) Add county voting context
# -------------------------------------------------
votes16 <- read_csv("./data/votes/raw/county_vote_16.csv", show_col_types = FALSE) %>%
  transmute(
    county_fips = make_fips5(combined_fips),
    gop_share_16 = safe_num(per_gop),
    dem_share_16 = safe_num(per_dem),
    margin_16 = safe_num(per_point_diff),
    swingness_16 = 1 - abs(safe_num(per_point_diff))
  )

votes20 <- read_csv("./data/votes/raw/county_vote_20.csv", show_col_types = FALSE) %>%
  transmute(
    county_fips = make_fips5(county_fips),
    gop_share_20 = safe_num(per_gop),
    dem_share_20 = safe_num(per_dem),
    margin_20 = safe_num(per_point_diff),
    swingness_20 = 1 - abs(safe_num(per_point_diff))
  )

votes24 <- read_csv("./data/votes/raw/county_vote_24.csv", show_col_types = FALSE) %>%
  transmute(
    county_fips = make_fips5(county_fips),
    state_name = as.character(state_name),
    gop_share_24 = safe_num(per_gop),
    dem_share_24 = safe_num(per_dem),
    margin_24 = safe_num(per_point_diff),
    swingness_24 = 1 - abs(safe_num(per_point_diff))
  )

county_panel <- county_panel %>%
  left_join(votes16, by = "county_fips") %>%
  left_join(votes20, by = "county_fips") %>%
  left_join(votes24, by = "county_fips") %>%
  mutate(
    gop_shift_16_20 = gop_share_20 - gop_share_16,
    gop_shift_20_24 = gop_share_24 - gop_share_20,
    exposure_rank_within_year = percent_rank(county_tariff_exposure_mean),
    political_salience_weighted_exposure = exposure_rank_within_year * swingness_20
  )

write_csv(county_panel, "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv")

# -------------------------------------------------
# 6) Tables
# -------------------------------------------------
latest_year <- max(county_panel$year, na.rm = TRUE)

top_latest <- county_panel %>%
  filter(year == latest_year) %>%
  arrange(desc(county_tariff_exposure_mean)) %>%
  select(
    county_fips, county_name, state_name, county_tariff_exposure_mean,
    county_tariff_exposure_per_worker, tariffed_emp_share,
    gop_share_20, swingness_20, gop_share_24
  ) %>%
  slice_head(n = 50)

exposure_change <- county_panel %>%
  filter(year %in% c(min(years), latest_year)) %>%
  select(county_fips, county_name, year, county_tariff_exposure_mean) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = county_tariff_exposure_mean,
    names_prefix = "exposure_"
  ) %>%
  mutate(
    exposure_change_abs = .data[[paste0("exposure_", latest_year)]] - .data[[paste0("exposure_", min(years))]],
    exposure_change_pct = if_else(
      .data[[paste0("exposure_", min(years))]] > 0,
      exposure_change_abs / .data[[paste0("exposure_", min(years))]],
      NA_real_
    )
  ) %>%
  arrange(desc(exposure_change_abs))

reg_data <- county_panel %>%
  filter(year == latest_year, county_tariff_exposure_mean > 0, !is.na(gop_share_20), !is.na(swingness_20)) %>%
  mutate(log_exposure = log10(county_tariff_exposure_mean + 1))

fit <- lm(log_exposure ~ gop_share_20 + swingness_20 + gop_share_20:swingness_20, data = reg_data)
fit_tbl <- as.data.frame(summary(fit)$coefficients)
fit_tbl$term <- rownames(fit_tbl)
fit_tbl <- fit_tbl %>%
  select(term, everything()) %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    t_value = `t value`,
    p_value = `Pr(>|t|)`
  )

write_csv(top_latest, "./output/overhaul/tables/top50_counties_latest_exposure.csv")
write_csv(exposure_change, "./output/overhaul/tables/county_exposure_change_2017_to_2023.csv")
write_csv(fit_tbl, "./output/overhaul/tables/regression_latest_exposure_on_politics.csv")

allocation_check <- county_industry_panel %>%
  group_by(naics2c, year) %>%
  summarise(
    allocated_exports_sum = sum(county_allocated_exports, na.rm = TRUE),
    us_exports = max(export_value_usd, na.rm = TRUE),
    allocation_ratio = if_else(us_exports > 0, allocated_exports_sum / us_exports, NA_real_),
    .groups = "drop"
  )

write_csv(allocation_check, "./output/overhaul/tables/allocation_check_county_vs_us_exports.csv")

# -------------------------------------------------
# 7) Figures and maps
# -------------------------------------------------
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
)

p_trend <- county_panel %>%
  group_by(year) %>%
  summarise(
    avg_exposure = mean(county_tariff_exposure_mean, na.rm = TRUE),
    median_exposure = median(county_tariff_exposure_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = avg_exposure, color = "Average"), linewidth = 1) +
  geom_line(aes(y = median_exposure, color = "Median"), linewidth = 1) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("Average" = "#1f78b4", "Median" = "#33a02c")) +
  labs(
    title = "County tariff exposure trend (2017-2023)",
    subtitle = "Exports allocated by county share of national sector employment",
    x = "Year",
    y = "Exposure index",
    color = NULL
  )

p_latest_scatter <- county_panel %>%
  filter(year == latest_year, county_tariff_exposure_mean > 0, !is.na(gop_share_20)) %>%
  ggplot(aes(x = gop_share_20, y = log10(county_tariff_exposure_mean + 1))) +
  geom_point(alpha = 0.45, color = "#1b7837") +
  geom_smooth(method = "lm", se = TRUE, color = "#762a83", linewidth = 0.9) +
  labs(
    title = paste0("Exposure vs GOP share (", latest_year, " exposure, 2020 vote share)"),
    x = "GOP share (2020)",
    y = "log10(county exposure + 1)"
  )

ggsave("./output/overhaul/figures/trend_county_exposure_2017_2023.png", p_trend, width = 9, height = 5.5, dpi = 320)
ggsave("./output/overhaul/figures/scatter_latest_exposure_vs_gop20.png", p_latest_scatter, width = 8.5, height = 5.5, dpi = 320)

options(tigris_use_cache = TRUE)

counties_sf <- tigris::counties(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  select(GEOID, STATEFP, geometry)

states_sf <- tigris::states(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  select(STATEFP, STUSPS, geometry)

exclude_statefp <- c("02", "15", "60", "66", "69", "72", "78")

latest_map <- county_panel %>%
  filter(year == latest_year) %>%
  mutate(county_fips = make_fips5(county_fips))

baseline_map <- county_panel %>%
  filter(year == min(years)) %>%
  transmute(
    county_fips = make_fips5(county_fips),
    exposure_baseline = county_tariff_exposure_mean
  )

map_df <- counties_sf %>%
  filter(!STATEFP %in% exclude_statefp) %>%
  left_join(latest_map, by = c("GEOID" = "county_fips")) %>%
  left_join(baseline_map, by = c("GEOID" = "county_fips")) %>%
  mutate(
    exposure_change_pct = if_else(
      !is.na(exposure_baseline) & exposure_baseline > 0,
      (county_tariff_exposure_mean - exposure_baseline) / exposure_baseline,
      NA_real_
    ),
    exposure_decile = ntile(county_tariff_exposure_mean, 3),
    swing_decile = ntile(swingness_20, 3),
    bi_class = if_else(!is.na(exposure_decile) & !is.na(swing_decile), paste0(exposure_decile, "-", swing_decile), NA_character_)
  )

states_map <- states_sf %>% filter(!STATEFP %in% exclude_statefp)

p_map_exposure <- ggplot(map_df) +
  geom_sf(aes(fill = county_tariff_exposure_mean + 1), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log10",
    na.value = "grey92",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = paste0("County tariff exposure map (", latest_year, ")"),
    subtitle = "Open-source panel: Census CBP + Census international trade + mapped tariff lines",
    fill = "Exposure\n(log scale)"
  ) +
  theme_void()

p_map_per_worker <- ggplot(map_df) +
  geom_sf(aes(fill = county_tariff_exposure_per_worker + 1), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    na.value = "grey92",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = paste0("County exposure per worker (", latest_year, ")"),
    subtitle = "Exposure divided by total county employment",
    fill = "Exposure/worker\n(log scale)"
  ) +
  theme_void()

p_map_change <- ggplot(map_df) +
  geom_sf(aes(fill = exposure_change_pct), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    na.value = "grey92",
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    title = paste0("County exposure change: ", min(years), " to ", latest_year),
    subtitle = "Percent change in exposure index",
    fill = "Change"
  ) +
  theme_void()

bi_palette <- c(
  "1-1" = "#e8e8e8", "1-2" = "#b5c0da", "1-3" = "#6c83b5",
  "2-1" = "#b8d6be", "2-2" = "#90b2b3", "2-3" = "#567994",
  "3-1" = "#73ae80", "3-2" = "#5a9178", "3-3" = "#2a5a5b"
)

p_map_bi <- ggplot(map_df) +
  geom_sf(aes(fill = bi_class), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_manual(values = bi_palette, na.value = "grey92") +
  labs(
    title = paste0("Bivariate map (", latest_year, "): exposure x competitiveness"),
    subtitle = "Terciles of county exposure and 2020 electoral competitiveness",
    fill = "Exposure-\nSwing"
  ) +
  theme_void()

ggsave("./output/overhaul/figures/map_county_exposure_latest.png", p_map_exposure, width = 13, height = 8, dpi = 320)
ggsave("./output/overhaul/figures/map_county_exposure_per_worker_latest.png", p_map_per_worker, width = 13, height = 8, dpi = 320)
ggsave("./output/overhaul/figures/map_county_exposure_change_2017_2023.png", p_map_change, width = 13, height = 8, dpi = 320)
ggsave("./output/overhaul/figures/map_bivariate_exposure_competitiveness.png", p_map_bi, width = 13, height = 8, dpi = 320)

# -------------------------------------------------
# 8) QA notes
# -------------------------------------------------
qa <- tibble::tibble(
  metric = c(
    "cbp_raw_rows",
    "cbp_us_raw_rows",
    "cbp_years",
    "county_industry_emp_rows",
    "us_sector_emp_missing_rows",
    "county_unique_in_panel",
    "exports_annual_rows",
    "tariff_mapped_naics2_rows",
    "county_panel_rows",
    "latest_year",
    "latest_year_counties",
    "latest_year_with_vote20",
    "latest_year_nonzero_exposure",
    "allocation_ratio_median",
    "cor_log_exposure_gop20_latest",
    "cor_log_exposure_swing20_latest"
  ),
  value = c(
    nrow(cbp_raw),
    nrow(cbp_us_raw),
    length(unique(cbp_raw$year)),
    nrow(county_industry_emp),
    sum(is.na(national_sector_emp$us_sector_emp)),
    n_distinct(county_panel$county_fips),
    nrow(exports_annual),
    nrow(tariff_summary),
    nrow(county_panel),
    latest_year,
    sum(county_panel$year == latest_year),
    sum(county_panel$year == latest_year & !is.na(county_panel$gop_share_20)),
    sum(county_panel$year == latest_year & county_panel$county_tariff_exposure_mean > 0, na.rm = TRUE),
    median(allocation_check$allocation_ratio, na.rm = TRUE),
    suppressWarnings(cor(log10(county_panel$county_tariff_exposure_mean[county_panel$year == latest_year] + 1), county_panel$gop_share_20[county_panel$year == latest_year], use = "pairwise.complete.obs")),
    suppressWarnings(cor(log10(county_panel$county_tariff_exposure_mean[county_panel$year == latest_year] + 1), county_panel$swingness_20[county_panel$year == latest_year], use = "pairwise.complete.obs"))
  )
)

write_csv(qa, "./output/overhaul/qa/pipeline_qa.csv")

message("Open-data overhaul complete.")
message("Main panel: ./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv")
message("Figures and tables: ./output/overhaul/")
