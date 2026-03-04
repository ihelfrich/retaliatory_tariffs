suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(httr2)
  library(jsonlite)
  library(ggplot2)
  library(scales)
  library(sf)
  library(tigris)
  library(viridis)
})

if (!file.exists("./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv")) {
  stop("Run code/overhaul_open_data_panel.R first.")
}

committee_file <- "./data/public/raw/fec_committees_focus.csv"
if (!file.exists(committee_file)) {
  stop("Missing committee file: ", committee_file)
}

dir.create("./data/public/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/public/derived", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/finance_extension/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/finance_extension/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/finance_extension/qa", recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

fec_api_key <- Sys.getenv("FEC_API_KEY", unset = "")
donations_zip_cache <- "./data/public/raw/fec_donations_by_zip_focus_committees.csv"
spending_zip_cache <- "./data/public/raw/fec_spending_by_zip_focus_committees.csv"
force_refresh <- tolower(Sys.getenv("FEC_FORCE_REFRESH", unset = "false")) %in% c("1", "true", "yes")
max_pages <- suppressWarnings(as.integer(Sys.getenv("FEC_MAX_PAGES", unset = "500")))
if (is.na(max_pages) || max_pages <= 0) {
  max_pages <- 500L
}

fetch_fec_paginated <- function(endpoint, query_params, max_pages = 500L, pause_sec = 0.15) {
  if (fec_api_key == "") {
    stop("FEC_API_KEY is required when cache files are not available.")
  }

  base_url <- paste0("https://api.open.fec.gov/v1/", endpoint)
  page <- 1L
  out <- list()
  truncated <- FALSE

  repeat {
    req <- request(base_url) |>
      req_url_query(
        api_key = fec_api_key,
        page = page,
        per_page = 100,
        !!!query_params
      ) |>
      req_timeout(60)

    resp <- tryCatch(req_perform(req), error = identity)
    if (inherits(resp, "error")) {
      warning("FEC request failed (", endpoint, ", page ", page, "): ", conditionMessage(resp))
      break
    }

    txt <- resp_body_string(resp)
    js <- tryCatch(fromJSON(txt, simplifyDataFrame = TRUE), error = identity)
    if (inherits(js, "error")) {
      warning("FEC JSON parse failed (", endpoint, ", page ", page, ").")
      break
    }

    if (!("results" %in% names(js)) || length(js$results) == 0) {
      break
    }

    out[[length(out) + 1L]] <- as_tibble(js$results)

    pages <- js$pagination$pages %||% page
    if (page >= pages) {
      break
    }
    if (page >= max_pages) {
      truncated <- TRUE
      break
    }

    page <- page + 1L
    Sys.sleep(pause_sec)
  }

  if (truncated) {
    warning(
      "Stopped early at max_pages=", max_pages, " for endpoint ", endpoint,
      ". Increase FEC_MAX_PAGES if needed."
    )
  }

  bind_rows(out)
}

clean_zip5 <- function(x) {
  x <- as.character(x)
  x <- str_extract(x, "\\d+")
  x <- if_else(!is.na(x), str_pad(str_sub(x, 1, 5), width = 5, side = "left", pad = "0"), NA_character_)
  x
}

crosswalk_path <- "./data/public/raw/tab20_zcta520_county20_natl.txt"
if (!file.exists(crosswalk_path)) {
  message("Downloading Census ZIP-to-county relationship file...")
  req <- request("https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt") |>
    req_timeout(120)
  resp <- req_perform(req)
  writeBin(resp_body_raw(resp), crosswalk_path)
}

zip_county_xwalk <- read_delim(
  crosswalk_path,
  delim = "|",
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) %>%
  transmute(
    zip5 = clean_zip5(GEOID_ZCTA5_20),
    county_fips = str_pad(as.character(GEOID_COUNTY_20), width = 5, side = "left", pad = "0"),
    arealand_part = as.numeric(AREALAND_PART),
    areawater_part = as.numeric(AREAWATER_PART)
  ) %>%
  filter(!is.na(zip5), !is.na(county_fips)) %>%
  mutate(area_part = coalesce(arealand_part, 0) + coalesce(areawater_part, 0)) %>%
  arrange(zip5, desc(area_part)) %>%
  group_by(zip5) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(zip5, county_fips)

write_csv(zip_county_xwalk, "./data/public/derived/zip_to_county_primary_2020.csv")

committees <- read_csv(committee_file, show_col_types = FALSE) %>%
  transmute(
    committee_id = as.character(committee_id),
    committee_label = as.character(committee_label),
    committee_party = as.character(committee_party)
  ) %>%
  filter(!is.na(committee_id), committee_id != "")

if (nrow(committees) == 0) {
  stop("Committee file has no valid committee IDs.")
}

cycles <- c(2018L, 2020L, 2022L, 2024L)

pull_donations_for_committee <- function(cid, cyc) {
  dat <- fetch_fec_paginated(
    endpoint = "schedules/schedule_a/by_zip/",
    query_params = list(
      committee_id = cid,
      two_year_transaction_period = cyc
    ),
    max_pages = max_pages
  )

  if (nrow(dat) == 0) {
    return(tibble())
  }

  dat %>%
    transmute(
      committee_id = as.character(committee_id),
      cycle = as.integer(cycle %||% cyc),
      zip5 = clean_zip5(zip),
      state = as.character(state),
      donation_total = as.numeric(total),
      donation_count = as.numeric(count)
    ) %>%
    filter(!is.na(zip5))
}

pull_spending_for_committee <- function(cid, cyc) {
  dat <- fetch_fec_paginated(
    endpoint = "schedules/schedule_b/by_recipient_zip/",
    query_params = list(
      committee_id = cid,
      two_year_transaction_period = cyc
    ),
    max_pages = max_pages
  )

  if (nrow(dat) == 0) {
    return(tibble())
  }

  dat %>%
    transmute(
      committee_id = as.character(committee_id),
      cycle = as.integer(cycle %||% cyc),
      zip5 = clean_zip5(zip),
      state = as.character(state),
      spending_total = as.numeric(total),
      spending_count = as.numeric(count)
    ) %>%
    filter(!is.na(zip5))
}

if (file.exists(donations_zip_cache) && file.exists(spending_zip_cache) && !force_refresh) {
  message("Using cached FEC ZIP files. Set FEC_FORCE_REFRESH=true to re-pull.")
  donations_zip <- read_csv(donations_zip_cache, show_col_types = FALSE)
  spending_zip <- read_csv(spending_zip_cache, show_col_types = FALSE)
} else {
  if (fec_api_key == "") {
    stop(
      "FEC_API_KEY is not set and no cache files were found.\n",
      "Set FEC_API_KEY or place cached files at:\n",
      donations_zip_cache, "\n", spending_zip_cache
    )
  }

  message("Pulling committee-level donations by ZIP from FEC...")
  donations_zip <- map_dfr(cycles, function(cyc) {
    map_dfr(committees$committee_id, function(cid) {
      pull_donations_for_committee(cid, cyc)
    })
  })

  message("Pulling committee-level disbursements by recipient ZIP from FEC...")
  spending_zip <- map_dfr(cycles, function(cyc) {
    map_dfr(committees$committee_id, function(cid) {
      pull_spending_for_committee(cid, cyc)
    })
  })

  write_csv(donations_zip, donations_zip_cache)
  write_csv(spending_zip, spending_zip_cache)
}

if (nrow(donations_zip) == 0) {
  stop("No donation data returned from FEC. Check API key, committee IDs, and rate limits.")
}

donations_county <- donations_zip %>%
  left_join(zip_county_xwalk, by = "zip5") %>%
  left_join(committees, by = "committee_id") %>%
  filter(!is.na(county_fips)) %>%
  group_by(cycle, county_fips, committee_party) %>%
  summarise(
    donations_total = sum(donation_total, na.rm = TRUE),
    donations_count = sum(donation_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    committee_party = if_else(is.na(committee_party), "OTHER", committee_party)
  ) %>%
  group_by(cycle, county_fips) %>%
  summarise(
    donations_total = sum(donations_total, na.rm = TRUE),
    donations_count = sum(donations_count, na.rm = TRUE),
    donations_dem = sum(if_else(committee_party == "DEM", donations_total, 0), na.rm = TRUE),
    donations_rep = sum(if_else(committee_party == "REP", donations_total, 0), na.rm = TRUE),
    donations_other = sum(if_else(!(committee_party %in% c("DEM", "REP")), donations_total, 0), na.rm = TRUE),
    .groups = "drop"
  )

spending_county <- if (nrow(spending_zip) > 0) {
  spending_zip %>%
    left_join(zip_county_xwalk, by = "zip5") %>%
    left_join(committees, by = "committee_id") %>%
    filter(!is.na(county_fips)) %>%
    group_by(cycle, county_fips, committee_party) %>%
    summarise(
      spending_total = sum(spending_total, na.rm = TRUE),
      spending_count = sum(spending_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      committee_party = if_else(is.na(committee_party), "OTHER", committee_party)
    ) %>%
    group_by(cycle, county_fips) %>%
    summarise(
      spending_total = sum(spending_total, na.rm = TRUE),
      spending_count = sum(spending_count, na.rm = TRUE),
      spending_dem = sum(if_else(committee_party == "DEM", spending_total, 0), na.rm = TRUE),
      spending_rep = sum(if_else(committee_party == "REP", spending_total, 0), na.rm = TRUE),
      spending_other = sum(if_else(!(committee_party %in% c("DEM", "REP")), spending_total, 0), na.rm = TRUE),
      .groups = "drop"
    )
} else {
  tibble::tibble(
    cycle = integer(),
    county_fips = character(),
    spending_total = double(),
    spending_count = double(),
    spending_dem = double(),
    spending_rep = double(),
    spending_other = double()
  )
}

write_csv(donations_county, "./data/public/derived/county_campaign_donations_focus_committees.csv")
write_csv(spending_county, "./data/public/derived/county_campaign_spending_focus_committees.csv")

finance_county <- donations_county %>%
  full_join(spending_county, by = c("cycle", "county_fips")) %>%
  mutate(
    donations_total = coalesce(donations_total, 0),
    donations_count = coalesce(donations_count, 0),
    donations_dem = coalesce(donations_dem, 0),
    donations_rep = coalesce(donations_rep, 0),
    donations_other = coalesce(donations_other, 0),
    spending_total = coalesce(spending_total, 0),
    spending_count = coalesce(spending_count, 0),
    spending_dem = coalesce(spending_dem, 0),
    spending_rep = coalesce(spending_rep, 0),
    spending_other = coalesce(spending_other, 0),
    net_finance_flow = donations_total - spending_total,
    rep_dem_donation_ratio = if_else(donations_dem > 0, donations_rep / donations_dem, NA_real_),
    donations_avg_size = if_else(donations_count > 0, donations_total / donations_count, NA_real_),
    spending_avg_size = if_else(spending_count > 0, spending_total / spending_count, NA_real_),
    dem_donation_share = if_else(donations_total > 0, donations_dem / donations_total, NA_real_),
    rep_donation_share = if_else(donations_total > 0, donations_rep / donations_total, NA_real_)
  )

write_csv(finance_county, "./data/public/derived/county_campaign_finance_focus_committees.csv")

panel <- read_csv(
  "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv",
  show_col_types = FALSE
) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    cycle = case_when(
      year %in% c(2017L, 2018L) ~ 2018L,
      year %in% c(2019L, 2020L) ~ 2020L,
      year %in% c(2021L, 2022L) ~ 2022L,
      year %in% c(2023L) ~ 2024L,
      TRUE ~ NA_integer_
    )
  )

panel_finance <- panel %>%
  left_join(finance_county, by = c("county_fips", "cycle")) %>%
  mutate(
    donations_total = coalesce(donations_total, 0),
    spending_total = coalesce(spending_total, 0),
    donations_per_worker = if_else(county_emp_total > 0, donations_total / county_emp_total, NA_real_),
    spending_per_worker = if_else(county_emp_total > 0, spending_total / county_emp_total, NA_real_),
    net_finance_per_worker = if_else(county_emp_total > 0, net_finance_flow / county_emp_total, NA_real_),
    log_donations = log10(donations_total + 1),
    log_spending = log10(spending_total + 1),
    log_net_finance = sign(net_finance_flow) * log10(abs(net_finance_flow) + 1),
    log_exposure = log10(county_tariff_exposure_mean + 1)
  )

write_csv(panel_finance, "./data/public/derived/county_tariff_panel_with_campaign_finance.csv")

latest_year <- max(panel_finance$year, na.rm = TRUE)
latest <- panel_finance %>% filter(year == latest_year)

fit_finance <- lm(
  log_exposure ~ gop_share_20 + swingness_20 + log_donations + log_spending + rep_dem_donation_ratio,
  data = latest %>% filter(is.finite(log_exposure), !is.na(gop_share_20), !is.na(swingness_20))
)

fit_tbl <- as.data.frame(summary(fit_finance)$coefficients)
fit_tbl$term <- rownames(fit_tbl)
fit_tbl <- fit_tbl %>%
  select(term, everything()) %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    t_value = `t value`,
    p_value = `Pr(>|t|)`
  )
write_csv(fit_tbl, "./output/finance_extension/tables/regression_exposure_with_campaign_finance.csv")

top_donation_counties <- latest %>%
  arrange(desc(donations_total)) %>%
  select(
    county_fips, county_name, state_name, donations_total, spending_total,
    net_finance_flow, county_tariff_exposure_mean, donations_per_worker, spending_per_worker
  ) %>%
  slice_head(n = 50)
write_csv(top_donation_counties, "./output/finance_extension/tables/top50_counties_campaign_donations.csv")

finance_corr <- latest %>%
  transmute(
    log_exposure = log10(county_tariff_exposure_mean + 1),
    log_donations = log10(donations_total + 1),
    log_spending = log10(spending_total + 1),
    log_net = sign(net_finance_flow) * log10(abs(net_finance_flow) + 1),
    gop_share_20 = gop_share_20,
    swingness_20 = swingness_20
  )

corr_tbl <- tibble::tibble(
  pair = c(
    "log_exposure_vs_log_donations",
    "log_exposure_vs_log_spending",
    "log_exposure_vs_log_net_finance",
    "log_exposure_vs_gop_share_20",
    "log_exposure_vs_swingness_20"
  ),
  correlation = c(
    suppressWarnings(cor(finance_corr$log_exposure, finance_corr$log_donations, use = "pairwise.complete.obs")),
    suppressWarnings(cor(finance_corr$log_exposure, finance_corr$log_spending, use = "pairwise.complete.obs")),
    suppressWarnings(cor(finance_corr$log_exposure, finance_corr$log_net, use = "pairwise.complete.obs")),
    suppressWarnings(cor(finance_corr$log_exposure, finance_corr$gop_share_20, use = "pairwise.complete.obs")),
    suppressWarnings(cor(finance_corr$log_exposure, finance_corr$swingness_20, use = "pairwise.complete.obs"))
  )
)

write_csv(corr_tbl, "./output/finance_extension/tables/correlations_exposure_finance_latest.csv")

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
)

p_scatter <- latest %>%
  filter(donations_total > 0, county_tariff_exposure_mean > 0) %>%
  ggplot(aes(x = log10(donations_total + 1), y = log10(county_tariff_exposure_mean + 1))) +
  geom_point(alpha = 0.45, color = "#1b7837") +
  geom_smooth(method = "lm", se = TRUE, color = "#762a83", linewidth = 0.9) +
  labs(
    title = "Tariff exposure vs campaign donations (county-level, latest year)",
    x = "log10(campaign donations + 1)",
    y = "log10(tariff exposure + 1)"
  )
ggsave("./output/finance_extension/figures/scatter_exposure_vs_donations_latest.png", p_scatter, width = 8.5, height = 5.5, dpi = 320)

options(tigris_use_cache = TRUE)
counties_sf <- tigris::counties(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  select(GEOID, STATEFP, geometry)
states_sf <- tigris::states(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  select(STATEFP, geometry)
exclude_statefp <- c("02", "15", "60", "66", "69", "72", "78")

map_df <- counties_sf %>%
  filter(!STATEFP %in% exclude_statefp) %>%
  left_join(latest, by = c("GEOID" = "county_fips"))
states_map <- states_sf %>% filter(!STATEFP %in% exclude_statefp)

p_map_don <- ggplot(map_df) +
  geom_sf(aes(fill = donations_total + 1), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_viridis_c(option = "magma", trans = "log10", na.value = "grey92", labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    title = "Campaign donations by county (focus committees)",
    subtitle = "Mapped from FEC ZIP aggregates via Census ZIP-county crosswalk",
    fill = "Donations\n(log scale)"
  ) +
  theme_void()
ggsave("./output/finance_extension/figures/map_county_campaign_donations_latest.png", p_map_don, width = 13, height = 8, dpi = 320)

p_map_net <- ggplot(map_df) +
  geom_sf(aes(fill = net_finance_flow), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    na.value = "grey92",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = "Net campaign flow by county (donations - spending)",
    subtitle = "Focus committees; county mapped from ZIP via Census crosswalk",
    fill = "Net flow"
  ) +
  theme_void()
ggsave("./output/finance_extension/figures/map_county_campaign_net_flow_latest.png", p_map_net, width = 13, height = 8, dpi = 320)

p_scatter_net <- latest %>%
  filter(county_tariff_exposure_mean > 0, !is.na(net_finance_flow)) %>%
  ggplot(aes(x = sign(net_finance_flow) * log10(abs(net_finance_flow) + 1), y = log10(county_tariff_exposure_mean + 1))) +
  geom_point(alpha = 0.45, color = "#2c7fb8") +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f0e", linewidth = 0.9) +
  labs(
    title = "Tariff exposure vs net campaign flow",
    subtitle = "X-axis is signed log10(|donations-spending| + 1)",
    x = "Signed log10(net campaign flow + 1)",
    y = "log10(tariff exposure + 1)"
  )
ggsave("./output/finance_extension/figures/scatter_exposure_vs_net_finance_latest.png", p_scatter_net, width = 8.5, height = 5.5, dpi = 320)

qa <- tibble::tibble(
  metric = c(
    "committees_count",
    "donations_zip_rows",
    "spending_zip_rows",
    "donations_county_rows",
    "spending_county_rows",
    "panel_rows",
    "latest_year",
    "latest_counties",
    "latest_with_nonzero_donations",
    "latest_with_nonzero_spending",
    "latest_with_nonzero_net_flow",
    "used_cached_fec_zip_files"
  ),
  value = c(
    nrow(committees),
    nrow(donations_zip),
    nrow(spending_zip),
    nrow(donations_county),
    nrow(spending_county),
    nrow(panel_finance),
    latest_year,
    nrow(latest),
    sum(latest$donations_total > 0, na.rm = TRUE),
    sum(latest$spending_total > 0, na.rm = TRUE),
    sum(latest$net_finance_flow != 0, na.rm = TRUE),
    as.integer(file.exists(donations_zip_cache) && file.exists(spending_zip_cache))
  )
)

write_csv(qa, "./output/finance_extension/qa/finance_extension_qa.csv")

message("Campaign-finance county extension complete.")
message("Panel with finance: ./data/public/derived/county_tariff_panel_with_campaign_finance.csv")
message("Outputs: ./output/finance_extension/")
