suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(sf)
  library(tigris)
  library(viridis)
})

if (!file.exists("./data/county_exposure/employment_by_county/derived/county_industry_panel_full.csv")) {
  stop("Run from project root (open retaliatory-tariff.Rproj).")
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

dir.create("./output/comprehensive_results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/comprehensive_results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/comprehensive_results/qa", recursive = TRUE, showWarnings = FALSE)
dir.create("./data/county_exposure/fixed/derived", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1) Load and build county-level tariff-weighted exposure
# -----------------------------
panel <- read_csv(
  "./data/county_exposure/employment_by_county/derived/county_industry_panel_full.csv",
  show_col_types = FALSE
) %>%
  mutate(
    county_fips = make_fips5(county_fips),
    naics2 = str_pad(str_replace(as.character(naics2), "\\.0$", ""), width = 2, side = "left", pad = "0"),
    naics2c = collapse_naics2(naics2),
    year = as.integer(year),
    emp = as.numeric(emp),
    emp_total = as.numeric(emp_total),
    emp_share = as.numeric(emp_share),
    export_value = as.numeric(export_value),
    export_exposure = as.numeric(export_exposure)
  ) %>%
  filter(year == 2023, !is.na(county_fips), !is.na(naics2c), !is.na(emp_share))

tariff_by_naics2 <- read_csv(
  "./data/county_exposure/fixed/derived/tariff_by_naics2_summary_fixed.csv",
  show_col_types = FALSE
) %>%
  mutate(naics2c = as.character(naics2c))

panel_tariff <- panel %>%
  left_join(tariff_by_naics2, by = "naics2c") %>%
  mutate(
    tariff_rate_mean = coalesce(tariff_rate_mean, 0),
    tariff_rate_p90 = coalesce(tariff_rate_p90, 0),
    n_tariff_lines = coalesce(n_tariff_lines, 0),
    sector_tariffed = n_tariff_lines > 0,
    exposure_component_mean = emp_share * export_value * tariff_rate_mean,
    exposure_component_p90 = emp_share * export_value * tariff_rate_p90
  )

dominant_sector <- panel_tariff %>%
  group_by(county_fips) %>%
  slice_max(order_by = exposure_component_mean, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    county_fips,
    dominant_naics2c = naics2c,
    dominant_component = exposure_component_mean
  )

county_exposure <- panel_tariff %>%
  group_by(county_fips, county_name, year) %>%
  summarise(
    county_exposure_mean = sum(exposure_component_mean, na.rm = TRUE),
    county_exposure_p90 = sum(exposure_component_p90, na.rm = TRUE),
    export_exposure_total = sum(export_exposure, na.rm = TRUE),
    tariffed_emp_share = sum(if_else(sector_tariffed, emp_share, 0), na.rm = TRUE),
    agriculture_emp_share = sum(if_else(naics2 == "11", emp_share, 0), na.rm = TRUE),
    mining_emp_share = sum(if_else(naics2 == "21", emp_share, 0), na.rm = TRUE),
    manufacturing_emp_share = sum(if_else(naics2 %in% c("31", "32", "33"), emp_share, 0), na.rm = TRUE),
    county_emp_total = max(emp_total, na.rm = TRUE),
    sectors_with_tariff = sum(sector_tariffed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(dominant_sector, by = "county_fips") %>%
  mutate(
    county_exposure_per_worker = if_else(
      !is.na(county_emp_total) & county_emp_total > 0,
      county_exposure_mean / county_emp_total,
      NA_real_
    )
  )

# -----------------------------
# 2) Merge with county vote data (political salience)
# -----------------------------
votes20 <- read_csv("./data/votes/raw/county_vote_20.csv", show_col_types = FALSE) %>%
  transmute(
    county_fips = make_fips5(county_fips),
    gop_share_20 = as.numeric(per_gop),
    dem_share_20 = as.numeric(per_dem),
    margin_20 = as.numeric(per_point_diff),
    swingness_20 = 1 - abs(as.numeric(per_point_diff))
  )

votes24 <- read_csv("./data/votes/raw/county_vote_24.csv", show_col_types = FALSE) %>%
  transmute(
    county_fips = make_fips5(county_fips),
    state_name = as.character(state_name),
    county_name_vote = as.character(county_name),
    gop_share_24 = as.numeric(per_gop),
    dem_share_24 = as.numeric(per_dem),
    margin_24 = as.numeric(per_point_diff),
    swingness_24 = 1 - abs(as.numeric(per_point_diff))
  )

county_analysis <- county_exposure %>%
  left_join(votes20, by = "county_fips") %>%
  left_join(votes24, by = "county_fips") %>%
  mutate(
    exposure_rank = percent_rank(county_exposure_mean),
    exposure_per_worker_rank = percent_rank(county_exposure_per_worker),
    political_salience_index = exposure_rank * swingness_24,
    exposure_decile = ntile(county_exposure_mean, 10),
    gop_decile = ntile(gop_share_24, 10),
    bivariate_class = if_else(
      !is.na(exposure_decile) & !is.na(gop_decile),
      paste0(exposure_decile, "-", gop_decile),
      NA_character_
    ),
    log_exposure = log10(county_exposure_mean + 1),
    log_exposure_per_worker = log10(county_exposure_per_worker + 1)
  )

write_csv(
  county_analysis,
  "./data/county_exposure/fixed/derived/county_exposure_analysis_2023.csv"
)

# -----------------------------
# 3) Tables and diagnostics
# -----------------------------
top_counties <- county_analysis %>%
  filter(!is.na(county_exposure_mean)) %>%
  arrange(desc(county_exposure_mean)) %>%
  mutate(
    exposure_billions = county_exposure_mean / 1e9,
    exposure_per_worker = county_exposure_per_worker
  ) %>%
  select(
    county_fips, county_name, state_name, county_exposure_mean, exposure_billions,
    county_exposure_per_worker, dominant_naics2c, swingness_24, gop_share_24, margin_24
  ) %>%
  slice_head(n = 50)

top_salience <- county_analysis %>%
  filter(!is.na(political_salience_index)) %>%
  arrange(desc(political_salience_index)) %>%
  select(
    county_fips, county_name, state_name, political_salience_index,
    county_exposure_mean, swingness_24, gop_share_24, margin_24, dominant_naics2c
  ) %>%
  slice_head(n = 50)

write_csv(top_counties, "./output/comprehensive_results/tables/top50_counties_exposure.csv")
write_csv(top_salience, "./output/comprehensive_results/tables/top50_counties_political_salience.csv")

reg_data <- county_analysis %>%
  filter(
    is.finite(log_exposure),
    !is.na(gop_share_24),
    !is.na(swingness_24)
  )

fit <- lm(log_exposure ~ gop_share_24 + swingness_24 + gop_share_24:swingness_24, data = reg_data)
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
write_csv(fit_tbl, "./output/comprehensive_results/tables/regression_log_exposure_on_politics.csv")

diagnostics <- tibble::tibble(
  metric = c(
    "panel_rows_2023",
    "panel_unique_counties",
    "tariff_joined_sector_rows",
    "analysis_rows",
    "analysis_rows_with_votes24",
    "analysis_rows_with_nonzero_exposure",
    "median_exposure",
    "median_exposure_per_worker",
    "cor_log_exposure_gop_share_24",
    "cor_log_exposure_swingness_24",
    "reg_n"
  ),
  value = c(
    nrow(panel),
    n_distinct(panel$county_fips),
    nrow(panel_tariff),
    nrow(county_analysis),
    sum(!is.na(county_analysis$gop_share_24)),
    sum(county_analysis$county_exposure_mean > 0, na.rm = TRUE),
    median(county_analysis$county_exposure_mean, na.rm = TRUE),
    median(county_analysis$county_exposure_per_worker, na.rm = TRUE),
    suppressWarnings(cor(county_analysis$log_exposure, county_analysis$gop_share_24, use = "pairwise.complete.obs")),
    suppressWarnings(cor(county_analysis$log_exposure, county_analysis$swingness_24, use = "pairwise.complete.obs")),
    nrow(reg_data)
  )
)
write_csv(diagnostics, "./output/comprehensive_results/qa/diagnostics.csv")

# -----------------------------
# 4) Non-spatial visuals
# -----------------------------
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
)

p_dist <- county_analysis %>%
  filter(county_exposure_mean > 0) %>%
  ggplot(aes(x = log10(county_exposure_mean))) +
  geom_histogram(bins = 40, fill = "#2b8cbe", color = "white") +
  labs(
    title = "Distribution of county tariff exposure (log10 scale)",
    subtitle = "Exposure = sum_i(emp_share_i x export_value_i x tariff_rate_i)",
    x = "log10(county exposure)",
    y = "Counties"
  )

p_scatter_gop <- county_analysis %>%
  filter(!is.na(gop_share_24), county_exposure_mean > 0) %>%
  ggplot(aes(x = gop_share_24, y = log10(county_exposure_mean + 1))) +
  geom_point(alpha = 0.45, color = "#1b7837") +
  geom_smooth(method = "lm", se = TRUE, color = "#762a83", linewidth = 0.9) +
  labs(
    title = "Tariff exposure vs. GOP vote share (2024)",
    x = "GOP vote share (2024)",
    y = "log10(county exposure + 1)"
  )

p_scatter_swing <- county_analysis %>%
  filter(!is.na(swingness_24), county_exposure_mean > 0) %>%
  ggplot(aes(x = swingness_24, y = log10(county_exposure_mean + 1))) +
  geom_point(alpha = 0.45, color = "#2c7fb8") +
  geom_smooth(method = "lm", se = TRUE, color = "#d95f0e", linewidth = 0.9) +
  labs(
    title = "Tariff exposure vs. electoral competitiveness (2024)",
    subtitle = "Competitiveness = 1 - |GOP-Dem two-party margin|",
    x = "Electoral competitiveness (2024)",
    y = "log10(county exposure + 1)"
  )

p_top <- top_counties %>%
  slice_head(n = 20) %>%
  mutate(label = paste0(county_name, ", ", state_name)) %>%
  ggplot(aes(x = reorder(label, county_exposure_mean), y = county_exposure_mean, fill = dominant_naics2c)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    title = "Top 20 counties by tariff exposure",
    x = NULL,
    y = "County exposure (USD-like index)",
    fill = "Dominant\nsector"
  )

ggsave("./output/comprehensive_results/figures/dist_log_exposure.png", p_dist, width = 9, height = 5.5, dpi = 320)
ggsave("./output/comprehensive_results/figures/scatter_exposure_vs_gop24.png", p_scatter_gop, width = 8.5, height = 5.5, dpi = 320)
ggsave("./output/comprehensive_results/figures/scatter_exposure_vs_swingness24.png", p_scatter_swing, width = 8.5, height = 5.5, dpi = 320)
ggsave("./output/comprehensive_results/figures/top20_counties_exposure.png", p_top, width = 10.5, height = 7.5, dpi = 320)

# -----------------------------
# 5) Geospatial mapping
# -----------------------------
options(tigris_use_cache = TRUE)

counties_sf <- tigris::counties(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  select(GEOID, NAME, STATEFP, geometry)

states_sf <- tigris::states(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  select(STATEFP, STUSPS, geometry)

exclude_statefp <- c("02", "15", "60", "66", "69", "72", "78")
counties_map <- counties_sf %>%
  filter(!STATEFP %in% exclude_statefp) %>%
  left_join(county_analysis, by = c("GEOID" = "county_fips"))

states_map <- states_sf %>%
  filter(!STATEFP %in% exclude_statefp)

p_map_exposure <- ggplot(counties_map) +
  geom_sf(aes(fill = county_exposure_mean + 1), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log10",
    na.value = "grey92",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = "County tariff exposure map (2023)",
    subtitle = "Exposure = sum_i(emp_share_i x export_value_i x tariff_rate_i); contiguous U.S.",
    fill = "Exposure\n(log scale)"
  ) +
  theme_void()

p_map_per_worker <- ggplot(counties_map) +
  geom_sf(aes(fill = county_exposure_per_worker + 1), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log10",
    na.value = "grey92",
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  labs(
    title = "County tariff exposure per worker (2023)",
    subtitle = "Normalizes county exposure by county employment",
    fill = "Exposure/worker\n(log scale)"
  ) +
  theme_void()

p_map_salience <- ggplot(counties_map) +
  geom_sf(aes(fill = political_salience_index), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_viridis_c(
    option = "cividis",
    na.value = "grey92",
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    title = "Political-salience weighted tariff exposure map",
    subtitle = "Index = exposure percentile x electoral competitiveness (2024)",
    fill = "Salience\nindex"
  ) +
  theme_void()

ggsave("./output/comprehensive_results/figures/map_county_exposure_2023.png", p_map_exposure, width = 13, height = 8, dpi = 320)
ggsave("./output/comprehensive_results/figures/map_county_exposure_per_worker_2023.png", p_map_per_worker, width = 13, height = 8, dpi = 320)
ggsave("./output/comprehensive_results/figures/map_county_political_salience_exposure_2023.png", p_map_salience, width = 13, height = 8, dpi = 320)

# Bivariate-style map (exposure tercile x GOP share tercile).
county_bi <- counties_map %>%
  mutate(
    exposure_tercile = ntile(county_exposure_mean, 3),
    gop_tercile = ntile(gop_share_24, 3),
    bi_class = if_else(!is.na(exposure_tercile) & !is.na(gop_tercile), paste0(exposure_tercile, "-", gop_tercile), NA_character_)
  )

bi_palette <- c(
  "1-1" = "#e8e8e8", "1-2" = "#b5c0da", "1-3" = "#6c83b5",
  "2-1" = "#b8d6be", "2-2" = "#90b2b3", "2-3" = "#567994",
  "3-1" = "#73ae80", "3-2" = "#5a9178", "3-3" = "#2a5a5b"
)

p_map_bi <- ggplot(county_bi) +
  geom_sf(aes(fill = bi_class), color = NA) +
  geom_sf(data = states_map, fill = NA, color = "grey50", linewidth = 0.15) +
  scale_fill_manual(values = bi_palette, na.value = "grey92") +
  labs(
    title = "Bivariate county map: exposure x GOP vote share (2024)",
    subtitle = "Low-to-high terciles for exposure (rows) and GOP share (columns)",
    fill = "Exposure-GOP\ntercile"
  ) +
  theme_void()

ggsave("./output/comprehensive_results/figures/map_bivariate_exposure_gop_2024.png", p_map_bi, width = 13, height = 8, dpi = 320)

message("Comprehensive results pipeline complete.")
message("Outputs: ./output/comprehensive_results/")
