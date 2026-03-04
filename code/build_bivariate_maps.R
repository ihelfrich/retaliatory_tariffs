suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(sf)
  library(tigris)
  library(ggplot2)
  library(scales)
  library(biscale)
})

options(tigris_use_cache = TRUE)

panel_path <- "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv"
if (!file.exists(panel_path)) {
  stop("Missing panel file. Run code/overhaul_open_data_panel.R first.")
}

out_fig_dir <- "./output/advanced_maps/figures"
out_tbl_dir <- "./output/advanced_maps/tables"
dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_tbl_dir, recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), 5, pad = "0"),
    year = as.integer(year),
    county_tariff_exposure_mean = as.numeric(county_tariff_exposure_mean),
    gop_share_20 = as.numeric(gop_share_20),
    gop_share_24 = as.numeric(gop_share_24)
  )

required_cols <- c("county_fips", "year", "county_tariff_exposure_mean", "gop_share_20")
missing_cols <- required_cols[!required_cols %in% names(panel)]
if (length(missing_cols) > 0) {
  stop("Panel missing columns: ", paste(missing_cols, collapse = ", "))
}

latest_year <- max(panel$year, na.rm = TRUE)

latest <- panel %>%
  filter(year == latest_year) %>%
  mutate(
    gop_share_latest = dplyr::coalesce(gop_share_24, gop_share_20),
    log_exposure = log10(pmax(county_tariff_exposure_mean, 0) + 1)
  ) %>%
  select(county_fips, log_exposure, gop_share_latest)

counties_sf <- tigris::counties(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  mutate(county_fips = str_pad(as.character(GEOID), 5, pad = "0")) %>%
  filter(!substr(county_fips, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78"))

states_sf <- tigris::states(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
  st_transform(5070) %>%
  mutate(state_fips = str_pad(as.character(GEOID), 2, pad = "0")) %>%
  filter(!state_fips %in% c("02", "15", "60", "66", "69", "72", "78"))

map_sf <- counties_sf %>%
  left_join(latest, by = "county_fips")

coverage <- map_sf %>%
  st_drop_geometry() %>%
  summarise(
    latest_year = latest_year,
    counties_total = n(),
    counties_with_data = sum(!is.na(log_exposure) & !is.na(gop_share_latest)),
    share_with_data = counties_with_data / counties_total
  )

write_csv(coverage, file.path(out_tbl_dir, "bivariate_map_coverage.csv"))

map_sf <- map_sf %>%
  filter(!is.na(log_exposure), !is.na(gop_share_latest))

map_sf <- biscale::bi_class(
  .data = map_sf,
  x = log_exposure,
  y = gop_share_latest,
  style = "quantile",
  dim = 3
)

map_plot <- ggplot() +
  geom_sf(data = map_sf, aes(fill = bi_class), color = NA, linewidth = 0) +
  biscale::bi_scale_fill(pal = "DkBlue", dim = 3) +
  geom_sf(data = states_sf, fill = NA, color = "white", linewidth = 0.18) +
  labs(
    title = paste0("Bivariate County Map (", latest_year, ")"),
    subtitle = "Tariff exposure (log scale) vs GOP vote share",
    caption = "Data: open county exposure panel with county election merge"
  ) +
  biscale::bi_theme() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8)
  )

legend_plot <- biscale::bi_legend(
  pal = "DkBlue",
  dim = 3,
  xlab = "Higher exposure",
  ylab = "Higher GOP share",
  size = 9
)

ggsave(
  filename = file.path(out_fig_dir, "map_bivariate_exposure_vs_gop_latest.png"),
  plot = map_plot,
  width = 13,
  height = 8,
  dpi = 320
)

ggsave(
  filename = file.path(out_fig_dir, "map_bivariate_exposure_vs_gop_latest_legend.png"),
  plot = legend_plot,
  width = 3.8,
  height = 3.4,
  dpi = 320
)

state_class_share <- map_sf %>%
  st_drop_geometry() %>%
  mutate(state_fips = substr(county_fips, 1, 2)) %>%
  count(state_fips, bi_class, name = "n") %>%
  group_by(state_fips) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  arrange(state_fips, bi_class)

write_csv(state_class_share, file.path(out_tbl_dir, "bivariate_class_share_by_state.csv"))

message("Bivariate map build complete.")
message("Figures: ", out_fig_dir)
message("Tables: ", out_tbl_dir)
