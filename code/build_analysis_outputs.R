suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(broom)
  library(lmtest)
  library(sandwich)
})

panel_path <- "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv"
validation_path <- "./output/overhaul/qa/validation_checks.csv"
finance_panel_path <- "./data/public/derived/county_tariff_panel_with_campaign_finance.csv"

if (!file.exists(panel_path)) {
  stop("Run code/overhaul_open_data_panel.R first.")
}

if (!file.exists(validation_path)) {
  stop("Run code/validate_core_outputs.R first.")
}

validation <- read_csv(validation_path, show_col_types = FALSE)
if (any(validation$status != "pass")) {
  stop("Validation checks are not all passing. Resolve before building analysis outputs.")
}

dir.create("./output/analysis/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/analysis/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/analysis/reports", recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year),
    state_name = as.character(state_name),
    county_tariff_exposure_mean = as.numeric(county_tariff_exposure_mean),
    county_tariff_exposure_per_worker = as.numeric(county_tariff_exposure_per_worker),
    tariffed_emp_share = as.numeric(tariffed_emp_share),
    gop_share_20 = as.numeric(gop_share_20),
    gop_share_24 = as.numeric(gop_share_24),
    swingness_20 = as.numeric(swingness_20),
    swingness_24 = as.numeric(swingness_24),
    log_exposure = log10(county_tariff_exposure_mean + 1),
    log_exposure_per_worker = log10(county_tariff_exposure_per_worker + 1)
  )

latest_year <- max(panel$year, na.rm = TRUE)
latest <- panel %>% filter(year == latest_year)

year_summary <- panel %>%
  group_by(year) %>%
  summarise(
    counties = n_distinct(county_fips),
    counties_with_vote20 = sum(!is.na(gop_share_20)),
    nonzero_exposure_share = mean(county_tariff_exposure_mean > 0, na.rm = TRUE),
    exposure_mean = mean(county_tariff_exposure_mean, na.rm = TRUE),
    exposure_median = median(county_tariff_exposure_mean, na.rm = TRUE),
    exposure_p90 = quantile(county_tariff_exposure_mean, 0.9, na.rm = TRUE),
    exposure_per_worker_median = median(county_tariff_exposure_per_worker, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(year_summary, "./output/analysis/tables/year_summary.csv")

latest_quantiles <- latest %>%
  mutate(exposure_decile = ntile(county_tariff_exposure_mean, 10)) %>%
  group_by(exposure_decile) %>%
  summarise(
    n_counties = n(),
    avg_exposure = mean(county_tariff_exposure_mean, na.rm = TRUE),
    avg_gop_20 = mean(gop_share_20, na.rm = TRUE),
    avg_swing_20 = mean(swingness_20, na.rm = TRUE),
    avg_tariffed_emp_share = mean(tariffed_emp_share, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(latest_quantiles, "./output/analysis/tables/latest_exposure_decile_summary.csv")

robust_table <- function(model, vcov_mat, spec_name, se_type) {
  est <- lmtest::coeftest(model, vcov. = vcov_mat)
  mat <- as.matrix(est)
  tibble::tibble(
    term = rownames(mat),
    estimate = as.numeric(mat[, 1]),
    std_error = as.numeric(mat[, 2]),
    statistic = as.numeric(mat[, 3]),
    p_value = as.numeric(mat[, 4])
  ) %>%
    mutate(
      specification = spec_name,
      se_type = se_type,
      .before = term
    )
}

spec_data <- latest %>%
  filter(is.finite(log_exposure), !is.na(gop_share_20), !is.na(swingness_20))

mod1 <- lm(log_exposure ~ gop_share_20 + swingness_20 + gop_share_20:swingness_20, data = spec_data)
mod1_vcov <- sandwich::vcovHC(mod1, type = "HC1")

spec_data_state <- spec_data %>% filter(!is.na(state_name), state_name != "")
mod2 <- lm(log_exposure ~ gop_share_20 + swingness_20 + gop_share_20:swingness_20 + factor(state_name), data = spec_data_state)
mod2_vcov <- sandwich::vcovCL(mod2, cluster = ~state_name, type = "HC1")

panel_data <- panel %>%
  filter(is.finite(log_exposure), !is.na(gop_share_20), !is.na(swingness_20), !is.na(state_name), state_name != "")
mod3 <- lm(log_exposure ~ gop_share_20 + swingness_20 + gop_share_20:swingness_20 + factor(year) + factor(state_name), data = panel_data)
mod3_vcov <- sandwich::vcovCL(mod3, cluster = ~state_name, type = "HC1")

reg_tbl <- bind_rows(
  robust_table(mod1, mod1_vcov, "Latest year baseline", "HC1"),
  robust_table(mod2, mod2_vcov, "Latest year + state FE", "State-clustered HC1"),
  robust_table(mod3, mod3_vcov, "Panel + year FE + state FE", "State-clustered HC1")
)
write_csv(reg_tbl, "./output/analysis/tables/regression_specs_robust.csv")

main_terms <- c("(Intercept)", "gop_share_20", "swingness_20", "gop_share_20:swingness_20")
reg_tbl_main <- reg_tbl %>%
  filter(term %in% main_terms)
write_csv(reg_tbl_main, "./output/analysis/tables/regression_specs_main_terms.csv")

model_meta <- tibble::tibble(
  specification = c("Latest year baseline", "Latest year + state FE", "Panel + year FE + state FE"),
  observations = c(nobs(mod1), nobs(mod2), nobs(mod3)),
  r_squared = c(summary(mod1)$r.squared, summary(mod2)$r.squared, summary(mod3)$r.squared)
)
write_csv(model_meta, "./output/analysis/tables/regression_model_meta.csv")

if (file.exists(finance_panel_path)) {
  finance <- read_csv(finance_panel_path, show_col_types = FALSE) %>%
    mutate(
      year = as.integer(year),
      state_name = as.character(state_name),
      gop_share_20 = as.numeric(gop_share_20),
      swingness_20 = as.numeric(swingness_20),
      county_tariff_exposure_mean = as.numeric(county_tariff_exposure_mean),
      log_exposure = log10(county_tariff_exposure_mean + 1),
      log_donations = log10(as.numeric(donations_total) + 1),
      log_spending = log10(as.numeric(spending_total) + 1)
    )

  fin_latest <- finance %>%
    filter(
      year == latest_year,
      is.finite(log_exposure),
      !is.na(gop_share_20),
      !is.na(swingness_20),
      is.finite(log_donations),
      is.finite(log_spending)
    )

  if (nrow(fin_latest) > 200) {
    mod_fin <- lm(log_exposure ~ gop_share_20 + swingness_20 + log_donations + log_spending, data = fin_latest)
    mod_fin_vcov <- sandwich::vcovCL(mod_fin, cluster = ~state_name, type = "HC1")
    fin_tbl <- robust_table(mod_fin, mod_fin_vcov, "Latest year + campaign finance", "State-clustered HC1")
    write_csv(fin_tbl, "./output/analysis/tables/regression_finance_extension_robust.csv")
  }
}

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
)

p_trend <- year_summary %>%
  ggplot(aes(x = year, y = exposure_median)) +
  geom_line(linewidth = 1, color = "#1f78b4") +
  geom_point(size = 2, color = "#1f78b4") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    title = "Median county tariff exposure over time",
    subtitle = "Core county exposure measure, 2017-2023",
    x = "Year",
    y = "Median exposure"
  )
ggsave("./output/analysis/figures/trend_median_exposure.png", p_trend, width = 8.5, height = 5.2, dpi = 320)

p_decile <- latest_quantiles %>%
  ggplot(aes(x = factor(exposure_decile), y = avg_gop_20)) +
  geom_col(fill = "#2c7fb8") +
  labs(
    title = paste0("2020 GOP share by ", latest_year, " exposure decile"),
    x = "Exposure decile (lowest to highest)",
    y = "Average GOP share (2020)"
  )
ggsave("./output/analysis/figures/gop_share_by_exposure_decile.png", p_decile, width = 8.5, height = 5.2, dpi = 320)

top_latest <- latest %>%
  arrange(desc(county_tariff_exposure_mean)) %>%
  select(
    county_fips, county_name, state_name, county_tariff_exposure_mean,
    county_tariff_exposure_per_worker, tariffed_emp_share, gop_share_20, swingness_20
  ) %>%
  slice_head(n = 25)
write_csv(top_latest, "./output/analysis/tables/top25_latest_exposure.csv")

brief_lines <- c(
  "# County Tariff Exposure: Analysis Brief",
  "",
  paste0("Generated: ", Sys.Date()),
  "",
  "## Core research question",
  "Did retaliatory tariffs concentrate in politically salient counties?",
  "",
  "## Build status",
  "- Validation checks: PASS",
  paste0("- Years covered: ", paste(sort(unique(panel$year)), collapse = ", ")),
  paste0("- Latest year: ", latest_year),
  paste0("- Counties in latest year: ", n_distinct(latest$county_fips)),
  paste0("- Latest-year nonzero exposure share: ", percent(mean(latest$county_tariff_exposure_mean > 0, na.rm = TRUE), accuracy = 0.1)),
  "",
  "## Exposure concentration",
  paste0("- Median latest-year exposure: ", label_number(scale_cut = cut_short_scale())(median(latest$county_tariff_exposure_mean, na.rm = TRUE))),
  paste0("- 90th percentile latest-year exposure: ", label_number(scale_cut = cut_short_scale())(quantile(latest$county_tariff_exposure_mean, 0.9, na.rm = TRUE))),
  "",
  "## Political relationship (latest-year baseline model)",
  paste0("- Observations: ", nobs(mod1)),
  paste0("- R-squared: ", round(summary(mod1)$r.squared, 4)),
  "- Robust standard errors: HC1 (state-clustered in FE specifications)",
  "",
  "See output/analysis/tables/regression_specs_main_terms.csv for main coefficients."
)

writeLines(brief_lines, "./output/analysis/reports/analysis_brief.md")

data_dictionary <- tibble::tribble(
  ~variable, ~definition,
  "county_fips", "5-digit county FIPS code",
  "year", "Calendar year",
  "county_tariff_exposure_mean", "County tariff exposure index using mean sector tariff rates",
  "county_tariff_exposure_p90", "County tariff exposure index using p90 sector tariff rates",
  "county_tariff_exposure_per_worker", "Mean exposure index divided by county total employment",
  "tariffed_emp_share", "Share of county employment in sectors with mapped tariff lines",
  "gop_share_20", "County GOP two-party vote share in 2020",
  "gop_share_24", "County GOP two-party vote share in 2024",
  "swingness_20", "County electoral competitiveness in 2020 (1 - absolute margin)",
  "political_salience_weighted_exposure", "Exposure percentile multiplied by competitiveness"
)
write_csv(data_dictionary, "./output/analysis/tables/data_dictionary_core_panel.csv")

message("Analysis outputs complete.")
message("Tables: ./output/analysis/tables/")
message("Figures: ./output/analysis/figures/")
message("Brief: ./output/analysis/reports/analysis_brief.md")
