suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(fixest)
})

panel_path <- "./data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv"
if (!file.exists(panel_path)) {
  stop("Missing panel file. Run code/overhaul_open_data_panel.R first.")
}

out_tbl_dir <- "./output/fixest_models/tables"
out_diag_dir <- "./output/fixest_models/diagnostics"
out_report_dir <- "./output/fixest_models/reports"
dir.create(out_tbl_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_diag_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_report_dir, recursive = TRUE, showWarnings = FALSE)

panel <- read_csv(panel_path, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), 5, pad = "0"),
    year = as.integer(year),
    county_tariff_exposure_mean = as.numeric(county_tariff_exposure_mean),
    county_tariff_exposure_per_worker = as.numeric(county_tariff_exposure_per_worker),
    tariffed_emp_share = as.numeric(tariffed_emp_share),
    gop_share_16 = as.numeric(gop_share_16),
    gop_share_20 = as.numeric(gop_share_20),
    gop_shift_16_20 = as.numeric(gop_shift_16_20),
    swingness_20 = as.numeric(swingness_20)
  ) %>%
  mutate(
    log_exposure = log10(pmax(county_tariff_exposure_mean, 0) + 1),
    log_exposure_per_worker = log10(pmax(county_tariff_exposure_per_worker, 0) + 1)
  )

latest_year <- max(panel$year, na.rm = TRUE)

latest <- panel %>%
  filter(
    year == latest_year,
    !is.na(log_exposure),
    !is.na(gop_share_20),
    !is.na(swingness_20),
    !is.na(tariffed_emp_share),
    !is.na(state_name)
  )

panel_model <- panel %>%
  filter(
    !is.na(log_exposure),
    !is.na(gop_share_20),
    !is.na(swingness_20),
    !is.na(tariffed_emp_share),
    !is.na(state_name)
  )

if (nrow(latest) < 300) {
  stop("Latest-year sample is too small for stable fixest models.")
}

m_latest_hc1 <- feols(
  log_exposure ~ gop_share_20 + swingness_20 + tariffed_emp_share,
  data = latest,
  vcov = "hetero"
)

m_latest_statefe_cluster <- feols(
  log_exposure ~ gop_share_20 + swingness_20 + tariffed_emp_share | state_name,
  data = latest,
  vcov = ~state_name
)

m_panel_statefe_cluster <- feols(
  log_exposure ~ gop_share_20 + swingness_20 + tariffed_emp_share + i(year) | state_name,
  data = panel_model,
  vcov = ~state_name
)

m_shift_hc1 <- feols(
  gop_shift_16_20 ~ log_exposure_per_worker + tariffed_emp_share + gop_share_16,
  data = latest,
  vcov = "hetero"
)

models <- list(
  latest_hc1 = m_latest_hc1,
  latest_statefe_cluster = m_latest_statefe_cluster,
  panel_statefe_cluster = m_panel_statefe_cluster,
  shift_hc1 = m_shift_hc1
)

tidy_fixest <- function(model, model_name) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)

  ci <- as.data.frame(confint(model))
  ci$term <- rownames(ci)

  out <- ct %>%
    as_tibble() %>%
    rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      p_value = `Pr(>|t|)`
    ) %>%
    left_join(
      ci %>%
        as_tibble() %>%
        rename(ci_low = `2.5 %`, ci_high = `97.5 %`),
      by = "term"
    ) %>%
    mutate(model = model_name) %>%
    select(model, term, estimate, std_error, statistic, p_value, ci_low, ci_high)

  out
}

coef_tbl <- bind_rows(
  tidy_fixest(m_latest_hc1, "latest_hc1"),
  tidy_fixest(m_latest_statefe_cluster, "latest_statefe_cluster"),
  tidy_fixest(m_panel_statefe_cluster, "panel_statefe_cluster"),
  tidy_fixest(m_shift_hc1, "shift_hc1")
)

meta_tbl <- tibble::tibble(
  model = names(models),
  n = vapply(models, nobs, numeric(1)),
  r2 = vapply(models, function(m) as.numeric(fitstat(m, "r2")[[1]]), numeric(1)),
  r2_within = vapply(models, function(m) as.numeric(fitstat(m, "wr2")[[1]]), numeric(1)),
  adj_r2 = vapply(models, function(m) as.numeric(fitstat(m, "ar2")[[1]]), numeric(1))
)

write_csv(coef_tbl, file.path(out_tbl_dir, "fixest_coefficients.csv"))
write_csv(meta_tbl, file.path(out_tbl_dir, "fixest_model_metadata.csv"))

if (requireNamespace("modelsummary", quietly = TRUE)) {
  modelsummary::modelsummary(
    models,
    stars = TRUE,
    fmt = 3,
    output = file.path(out_report_dir, "fixest_modelsummary.md")
  )
}

spatial_status <- tibble::tibble(
  check = "sfdep_status",
  status = "not_run",
  detail = "sfdep not installed"
)

if (requireNamespace("sfdep", quietly = TRUE) && requireNamespace("sf", quietly = TRUE) && requireNamespace("tigris", quietly = TRUE)) {
  spatial_result <- tryCatch(
    {
      counties_sf <- tigris::counties(cb = TRUE, year = 2023, class = "sf", progress_bar = FALSE) %>%
        sf::st_transform(5070) %>%
        mutate(county_fips = str_pad(as.character(GEOID), 5, pad = "0")) %>%
        filter(!substr(county_fips, 1, 2) %in% c("02", "15", "60", "66", "69", "72", "78"))

      map_sf <- counties_sf %>%
        left_join(
          latest %>% select(county_fips, log_exposure),
          by = "county_fips"
        ) %>%
        filter(!is.na(log_exposure))

      nb <- sfdep::st_contiguity(map_sf)
      wt <- sfdep::st_weights(nb)

      global <- sfdep::global_moran(map_sf$log_exposure, nb, wt)
      local <- sfdep::local_moran(map_sf$log_exposure, nb, wt)

      local_tbl <- tibble::as_tibble(local) %>%
        mutate(county_fips = map_sf$county_fips)

      write_csv(local_tbl, file.path(out_diag_dir, "sfdep_local_moran_latest.csv"))

      tibble::tibble(
        check = "sfdep_status",
        status = "pass",
        detail = paste0(
          "global_moran_i=", round(as.numeric(global$estimate[[1]]), 4),
          "; p_value=", signif(as.numeric(global$p.value[[1]]), 4)
        )
      )
    },
    error = function(e) {
      tibble::tibble(
        check = "sfdep_status",
        status = "warning",
        detail = paste0("sfdep diagnostics failed: ", e$message)
      )
    }
  )

  spatial_status <- spatial_result
}

write_csv(spatial_status, file.path(out_diag_dir, "spatial_diagnostics.csv"))

message("Fixest model build complete.")
message("Tables: ", out_tbl_dir)
message("Diagnostics: ", out_diag_dir)
