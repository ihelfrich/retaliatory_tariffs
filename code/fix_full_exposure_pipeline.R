suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(concordance)
})

if (!file.exists("./data/tools/export_with_fips.csv")) {
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

clean_county_fips <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace(x, "\\.0$", "")
  x <- str_extract(x, "\\d{5}")
  x <- if_else(!is.na(x), str_pad(x, width = 5, side = "left", pad = "0"), NA_character_)
  x
}

map_hs_to_naics2017 <- function(hs_digits_vec, hs6_vec) {
  map_codes <- function(codes) {
    out <- rep(NA_character_, length(codes))
    valid <- !is.na(codes) & codes != ""
    if (any(valid)) {
      out[valid] <- suppressWarnings(
        concord_hs_naics(
          sourcevar = codes[valid],
          origin = "HS6",
          destination = "NAICS2017",
          dest.digit = 6,
          all = FALSE
        )
      )
    }
    out
  }

  hs4_vec <- str_sub(hs6_vec, 1, 4)
  hs2_vec <- str_sub(hs6_vec, 1, 2)

  naics6_6d <- map_codes(hs6_vec)
  naics6_4d <- map_codes(hs4_vec)
  naics6_2d <- map_codes(hs2_vec)

  naics6 <- naics6_6d
  mapping_level <- if_else(!is.na(naics6_6d), "hs6", NA_character_)

  fill_4d <- is.na(naics6) & !is.na(naics6_4d)
  naics6[fill_4d] <- naics6_4d[fill_4d]
  mapping_level[fill_4d] <- "hs4"

  fill_2d <- is.na(naics6) & !is.na(naics6_2d)
  naics6[fill_2d] <- naics6_2d[fill_2d]
  mapping_level[fill_2d] <- "hs2"

  # Rescue pass for malformed leading-zero tariff lines.
  hs_trim <- str_replace(hs_digits_vec, "^0+", "")
  hs_trim[hs_trim == ""] <- NA_character_

  hs_trim6 <- if_else(!is.na(hs_trim) & str_length(hs_trim) >= 6, str_sub(hs_trim, 1, 6), NA_character_)
  hs_trim4 <- if_else(
    !is.na(hs_trim) & str_length(hs_trim) >= 4,
    str_sub(hs_trim, 1, 4),
    if_else(!is.na(hs_trim) & str_length(hs_trim) == 3, str_pad(hs_trim, width = 4, side = "left", pad = "0"), NA_character_)
  )
  hs_trim2 <- if_else(!is.na(hs_trim) & str_length(hs_trim) >= 2, str_sub(hs_trim, 1, 2), NA_character_)

  naics6_trim6 <- map_codes(hs_trim6)
  naics6_trim4 <- map_codes(hs_trim4)
  naics6_trim2 <- map_codes(hs_trim2)

  fill_trim6 <- is.na(naics6) & !is.na(naics6_trim6)
  naics6[fill_trim6] <- naics6_trim6[fill_trim6]
  mapping_level[fill_trim6] <- "trim_hs6"

  fill_trim4 <- is.na(naics6) & !is.na(naics6_trim4)
  naics6[fill_trim4] <- naics6_trim4[fill_trim4]
  mapping_level[fill_trim4] <- "trim_hs4"

  fill_trim2 <- is.na(naics6) & !is.na(naics6_trim2)
  naics6[fill_trim2] <- naics6_trim2[fill_trim2]
  mapping_level[fill_trim2] <- "trim_hs2"

  tibble(naics6 = naics6, mapping_level = mapping_level)
}

dir.create("./data/county_exposure/fixed/derived", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/fixed_pipeline/qa", recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1) County export exposure by industry
# -----------------------------
exports_raw <- read_csv(
  "./data/tools/export_with_fips.csv",
  show_col_types = FALSE,
  col_types = cols(
    Commodity = col_character(),
    District = col_character(),
    Time = col_character(),
    total_export_value_USD = col_double(),
    fips_code = col_character()
  )
)

exports_county <- exports_raw %>%
  mutate(
    commodity_code = str_extract(Commodity, "^\\d{2,6}"),
    naics2_raw = str_sub(commodity_code, 1, 2),
    naics2c = collapse_naics2(naics2_raw),
    fips_code = str_replace_all(fips_code, ";", ",")
  ) %>%
  separate_rows(fips_code, sep = ",") %>%
  mutate(county_fips = clean_county_fips(fips_code)) %>%
  group_by(Commodity, District, Time, total_export_value_USD, commodity_code, naics2c) %>%
  mutate(
    n_counties = n_distinct(county_fips[!is.na(county_fips)]),
    export_value_weighted = if_else(n_counties > 0, total_export_value_USD / n_counties, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(county_fips), !is.na(naics2c)) %>%
  group_by(county_fips, Time, naics2c) %>%
  summarise(
    county_export_usd = sum(export_value_weighted, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(exports_county, "./data/county_exposure/fixed/derived/county_exports_by_naics2.csv")

# -----------------------------
# 2) County employment shares by industry
# -----------------------------
emp_raw <- read_csv(
  "./data/county_exposure/employment_by_county/derived/employment_all_establishments.csv",
  show_col_types = FALSE,
  col_types = cols(
    GEO_ID = col_character(),
    NAICS2017 = col_character(),
    YEAR = col_double(),
    EMP = col_character(),
    ESTAB = col_character(),
    PAYANN = col_character(),
    .default = col_guess()
  )
)

emp_base <- emp_raw %>%
  mutate(
    county_fips = str_extract(GEO_ID, "\\d{5}$"),
    naics2c = NAICS2017,
    EMP_num = parse_number(EMP),
    year = as.integer(YEAR)
  ) %>%
  filter(year == 2023, !is.na(county_fips))

county_total_emp <- emp_base %>%
  filter(naics2c == "00") %>%
  select(county_fips, total_emp = EMP_num)

emp_share <- emp_base %>%
  filter(naics2c != "00") %>%
  select(county_fips, naics2c, EMP_num, ESTAB, PAYANN) %>%
  left_join(county_total_emp, by = "county_fips") %>%
  mutate(
    emp_share = if_else(!is.na(total_emp) & total_emp > 0, EMP_num / total_emp, NA_real_)
  )

write_csv(emp_share, "./data/county_exposure/fixed/derived/county_employment_share_by_naics2.csv")

# -----------------------------
# 3) Tariff HS -> NAICS mapping and NAICS tariff summary
# -----------------------------
tariff_raw <- read_csv(
  "./data/county_exposure/tariff_by_industry/derived/summarized_trade_retaliations.csv",
  show_col_types = FALSE,
  col_types = cols(
    `Foreign National Tariff Line` = col_character(),
    Country = col_character(),
    `Foreign Tariff Line Description` = col_character(),
    `Current Total Estimated Retaliatory Tariff` = col_character()
  )
)

tariff_naics <- tariff_raw %>%
  rename(
    hs8_raw = `Foreign National Tariff Line`,
    desc = `Foreign Tariff Line Description`,
    tariff = `Current Total Estimated Retaliatory Tariff`
  ) %>%
  mutate(
    hs_digits = str_replace_all(hs8_raw, "[^0-9]", ""),
    hs8 = str_pad(str_sub(hs_digits, 1, 8), width = 8, side = "left", pad = "0"),
    hs6 = str_sub(hs8, 1, 6)
  ) %>%
  bind_cols(map_hs_to_naics2017(.$hs_digits, .$hs6)) %>%
  mutate(
    naics2_raw = str_sub(naics6, 1, 2),
    naics2c = collapse_naics2(naics2_raw),
    tariff_rate = parse_number(tariff) / 100
  )

write_csv(tariff_naics, "./data/county_exposure/fixed/derived/tariff_lines_mapped_fixed.csv")

tariff_unmatched <- tariff_naics %>%
  filter(is.na(naics6)) %>%
  count(hs8_raw, hs8, hs6, desc, sort = TRUE, name = "n_rows")

write_csv(tariff_unmatched, "./output/fixed_pipeline/qa/unmatched_hs_codes_fixed.csv")

tariff_by_naics2 <- tariff_naics %>%
  filter(!is.na(naics2c), !is.na(tariff_rate)) %>%
  group_by(naics2c) %>%
  summarise(
    tariff_rate_mean = mean(tariff_rate, na.rm = TRUE),
    tariff_rate_median = median(tariff_rate, na.rm = TRUE),
    tariff_rate_p90 = quantile(tariff_rate, probs = 0.9, na.rm = TRUE),
    n_tariff_lines = n(),
    .groups = "drop"
  )

write_csv(tariff_by_naics2, "./data/county_exposure/fixed/derived/tariff_by_naics2_summary_fixed.csv")

# -----------------------------
# 4) County tariff exposure construction
# -----------------------------
exposure_components <- exports_county %>%
  left_join(
    emp_share %>% select(county_fips, naics2c, emp_share, EMP_num, total_emp),
    by = c("county_fips", "naics2c")
  ) %>%
  left_join(tariff_by_naics2, by = "naics2c") %>%
  mutate(
    matched_emp = !is.na(emp_share),
    matched_tariff = !is.na(tariff_rate_mean),
    matched_all = matched_emp & matched_tariff,
    exposure_component_mean = if_else(matched_all, county_export_usd * emp_share * tariff_rate_mean, NA_real_),
    exposure_component_p90 = if_else(matched_all, county_export_usd * emp_share * tariff_rate_p90, NA_real_)
  )

write_csv(exposure_components, "./data/county_exposure/fixed/derived/county_exposure_components.csv")

county_exposure <- exposure_components %>%
  group_by(county_fips, Time) %>%
  summarise(
    county_exposure_mean = sum(exposure_component_mean, na.rm = TRUE),
    county_exposure_p90 = sum(exposure_component_p90, na.rm = TRUE),
    total_export_usd = sum(county_export_usd, na.rm = TRUE),
    industry_rows = n(),
    matched_rows = sum(matched_all, na.rm = TRUE),
    matched_share = if_else(industry_rows > 0, matched_rows / industry_rows, NA_real_),
    .groups = "drop"
  )

write_csv(county_exposure, "./data/county_exposure/fixed/derived/county_exposure_index_fixed.csv")

# -----------------------------
# 5) QA diagnostics for teaching/debugging
# -----------------------------
qa <- tibble(
  metric = c(
    "exports_raw_rows",
    "exports_county_rows",
    "employment_rows_2023",
    "employment_unique_counties",
    "tariff_rows",
    "tariff_rows_mapped",
    "tariff_mapping_rate",
    "tariff_unmatched_rows",
    "exposure_component_rows",
    "exposure_components_with_emp",
    "exposure_components_with_tariff",
    "exposure_components_matched_all",
    "county_exposure_rows"
  ),
  value = c(
    nrow(exports_raw),
    nrow(exports_county),
    nrow(emp_base),
    n_distinct(emp_base$county_fips),
    nrow(tariff_naics),
    sum(!is.na(tariff_naics$naics6)),
    round(mean(!is.na(tariff_naics$naics6)), 6),
    nrow(tariff_unmatched),
    nrow(exposure_components),
    sum(exposure_components$matched_emp, na.rm = TRUE),
    sum(exposure_components$matched_tariff, na.rm = TRUE),
    sum(exposure_components$matched_all, na.rm = TRUE),
    nrow(county_exposure)
  )
)

write_csv(qa, "./output/fixed_pipeline/qa/pipeline_diagnostics.csv")

message("Fixed pipeline complete.")
message("Key output: data/county_exposure/fixed/derived/county_exposure_index_fixed.csv")
