library(readr)
library(dplyr)
library(tidyr)
library(stringr)

if (!file.exists("./data/tools/export_with_fips.csv")) {
  stop("Run this script from the project root (open retaliatory-tariff.Rproj first).")
}

# Turn district-level exports into county-level exports using the FIPS crosswalk.
df <- read_csv("./data/tools/export_with_fips.csv")

county_level <- df %>%
  # standardize separators and trim whitespace
  mutate(
    fips_code = str_replace_all(fips_code, ";", ","),
    fips_code = str_trim(fips_code)
  ) %>%
  # split multiple FIPS into separate rows
  separate_rows(fips_code, sep = ",") %>%
  mutate(fips_code = str_trim(fips_code))

county_level <- county_level %>%
  group_by(Commodity, District, Time) %>%
  mutate(
    n_counties = sum(!is.na(fips_code)),
    export_value_weighted = total_export_value_USD / n_counties
  ) %>%
  ungroup()

county_exports <- county_level %>%
  filter(!is.na(fips_code)) %>%   # drop rows with no FIPS
  group_by(fips_code, Commodity, Time) %>%
  summarise(
    county_export_value_USD = sum(export_value_weighted, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  county_level,
  "./data/county_exposure/industry_exports/derived/county_level_exports.csv",
  row.names = FALSE
)
emp_df <- read_csv("./data/county_exposure/employment_by_county/derived/employment_all_establishments.csv")

# merge emp_df and county_level

# save into county_exposure / employment_by_county / derived
emp_df_clean <- emp_df %>%
  mutate(
    GEO_ID = as.character(GEO_ID),
    county_fips = str_extract(GEO_ID, "\\d{5}")  # pulls the 5-digit county FIPS
  ) %>%
  select(-GEO_ID)

county_level_clean <- county_level %>%
  mutate(
    fips_code = as.character(fips_code),
    county_fips = fips_code
  ) %>%
  select(-fips_code)
merged_df <- county_level_clean %>%
  left_join(emp_df_clean, by = "county_fips")
merged_df <- merged_df %>%
  select(
    Commodity, District, Time, county_fips,
    total_export_value_USD, n_counties, export_value_weighted,
    NAICS2017, EMP, ESTAB, PAYANN
  )
district_level_exports_merged <- merged_df
write.csv(district_level_exports_merged, "./data/county_exposure/industry_exports.csv", row.names = FALSE)
