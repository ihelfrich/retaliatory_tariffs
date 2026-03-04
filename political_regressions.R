library(readr)
library(dplyr)

panel_path <- "data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv"
if (!file.exists(panel_path)) {
  stop("Missing panel file: ", panel_path, "\nRun source('code/overhaul_open_data_panel.R') first.")
}

latest_year <- read_csv(panel_path, show_col_types = FALSE) %>%
  pull(year) %>%
  max(na.rm = TRUE)

df <- read_csv(panel_path, show_col_types = FALSE) %>%
  filter(year == latest_year) %>%
  mutate(
    exposure_billions = county_tariff_exposure_mean / 1e9,
    change_gop_20_16 = gop_shift_16_20,
    change_gop_24_20 = gop_shift_20_24
  ) %>%
  filter(
    !is.na(exposure_billions),
    !is.na(tariffed_emp_share),
    !is.na(gop_share_16),
    !is.na(gop_share_20),
    !is.na(change_gop_20_16),
    !is.na(change_gop_24_20)
  )

cat("\n=== Regression 1: Impact of 2016 Political Salience on Tariff Exposure ===\n")
mod1 <- lm(exposure_billions ~ gop_share_16 + tariffed_emp_share, data = df)
print(summary(mod1))

cat("\n=== Regression 2: Impact of Tariff Exposure on 2020 GOP Vote Shift ===\n")
mod2 <- lm(change_gop_20_16 ~ exposure_billions + tariffed_emp_share + gop_share_16, data = df)
print(summary(mod2))

cat("\n=== Regression 3: Persistent Impact on 2024 GOP Vote Shift ===\n")
mod3 <- lm(change_gop_24_20 ~ exposure_billions + tariffed_emp_share + gop_share_20, data = df)
print(summary(mod3))
