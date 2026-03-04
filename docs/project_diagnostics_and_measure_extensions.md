# Project Diagnostics and Extension Notes

## 1) Core exposure measure currently implemented

County tariff exposure is built as:

`county exposure = sum_over_sectors(county allocated exports * sector tariff severity)`

with:

- `sector tariff severity`: mean (or p90) retaliatory tariff rate from mapped HS lines.
- `county allocated exports`: county share of U.S. sector employment times U.S. sector exports.

This keeps allocated exports internally consistent with the U.S. export totals by sector-year.

## 2) Current pipeline health check (open-data overhaul)

From `output/overhaul/qa/pipeline_qa.csv`:

- `cbp_years = 7` (2017-2023)
- `county_panel_rows = 22659`
- `latest_year = 2023`
- `latest_year_counties = 3237`
- `latest_year_nonzero_exposure = 3073`
- `allocation_ratio_median ~= 0.95` (county allocations are close to national exports; gap reflects suppressed county employment cells)

## 3) Main issues in original workflow to teach

- The old county export workflow had very limited county coverage (53 counties in one intermediate product), which can create severe geographic bias.
- Exposure allocation was previously tied to within-county employment share; that can overstate tiny counties and break national add-up logic.
- FIPS keys appeared in mixed formats across files; always force 5-character padded strings before joins.
- Tariff mapping is concentrated in a few sectors (`31-33`, `11`, `21`, and tiny `51`), so interpretation should stress sector concentration, not broad economy-wide coverage.

## 4) Campaign finance county measures (recommended)

The extension script now builds county-cycle finance measures from FEC ZIP-level aggregates:

- `donations_total`, `spending_total`
- `net_finance_flow = donations_total - spending_total`
- `donations_per_worker`, `spending_per_worker`, `net_finance_per_worker`
- `rep_dem_donation_ratio`
- `dem_donation_share`, `rep_donation_share`
- `donations_avg_size`, `spending_avg_size`

Output panel:

- `data/public/derived/county_tariff_panel_with_campaign_finance.csv`

## 5) Running the finance extension

1. Build the open-data panel first:
   - `source("code/overhaul_open_data_panel.R")`
2. Run finance extension:
   - `source("code/add_campaign_finance_county_measures.R")`

If no key is set:

- The script reuses cached raw ZIP files if they already exist.
- Otherwise set `FEC_API_KEY` and rerun.
- Optional: `FEC_FORCE_REFRESH=true` to refresh pulls.

## 6) High-value next data additions (county level)

- BEA county personal income / GDP: economic controls for exposure normalization.
- BLS QCEW county wages/employment: labor-market vulnerability controls.
- USDA ERS county typology + farm dependence: agricultural channel heterogeneity.
- FCC broadband and Census internet adoption: local information environment controls.
