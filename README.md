# retaliatory-tariff

County-level analysis of retaliatory tariff exposure and political salience in the United States.

## Core question

Did retaliatory tariffs land more heavily in politically salient places?

The project keeps this original framing and builds county-level exposure measures that can be linked to county election outcomes.

## Core measure

County exposure is built as:

`Exposure_ct = sum_i(CountyAllocatedExports_ict * TariffSeverity_it)`

where:

- `CountyAllocatedExports_ict` is county `c` share of U.S. sector `i` employment in year `t`, multiplied by U.S. exports for sector `i` in year `t`.
- `TariffSeverity_it` comes from HS-line retaliatory tariff rates mapped into NAICS sectors.

## RStudio workflow

1. Open `retaliatory-tariff.Rproj`.
2. Run dependency setup:
   `source("setup_project.R")`
3. Run the full core build:
   `source("code/overhaul_open_data_panel.R")`
4. Run data validation checks:
   `source("code/validate_core_outputs.R")`
5. Build analysis tables/brief:
   `source("code/build_analysis_outputs.R")`
6. Build site data for the web explorer:
   `source("code/build_site_data.R")`
7. Build presentation-ready maps and tables:
   `source("code/comprehensive_results_geospatial.R")`
8. Launch the visual explorer:
   `shiny::runApp("app.R")`

Optional one-shot runner:
`source("run_all.R")`

## Portable local run (fresh clone)

From a terminal, this now works without any machine-specific paths:

1. `Rscript setup_project.R`
2. `Rscript run_all.R`
3. Optional preflight only: `Rscript run_all.R --check-only`

Core scripts depend on standard project files (`.R`, `.csv`, `.txt`, `.json`, `.md`) and do not require local `.RData` or `.xlsx` artifacts.

## Optional campaign-finance extension

Run:
`source("code/add_campaign_finance_county_measures.R")`

Notes:

- Requires `FEC_API_KEY` unless cached ZIP files already exist.
- Reuses cache if present:
  - `data/public/raw/fec_donations_by_zip_focus_committees.csv`
  - `data/public/raw/fec_spending_by_zip_focus_committees.csv`
- Optional controls:
  - `Sys.setenv(FEC_FORCE_REFRESH = "true")`
  - `Sys.setenv(FEC_MAX_PAGES = "800")`

## Key outputs

- Core panel:
  - `data/public/derived/county_tariff_exposure_panel_with_votes_2017_2023.csv`
- Validation:
  - `output/overhaul/qa/validation_checks.csv`
  - `output/overhaul/qa/pipeline_qa.csv`
- Analysis package:
  - `output/analysis/tables/`
  - `output/analysis/figures/`
  - `output/analysis/reports/analysis_brief.md`
- Web explorer:
  - `site/index.html`
  - `site/data/`
- Maps and figures:
  - `output/overhaul/figures/`
  - `output/comprehensive_results/figures/`
- Tables:
  - `output/overhaul/tables/`
  - `output/comprehensive_results/tables/`

## Documentation

- `docs/analysis_protocol.md`
- `docs/project_diagnostics_and_measure_extensions.md`

## GitHub Pages

Deployment workflow:

- `.github/workflows/deploy-pages.yml`

How it works:

1. Run `source("code/build_site_data.R")` locally before pushing.
2. On push to `main`, GitHub Actions uploads `site/` as the Pages artifact.
3. GitHub deploys that artifact to Pages.

Expected URL for this repository:

- `https://ihelfrich.github.io/retaliatory_tariffs/`

## Legacy build scripts

The original student scripts are kept in `data/data_cleaning_code/`:

- `build_county_exposure.R`
- `clean_tariff_retaliation.R`

These remain useful for tracing the original workflow and teaching where refinements were needed.
