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
7. Build bivariate research maps:
   `source("code/build_bivariate_maps.R")`
8. Build fixed-effects model tables:
   `source("code/build_fixest_models.R")`
9. Build presentation-ready maps and tables:
   `source("code/comprehensive_results_geospatial.R")`
10. Render the report (optional):
   `quarto::quarto_render("analysis/report.qmd")`
11. Launch the visual explorer:
   `shiny::runApp("app.R")`

Optional one-shot runner:
`source("run_all.R")`

Optional one-shot runner with notebook rendering:
`Rscript run_all.R --build-teaching`

## Portable local run (fresh clone)

From a terminal, this now works without any machine-specific paths:

1. `Rscript setup_project.R`
2. `Rscript run_all.R`
3. Optional preflight only: `Rscript run_all.R --check-only`
4. Optional include client-facing notebook render: `Rscript run_all.R --build-teaching`

Core scripts depend on standard project files (`.R`, `.csv`, `.txt`, `.json`, `.md`) and do not require local `.RData` or `.xlsx` artifacts.

## Client-facing notebook QA and render

1. Scan source files for disallowed AI-signaling phrases:
   `source("code/check_client_text.R")`
2. Render all client-facing teaching notebooks:
   `source("analysis/render_teaching_series.R")`

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
- Advanced geospatial outputs:
  - `output/advanced_maps/figures/`
  - `output/advanced_maps/tables/`
- Fixest model outputs:
  - `output/fixest_models/tables/`
  - `output/fixest_models/diagnostics/`
  - `output/fixest_models/reports/`
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
2. On push to `main`, GitHub Actions deploys production at the root URL.
3. On push to `staging`, GitHub Actions deploys a preview under `/staging/`.
4. Deploy is gated by smoke checks (site files + JSON schema presence + JS syntax).

Expected URL for this repository:

- `https://ihelfrich.github.io/retaliatory_tariffs/`
- `https://ihelfrich.github.io/retaliatory_tariffs/staging/` (preview lane)

## Legacy build scripts

The original student scripts are kept in `data/data_cleaning_code/`:

- `build_county_exposure.R`
- `clean_tariff_retaliation.R`

These remain useful for tracing the original workflow and teaching where refinements were needed.
