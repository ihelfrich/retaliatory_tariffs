# Analysis Protocol

## Scope

This protocol preserves the original research question:

How strongly are counties exposed to retaliatory tariffs, and how does that exposure align with electoral salience?

## Data inputs

- County Business Patterns (CBP), county-level employment by NAICS, 2017-2023.
- CBP U.S.-level employment by NAICS, 2017-2023.
- Census international trade API, U.S. exports by NAICS.
- Retaliatory tariff lines mapped from HS to NAICS.
- County vote data for 2016, 2020, and 2024.

## Construction steps

1. Build sector tariff severity from mapped HS lines.
2. Build county employment panel by NAICS and year.
3. Build U.S. employment totals by NAICS and year.
4. Build U.S. exports by NAICS and year.
5. Allocate U.S. sector exports to counties by county share of U.S. sector employment.
6. Multiply allocated exports by tariff severity and sum across sectors.
7. Merge county exposure panel with county election variables.

## QA requirements

The build is considered valid only if:

- Years are complete (2017-2023).
- County-year keys are unique.
- County FIPS keys are valid 5-digit strings.
- Exposure is nonnegative.
- Latest-year county coverage is near national county count.
- Vote-link coverage is high enough for regression use.
- Allocation diagnostics are within acceptable bounds.

Automated checks run in:

- `code/validate_core_outputs.R`

Standard analysis artifacts are built in:

- `code/build_analysis_outputs.R`

## Modeling conventions

- Main outcome uses `log10(exposure + 1)`.
- Baseline political regressions include county GOP share and competitiveness.
- Campaign-finance extension is additive and optional; it does not redefine the core exposure measure.

## Interpretation guardrails

- Exposure here is an index based on allocated exports and tariff rates, not a direct measure of realized income loss.
- Sector exposure is concentrated in a limited set of tariffed sectors; interpretation should reflect that concentration.
- County-level allocation inherits CBP suppression and measurement constraints.
