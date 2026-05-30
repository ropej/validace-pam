# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

R project for validating PAM (Personalistika a Mzdy) r-variable data from Czech school data models (Datový model školy, MŠMT). The validation checks structural integrity and statistical quality of r-kové proměnné in PAM reports (P1-04, P1a).

## Running the validation

Open R or RStudio and run the main script:

```r
source("validace_PAM.R")
```

Before running, update the two date variables at the top of `validace_PAM.R`:

```r
datum_test    <- '260421' |> as.Date(format = "%y%m%d")   # date of parquet data to validate
datum_control <- '260204' |> as.Date(format = "%y%m%d")   # date of the reference/control run
```

Date format is `YYMMDD` (e.g. `260421` = 2026-04-21).

Required packages: `arrow`, `dplyr`, `tibble`, `stringr`, `tidyr`, `purrr`, `readxl`, `openxlsx`.

## Architecture

Two files:

- **`validace_PAM_funkce.R`** — all reusable functions; sourced at the start of the main script
- **`validace_PAM.R`** — orchestration: sets paths, loads data, calls `validace_pam()`, writes output

### Data flow

```
Datový model – výstupy/PaM/<datum_test>/
  P1-04/parquet/pam_1_04_all_long_raw.parquet
  P1a/pam_1a_all_long_raw.parquet

Datový model – metadata/PaM/pamvykpol_labels.xlsx
  → variable labels (polozka, polozka_index, zkr, vykaz)

→ validace_pam(data, labels_clean, path_out)
  → prep_pam_labels()            loads pamvykpol_labels.xlsx, filters r-variables, renames zkr → label_result
  → detect_mereni_frequency()    classifies each variable by measurement frequency
  → check_mereni_coverage()      verifies year-level coverage against expected frequency; wide table rok × variable
  → check_pam_structure()        structural overview + duplicate detection
  → summarise_pam_variables()    per-variable statistics (NA ratio, zeros, outliers, ...)
  → make_pam_strange_behaviour() flags suspicious variables
  → make_pam_examples()          concrete examples of negative / non-integer / outlier values
  → write_pam_validation_xlsx()  writes multi-sheet .xlsx report

Output: Výkazy-validace/<YYMMDD>/pam_1_04_validace_<YYMMDD>.xlsx
```

### Functions in `validace_PAM_funkce.R`

| Function | Purpose |
|---|---|
| `modus_or_median(x, na.rm)` | Mode of a vector; returns median of candidates when mode is tied |
| `detect_mereni_frequency(data, r_pattern)` | Per r-variable: mode (via `modus_or_median`) of non-NA months/year → `frekvence_mereni` (`"roční"` / `"pololetní"` / `"čtvrtletní"` / `"měsíční"` / `"nepravidelná"`); exposes `modus_mesicu_za_rok` |
| `check_mereni_coverage(data, freq_table, r_pattern)` | Verifies each year meets expected frequency; returns long table (rok × variable with `mereni_ok`, `merene_mesice`, `chybejici_mesice`). Wide pivot into `year<rok>` columns is done later in `write_pam_validation_xlsx()`. |
| `prep_pam_labels(path_labels, r_pattern)` | Loads label Excel, filters r-variables, renames `zkr` → `label_result` |
| `is_integer_tol(x, tol)` | Returns TRUE for values that are integers within floating-point tolerance |
| `upper_outlier_limit(x, multiplier)` | IQR-based upper outlier threshold (default: Q3 + 3×IQR, zeros/NAs excluded) |
| `check_pam_structure(data, id_cols, r_pattern)` | Row/col counts, missing ID columns, duplicate key detection |
| `summarise_pam_variables(data, id_cols, labels_clean, r_pattern, outlier_multiplier)` | Per-variable statistics table. Does **not** compute `count_rows` / `count_non_na` (removed). Computes `is_negative`, `count_notintegers`, `outlier_limit` for downstream use but the latter two are hidden in the XLSX Summary sheet. For P1-04 (`r_pattern == "^r\\d{3}$"`), `polozka_index` is transformed via `str_replace("^r(\\d)(\\d{2})$", "r0\\10\\2")` before joining to `labels_clean`. |
| `make_pam_strange_behaviour(summary_table, max_na_ratio, max_outliers_ratio)` | Adds flag columns; sorts by severity |
| `make_pam_examples(data, id_cols, summary_table, r_pattern, max_examples_per_type)` | Up to 100 example rows per problem type (negative, non-integer, outlier) |
| `write_pam_validation_xlsx(report, path_out)` | Writes 4-sheet XLSX (Report overview, Summary, Strange behaviour, Example of strange behaviour). Summary column order: `polozka_index`, `typ_promenne`, `count_years`, `count_mes`, `modus_mesicu_za_rok`, `frekvence_mereni`, `na_ratio`, `zeros_ratio`, `count_uniques`, `min`, `max`, `is_negative`, `outliers_ratio`, `label_result`, `first_year`, `last_year`, then `year<rok>` coverage columns. `min` and `max` formatted with thousands separator. |
| `validace_pam(data, labels_clean, path_out, id_cols, max_rok)` | Main orchestrator; calls all of the above and returns the report list. `max_rok` filters data to `rok <= max_rok` before processing (default: current year − 1; `NULL` skips filtering) |

### Output XLSX sheets

| Sheet | Content |
|---|---|
| Report overview | Row/col counts, duplicate count, summary |
| Summary | Per-variable statistics. Hidden columns: `count_notintegers`, `outlier_limit` (used internally). `min` and `max` formatted with thousands separator. Appended `year<rok>` coverage columns. |
| Strange behaviour | Flag columns: `count_serious_errors`, `count_strange_behaviour`, individual flags |
| Example of strange behaviour | Rows with negative values, non-integers, or outliers |

### Key conventions

- R-variable columns match `^r\d{3}$` in P1-04 and `^r\d{1}` in P1a.
- ID columns for P1-04: `rok`, `mes`, `red_izo`, `hosp_druh`, `plat_rad`, `druh_pam`.
- Outlier threshold uses IQR × 3 on non-zero, non-NA values (`upper_outlier_limit()`).
- All paths are absolute and hardcoded to the user's OneDrive. When modifying paths, keep the base root `C:/Users/pejcalovar/OneDrive - MSMT/Analytický útvar - KA 4 - Vybudování datové základny - 7. Datový model školy/`.
