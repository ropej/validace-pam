rm(list = ls())

library(arrow)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(purrr)
library(readxl)
library(openxlsx)

setwd("C:/Users/pejcalovar/OneDrive - MSMT/Analytický útvar - KA 4 - Vybudování datové základny - 7. Datový model školy/Datový model – navazující analýzy/Validace PAM r-kových proměnných")

source("validace_PAM_funkce.R")


# ==============================================================================
# NASTAVENÍ CEST
# ==============================================================================

datum_test <- "260421"   # YYMMDD – datum testovací sady

base_path <- "C:/Users/pejcalovar/OneDrive - MSMT/Analytický útvar - KA 4 - Vybudování datové základny - 7. Datový model školy"

path_parquet <- file.path(
  base_path, "Datový model – výstupy/PaM",
  format(as.Date(datum_test, format = "%y%m%d"), "%Y-%m-%d")
)

path_labels <- file.path(
  base_path, "Datový model – metadata/PaM/pamvykpol_labels.xlsx"
)

path_out_dir <- file.path(
  base_path, "Datový model – navazující analýzy/Validace PAM r-kových proměnných/Výkazy-validace",
  datum_test
)

if (!dir.exists(path_out_dir)) dir.create(path_out_dir, recursive = TRUE)


# ==============================================================================
# NAČTENÍ DAT
# ==============================================================================

# --- P1-04 ---
pam_1_04_all <- read_parquet(
  file.path(path_parquet, "P1-04", "parquet", "pam_1_04_all_long_raw.parquet")
)

id_cols <- c("rok", "mes", "red_izo", "hosp_druh", "plat_rad", "druh_pam")

pam_1_04 <- pam_1_04_all |>
  select(all_of(id_cols), matches("^r\\d{3}$"))

# --- P1a ---
pam_1a_all <- read_parquet(
  file.path(path_parquet, "P1a", "parquet", "pam_1a_all_long_raw.parquet")
)

pam_1a <- pam_1a_all |>
  select(any_of(id_cols), matches("^r\\d{1}")) # bez druh_pam



# ==============================================================================
# NAČTENÍ LABELS
# ==============================================================================

labels_clean <- prep_pam_labels(path_labels)


# ==============================================================================
# SPUŠTĚNÍ VALIDACE
# ==============================================================================

path_out_pam <- file.path(
  path_out_dir,
  paste0("p1_04_validace_", datum_test, ".xlsx")
)

report_pam <- validace_pam_1_04(
  data         = pam_1_04,
  labels_clean = labels_clean,
  path_out     = path_out_pam,
  id_cols      = id_cols,
  max_rok      = 2024
)


# ==============================================================================
# PŘÍKLADY NÁSLEDNÉ ANALÝZY
# ==============================================================================

# Přehled frekvencí měření
report_pam$freq_table |>
  count(frekvence_mereni) |>
  arrange(desc(n))

# Proměnné s nekonzistentním pokrytím
report_pam$strange_behaviour |>
  filter(inconsistent_mereni == TRUE) |>
  select(polozka_index, any_of("label_result"), frekvence_mereni, mereni_ok_ratio) |>
  arrange(mereni_ok_ratio)

# Detail chybějících měsíců pro konkrétní proměnnou
report_pam$coverage |>
  filter(polozka_index == "r001", !mereni_ok) |>
  arrange(rok)

# Nejproblematičtější proměnné
report_pam$strange_behaviour |>
  filter(count_serious_errors > 0 | count_strange_behaviour >= 2) |>
  select(polozka_index, any_of("label_result"),
         count_serious_errors, count_strange_behaviour,
         any_of("frekvence_mereni"), any_of("mereni_ok_ratio"),
         everything()) |>
  print(n = 50)

# Proměnné s outliery
report_pam$examples |>
  filter(!is.na(outliers_per_year)) |>
  select(polozka_index, outliers_per_year, outliers_per_facility)

# Proměnné s chybějícími měsíci
report_pam$examples |>
  filter(!is.na(inconsistent_mereni)) |>
  print(n = 30)

# Coverage v širokém formátu: proměnná × rok
report_pam$coverage |>
  select(polozka_index, rok, mereni_ok) |>
  pivot_wider(names_from = rok, values_from = mereni_ok) |>
  View()
