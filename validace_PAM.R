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

datum_test <- "260612"   # YYMMDD – datum testovací sady; 260604, 260421, 260612

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

# Kontrolní (starší) várka, vůči které se porovnává
datum_control    <- "260421"   # YYMMDD – datum kontrolní sady
path_control_dir <- file.path(dirname(path_out_dir), datum_control)

# Jsou reporty pro datum_test už vygenerované? Pokud ano, generování přeskočíme
# a rovnou se jen porovnává (viz sekce SROVNÁNÍ na konci skriptu).
reporty_existuji <- dir.exists(path_out_dir) &&
  length(list.files(path_out_dir, pattern = "validace_.*\\.xlsx$")) > 0

if (reporty_existuji) {

  message("Reporty pro ", datum_test, " už existují → generování přeskakuji.")

} else {

  message("Reporty pro ", datum_test, " neexistují → spouštím validace…")


# ==============================================================================
# NAČTENÍ DAT
# ==============================================================================

facility_id <- c("red_izo", "rid", "ico")
id_cols <- c("rok", "mes", "hosp_druh", "plat_rad", "druh_pam", "druh")

# --- P1-04 ---
pam_1_04_all <- read_parquet(
  file.path(path_parquet, "P1-04", "parquet", "pam_1_04_all_long_raw.parquet")
)

pam_1_04 <- pam_1_04_all |>
  select(any_of(id_cols), any_of(facility_id), matches("^r\\d{3}")) |>
  rename_with(~ str_remove(., "\\.P.*$"), matches("^r\\d{3}"))

# --- P1a ---
pam_1a_all <- read_parquet(
  file.path(path_parquet, "P1a", "parquet", "pam_1a_all_long_raw.parquet")
)

pam_1a <- pam_1a_all |>
  select(any_of(id_cols), any_of(facility_id), matches("^r\\d{1}")) |>
  rename_with(~ str_remove(., "\\.P.*$"), matches("^r\\d{1}"))


# --- P1c ---
files_p1c   <- list.files(
  file.path(path_parquet, "P1c", "parquet"),
  pattern    = "^odd_.*\\.parquet$",
  full.names = TRUE
)
pam_p1c_all <- setNames(
  map(files_p1c, read_parquet),
  str_match(basename(files_p1c), "^odd_(.+)\\.parquet$")[, 2]
)


# ==============================================================================
# NAČTENÍ LABELS
# ==============================================================================

labels_clean <- prep_pam_labels(path_labels)


# ==============================================================================
# SPUŠTĚNÍ VALIDACE P1-04
# ==============================================================================

path_out_pam <- file.path(
  path_out_dir,
  paste0("p1_04_validace_", datum_test, ".xlsx")
)

report_pam <- validace_pam(
  data         = pam_1_04,
  labels_clean = labels_clean,
  path_out     = path_out_pam,
  id_cols      = intersect(id_cols, names(pam_1_04)),
  facility_id  = "red_izo",
  max_rok      = 2024,
  vykaz        = "p1"
)


# Příklady následné analýzy
# ------------------------------------------------------------------------------

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


# ==============================================================================
# SPUŠTĚNÍ VALIDACE P1a
# ==============================================================================

path_out_pam_1a <- file.path(
  path_out_dir,
  paste0("p1a_validace_", datum_test, ".xlsx")
)

report_pam_1a <- validace_pam(
  data         = pam_1a,
  labels_clean = labels_clean,
  path_out     = path_out_pam_1a,
  id_cols      = intersect(id_cols, names(pam_1a)),
  facility_id  = "rid",
  r_pattern    = "^r\\d{1}",
  max_rok      = 2024,
  vykaz        = "1a"
)


# ==============================================================================
# SPUŠTĚNÍ VALIDACE P1c
# ==============================================================================

iwalk(pam_p1c_all, function(d, oddil) {
  if (oddil == "0") return(invisible(NULL))

  path_out_f <- file.path(
    path_out_dir,
    paste0("p1c_", oddil, "_validace_", datum_test, ".xlsx")
  )

  validace_pam(
    data         = d,
    labels_clean = labels_clean,
    path_out     = path_out_f,
    id_cols      = intersect(id_cols, names(d)),
    facility_id  = "red_izo",
    r_pattern    = "^r\\d{1}",
    max_rok      = 2024,
    vykaz        = "1c",
    oddil        = oddil
  )
})

}  # konec else (blok generování reportů)


# ==============================================================================
# SROVNÁNÍ S KONTROLNÍ VÁRKOU
# ==============================================================================
# Porovná hotové reporty z path_out_dir s kontrolní várkou v path_control_dir.
# Funguje, ať se reporty právě vygenerovaly, nebo už existovaly z dřívějška.

if (dir.exists(path_control_dir)) {
  compare_all <- porovnej_pam_varky(path_test    = path_out_dir,
                                    path_control = path_control_dir)

  print(compare_all$count_neshod)   # počty neshod po výkazech + řádek CELKEM

  # Ulož compare výsledky do Excel reportu
  path_compare_xlsx <- file.path(path_out_dir, paste0("Porovnani_", datum_test, "_vs_", datum_control, ".xlsx"))
  write_pam_compare_xlsx(compare_all, path_compare_xlsx)
  message("Compare report uložen: ", basename(path_compare_xlsx))
} else {
  message("Kontrolní složka neexistuje: ", path_control_dir,
          "\nSrovnání přeskočeno – zkontroluj 'datum_control'.")
}
