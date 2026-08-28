rm(list = ls())

library(arrow)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(purrr)
library(readxl)
library(openxlsx)
library(yaml)

setwd("C:/Users/pejcalovar/OneDrive - MSMT/Analytický útvar - KA 4 - Vybudování datové základny - 7. Datový model školy/Datový model – navazující analýzy/Validace PAM r-kových proměnných")

source("validace_PAM_funkce.R")


# ==============================================================================
# NASTAVENÍ CEST
# ==============================================================================

datum_test <- "260826"   # YYMMDD – datum testovací sady; 260204, 260421, 260604, 260612, 260703, 260821, 260826

base_path <- "C:/Users/pejcalovar/OneDrive - MSMT/Analytický útvar - KA 4 - Vybudování datové základny - 7. Datový model školy"

path_parquet <- file.path(
  base_path, "Datový model – výstupy/PaM",
  format(as.Date(datum_test, format = "%y%m%d"), "%Y-%m-%d")
)

path_labels <- file.path(
  base_path, "_WIP/PaM/vykpol_new.xlsx"
)

path_out_dir <- file.path(
  base_path, "Datový model – navazující analýzy/Validace PAM r-kových proměnných/Výkazy-validace",
  datum_test
)

if (!dir.exists(path_out_dir)) dir.create(path_out_dir, recursive = TRUE)

# Kontrolní (starší) várka, vůči které se porovnává
datum_control    <- "260703"   # YYMMDD – datum kontrolní sady
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
id_cols <- c("rok", "mes", "hosp_druh", "plat_rad", "druh_pam", "druh", "kategorie", "stupen", "zdroj")

# --- P1-04 ---
message("Načítám P1-04...")
p1_04_file <- find_pam_raw_file(
  file.path(path_parquet, "P1-04", "parquet"),
  "p1-04"
)
pam_1_04_all <- read_parquet(p1_04_file)

pam_1_04 <- pam_1_04_all |>
  select(any_of(id_cols), any_of(facility_id), matches("^r\\d{3}")) |>
  rename_with(~ str_remove(., "\\.P.*$"), matches("^r\\d{3}"))

# --- P1a ---
message("Načítám P1a...")
p1a_file <- find_pam_raw_file(
  file.path(path_parquet, "P1a", "parquet"),
  "p1a"
)
pam_1a_all <- read_parquet(p1a_file)

pam_1a <- pam_1a_all |>
  select(any_of(id_cols), any_of(facility_id), matches("^r\\d{1}")) |>
  rename_with(~ str_remove(., "\\.P.*$"), matches("^r\\d{1}"))


# --- P1b ---
message("Načítám P1b...")
p1b_file <- find_pam_raw_file(
  file.path(path_parquet, "P1b", "parquet"),
  "p1b"
)
pam_1b_all <- read_parquet(p1b_file)

pam_1b <- pam_1b_all |>
  select(any_of(id_cols), any_of(facility_id), matches("^r\\d{6}")) |>
  rename_with(~ str_remove(., "\\.P.*$"), matches("^r\\d{6}"))


# --- P1c ---
files_1c   <- list.files(
  file.path(path_parquet, "P1c", "parquet"),
  pattern    = "^odd_.*\\.parquet$",
  full.names = TRUE
)
pam_1c_all <- setNames(
  map(files_1c, function(f) {
    read_parquet(f) |>
      select(any_of(id_cols), any_of(facility_id), matches("^r\\d{1}")) |>
      rename_with(~ str_remove(., "\\.P.*$"), matches("^r\\d{1}"))
  }),
  str_match(basename(files_1c), "^odd_(.+)\\.parquet$")[, 2]
)


# ==============================================================================
# NAČTENÍ LABELS
# ==============================================================================

labels_clean <- prep_pam_labels(path_labels)

# Vygeneruj automatickou mapovací tabulku na základě stringů v labels_clean
auto_freq_mapping <- generate_auto_frequency_mapping(
  labels_clean = labels_clean,
  path_out_yaml = "docs/frekvence_manualni_maping_autogenerovana.yml"
)


# ==============================================================================
# SPUŠTĚNÍ VALIDACE P1-04
# ==============================================================================

path_out_pam_1_04 <- file.path(
  path_out_dir,
  paste0("p1_04_validace_", datum_test, ".xlsx")
)

report_pam_1_04 <- validace_pam(
  data         = pam_1_04,
  labels_clean = labels_clean,
  path_out     = path_out_pam_1_04,
  id_cols      = intersect(id_cols, names(pam_1_04)),
  facility_id  = "ico",
  max_rok      = 2025,
  vykaz        = "p1"
)


# Příklady následné analýzy
# ------------------------------------------------------------------------------

# Přehled frekvencí měření
report_pam_1_04$freq_table |>
  count(frekvence_mereni) |>
  arrange(desc(n))

# Proměnné s nekonzistentním pokrytím
report_pam_1_04$strange_behaviour |>
  filter(inconsistent_mereni == TRUE) |>
  select(polozka_index, any_of("label_result"), frekvence_mereni, mereni_ok_ratio) |>
  arrange(mereni_ok_ratio)

# Detail chybějících měsíců pro konkrétní proměnnou
report_pam_1_04$coverage |>
  filter(polozka_index == "r001", !mereni_ok) |>
  arrange(rok)

# Nejproblematičtější proměnné
report_pam_1_04$strange_behaviour |>
  filter(count_serious_errors > 0 | count_strange_behaviour >= 2) |>
  select(polozka_index, any_of("label_result"),
         count_serious_errors, count_strange_behaviour,
         any_of("frekvence_mereni"), any_of("mereni_ok_ratio"),
         everything()) |>
  print(n = 50)

# Proměnné s outliery
report_pam_1_04$examples |>
  filter(!is.na(outliers_per_year)) |>
  select(polozka_index, outliers_per_year, outliers_per_facility)

# Proměnné s chybějícími měsíci
report_pam_1_04$examples |>
  filter(!is.na(inconsistent_mereni)) |>
  print(n = 30)

# Coverage v širokém formátu: proměnná × rok
report_pam_1_04$coverage |>
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
  max_rok      = 2025,
  vykaz        = "1a"
)

# ==============================================================================
# SPUŠTĚNÍ VALIDACE P1b
# ==============================================================================

path_out_pam_1b <- file.path(
  path_out_dir,
  paste0("p1b_validace_", datum_test, ".xlsx")
)

report_pam_1b <- validace_pam(
  data         = pam_1b,
  labels_clean = labels_clean,
  path_out     = path_out_pam_1b,
  id_cols      = intersect(id_cols, names(pam_1b)),
  facility_id  = "rid",
  r_pattern    = "^r\\d{3}",
  max_rok      = 2025,
  vykaz        = "1b"
)

# ==============================================================================
# SPUŠTĚNÍ VALIDACE P1c
# ==============================================================================

report_pam_1c_all <- imap(pam_1c_all, function(d, oddil) {
  if (oddil %in% c("0")) return(NULL)

  path_out_f <- file.path(
    path_out_dir,
    paste0("p1c_", oddil, "_validace_", datum_test, ".xlsx")
  )

  validace_pam(
    data         = d,
    labels_clean = labels_clean,
    path_out     = path_out_f,
    id_cols      = intersect(id_cols, names(d)),
    facility_id  = "ico",
    r_pattern    = "^r\\d{1}",
    max_rok      = 2025,
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
