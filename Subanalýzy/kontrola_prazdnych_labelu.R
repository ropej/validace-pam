# Kontrola prázdných labelů v PAM validačních reportech
# Načte Summary listy ze všech xlsx reportů ve zvolené složce a zobrazí
# proměnné bez vyplněného label_result.
#
# Použití:
#   Nastav datum_validace níže (YYMMDD) a spusť skript.

library(dplyr)
library(purrr)
library(stringr)
library(readxl)

datum_validace <- "260421"   # YYMMDD

# ------------------------------------------------------------------------------

base_path <- "C:/Users/pejcalovar/OneDrive - MSMT/Analytický útvar - KA 4 - Vybudování datové základny - 7. Datový model školy"
project_path <- file.path(base_path, "Datový model – navazující analýzy/Validace PAM r-kových proměnných")

source(file.path(project_path, "validace_PAM_funkce.R"))   # pro oddil_to_code()

labels_clean <- readRDS(file.path(project_path, "labels_clean.rds"))

path_reporty <- file.path(project_path, "Výkazy-validace", datum_validace)

xlsx_soubory <- list.files(path_reporty, pattern = "\\.xlsx$", full.names = TRUE)

if (length(xlsx_soubory) == 0)
  stop("Ve složce nebyly nalezeny žádné xlsx soubory: ", path_reporty)

# Načtení Summary listu ze všech reportů
summary_vse <- map_dfr(xlsx_soubory, function(f) {
  sheets <- excel_sheets(f)
  if (!"Summary" %in% sheets) return(NULL)
  read_excel(f, sheet = "Summary") |>
    mutate(soubor = str_remove(basename(f), "_validace_\\d{6}\\.xlsx$"))
})

# Proměnné bez labelu
prazdne <- summary_vse |>
  filter(is.na(label_result)) |>
  mutate(
    polozka_index_vykpol = case_when(
      soubor == "p1_04" ~
        paste0(str_replace(polozka_index, "^r(\\d)(\\d{2})$", "r0\\10\\2"), ".p1"),
      soubor == "p1a" ~
        paste0(polozka_index, ".1a"),
      str_starts(soubor, "p1c_") ~
        paste0("r", oddil_to_code(str_remove(soubor, "^p1c_")), "0",
               str_extract(polozka_index, "\\d{2}$"), ".1c"),
      TRUE ~ polozka_index
    )
  ) |>
  select(soubor, polozka_index, polozka_index_vykpol,
         any_of(c("typ_promenne", "frekvence_mereni", "na_ratio", "count_years")),
         matches("^year\\d{2}$")) |>
  arrange(soubor, polozka_index)

cat("\n=== Počet proměnných bez labelu ===\n")
prazdne |>
  count(soubor, name = "pocet_bez_labelu") |>
  print(n = Inf)

cat("\n=== Detail chybějících labelů ===\n")
print(prazdne, n = Inf)

# Uložení výstupu
path_rds <- file.path(path_reporty, paste0("missing_labels_", datum_validace, ".rds"))
saveRDS(prazdne, path_rds)
message("Uloženo: ", path_rds)
