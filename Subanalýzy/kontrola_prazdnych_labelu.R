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

path_reporty <- file.path(
  base_path,
  "Datový model – navazující analýzy/Validace PAM r-kových proměnných/Výkazy-validace",
  datum_validace
)

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
  select(soubor, polozka_index, any_of(c("typ_promenne", "frekvence_mereni",
                                          "na_ratio", "count_years"))) |>
  arrange(soubor, polozka_index)

cat("\n=== Počet proměnných bez labelu ===\n")
prazdne |>
  count(soubor, name = "pocet_bez_labelu") |>
  print(n = Inf)

cat("\n=== Detail chybějících labelů ===\n")
print(prazdne, n = Inf)

# Uložení výstupu
path_rds <- file.path(path_reporty, paste0("prazdne_labely_", datum_validace, ".rds"))
saveRDS(prazdne, path_rds)
message("Uloženo: ", path_rds)
