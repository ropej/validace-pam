library(readxl)
library(dplyr)
library(stringr)

path_compare <- "Výkazy-validace/260612/Porovnani_260612_vs_260421.xlsx"

cat("=== ANALÝZA COMPARE REPORTU ===\n\n")

# 1. Overview
cat("--- OVERVIEW ---\n")
for (sheet in c("Overview", "Overview P1-04", "Overview P1a", "Overview P1c")) {
  tryCatch({
    overview <- read_excel(path_compare, sheet = sheet)
    if (nrow(overview) > 0) {
      cat("\n", sheet, ":\n")
      print(overview)
    }
  }, error = function(e) NULL)
}

# 2. Summary - počty neshod
cat("\n\n--- SUMMARY NESHODY ---\n")
for (sheet in c("Summary P1-04", "Summary P1a", "Summary P1c")) {
  tryCatch({
    summary <- read_excel(path_compare, sheet = sheet)
    if (nrow(summary) > 0) {
      cat("\n", sheet, " - ", nrow(summary), " proměnných s neshody\n")

      # Které metriky se změnily
      changing_cols <- names(summary)[!names(summary) %in% c("vykaz", "polozka_index")]
      changes <- 0
      for (col in changing_cols) {
        if (!all(is.na(summary[[col]]))) {
          changes <- changes + sum(!is.na(summary[[col]]))
        }
      }
      cat("  Celkový počet změn:", changes, "\n")

      # Top 5 proměnných s nejvíc změnami
      summary_changes <- summary |>
        mutate(n_changes = rowSums(!is.na(select(across(-vykaz, -polozka_index))))) |>
        filter(n_changes > 0) |>
        arrange(desc(n_changes)) |>
        head(5)

      cat("  Top 5 proměnných:\n")
      for (i in 1:nrow(summary_changes)) {
        cat("   ", summary_changes$polozka_index[i], " (", summary_changes$n_changes[i], " změn)\n")
      }
    }
  }, error = function(e) NULL)
}

cat("\n\n=== KRITICKÉ PROBLÉMY ===\n")

# Zkontroluj "vyšší ***" (více než 20% rozdíl)
for (sheet in c("Summary P1-04", "Summary P1a", "Summary P1c")) {
  tryCatch({
    summary <- read_excel(path_compare, sheet = sheet)
    if (nrow(summary) > 0) {
      # Hledej "***" (velké rozdíly)
      big_changes <- c()
      for (i in 1:nrow(summary)) {
        row_text <- paste(summary[i, ], collapse = " | ")
        if (grepl("\\*\\*\\*", row_text)) {
          big_changes <- c(big_changes, summary$polozka_index[i])
        }
      }

      if (length(big_changes) > 0) {
        cat("\n", sheet, " - Velké rozdíly (> 20%):\n")
        cat(" ", paste(big_changes, collapse = ", "), "\n")
      }
    }
  }, error = function(e) NULL)
}

cat("\n\nDOBRO - hotovo.\n")
