# ==============================================================================
# Kontrola cyklických nul per proměnná × kombinace ID
# ==============================================================================

# Použití: po spuštění validace_PAM.R

# --- P1-04 ---
# Identifikátory ve výstupu: hosp_druh, plat_rad
cycling_p1_04 <- check_cycling_zeros_per_var(
  data = pam_1_04,
  r_pattern = "^r\\d{3}$",
  id_cols_full = c("hosp_druh", "plat_rad"),
  facility_id = "ico"
)

# --- P1a ---
# Žádné identifikátory (jen rok × měsíc) - vrátí jednoduchou tabulku
cycling_p1a <- check_cycling_zeros_per_var(
  data = pam_1a,
  r_pattern = "^r\\d{1}",
  id_cols_full = c(),
  facility_id = "rid"
)

# --- P1c ---
cycling_p1c_all <- imap(pam_p1c_all, function(d, oddil) {
  if (oddil == "0") return(NULL)

  # Identifikátory ve výstupu: druh, kategorie, stupen, zdroj (pokud existují v datech)
  id_cols <- intersect(c("druh", "kategorie", "stupen", "zdroj"), names(d))

  check_cycling_zeros_per_var(
    data = d,
    r_pattern = "^r\\d{1}",
    id_cols_full = id_cols,
    facility_id = "ico"
  ) |>
    mutate(oddil = oddil)
})

# Kombinované výstupy
cycling_results <- list(
  p1_04 = cycling_p1_04,
  p1a = cycling_p1a,
  p1c = bind_rows(cycling_p1c_all)
)

# Příklady:
# cycling_results$p1_04 |> filter(cycling_zeros == TRUE) |> head(20)
# cycling_results$p1a |> filter(cycling_zeros == TRUE) |> head(20)
# cycling_results$p1c |> filter(cycling_zeros == TRUE) |> head(20)

# ==============================================================================
# Souhrn: cyklické nuly per proměnná
# ==============================================================================

summarise_cycling_zeros <- function(cycling_results) {

  summary_list <- list()

  # P1-04
  if (!is.null(cycling_results$p1_04)) {
    summary_list$p1_04 <- cycling_results$p1_04 |>
      group_by(polozka_index) |>
      summarise(
        cycle_TRUE = sum(cycling_zeros == TRUE, na.rm = TRUE),
        cycle_FALSE = sum(cycling_zeros == FALSE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        vykaz = "p1_04",
        difference_cycling = !(cycle_TRUE == 0 | cycle_FALSE == 0)
      ) |>
      select(vykaz, polozka_index, cycle_TRUE, cycle_FALSE, difference_cycling)
  }

  # P1a
  if (!is.null(cycling_results$p1a)) {
    summary_list$p1a <- cycling_results$p1a |>
      group_by(polozka_index) |>
      summarise(
        cycle_TRUE = sum(cycling_zeros == TRUE, na.rm = TRUE),
        cycle_FALSE = sum(cycling_zeros == FALSE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        vykaz = "p1a",
        difference_cycling = !(cycle_TRUE == 0 | cycle_FALSE == 0)
      ) |>
      select(vykaz, polozka_index, cycle_TRUE, cycle_FALSE, difference_cycling)
  }

  # P1c (s oddílem)
  if (!is.null(cycling_results$p1c)) {
    summary_list$p1c <- cycling_results$p1c |>
      group_by(oddil, polozka_index) |>
      summarise(
        cycle_TRUE = sum(cycling_zeros == TRUE, na.rm = TRUE),
        cycle_FALSE = sum(cycling_zeros == FALSE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        vykaz = paste0("p1c_", oddil),
        difference_cycling = !(cycle_TRUE == 0 | cycle_FALSE == 0)
      ) |>
      select(vykaz, polozka_index, cycle_TRUE, cycle_FALSE, difference_cycling)
  }

  # Kombinuj všechny
  bind_rows(summary_list)
}

# Spustit
cycling_summary <- summarise_cycling_zeros(cycling_results)

# Příklady:
# cycling_summary |> filter(difference_cycling == TRUE)  # Smíšené (někdy TRUE, někdy FALSE)
# cycling_summary |> filter(cycle_TRUE > 0)              # Má cyklické nuly
# cycling_summary |> filter(vykaz == "p1_04") |> head(20)


cycling_summary |>
  filter(difference_cycling) |>
  count(vykaz, name = "count_zero_cycling_failures") |>
  mutate(
    percent_difference = scales::percent(
      count_zero_cycling_failures / sum(count_zero_cycling_failures),
      accuracy = 0.1
    ),
    percent_all = scales::percent(
      count_zero_cycling_failures / nrow(cycling_summary),
      accuracy = 0.1
    )
  )
