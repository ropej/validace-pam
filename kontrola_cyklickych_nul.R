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
