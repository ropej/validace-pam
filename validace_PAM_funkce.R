# ==============================================================================
# validace_PAM_funkce.R
# Pomocné funkce pro validaci PAM výkazů (P1-04)
#
# Struktura:
#   1. is_integer_tol()            – kontrola celočíselnosti
#   2. upper_outlier_limit()       – horní mez outlierů (IQR)
#   3. modus_or_median()           – modus s mediánem při nerozhodnosti
#   4. prep_pam_labels()           – příprava labelů proměnných
#   5. check_pam_structure()       – struktura dat, duplicity klíče
#   6. detect_mereni_frequency()   – frekvence měření per proměnná
#   7. check_mereni_coverage()     – pokrytí měření dle frekvence
#   8. summarise_pam_variables()   – souhrnné statistiky r-kových proměnných
#   9. make_pam_strange_behaviour()– příznaky podezřelého chování
#  10. make_pam_examples()         – konkrétní příklady problémů
#  11. write_pam_validation_xlsx() – zápis reportu do .xlsx
#  12. validace_pam_1_04()         – hlavní orchestrační funkce
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. is_integer_tol()
# Kontrola, zda jsou numerické hodnoty celočíselné (s tolerancí na float)
# ------------------------------------------------------------------------------

is_integer_tol <- function(x, tol = .Machine$double.eps^0.5) {
  if (!is.numeric(x)) return(rep(FALSE, length(x)))
  is.na(x) | abs(x - round(x)) < tol
}


# ------------------------------------------------------------------------------
# 2. upper_outlier_limit()
# Výpočet horní hranice outlierů pomocí IQR pravidla
# Ignoruje nuly a NA při výpočtu kvantilů.
# ------------------------------------------------------------------------------

upper_outlier_limit <- function(x, multiplier = 3) {
  x <- x[!is.na(x) & x != 0]
  if (length(x) < 2) return(NA_real_)
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q)
  as.numeric(q[2] + multiplier * iqr)
}


# ------------------------------------------------------------------------------
# 3. modus_or_median()
# Modus vektoru; při nerozhodném výsledku vrátí medián kandidátů.
# ------------------------------------------------------------------------------

modus_or_median <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  tab            <- table(x)
  mod_candidates <- as.numeric(names(tab)[tab == max(tab)])
  if (length(mod_candidates) == 1) mod_candidates else median(x)
}


# ------------------------------------------------------------------------------
# 4. prep_pam_labels()
# Načtení a příprava labelů proměnných z Excel souboru
# ------------------------------------------------------------------------------

clean_excel_text <- function(x) {
  x |>
    stringr::str_replace_all("[[:cntrl:]]", " ") |>
    stringr::str_squish()
}

prep_pam_labels <- function(path_labels, r_pattern = "^r\\d{1}") {
  readxl::read_excel(path_labels) |>
    filter(str_detect(polozka_index, r_pattern)) |>
    mutate(across(everything(), as.character)) |>
    rename(label_result = "zkr") |>
    mutate(label_result = clean_excel_text(label_result)) |>
    filter(!if_all(everything(), is.na)) |>
    distinct(polozka_index, .keep_all = TRUE) |>
    select(polozka, polozka_index, vykaz, label_result)
}


# ------------------------------------------------------------------------------
# 5. check_pam_structure()
# Kontrola struktury dat a duplicit kombinačního klíče.
#
# INPUT:
#   data      – datový rámec
#   id_cols   – sloupce tvořící unikátní klíč řádku
#   r_pattern – regex pro identifikaci r-kových proměnných
#
# OUTPUT: list s $n_rows, $n_cols, $missing_id_cols, $n_r_variables,
#              $id_uniques, $existence_of_duplicates,
#              $n_duplicate_keys, $duplicate_examples
# ------------------------------------------------------------------------------

check_pam_structure <- function(data, id_cols, r_pattern = "^r\\d{3}$") {

  missing_id_cols <- setdiff(id_cols, names(data))
  r_cols          <- names(data)[str_detect(names(data), r_pattern)]
  valid_id_cols   <- id_cols[id_cols %in% names(data)]

  id_uniques <- map_int(valid_id_cols,
                        ~ n_distinct(data[[.x]], na.rm = TRUE)) |>
    setNames(valid_id_cols)

  duplicates <- data |>
    count(across(all_of(valid_id_cols)), name = "n") |>
    filter(n > 1) |>
    arrange(desc(n))

  list(
    n_rows                  = nrow(data),
    n_cols                  = ncol(data),
    missing_id_cols         = if (length(missing_id_cols) == 0) "žádné"
                              else paste(missing_id_cols, collapse = ", "),
    n_r_variables           = length(r_cols),
    id_uniques              = id_uniques,
    existence_of_duplicates = nrow(duplicates) > 0,
    n_duplicate_keys        = nrow(duplicates),
    duplicate_examples      = duplicates |> slice_head(n = 100)
  )
}


# ------------------------------------------------------------------------------
# 6. detect_mereni_frequency()
# Pro každou r-kovou proměnnou zjistí kategorii frekvence měření.
#
# Logika: modus počtu měřených měsíců přes roky → kategorie.
#
# Kategorie:
#   "měsíční"      … modus == 12
#   "čtvrtletní"   … modus %in% c(3, 4)
#   "pololetní"    … modus == 2
#   "roční"        … modus == 1
#   "nepravidelná" … jinak
#
# OUTPUT: tibble polozka_index | frekvence_mereni | modus_mesicu_za_rok |
#                              | roky_s_merenims  | ocekavane_mesice
# ------------------------------------------------------------------------------

detect_mereni_frequency <- function(data, r_pattern = "^r\\d{3}$") {

  stopifnot(all(c("rok", "mes") %in% names(data)))
  r_cols <- names(data)[str_detect(names(data), r_pattern)]

  long <- data |>
    select(rok, mes, all_of(r_cols)) |>
    pivot_longer(cols = all_of(r_cols),
                 names_to  = "polozka_index",
                 values_to = "hodnota") |>
    mutate(hodnota_num = suppressWarnings(as.numeric(hodnota)))

  mesice_per_rok <- long |>
    filter(!is.na(hodnota_num)) |>
    group_by(polozka_index, rok) |>
    summarise(n_mesicu = n_distinct(mes), .groups = "drop")

  freq_summary <- mesice_per_rok |>
    group_by(polozka_index) |>
    summarise(
      roky_s_merenims      = n(),
      modus_mesicu_za_rok  = modus_or_median(n_mesicu),
      modus_mesicu         = {
        tbl <- table(n_mesicu)
        as.integer(names(tbl)[which.max(tbl)])
      },
      .groups = "drop"
    )

  freq_summary |>
    mutate(
      frekvence_mereni = case_when(
        modus_mesicu == 12          ~ "měsíční",
        modus_mesicu %in% c(3, 4)  ~ "čtvrtletní",
        modus_mesicu == 2           ~ "pololetní",
        modus_mesicu == 1           ~ "roční",
        TRUE                        ~ "nepravidelná"
      ),
      ocekavane_mesice = case_when(
        frekvence_mereni == "měsíční" ~ list(str_pad(1:12, width = 2, pad = "0")),
        frekvence_mereni == "čtvrtletní" ~ list(c("03", "06", "09", "12")),
        frekvence_mereni == "pololetní" ~ list(c("06", "12")),
        frekvence_mereni == "roční"      ~ list("12"),
        TRUE                             ~ list(integer(0))
      )
    ) |>
    select(polozka_index, frekvence_mereni, modus_mesicu_za_rok,
           roky_s_merenims, ocekavane_mesice)
}


# ------------------------------------------------------------------------------
# 7. check_mereni_coverage()
# Ověří, zda v každém roce jsou přítomna očekávaná měsíční měření.
# Vychází z výstupu detect_mereni_frequency().
# "nepravidelné" proměnné přeskočí (mereni_ok = NA).
#
# OUTPUT: tibble polozka_index | frekvence_mereni | rok |
#                              | merene_mesice | chybejici_mesice | mereni_ok
# ------------------------------------------------------------------------------

check_mereni_coverage <- function(data, freq_table, r_pattern = "^r\\d{3}$") {

  stopifnot(all(c("rok", "mes") %in% names(data)))

  freq_kontrolovane <- freq_table |>
    filter(frekvence_mereni != "nepravidelná") |>
    filter(map_int(ocekavane_mesice, length) > 0)

  if (nrow(freq_kontrolovane) == 0) {
    message("Žádná proměnná nemá definovatelnou frekvenci – coverage přeskočena.")
    return(tibble())
  }

  long <- data |>
    select(rok, mes, all_of(freq_kontrolovane$polozka_index)) |>
    pivot_longer(cols = all_of(freq_kontrolovane$polozka_index),
                 names_to  = "polozka_index",
                 values_to = "hodnota") |>
    mutate(hodnota_num = suppressWarnings(as.numeric(hodnota))) |>
    filter(!is.na(hodnota_num))

  merene <- long |>
    group_by(polozka_index, rok) |>
    summarise(merene_mesice = list(sort(unique(mes))), .groups = "drop")

  merene |>
    left_join(freq_table |> select(polozka_index, frekvence_mereni, ocekavane_mesice),
              by = "polozka_index") |>
    mutate(chybejici_mesice = map2(ocekavane_mesice, merene_mesice, ~ setdiff(.x, .y)),
           mereni_ok        = map_lgl(chybejici_mesice, ~ length(.x) == 0),
           merene_mesice    = map_chr(merene_mesice,    ~ paste(.x, collapse = ",")),
           chybejici_mesice = map_chr(chybejici_mesice,
                                      ~ if (length(.x) == 0) "" else paste(.x, collapse = ",")
                                      )
           ) |>
    select(polozka_index, frekvence_mereni, rok, merene_mesice, chybejici_mesice, mereni_ok)
}


# ------------------------------------------------------------------------------
# 8a. var_range_diffmeans()
# Variační rozsah (max − min) rozdílů ročních výběrových průměrů.
# Vrací pojmenovaný numerický vektor (název = název proměnné).
# ------------------------------------------------------------------------------

var_range_diffmeans <- function(data) {
  data |>
    group_by(rok) |>
    summarise(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop") |>
    select(-rok) |>
    mutate(across(where(is.numeric), ~ c(NA, diff(.)))) |>
    apply(2, function(x) if (all(is.na(x))) NA else diff(range(x, na.rm = TRUE)))
}


# ------------------------------------------------------------------------------
# 8. summarise_pam_variables()
# Výpočet validačních statistik pro r-kové proměnné.
# Volitelně připojuje frekvenci měření a mereni_ok_ratio.
#
# INPUT:
#   data               – datový rámec (obsahuje rok, mes, id_cols + r-kové proměnné)
#   id_cols            – identifikátory
#   labels_clean       – tabulka polozka_index → label_result (nebo NULL)
#   freq_table         – výstup z detect_mereni_frequency() (nebo NULL)
#   coverage_table     – výstup z check_mereni_coverage() (nebo NULL)
#   r_pattern          – regex pro r-kové proměnné
#   outlier_multiplier – násobek IQR pro outlier mez
# ------------------------------------------------------------------------------

summarise_pam_variables <- function(data, id_cols,
                                    labels_clean       = NULL,
                                    freq_table         = NULL,
                                    coverage_table     = NULL,
                                    r_pattern          = "^r\\d{1}",
                                    outlier_multiplier = 3) {

  r_cols <- names(data)[str_detect(names(data), r_pattern)]

  summary_table <- map_dfr(r_cols, \(var) {

    x       <- data[[var]]
    x_num   <- suppressWarnings(as.numeric(x))
    x_noNA  <- x_num[!is.na(x_num)]
    out_lim <- upper_outlier_limit(x_num, multiplier = outlier_multiplier)
    mask    <- !is.na(x_num)

    # na_ratio: vyloučit roky, kde je proměnná 100% NA (= neměřena v daném roce)
    measured_roks          <- unique(data$rok[mask])
    mask_measured_years    <- data$rok %in% measured_roks
    x_num_measured         <- x_num[mask_measured_years]
    na_ratio_val           <- if (length(x_num_measured) > 0)
                                mean(is.na(x_num_measured)) else NA_real_

    tibble(
      polozka_index     = var,
      typ_promenne      = class(x)[1],
      na_ratio          = na_ratio_val,
      zeros_ratio       = if (sum(mask) > 0) mean(x_noNA == 0) else NA_real_,
      count_uniques     = n_distinct(x_num, na.rm = TRUE),
      min               = if (length(x_noNA) > 0) min(x_noNA)  else NA_real_,
      max               = if (length(x_noNA) > 0) max(x_noNA)  else NA_real_,
      is_negative       = if (length(x_noNA) > 0) any(x_noNA < 0) else NA,
      count_notintegers = sum(!is_integer_tol(x_num), na.rm = TRUE),
      outlier_limit     = out_lim,
      outliers_ratio    = if (is.na(out_lim)) NA_real_
                          else mean(x_num[mask] > out_lim, na.rm = TRUE),
      count_years       = n_distinct(data$rok[mask]),
      first_year        = suppressWarnings(min(data$rok[mask], na.rm = TRUE)),
      last_year         = suppressWarnings(max(data$rok[mask], na.rm = TRUE)),
      count_mes         = n_distinct(paste(data$rok[mask], data$mes[mask]))
    )
  })

  if (!is.null(freq_table)) {
    summary_table <- summary_table |>
      left_join(freq_table |> select(polozka_index, frekvence_mereni,
                                     modus_mesicu_za_rok, roky_s_merenims),
                by = "polozka_index")
  }

  if (!is.null(coverage_table) && nrow(coverage_table) > 0) {
    mereni_ok_ratio <- coverage_table |>
      group_by(polozka_index) |>
      summarise(mereni_ok_ratio = mean(mereni_ok, na.rm = TRUE), .groups = "drop")

    summary_table <- summary_table |>
      left_join(mereni_ok_ratio, by = "polozka_index")
  }

  # --- diff-based statistiky (jen pro sub-roční frekvence) ---
  diff_group_cols <- intersect(c("rok", "hosp_druh", "plat_rad", "druh_pam"), names(data))
  sort_by_mes     <- "mes" %in% names(data)

  vars_for_diff <- if ("frekvence_mereni" %in% names(summary_table))
    summary_table |>
      filter(!is.na(frekvence_mereni),
             !frekvence_mereni %in% c("roční", "nepravidelná")) |>
      pull(polozka_index)
  else
    character(0)

  # Předpočítaný zdiferencovaný dataset — použit pro diff stats i var_range_diffmeans
  diff_data <- if (length(vars_for_diff) > 0) {
    sel_all <- c(diff_group_cols, if (sort_by_mes) "mes", vars_for_diff)
    raw     <- data |> select(all_of(sel_all))
    if (sort_by_mes)
      raw <- raw |> arrange(across(all_of(diff_group_cols)), mes)
    raw |>
      group_by(across(all_of(diff_group_cols))) |>
      mutate(across(all_of(vars_for_diff), ~ {
        v <- suppressWarnings(as.numeric(.))
        c(NA, diff(v))
      })) |>
      ungroup()
  } else NULL

  if (!is.null(diff_data)) {
    diff_stats <- map_dfr(vars_for_diff, \(var) {
      diffs_num  <- diff_data[[var]]
      diffs_noNA <- diffs_num[!is.na(diffs_num)]
      out_lim_d  <- upper_outlier_limit(diffs_num, multiplier = outlier_multiplier)
      mask_d     <- !is.na(diffs_num)

      tibble(
        polozka_index       = var,
        count_uniques_diff  = n_distinct(diffs_num, na.rm = TRUE),
        min_diff            = if (length(diffs_noNA) > 0) min(diffs_noNA) else NA_real_,
        max_diff            = if (length(diffs_noNA) > 0) max(diffs_noNA) else NA_real_,
        outliers_ratio_diff = if (is.na(out_lim_d)) NA_real_
                              else mean(diffs_num[mask_d] > out_lim_d, na.rm = TRUE)
      )
    })
    summary_table <- summary_table |> left_join(diff_stats, by = "polozka_index")
  }

  # --- diffmean_range na zdiferencovaných datech ---
  if (!is.null(diff_data)) {
    dmr_vals <- var_range_diffmeans(
      diff_data |> select(rok, all_of(vars_for_diff)) |> mutate(across(-rok, as.numeric))
    )
    summary_table <- summary_table |>
      left_join(tibble(polozka_index  = names(dmr_vals),
                       diffmean_range = as.numeric(dmr_vals)),
                by = "polozka_index")
  }

  if (!is.null(labels_clean)) {
    if (r_pattern == "^r\\d{3}$") {
      idx_t <- str_replace(summary_table$polozka_index,
                           "^r(\\d)(\\d{2})$", "r0\\10\\2")
      summary_table <- summary_table |>
        mutate(label_result = labels_clean$label_result[
                                match(idx_t, labels_clean$polozka)]) |>
        relocate(label_result, .after = polozka_index)
    } else {
      summary_table <- summary_table |>
        mutate(label_result = labels_clean$label_result[
                                match(polozka_index, labels_clean$polozka)]) |>
        relocate(label_result, .after = polozka_index)
    }
  }

  summary_table
}


# ------------------------------------------------------------------------------
# 9. make_pam_strange_behaviour()
# Příznaky podezřelého chování na úrovni proměnných.
#
# INPUT:
#   summary_table      – výstup ze summarise_pam_variables()
#   max_na_ratio       – práh pro „vysoké" NA (default 0.95)
#   max_outliers_ratio – práh pro „vysoké" outliery (default 0.01)
#
# OUTPUT: tibble s jedním řádkem na proměnnou, seřazený dle závažnosti
# ------------------------------------------------------------------------------

make_pam_strange_behaviour <- function(summary_table,
                                       max_na_ratio       = 0.95,
                                       max_outliers_ratio = 0.01) {

  has_freq     <- "frekvence_mereni"  %in% names(summary_table)
  has_coverage <- "mereni_ok_ratio"   %in% names(summary_table)
  has_label    <- "label_result"      %in% names(summary_table)
  has_dmr      <- "diffmean_range"    %in% names(summary_table)

  summary_table |>
    mutate(
      not_negative               = !is_negative,
      is_integer                 = count_notintegers == 0,
      empty_variable             = na_ratio == 1,
      without_na                 = na_ratio == 0,
      high_na_ratio              = na_ratio >= max_na_ratio,
      one_unique_value           = count_uniques <= 1,
      significant_diffmean_range = if (has_dmr)
        if_else(diffmean_range < 0.001, FALSE,
                diffmean_range >= quantile(diffmean_range, probs = 0.95, na.rm = TRUE))
      else NA,
      greater_outliers_ratio     = !is.na(outliers_ratio) & outliers_ratio > max_outliers_ratio,
      existing_label             = if (has_label) (!is.na(label_result) & label_result != "") else NA,
      inconsistent_mereni        = if (has_coverage) {
        if (has_freq) {
          frekvence_mereni != "nepravidelná" & !is.na(mereni_ok_ratio) & mereni_ok_ratio < 1
        } else {
          !is.na(mereni_ok_ratio) & mereni_ok_ratio < 1
        }
      } else NA,
      count_serious_errors = as.integer(empty_variable) +
                             as.integer(!existing_label %in% c(TRUE, NA)) +
                             as.integer(inconsistent_mereni %in% TRUE),
      count_strange_behaviour = as.integer(!not_negative) +
                                as.integer(!is_integer) +
                                as.integer(!without_na) +
                                as.integer(high_na_ratio) +
                                as.integer(one_unique_value) +
                                as.integer(significant_diffmean_range %in% TRUE) +
                                as.integer(greater_outliers_ratio)
    ) |>
    select(
      polozka_index,
      any_of("label_result"),
      any_of("frekvence_mereni"),
      any_of("mereni_ok_ratio"),
      count_serious_errors,
      count_strange_behaviour,
      not_negative,
      is_integer,
      empty_variable,
      without_na,
      high_na_ratio,
      one_unique_value,
      any_of("significant_diffmean_range"),
      greater_outliers_ratio,
      any_of("existing_label"),
      any_of("inconsistent_mereni")
    ) |>
    arrange(desc(count_serious_errors), desc(count_strange_behaviour), polozka_index)
}


# ------------------------------------------------------------------------------
# 10. make_pam_examples()
# Konkrétní příklady problematických hodnot z dat.
# Typy problémů: negative_value, non_integer_value, upper_outlier, missing_mereni
#
# INPUT:
#   data                  – datový rámec
#   id_cols               – identifikátorové sloupce
#   summary_table         – výstup ze summarise_pam_variables()
#   coverage_table        – výstup z check_mereni_coverage() (nebo NULL)
#   r_pattern             – regex pro r-kové proměnné
#   max_examples_per_type – max počet příkladů na typ problému
# ------------------------------------------------------------------------------

make_pam_examples <- function(data, id_cols, summary_table,
                              coverage_table        = NULL,
                              r_pattern             = "^r\\d{3}$",
                              max_examples_per_type = 100) {

  r_cols        <- names(data)[str_detect(names(data), r_pattern)]
  valid_id_cols <- id_cols[id_cols %in% names(data)]

  long_data <- data |>
    select(all_of(valid_id_cols), all_of(r_cols)) |>
    pivot_longer(cols      = all_of(r_cols),
                 names_to  = "polozka_index",
                 values_to = "value") |>
    mutate(value_num = suppressWarnings(as.numeric(value))) |>
    left_join(summary_table |> select(polozka_index, outlier_limit),
              by = "polozka_index")

  # --- NAs_example: počet NA per rok (jen v měřených rocích) ---
  nas_tbl <- long_data |>
    group_by(polozka_index, rok) |>
    mutate(rok_has_value = any(!is.na(value_num))) |>
    filter(rok_has_value, is.na(value_num)) |>
    count(polozka_index, rok, name = "n_na") |>
    arrange(polozka_index, rok) |>
    group_by(polozka_index) |>
    summarise(NAs_example = paste(paste0("v roce ", rok, ": ", n_na), collapse = ", "),
              .groups = "drop")

  # --- outliers_per_year: počet outlierů per rok ---
  outl_year_tbl <- long_data |>
    filter(!is.na(outlier_limit), !is.na(value_num), value_num > outlier_limit) |>
    count(polozka_index, rok, name = "n_outl") |>
    arrange(polozka_index, rok) |>
    group_by(polozka_index) |>
    summarise(outliers_per_year = paste(paste0("v roce ", rok, ": ", n_outl), collapse = ", "),
              .groups = "drop")

  # --- outliers_per_facility: red_izo s > 5 outl, jen nejvyšší úroveň ---
  outl_fac_tbl <- if ("red_izo" %in% valid_id_cols) {
    long_data |>
      filter(!is.na(outlier_limit), !is.na(value_num), value_num > outlier_limit) |>
      count(polozka_index, red_izo, name = "n_outl") |>
      filter(n_outl > 5) |>
      group_by(polozka_index) |>
      filter(n_outl == max(n_outl)) |>
      summarise(
        outliers_per_facility = paste0("Po ", max(n_outl), " outl: ",
                                       paste(sort(red_izo), collapse = ", ")),
        .groups = "drop"
      )
  } else tibble(polozka_index = character(), outliers_per_facility = character())

  # --- inconsistent_mereni: chybějící měsíce per rok ---
  inkonz_tbl <- if (!is.null(coverage_table) && nrow(coverage_table) > 0) {
    coverage_table |>
      filter(!mereni_ok, nchar(chybejici_mesice) > 0) |>
      arrange(polozka_index, rok) |>
      group_by(polozka_index) |>
      summarise(
        inconsistent_mereni = paste(
          paste0("v roce ", rok, " chybí měsíce ",
                 str_replace_all(chybejici_mesice, ",", ", ")),
          collapse = "; "),
        .groups = "drop"
      )
  } else tibble(polozka_index = character(), inconsistent_mereni = character())

  # --- wide tabulka: jeden řádek per proměnná ---
  summary_table |>
    select(polozka_index) |>
    left_join(nas_tbl,       by = "polozka_index") |>
    left_join(outl_year_tbl, by = "polozka_index") |>
    left_join(outl_fac_tbl,  by = "polozka_index") |>
    left_join(inkonz_tbl,    by = "polozka_index") |>
    filter(!is.na(NAs_example) | !is.na(outliers_per_year) |
           !is.na(outliers_per_facility) | !is.na(inconsistent_mereni))
}


# ------------------------------------------------------------------------------
# 11. write_pam_validation_xlsx()
# Uložení validačního reportu do .xlsx souboru.
#
# INPUT:
#   report   – list s prvky $overview, $freq_table, $coverage, $summary,
#              $strange_behaviour, $examples
#   path_out – cesta k výstupnímu .xlsx souboru
# ------------------------------------------------------------------------------

write_pam_validation_xlsx <- function(report, path_out) {

  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "Report overview")
  sec_style <- createStyle(textDecoration = "bold")

  # --- předpočítané hodnoty ---
  nm_vykaz <- str_match(basename(path_out), "^(.+?)_validace")[, 2]
  vykaz    <- if (!is.na(nm_vykaz)) {
    x <- str_replace(nm_vykaz, "_", "-")
    paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  } else ""

  max_years <- if (!is.null(report$summary) && "count_years" %in% names(report$summary))
    max(report$summary$count_years, na.rm = TRUE) else NA_integer_

  n_neg <- if (!is.null(report$summary) && "is_negative" %in% names(report$summary))
    sum(report$summary$is_negative, na.rm = TRUE) else NA_integer_

  n_empty_lbl <- if (!is.null(report$strange_behaviour) &&
                     "existing_label" %in% names(report$strange_behaviour))
    sum(report$strange_behaviour$existing_label %in% FALSE) else NA_integer_

  exist_inkonz <- if (!is.null(report$strange_behaviour) &&
                      "inconsistent_mereni" %in% names(report$strange_behaviour))
    any(report$strange_behaviour$inconsistent_mereni, na.rm = TRUE) else NA

  tbl_typy <- if (!is.null(report$summary) && "typ_promenne" %in% names(report$summary))
    report$summary |> count(typ_promenne, name = "n")
  else tibble(typ_promenne = character(), n = integer())

  tbl_freq <- if (!is.null(report$summary) && "frekvence_mereni" %in% names(report$summary))
    report$summary |>
      mutate(frekvence_mereni = if_else(is.na(frekvence_mereni), "neměřeno", frekvence_mereni)) |>
      count(frekvence_mereni, name = "n")
  else tibble(frekvence_mereni = character(), n = integer())

  dupl_ex <- report$overview$duplicate_examples

  # pomocná funkce pro zápis bold nadpisu bez záhlaví sloupců
  wr_header <- function(label, row) {
    writeData(wb, "Report overview", tibble(x = label), startRow = row, colNames = FALSE)
    addStyle(wb, "Report overview", sec_style, rows = row, cols = 1)
    row + 1L
  }

  r <- 1L

  # Název výkazu
  r <- wr_header("Název výkazu", r)
  writeData(wb, "Report overview", tibble(x = vykaz), startRow = r, colNames = FALSE)
  r <- r + 2L

  # Typy proměnných
  r <- wr_header("Typy proměnných", r)
  writeData(wb, "Report overview", tbl_typy, startRow = r, colNames = FALSE)
  r <- r + nrow(tbl_typy) + 1L

  # Frekvence měření
  r <- wr_header("Frekvence měření", r)
  writeData(wb, "Report overview", tbl_freq, startRow = r, colNames = FALSE)
  r <- r + nrow(tbl_freq) + 1L

  # Metriky (label | hodnota)
  kv <- tibble(
    label = c("Počet let", "Počet řádků",
              "Počet záporných proměnných", "Počet prázdných labelů",
              "Existence nekonzistentního měření",
              "Existence duplicit", "Počet duplicit"),
    value = c(as.character(max_years), as.character(report$overview$n_rows),
              as.character(n_neg), as.character(n_empty_lbl),
              as.character(exist_inkonz),
              as.character(report$overview$existence_of_duplicates),
              as.character(report$overview$n_duplicate_keys))
  )
  writeData(wb, "Report overview", kv, startRow = r, colNames = FALSE)
  r <- r + nrow(kv) + 1L

  # Příklady duplicit
  r <- wr_header("Příklady duplicit", r)
  if (!is.null(dupl_ex) && nrow(dupl_ex) > 0) {
    writeData(wb, "Report overview", dupl_ex, startRow = r, colNames = TRUE)
  } else {
    writeData(wb, "Report overview", tibble(x = "žádné duplicity"),
              startRow = r, colNames = FALSE)
  }

  coverage_wide_cols <- if (!is.null(report$coverage) && nrow(report$coverage) > 0) {
    report$coverage |>
      select(polozka_index, rok, mereni_ok) |>
      pivot_wider(names_from = rok, values_from = mereni_ok, names_prefix = "year")
  } else {
    tibble(polozka_index = character())
  }

  summary_out <- report$summary |>
    select(-any_of(c("count_notintegers", "outlier_limit",
                     "mereni_ok_ratio", "roky_s_merenims"))) |>
    left_join(coverage_wide_cols, by = "polozka_index") |>
    select(
      any_of(c("polozka_index", "typ_promenne", "count_years", "count_mes",
               "frekvence_mereni", "na_ratio", "zeros_ratio",
               "is_negative",
               "count_uniques_diff", "min_diff", "max_diff", "diffmean_range",
               "outliers_ratio_diff",
               "label_result", "first_year", "last_year")),
      starts_with("year")
    )

  addWorksheet(wb, sheetName = "Summary")
  writeData(wb, "Summary", summary_out)

  # --- příprava Strange behaviour ---
  sb <- report$strange_behaviour

  # 2) count_years ze summary
  if (!is.null(report$summary) && "count_years" %in% names(report$summary))
    sb <- sb |> left_join(report$summary |> select(polozka_index, count_years),
                          by = "polozka_index")

  # 4) u prázdných proměnných (count_years == 0): NA na is_integer, one_unique_value,
  #    greater_outliers_ratio; mereni_ok_ratio = 0
  if ("count_years" %in% names(sb)) {
    empty_mask <- !is.na(sb$count_years) & sb$count_years == 0
    if ("empty_variable" %in% names(sb))  sb$empty_variable[empty_mask]        <- TRUE
    for (cn in intersect(c("is_integer", "one_unique_value", "greater_outliers_ratio"), names(sb)))
      sb[[cn]][empty_mask] <- NA
    if ("mereni_ok_ratio" %in% names(sb)) sb$mereni_ok_ratio[empty_mask]        <- 0
  }

  # 3) relocate mereni_ok_ratio, count_serious_errors, count_strange_behaviour,
  #    count_years, label_result na konec
  sb <- sb |>
    select(
      polozka_index,
      any_of("frekvence_mereni"),
      any_of(c("not_negative", "is_integer", "empty_variable", "without_na",
               "high_na_ratio", "one_unique_value", "significant_diffmean_range",
               "greater_outliers_ratio", "existing_label", "inconsistent_mereni")),
      any_of(c("mereni_ok_ratio", "count_serious_errors", "count_strange_behaviour",
               "count_years", "label_result"))
    )

  addWorksheet(wb, sheetName = "Strange behaviour")
  writeData(wb, "Strange behaviour", sb)

  addWorksheet(wb, sheetName = "Example of strange behaviour")
  writeData(wb, "Example of strange behaviour", report$examples)

  header_style <- createStyle(textDecoration = "bold", wrapText = TRUE,
                              halign = "left", valign = "center")
  pct_style    <- createStyle(numFmt = "0.00%")
  num_style    <- createStyle(numFmt = "# ##0")
  dec2_style   <- createStyle(numFmt = "# ##0.00")
  dec3_style   <- createStyle(numFmt = "# ##0.000")

  for (sheet in names(wb)) {
    dat    <- suppressWarnings(readWorkbook(wb, sheet = sheet))
    n_cols <- ncol(dat)
    n_rows <- nrow(dat) + 1

    col_widths <- sapply(seq_len(n_cols), function(i) {
      header_w <- nchar(names(dat)[i])
      vals     <- dat[[i]]
      vals_w   <- if (length(vals) > 0) max(nchar(as.character(vals[!is.na(vals)])), 0) else 0
      min(max(header_w, vals_w, 8) + 2, 60)
    })

    if (sheet != "Report overview") addFilter(wb, sheet, rows = 1, cols = 1:n_cols)
    addStyle(wb, sheet, header_style, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
    setColWidths(wb, sheet, cols = 1:n_cols, widths = col_widths)

    if (sheet == "Strange behaviour" && n_rows > 1) {
      pct_sb <- which(names(dat) %in% "mereni_ok_ratio")
      if (length(pct_sb) > 0)
        addStyle(wb, sheet, pct_style, rows = 2:n_rows, cols = pct_sb, gridExpand = TRUE)

      # 5) skrýt sloupce kde nenastane žádný problém (ignorují se NA)
      no_prob <- list(not_negative = TRUE, is_integer = TRUE, empty_variable = FALSE,
                      without_na = TRUE, high_na_ratio = FALSE, one_unique_value = FALSE,
                      significant_diffmean_range = FALSE, greater_outliers_ratio = FALSE,
                      existing_label = TRUE, inconsistent_mereni = FALSE, mereni_ok_ratio = 1)
      hide_idx <- integer(0)
      for (cn in names(no_prob)) {
        ci <- which(names(dat) == cn)
        if (length(ci) == 0) next
        v <- dat[[cn]][!is.na(dat[[cn]])]
        if (length(v) == 0 || all(v == no_prob[[cn]])) hide_idx <- c(hide_idx, ci)
      }
      if (length(hide_idx) > 0)
        setColWidths(wb, sheet, cols = hide_idx, widths = col_widths[hide_idx], hidden = TRUE)
    }

    if (sheet == "Summary" && n_rows > 1) {
      pct_cols <- which(names(dat) %in% c("na_ratio", "zeros_ratio",
                                           "mereni_ok_ratio", "outliers_ratio_diff"))
      if (length(pct_cols) > 0)
        addStyle(wb, sheet, pct_style, rows = 2:n_rows, cols = pct_cols, gridExpand = TRUE)

      num_cols <- which(names(dat) %in% c("count_years", "count_mes",
                                           "count_uniques_diff"))

      dec3_cols <- which(names(dat) %in% "diffmean_range")
      if (length(dec3_cols) > 0)
        addStyle(wb, sheet, dec3_style, rows = 2:n_rows, cols = dec3_cols, gridExpand = TRUE)
      if (length(num_cols) > 0)
        addStyle(wb, sheet, num_style, rows = 2:n_rows, cols = num_cols, gridExpand = TRUE)

      dec2_cols <- which(names(dat) %in% c("min_diff", "max_diff"))
      if (length(dec2_cols) > 0)
        addStyle(wb, sheet, dec2_style, rows = 2:n_rows, cols = dec2_cols, gridExpand = TRUE)
    }
  }

  saveWorkbook(wb, path_out, overwrite = TRUE)
  message("Report uložen: ", path_out)
  invisible(path_out)
}


# ------------------------------------------------------------------------------
# 12. validace_pam_1_04()
# Hlavní orchestrační funkce PAM validace.
#
# PARAMETRY:
#   data               – datový rámec s PAM daty (id_cols + r-kové proměnné)
#   labels_clean       – výstup prep_pam_labels() nebo NULL
#   path_out           – cesta k výstupnímu .xlsx (nebo NULL = jen list)
#   id_cols            – sloupce tvořící klíč řádku
#   r_pattern          – regex identifikující r-kové proměnné
#   outlier_multiplier – násobek IQR pro outlier mez
#   max_na_ratio       – práh „vysokého" NA podílu
#   max_outliers_ratio – práh „vysokého" outlier podílu
#   verbose            – vypisovat průběžné zprávy
#
# VÝSTUP: list $overview | $freq_table | $coverage | $summary |
#              $strange_behaviour | $examples
# ------------------------------------------------------------------------------

validace_pam_1_04 <- function(
    data,
    labels_clean       = NULL,
    path_out           = NULL,
    id_cols            = c("rok", "mes", "red_izo", "hosp_druh", "plat_rad", "druh_pam"),
    r_pattern          = "^r\\d{3}$",
    outlier_multiplier = 3,
    max_na_ratio       = 0.95,
    max_outliers_ratio = 0.01,
    max_rok            = as.integer(format(Sys.Date(), "%Y")) - 1L,
    verbose            = TRUE
) {

  log <- function(...) if (verbose) message(...)

  missing_cols <- setdiff(c(id_cols, "rok", "mes"), names(data))
  if (length(missing_cols) > 0)
    stop("V datech chybí sloupce: ", paste(missing_cols, collapse = ", "))

  r_cols <- names(data)[str_detect(names(data), r_pattern)]
  if (length(r_cols) == 0)
    stop("V datech nebyla nalezena žádná r-ková proměnná odpovídající vzoru: ", r_pattern)

  if (!is.null(max_rok)) {
    rok_int <- as.integer(data$rok)
    effective_max_rok <- if (all(rok_int < 100, na.rm = TRUE) && max_rok >= 100) {
      max_rok %% 100
    } else {
      max_rok
    }
    data <- data |> filter(as.integer(rok) <= effective_max_rok)
    log("  Filtrováno na roky ≤ ", effective_max_rok, " (", nrow(data), " řádků)")
  }

  log("\n── Krok 1: Kontrola struktury dat ──────────────────────────────")
  overview <- check_pam_structure(data, id_cols = id_cols, r_pattern = r_pattern)
  log("  Řádků: ", overview$n_rows,
      " | r-kových proměnných: ", overview$n_r_variables,
      " | Duplicitní klíče: ", overview$n_duplicate_keys)

  log("\n── Krok 2: Detekce frekvence měření ────────────────────────────")
  freq_table <- detect_mereni_frequency(data, r_pattern = r_pattern)
  log(freq_table |> count(frekvence_mereni) |>
        mutate(txt = paste0(frekvence_mereni, ": ", n)) |>
        pull(txt) |> paste(collapse = " | "))

  log("\n── Krok 3: Kontrola pokrytí měření ─────────────────────────────")
  coverage <- check_mereni_coverage(data, freq_table = freq_table, r_pattern = r_pattern)
  if (nrow(coverage) > 0) {
    log(sprintf("  %.1f %% rok×proměnná kombinací má kompletní pokrytí.",
                mean(coverage$mereni_ok, na.rm = TRUE) * 100))
  }

  log("\n── Krok 4: Výpočet souhrnných statistik ────────────────────────")
  summary_table <- summarise_pam_variables(
    data               = data,
    id_cols            = id_cols,
    labels_clean       = labels_clean,
    freq_table         = freq_table,
    coverage_table     = coverage,
    r_pattern          = r_pattern,
    outlier_multiplier = outlier_multiplier
  )
  log("  Hotovo – ", nrow(summary_table), " proměnných zpracováno.")

  log("\n── Krok 5: Identifikace podezřelého chování ────────────────────")
  strange_behaviour <- make_pam_strange_behaviour(
    summary_table,
    max_na_ratio       = max_na_ratio,
    max_outliers_ratio = max_outliers_ratio
  )
  log("  Závažné chyby (count_serious_errors > 0): ",
      sum(strange_behaviour$count_serious_errors > 0, na.rm = TRUE), " proměnných.")
  log("  Měkká varování (count_strange_behaviour > 0): ",
      sum(strange_behaviour$count_strange_behaviour > 0, na.rm = TRUE), " proměnných.")

  log("\n── Krok 6: Sestavení příkladů problémů ─────────────────────────")
  examples <- make_pam_examples(
    data           = data,
    id_cols        = id_cols,
    summary_table  = summary_table,
    coverage_table = coverage,
    r_pattern      = r_pattern
  )
  log("  Příklady sestaveny: ", nrow(examples), " záznamů.")

  report <- list(
    overview          = overview,
    freq_table        = freq_table,
    coverage          = coverage,
    summary           = summary_table,
    strange_behaviour = strange_behaviour,
    examples          = examples
  )

  if (!is.null(path_out)) {
    log("\n── Krok 7: Export do .xlsx ──────────────────────────────────────")
    write_pam_validation_xlsx(report, path_out)
  }

  log("\n✓ Validace dokončena.\n")
  invisible(report)
}
