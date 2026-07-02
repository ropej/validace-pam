# ==============================================================================
# validace_PAM_funkce.R
# Pomocné funkce pro validaci PAM výkazů (P1-04)
#
# Struktura:
#   0. clean_cyclic_zeros()        – čištění cyklicky se vyskytujících nul
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
#  12. validace_pam()         – hlavní orchestrační funkce
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. clean_cyclic_zeros()
# Převede nuly na NA v měsících, které jsou VŠUDE (100%) nula
# pro danou r-proměnnou (měsíc je "neměřen")
# ------------------------------------------------------------------------------

clean_cyclic_zeros <- function(data, r_pattern = "^r\\d{3}$", max_rok = NULL, effective_max_rok = NULL) {

  r_cols <- names(data)[str_detect(names(data), r_pattern)]

  if (length(r_cols) == 0) {
    return(list(data = data, report = NULL))
  }

  # Pro každou r-proměnnou a měsíc: zkontroluj, zda jsou VŠECHNY hodnoty nula
  # Ignoruj řádky s mes = NA a data mimo max_rok limit
  zero_months <- map_dfr(r_cols, function(var) {
    data_filt <- data |>
      filter(!is.na(mes)) |>
      filter(if (!is.null(effective_max_rok)) as.integer(rok) <= effective_max_rok else TRUE)
    vals <- suppressWarnings(as.numeric(data_filt[[var]]))

    data_filt |>
      mutate(!!var := vals) |>
      group_by(mes) |>
      summarise(
        all_zero = all(is.na(!!sym(var)) | !!sym(var) == 0),
        n_values = sum(!is.na(!!sym(var))),
        .groups = "drop"
      ) |>
      filter(all_zero, n_values > 0) |>
      mutate(proměnná = var)
  })

  if (nrow(zero_months) == 0) {
    return(list(data = data, report = tibble(proměnná = character(), měsíc = character())))
  }

  # Převeď nuly na NA pro identifikované měsíce
  data_clean <- data |>
    mutate(across(all_of(r_cols), ~ {
      v <- suppressWarnings(as.numeric(.))
      ifelse(is.na(v), NA, v)
    }))

  for (i in seq_len(nrow(zero_months))) {
    var <- zero_months$proměnná[i]
    month <- zero_months$mes[i]

    mask <- !is.na(data_clean$mes) & data_clean$mes == month
    v <- suppressWarnings(as.numeric(data_clean[[var]]))
    v[mask & v == 0] <- NA
    data_clean[[var]] <- v
  }

  report <- zero_months |>
    select(proměnná, měsíc = mes) |>
    arrange(proměnná, měsíc) |>
    mutate(měsíc = as.character(měsíc))

  list(data = data_clean, report = report)
}

# ------------------------------------------------------------------------------
# 1. is_integer_tol()
# Kontrola, zda jsou numerické hodnoty celočíselné (s tolerancí na float)
# ------------------------------------------------------------------------------

is_integer_tol <- function(x, tol = .Machine$double.eps^0.5) {
  if (!is.numeric(x)) return(rep(FALSE, length(x)))
  is.na(x) | abs(x - round(x)) < tol
}


# ------------------------------------------------------------------------------
# 2. upper_outlier_limit() / lower_outlier_limit()
# Výpočet hranic outlierů pomocí IQR pravidla.
# upper: ignoruje nuly a NA (nuly jsou strukturální, ne datová hodnota).
# lower: ignoruje jen NA – nula v diferenci znamená „žádná změna" a musí
#        vstoupit do výpočtu kvantilů jako platný referenční bod.
# ------------------------------------------------------------------------------

upper_outlier_limit <- function(x, multiplier = 3) {
  x <- x[!is.na(x) & x != 0]
  if (length(x) < 2) return(NA_real_)
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q)
  as.numeric(q[2] + multiplier * iqr)
}

lower_outlier_limit <- function(x, multiplier = 3) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(q)
  as.numeric(q[1] - multiplier * iqr)
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
# 4. prep_pam_labels() + oddil_to_code()
# Načtení a příprava labelů proměnných z Excel souboru
# oddil_to_code(): převod oddílu P1c (římská číslice) na dvouciferný kód
# ------------------------------------------------------------------------------

clean_excel_text <- function(x) {
  x |>
    stringr::str_replace_all("[[:cntrl:]]", " ") |>
    stringr::str_squish()
}

oddil_to_code <- function(oddil) {
  roman_vals <- c(I=1L, II=2L, III=3L, IV=4L, V=5L, VI=6L,
                  VII=7L, VIII=8L, IX=9L, X=10L)
  suffix     <- str_extract(oddil, "[a-z]+$")
  has_suffix <- !is.na(suffix)
  suffix[!has_suffix] <- ""
  base <- str_remove(oddil, "[a-z]+$")
  num  <- roman_vals[base]
  ifelse(is.na(num), NA_character_,
         ifelse(has_suffix,
                paste0(num, suffix),
                str_pad(num, 2L, pad = "0")))
}

prep_pam_labels <- function(path_labels, r_pattern = "^r\\d{1}") {
  readxl::read_excel(path_labels) |>
    filter(str_detect(polozka_index, r_pattern)) |>
    mutate(across(everything(), as.character)) |>
    rename(label_result = "zkr") |>
    mutate(label_result = clean_excel_text(label_result)) |>
    filter(!if_all(everything(), is.na)) |>
    distinct(polozka, .keep_all = TRUE) |>
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

  n_rows_na_mes <- if ("mes" %in% names(data)) sum(is.na(data$mes)) else 0

  list(
    n_rows                  = nrow(data),
    n_cols                  = ncol(data),
    n_rows_with_na_mes      = n_rows_na_mes,
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
# Logika: modus počtu měřených měsíců za rok → kategorie.
#
# Kategorie (na základě modus_mesicu_za_rok):
#   "měsíční"      … 12 měsíců/rok
#   "čtvrtletní"   … 4 měsíce/rok (březen, červen, září, prosinec)
#   "pololetní"    … 2 měsíců/rok (březen, prosinec)
#   "roční"        … 1 měsíc/rok (prosinec)
#   "nepravidelná" … jinak (2–3, 5+ měsíců/rok, atd.)
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

  mes_modus <- long |>
    filter(!is.na(hodnota_num)) |>
    group_by(polozka_index) |>
    summarise(
      mes_modus = str_pad(as.character(round(modus_or_median(as.numeric(mes)))),
                          2, pad = "0"),
      .groups = "drop"
    )

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
    left_join(mes_modus, by = "polozka_index") |>
    mutate(
      frekvence_mereni = case_when(
        is.na(modus_mesicu_za_rok)  ~ "neměřeno",
        modus_mesicu_za_rok == 12   ~ "měsíční",
        modus_mesicu_za_rok == 4    ~ "čtvrtletní",
        modus_mesicu_za_rok == 2    ~ "pololetní",
        modus_mesicu_za_rok == 1    ~ "roční",
        TRUE                        ~ "nepravidelná"
      ),
      ocekavane_mesice = pmap(list(frekvence_mereni, mes_modus), function(freq, mm) {
        switch(freq,
          "měsíční"    = str_pad(as.character(1:12), width = 2, pad = "0"),
          "čtvrtletní" = c("03", "06", "09", "12"),
          "pololetní"  = c("06", "12"),
          "roční"      = if (is.na(mm)) "12" else mm,
          integer(0)
        )
      })
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

  # Proměnné s definovatelnou frekvencí (lze kontrolovat pokrytí)
  freq_kontrolovane <- freq_table |>
    filter(map_int(ocekavane_mesice, length) > 0) |>
    distinct(polozka_index, .keep_all = TRUE)

  # Proměnné s nedefinovatelnou frekvencí (nepravidelné)
  freq_nepravidelne <- freq_table |>
    filter(map_int(ocekavane_mesice, length) == 0) |>
    distinct(polozka_index, .keep_all = TRUE)

  # Pokud nejsou žádné kontrolovatelné proměnné, vrátit jen nepravidelné
  if (nrow(freq_kontrolovane) == 0) {
    if (nrow(freq_nepravidelne) == 0) {
      message("Žádná proměnná nemá definovatelnou frekvenci – coverage přeskočena.")
      return(tibble())
    }
    # Vrátit jen nepravidelné s NA mereni_ok
    return(
      freq_nepravidelne |>
        select(polozka_index, frekvence_mereni) |>
        crossing(rok = unique(data$rok)) |>
        mutate(merene_mesice = NA_character_,
               chybejici_mesice = NA_character_,
               mereni_ok = NA)
    )
  }

  # Pro všechny proměnné (kontrolované + nepravidelné)
  long_all <- data |>
    select(rok, mes, all_of(c(freq_kontrolovane$polozka_index, freq_nepravidelne$polozka_index))) |>
    pivot_longer(cols = all_of(c(freq_kontrolovane$polozka_index, freq_nepravidelne$polozka_index)),
                 names_to  = "polozka_index",
                 values_to = "hodnota") |>
    mutate(hodnota_num = suppressWarnings(as.numeric(hodnota))) |>
    filter(!is.na(hodnota_num))

  # Jen pro kontrolované (k podrobné kontrole)
  long <- long_all |>
    filter(polozka_index %in% freq_kontrolovane$polozka_index)

  merene <- long_all |>
    group_by(polozka_index, rok) |>
    summarise(merene_mesice = list(sort(unique(mes))), .groups = "drop")

  freq_info <- freq_kontrolovane |>
    select(polozka_index, frekvence_mereni, ocekavane_mesice)

  coverage_kontrolovane <- merene |>
    left_join(freq_info, by = "polozka_index") |>
    filter(!is.na(frekvence_mereni)) |>
    mutate(
      chybejici_mesice = map2(ocekavane_mesice, merene_mesice, ~ setdiff(.x, .y)),
      mereni_ok = map_lgl(chybejici_mesice, ~ length(.x) == 0),
      merene_mesice = map_chr(merene_mesice, ~ paste(.x, collapse = ",")),
      chybejici_mesice = map_chr(chybejici_mesice,
                                 ~ if (length(.x) == 0) "" else paste(.x, collapse = ","))
    ) |>
    select(polozka_index, frekvence_mereni, rok, merene_mesice, chybejici_mesice, mereni_ok)

  # Přidat nepravidelné proměnné - převzít merene_mesice z long_all, ale bez kontroly pokrytí
  if (nrow(freq_nepravidelne) > 0) {
    nepravidelne_merene <- merene |>
      filter(polozka_index %in% freq_nepravidelne$polozka_index) |>
      mutate(merene_mesice = map_chr(merene_mesice, ~ paste(.x, collapse = ",")))

    coverage_nepravidelne <- nepravidelne_merene |>
      left_join(freq_nepravidelne |> select(polozka_index, frekvence_mereni),
                by = "polozka_index") |>
      mutate(
        chybejici_mesice = NA_character_,
        mereni_ok = NA
      ) |>
      select(polozka_index, frekvence_mereni, rok, merene_mesice, chybejici_mesice, mereni_ok)

    bind_rows(coverage_kontrolovane, coverage_nepravidelne) |>
      arrange(polozka_index, rok)
  } else {
    coverage_kontrolovane
  }
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
                                    outlier_multiplier = 3,
                                    vykaz              = NULL,
                                    oddil              = NULL,
                                    facility_id        = "ico") {

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
      count_notintegers   = sum(!is_integer_tol(x_num), na.rm = TRUE),
      outlier_limit_upper = out_lim,
      outliers_ratio      = if (is.na(out_lim)) NA_real_
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
  diff_static_cols <- intersect(c("hosp_druh", "plat_rad", "druh_pam"), names(data))
  if (facility_id %in% names(data)) {
    diff_static_cols <- c(facility_id, diff_static_cols)
  }
  sort_by_mes      <- "mes" %in% names(data)

  vars_for_diff <- if ("frekvence_mereni" %in% names(summary_table)) {
    summary_table |>
      filter(!is.na(frekvence_mereni),
             !frekvence_mereni %in% c("roční", "nepravidelná")) |>
      pull(polozka_index)
  }  else {
    character(0)
    }

  # Skupiny jsou bez roku → přelom roku (prosinec→leden) je platná diference,
  # NA dostane jen první hodnota celé časové řady dané skupiny.
  diff_data <- if (length(vars_for_diff) > 0) {
    sel_all   <- unique(c("rok", diff_static_cols, if (sort_by_mes) "mes", vars_for_diff))
    sort_cols <- c(diff_static_cols, "rok", if (sort_by_mes) "mes")
    data |>
      select(all_of(sel_all)) |>
      arrange(across(all_of(sort_cols))) |>
      group_by(across(all_of(diff_static_cols))) |>
      mutate(across(all_of(vars_for_diff), ~ {
        v <- suppressWarnings(as.numeric(.))
        c(NA, diff(v))
      })) |>
      ungroup()
  } else NULL

  if (!is.null(diff_data)) {
    diff_stats <- map_dfr(vars_for_diff, \(var) {
      diffs_num   <- diff_data[[var]]
      diffs_noNA  <- diffs_num[!is.na(diffs_num)]
      out_upper   <- upper_outlier_limit(diffs_num, multiplier = outlier_multiplier)
      out_lower   <- lower_outlier_limit(diffs_num, multiplier = outlier_multiplier)
      mask_d      <- !is.na(diffs_num)
      n_valid     <- sum(mask_d)

      tibble(
        polozka_index       = var,
        count_uniques_diff  = n_distinct(diffs_num, na.rm = TRUE),
        min_diff            = if (length(diffs_noNA) > 0) min(diffs_noNA) else NA_real_,
        max_diff            = if (length(diffs_noNA) > 0) max(diffs_noNA) else NA_real_,
        outlier_limit_lower = out_lower,
        outliers_ratio_diff = if (n_valid == 0) NA_real_ else {
          up <- if (!is.na(out_upper)) diffs_num[mask_d] > out_upper else rep(FALSE, n_valid)
          lo <- if (!is.na(out_lower)) diffs_num[mask_d] < out_lower else rep(FALSE, n_valid)
          mean(up | lo, na.rm = TRUE)
        }
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

  # --- Statistiky pro roční proměnné (z raw hodnot, bez transformace na diference) ---
  vars_annual <- if ("frekvence_mereni" %in% names(summary_table))
    summary_table |> filter(frekvence_mereni == "roční") |> pull(polozka_index)
  else character(0)

  if (length(vars_annual) > 0) {
    annual_raw_stats <- map_dfr(vars_annual, \(var) {
      x_num  <- suppressWarnings(as.numeric(data[[var]]))
      x_noNA <- x_num[!is.na(x_num)]
      out_lim <- upper_outlier_limit(x_num, multiplier = outlier_multiplier)
      mask    <- !is.na(x_num)
      tibble(
        polozka_index       = var,
        count_uniques_diff  = n_distinct(x_num, na.rm = TRUE),
        min_diff            = if (length(x_noNA) > 0) min(x_noNA) else NA_real_,
        max_diff            = if (length(x_noNA) > 0) max(x_noNA) else NA_real_,
        outliers_ratio_diff = if (is.na(out_lim)) NA_real_
                              else mean(x_num[mask] > out_lim, na.rm = TRUE)
      )
    })

    dmr_ann <- var_range_diffmeans(
      data |> select(rok, all_of(vars_annual)) |> mutate(across(-rok, as.numeric))
    )
    annual_raw_stats <- annual_raw_stats |>
      left_join(tibble(polozka_index = names(dmr_ann),
                       diffmean_range = as.numeric(dmr_ann)),
                by = "polozka_index")

    if ("count_uniques_diff" %in% names(summary_table)) {
      summary_table <- summary_table |>
        rows_patch(annual_raw_stats, by = "polozka_index", unmatched = "ignore")
    } else {
      summary_table <- summary_table |>
        left_join(annual_raw_stats, by = "polozka_index")
    }
  }

  if (!is.null(labels_clean)) {
    vykaz_val <- vykaz
    lbl <- if (!is.null(vykaz_val) && "vykaz" %in% names(labels_clean))
      labels_clean |> filter(vykaz == vykaz_val)
    else
      labels_clean

    label_keys <- if (!is.null(oddil)) {
      code <- oddil_to_code(oddil)
      # Odseknout suffix .P1C (nebo jakýkoli suffix po tečce)
      polozka_clean <- str_remove(summary_table$polozka_index, "\\..*$")
      paste0("r", code, "0", str_extract(polozka_clean, "\\d{2}$"))
    } else if (r_pattern == "^r\\d{3}$") {
      str_replace(summary_table$polozka_index, "^r(\\d)(\\d{2})$", "r0\\10\\2")
    } else {
      summary_table$polozka_index
    }

    summary_table <- summary_table |>
      mutate(label_result = lbl$label_result[match(label_keys, lbl$polozka)]) |>
      relocate(label_result, .after = polozka_index)
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
      greater_outliers_ratio     = if ("outliers_ratio_diff" %in% names(summary_table))
        !is.na(outliers_ratio_diff) & outliers_ratio_diff > max_outliers_ratio
      else
        !is.na(outliers_ratio) & outliers_ratio > max_outliers_ratio,
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
                              max_examples_per_type = 100,
                              facility_id           = "ico") {

  r_cols        <- names(data)[str_detect(names(data), r_pattern)]
  valid_id_cols <- id_cols[id_cols %in% names(data)]

  fac_cols  <- if (facility_id %in% names(data)) facility_id else character(0)
  long_data <- data |>
    select(all_of(valid_id_cols), all_of(fac_cols), all_of(r_cols)) |>
    pivot_longer(cols      = all_of(r_cols),
                 names_to  = "polozka_index",
                 values_to = "value") |>
    mutate(value_num = suppressWarnings(as.numeric(value))) |>
    left_join(summary_table |> select(polozka_index, outlier_limit_upper),
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
    filter(!is.na(outlier_limit_upper), !is.na(value_num), value_num > outlier_limit_upper) |>
    count(polozka_index, rok, name = "n_outl") |>
    arrange(polozka_index, rok) |>
    group_by(polozka_index) |>
    summarise(outliers_per_year = paste(paste0("v roce ", rok, ": ", n_outl), collapse = ", "),
              .groups = "drop")

  # --- outliers_per_facility: ico s > 5 outl, jen nejvyšší úroveň ---
  outl_fac_tbl <- if (facility_id %in% names(data)) {
    long_data |>
      filter(!is.na(outlier_limit_upper), !is.na(value_num), value_num > outlier_limit_upper) |>
      count(polozka_index, .data[[facility_id]], name = "n_outl") |>
      filter(n_outl > 5) |>
      group_by(polozka_index) |>
      slice_max(n_outl, with_ties = TRUE) |>
      summarise(
        outliers_per_facility = paste0("Po ", first(n_outl), " outl: ",
                                       paste(sort(.data[[facility_id]]), collapse = ", ")),
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

  n_empty_fac <- if (!is.null(report$n_empty_facility_id)) report$n_empty_facility_id else NA_integer_

  # Počet chybějících secondary facility_id
  secondary_labels <- character()
  secondary_values <- character()

  if (!is.null(report$secondary_facilities) && length(report$secondary_facilities) > 0) {
    for (i in seq_along(report$secondary_facilities)) {
      sec_id <- report$secondary_facilities[[i]]
      n_empty <- report$secondary_empty_counts[i]

      if (!is.na(n_empty)) {
        pct <- round(n_empty / report$overview$n_rows * 100, 2)
        secondary_labels <- c(secondary_labels, paste0("Počet chybějících ", sec_id))
        secondary_values <- c(secondary_values, sprintf("%d (%.1f %%)", n_empty, pct))
      }
    }
  }

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
  r <- r + 1L

  # Report vytvořen
  writeData(wb, "Report overview", tibble(x = paste0("Report vytvořen: ", format(Sys.time(), "%d.%m.%Y v %H:%M"))),
            startRow = r, colNames = FALSE)
  r <- r + 1L

  if (!is.null(report$max_rok_filter)) {
    writeData(wb, "Report overview",
              tibble(x = paste0("Filtrováno na roky ≤ ", report$max_rok_filter)),
              startRow = r, colNames = FALSE)
    r <- r + 1L
  }
  r <- r + 1L

  # Typy proměnných
  r <- wr_header("Typy proměnných", r)
  writeData(wb, "Report overview", tbl_typy, startRow = r, colNames = FALSE)
  r <- r + nrow(tbl_typy) + 1L

  # Frekvence měření
  r <- wr_header("Frekvence měření", r)
  writeData(wb, "Report overview", tbl_freq, startRow = r, colNames = FALSE)
  r <- r + nrow(tbl_freq) + 1L

  # Metriky (label | hodnota)
  # Výskyt závažných chyb a zvláštního chování
  sb <- report$strange_behaviour
  n_total_vars <- nrow(sb)

  # Závažné chyby — počet proměnných s právě N chybami
  serious_by_count <- table(sb$count_serious_errors[sb$count_serious_errors > 0])
  kv_extra_labels <- character()
  kv_extra_values <- character()

  if (length(serious_by_count) > 0) {
    first <- TRUE
    for (n_chyb in sort(as.numeric(names(serious_by_count)), decreasing = TRUE)) {
      n_vars <- serious_by_count[as.character(n_chyb)]
      if (n_chyb == 1) label_chyb <- "chyba" else label_chyb <- "chyby"
      if (first) {
        kv_extra_labels <- c(kv_extra_labels, "Výskyt závažných chyb")
        first <- FALSE
      } else {
        kv_extra_labels <- c(kv_extra_labels, "")
      }
      kv_extra_values <- c(kv_extra_values, paste0(n_vars, " / ", n_total_vars, " (", n_chyb, " ", label_chyb, ")"))
    }
  }

  # Zvláštní chování — počet proměnných s právě N anomáliemi
  strange_by_count <- table(sb$count_strange_behaviour[sb$count_strange_behaviour > 0])

  if (length(strange_by_count) > 0) {
    first <- TRUE
    for (n_anom in sort(as.numeric(names(strange_by_count)), decreasing = TRUE)) {
      n_vars <- strange_by_count[as.character(n_anom)]
      if (n_anom == 1) label_anom <- "anomálie" else label_anom <- "anomálie"
      if (first) {
        kv_extra_labels <- c(kv_extra_labels, "Výskyt zvláštního chování")
        first <- FALSE
      } else {
        kv_extra_labels <- c(kv_extra_labels, "")
      }
      kv_extra_values <- c(kv_extra_values, paste0(n_vars, " / ", n_total_vars, " (", n_anom, " ", label_anom, ")"))
    }
  }

  cyclic_zeros_detected <- if (!is.null(report$cyclic_zeros_detected)) report$cyclic_zeros_detected else FALSE

  kv <- tibble(
    label = c("Identifikátor zařízení",
              "Počet let", "Počet řádků",
              if (report$overview$n_rows_with_na_mes > 0) "Počet řádků s mes=NA" else "",
              kv_extra_labels,
              "Počet záporných proměnných", paste0("Počet chybějících ", report$facility_id),
              secondary_labels,
              "Počet prázdných labelů",
              "Existence cyklických nul",
              "Existence nekonzistentního měření",
              "Existence duplicit", "Počet duplicit"),
    value = c(if (!is.null(report$facility_id)) report$facility_id else "",
              as.character(max_years), as.character(report$overview$n_rows),
              if (report$overview$n_rows_with_na_mes > 0) as.character(report$overview$n_rows_with_na_mes) else "",
              kv_extra_values,
              as.character(n_neg),
              if (!is.na(n_empty_fac)) sprintf("%d (%.1f %%)", n_empty_fac, round(n_empty_fac / report$overview$n_rows * 100, 2)) else "",
              secondary_values,
              as.character(n_empty_lbl),
              as.character(cyclic_zeros_detected),
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
    r <- r + nrow(dupl_ex) + 2L
  } else {
    writeData(wb, "Report overview", tibble(x = "žádné duplicity"),
              startRow = r, colNames = FALSE)
    r <- r + 2L
  }

  # Zařízení s nejvíce outlierů
  tof <- report$top_outlier_facilities
  r <- wr_header("Zařízení s nejvíce outlierů", r)
  if (!is.null(tof) && nrow(tof) > 0) {
    writeData(wb, "Report overview", tof, startRow = r, colNames = FALSE)
  } else {
    writeData(wb, "Report overview", tibble(x = "žádná zařízení s outlierů"),
              startRow = r, colNames = FALSE)
  }

  coverage_wide_cols <- if (!is.null(report$coverage) && nrow(report$coverage) > 0) {
    report$coverage |>
      select(polozka_index, rok, mereni_ok) |>
      pivot_wider(names_from = rok, values_from = mereni_ok, names_prefix = "year")
  } else {
    tibble(polozka_index = character())
  }

  # Přidat merene_mesice z coverage (distinct)
  merene_mesice_col <- if (!is.null(report$coverage) && nrow(report$coverage) > 0) {
    report$coverage |>
      select(polozka_index, merene_mesice) |>
      distinct(polozka_index, .keep_all = TRUE)
  } else {
    tibble(polozka_index = character())
  }

  summary_out <- report$summary |>
    select(-any_of(c("count_notintegers", "outlier_limit_upper", "outlier_limit_lower",
                     "mereni_ok_ratio", "roky_s_merenims"))) |>
    left_join(coverage_wide_cols, by = "polozka_index") |>
    left_join(merene_mesice_col, by = "polozka_index") |>
    select(
      any_of(c("polozka_index", "typ_promenne", "count_years", "count_mes",
               "frekvence_mereni", "merene_mesice", "na_ratio", "zeros_ratio",
               "is_negative",
               "count_uniques_diff", "min_diff", "max_diff", "diffmean_range",
               "outliers_ratio_diff",
               "label_result", "first_year", "last_year")),
      starts_with("year")
    ) |>
    (\(x) if (!is.null(report$freq_table) && nrow(report$freq_table) > 0 &&
               all(report$freq_table$frekvence_mereni == "roční", na.rm = TRUE))
        rename_with(x, ~ str_remove(.x, "_diff$"), matches("_diff$"))
      else x)()

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
  right_style  <- createStyle(halign = "right")
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
    if (sheet == "Report overview")
      addStyle(wb, sheet, right_style, rows = 1:200, cols = 2, gridExpand = TRUE)
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

    if (sheet == "Example of strange behaviour" && n_rows > 1) {
      wrap_top_style <- createStyle(wrapText = TRUE, valign = "top")
      addStyle(wb, sheet, wrap_top_style, rows = 2:n_rows, cols = 1:n_cols, gridExpand = TRUE)
      idx_first <- which(names(dat) == "polozka_index")
      if (length(idx_first) == 0) idx_first <- 1L
      other_cols <- setdiff(seq_len(n_cols), idx_first)
      setColWidths(wb, sheet, cols = idx_first,  widths = 14.67)
      if (length(other_cols) > 0)
        setColWidths(wb, sheet, cols = other_cols, widths = 41)
      empty_cols <- which(sapply(seq_len(n_cols), function(i) all(is.na(dat[[i]]))))
      if (length(empty_cols) > 0) {
        empty_widths <- ifelse(empty_cols == idx_first, 14.67, 41)
        setColWidths(wb, sheet, cols = empty_cols, widths = empty_widths, hidden = TRUE)
      }
    }

    if (sheet == "Summary" && n_rows > 1) {
      pct_cols <- which(names(dat) %in% c("na_ratio", "zeros_ratio", "mereni_ok_ratio",
                                           "outliers_ratio", "outliers_ratio_diff"))
      if (length(pct_cols) > 0)
        addStyle(wb, sheet, pct_style, rows = 2:n_rows, cols = pct_cols, gridExpand = TRUE)

      num_cols <- which(names(dat) %in% c("count_years", "count_mes",
                                           "count_uniques", "count_uniques_diff",
                                           "min", "min_diff", "max", "max_diff"))

      dec3_cols <- which(names(dat) %in% "diffmean_range")
      if (length(dec3_cols) > 0)
        addStyle(wb, sheet, dec3_style, rows = 2:n_rows, cols = dec3_cols, gridExpand = TRUE)
      if (length(num_cols) > 0)
        addStyle(wb, sheet, num_style, rows = 2:n_rows, cols = num_cols, gridExpand = TRUE)
    }

  }

  saveWorkbook(wb, path_out, overwrite = TRUE)
  message("Report uložen: ", path_out)
  invisible(path_out)
}


# ------------------------------------------------------------------------------
# 12. validace_pam()
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


# ------------------------------------------------------------------------------
# porovnej_pam_reporty(): srovná dvě várky PaM validačních xlsx reportů.
#   Analogie compare_data z DM (validace_dat.R) – oba reporty se čtou z disku.
# VSTUP:
#   path_control_xlsx – cesta ke kontrolnímu (staršímu) reportu .xlsx
#   path_test_xlsx    – cesta k testovanému (novému) reportu .xlsx
# VÝSTUP: list
#   $overview_compare – neshody v metrikách listu "Report overview"
#   $summary_compare  – neshody po proměnných (polozka_index × sledovaná hodnota)
#                       z listů "Summary" + "Strange behaviour"
#   $count_neshod     – počet neshod (overview / summary)
# ------------------------------------------------------------------------------
porovnej_pam_reporty <- function(path_control_xlsx, path_test_xlsx) {

  # bezpečné načtení listu (prázdný tibble, když list/soubor chybí)
  nacti_list <- function(path, sheet) {
    if (!file.exists(path) || !sheet %in% readxl::excel_sheets(path)) return(tibble())
    suppressMessages(readxl::read_excel(path, sheet = sheet))
  }

  # Summary + Strange behaviour spojené podle polozka_index do jedné tabulky
  nacti_promenne <- function(path) {
    s  <- nacti_list(path, "Summary")
    sb <- nacti_list(path, "Strange behaviour")
    if (nrow(s) == 0 && nrow(sb) == 0) return(tibble(polozka_index = character()))
    # ze SB zahoď sloupce duplicitní se Summary (count_years, label_result, …) kromě klíče
    if (nrow(sb) > 0 && nrow(s) > 0)
      sb <- sb |> select(-any_of(setdiff(intersect(names(sb), names(s)), "polozka_index")))
    if (nrow(s) > 0 && nrow(sb) > 0)      full_join(s, sb, by = "polozka_index")
    else if (nrow(s) > 0)                 s
    else                                  sb
  }

  # metriky (label | hodnota) z listu "Report overview" – jen whitelist kv řádků
  ov_labels <- c("Identifikátor zařízení", "Počet let", "Počet řádků",
                 "Počet záporných proměnných", "Počet nevyplněných identifikátorů",
                 "Počet prázdných labelů", "Existence nekonzistentního měření",
                 "Existence duplicit", "Počet duplicit")
  nacti_overview <- function(path) {
    if (!file.exists(path) || !"Report overview" %in% readxl::excel_sheets(path))
      return(tibble(variable = character(), value = character()))
    df <- suppressMessages(readxl::read_excel(path, sheet = "Report overview", col_names = FALSE))
    if (ncol(df) < 2) return(tibble(variable = character(), value = character()))
    names(df)[1:2] <- c("variable", "value")
    df |>
      transmute(variable = as.character(variable), value = as.character(value)) |>
      filter(variable %in% ov_labels) |>
      bind_rows(
        # Agreguj "Výskyt závažných chyb" a "Výskyt zvláštního chování" — sečti počty
        {
          df_all <- df |> transmute(variable = as.character(variable), value = as.character(value))

          # Závažné chyby
          serious_rows <- df_all |> filter(str_detect(variable, "^Výskyt závažných chyb"))
          if (nrow(serious_rows) > 0) {
            serious_sum <- sum(as.numeric(str_extract(serious_rows$value, "^\\d+")), na.rm = TRUE)
            serious_agg <- tibble(variable = "Výskyt závažných chyb",
                                 value = as.character(serious_sum))
          } else {
            serious_agg <- tibble(variable = character(), value = character())
          }

          # Zvláštní chování
          strange_rows <- df_all |> filter(str_detect(variable, "^Výskyt zvláštního chování"))
          if (nrow(strange_rows) > 0) {
            strange_sum <- sum(as.numeric(str_extract(strange_rows$value, "^\\d+")), na.rm = TRUE)
            strange_agg <- tibble(variable = "Výskyt zvláštního chování",
                                 value = as.character(strange_sum))
          } else {
            strange_agg <- tibble(variable = character(), value = character())
          }

          bind_rows(serious_agg, strange_agg)
        }
      )
  }

  # --- 1. overview compare ----------------------------------------------------
  overview_compare <- full_join(
    nacti_overview(path_control_xlsx) |> rename(value_control = value),
    nacti_overview(path_test_xlsx)    |> rename(value_test    = value),
    by = "variable"
  ) |>
    mutate(shoda = replace_na(value_control, "") == replace_na(value_test, "")) |>
    filter(!shoda) |>
    select(variable, value_control, value_test)

  # --- 2. summary + strange behaviour compare (po polozka_index) -------------
  prom_c <- nacti_promenne(path_control_xlsx) |>
    mutate(polozka_index = str_remove(polozka_index, "\\..*$"))
  prom_t <- nacti_promenne(path_test_xlsx) |>
    mutate(polozka_index = str_remove(polozka_index, "\\..*$"))

  # porovnávej jen sloupce přítomné v OBOU reportech (jinak by sloupec navíc
  # hlásil neshodu u každé proměnné)
  spolecne <- union("polozka_index", intersect(names(prom_c), names(prom_t)))
  prom_c <- prom_c |> select(any_of(spolecne))
  prom_t <- prom_t |> select(any_of(spolecne))

  # existence proměnné v obou várkách
  exist <- full_join(
    prom_c |> distinct(polozka_index) |> mutate(v_control = TRUE),
    prom_t |> distinct(polozka_index) |> mutate(v_test    = TRUE),
    by = "polozka_index"
  )

  long <- function(df) {
    if (nrow(df) == 0)
      return(tibble(polozka_index = character(), variable = character(), value = character()))
    df |>
      mutate(across(everything(), as.character)) |>
      pivot_longer(-polozka_index, names_to = "variable", values_to = "value")
  }

  # Nejdřív porovnání proměnných, které jsou v obou várkách
  summary_compare_shared <- full_join(
    long(prom_c) |> rename(value_control = value),
    long(prom_t) |> rename(value_test    = value),
    by = c("polozka_index", "variable")
  ) |>
    semi_join(exist |> filter(v_control & v_test), by = "polozka_index") |>
    mutate(shoda = case_when(
      is.na(value_control) & is.na(value_test) ~ TRUE,
      value_control == value_test              ~ TRUE,
      TRUE                                     ~ FALSE
    )) |>
    filter(!shoda) |>
    select(polozka_index, variable, value_control, value_test)

  # Pak proměnné, které jsou jen v jedné várce
  summary_compare_only <- exist |>
    filter(!(v_control & v_test)) |>
    mutate(
      value_control = case_when(is.na(v_test)    ~ "jen v control", TRUE ~ NA_character_),
      value_test    = case_when(is.na(v_control) ~ "jen v test",    TRUE ~ NA_character_)
    ) |>
    transmute(polozka_index,
              variable      = "exist",
              value_control,
              value_test)

  summary_compare <- bind_rows(summary_compare_shared, summary_compare_only) |>
    arrange(polozka_index, variable)

  list(
    overview_compare = overview_compare,
    summary_compare  = summary_compare,
    count_neshod     = tibble(cast = c("overview", "summary"),
                              n    = c(nrow(overview_compare), nrow(summary_compare)))
  )
}


# ------------------------------------------------------------------------------
# porovnej_pam_varky(): dávkové srovnání DVOU SLOŽEK s hotovými xlsx reporty.
#   Nic se nevaliduje znovu – jen se spárují a porovnají existující reporty.
# VSTUP:
#   path_test    – složka s novou (testovanou) várkou reportů
#   path_control – složka s kontrolní (starší) várkou reportů
# VÝSTUP: list
#   $overview_compare / $summary_compare – slepené neshody přes všechny výkazy
#                                          (se sloupcem `vykaz`)
#   $count_neshod – počty neshod po výkazech + řádek CELKEM
# ------------------------------------------------------------------------------
porovnej_pam_varky <- function(path_test, path_control) {
  if (!dir.exists(path_test))    stop("path_test neexistuje: ", path_test)
  if (!dir.exists(path_control)) stop("path_control neexistuje: ", path_control)

  bez_zamku <- function(x) x[!str_starts(basename(x), fixed("~$"))]  # vynech dočasné excel zámky
  test_files <- bez_zamku(list.files(path_test, pattern = "\\.xlsx$",
                                     full.names = TRUE, recursive = TRUE))
  control_files <- bez_zamku(list.files(path_control, pattern = "\\.xlsx$",
                                        full.names = TRUE, recursive = TRUE))
  if (length(test_files) == 0) stop("Ve složce 'path_test' nejsou žádné .xlsx reporty.")

  # Extrahuj steamy (názvy výkazů bez data a bez _validace)
  test_stems <- sub("_validace_\\d{6}\\.xlsx$", "", basename(test_files))
  control_stems <- sub("_validace_\\d{6}\\.xlsx$", "", basename(control_files))

  vysledky <- purrr::map(test_files, function(tf) {
    stem <- sub("_validace_\\d{6}\\.xlsx$", "", basename(tf))
    kand <- bez_zamku(list.files(path_control,
                                 pattern = paste0("^", stem, ".*\\.xlsx$"),
                                 full.names = TRUE, recursive = TRUE))
    if (length(kand) == 0) {
      message("⚠ '", stem, "': kontrolní report nenalezen – přeskočeno.")
      return(NULL)
    }
    cf <- kand[which.max(file.info(kand)$mtime)]  # nejnovější odpovídající
    list(vykaz = stem, cmp = porovnej_pam_reporty(cf, tf))
  }) |> purrr::compact()

  # Nové výkazy (v test ale ne v control)
  nove <- setdiff(test_stems, control_stems)

  # Ztracené výkazy (v control ale ne v test)
  ztrucene <- setdiff(control_stems, test_stems)

  # Tabulka chybějících/nových výkazů
  missing_vykaz <- tibble(
    vykaz = c(nove, ztrucene),
    status = c(rep("jen v test", length(nove)), rep("jen v control", length(ztrucene)))
  )

  if (length(vysledky) == 0 && nrow(missing_vykaz) == 0) {
    message("Nenalezeny žádné spárované reporty.")
    return(list())
  }

  slep <- function(slot) purrr::map_dfr(vysledky, function(x) {
    d <- x$cmp[[slot]]
    if (nrow(d) > 0) mutate(d, vykaz = x$vykaz, .before = 1) else NULL
  })

  count_neshod <- purrr::map_dfr(vysledky, function(x)
    tibble(vykaz    = x$vykaz,
           overview = nrow(x$cmp$overview_compare),
           summary  = nrow(x$cmp$summary_compare)))
  count_neshod <- bind_rows(
    count_neshod,
    tibble(vykaz = "CELKEM",
           overview = sum(count_neshod$overview),
           summary  = sum(count_neshod$summary))
  )

  result <- list(
    overview_compare = slep("overview_compare"),
    summary_compare  = slep("summary_compare"),
    count_neshod     = count_neshod
  )

  if (nrow(missing_vykaz) > 0) {
    result$missing_vykaz <- missing_vykaz
  }

  result
}


# ------------------------------------------------------------------------------
# format_summary_compare_wide()
# Konvertuje summary_compare z long do wide formátu se zkrácenými labely
# INPUT: summary_compare (long formát)
# OUTPUT: wide tibble (polozka_index na řádku, metriky ve sloupcích)
# ------------------------------------------------------------------------------

format_summary_compare_wide <- function(summary_compare) {
  if (nrow(summary_compare) == 0) return(tibble())

  # Vyloučit label_result a not_negative (zachovat jen is_negative z Summary)
  compare_long <- summary_compare |>
    filter(!variable %in% c("label_result", "not_negative")) |>
    mutate(
      val_c_num = suppressWarnings(as.numeric(value_control)),
      val_t_num = suppressWarnings(as.numeric(value_test)),
      is_numeric = !is.na(val_c_num) & !is.na(val_t_num),
      pct_change = if_else(
        is_numeric,
        abs((val_t_num - val_c_num) / pmax(abs(val_c_num), 0.0001)) * 100,
        NA_real_
      ),
      direction = if_else(is_numeric & val_t_num > val_c_num, "vyšší ", "nižší "),
      stars = case_when(
        !is_numeric ~ NA_character_,
        pct_change < 5 ~ "*",
        pct_change < 20 ~ "**",
        TRUE ~ "***"
      ),
      display_value = as.character(if_else(
        variable == "exist",
        paste(value_control, "/", value_test),
        if_else(
          is.na(value_control),
          paste("(nový) →", value_test),
          if_else(
            is.na(value_test),
            paste(value_control, "→ (chybí)"),
            if_else(
              is_numeric & val_c_num == val_t_num,
              "(stejný)",
              if_else(
                is_numeric,
                paste0(direction, stars),
                if_else(
                  value_control == value_test,
                  "(stejný)",
                  paste(value_control, "→", value_test)
                )
              )
            )
          )
        )
      ))
    ) |>
    select(vykaz, polozka_index, variable, display_value)

  result <- compare_long |>
    pivot_wider(
      id_cols = c(vykaz, polozka_index),
      names_from = variable,
      values_from = display_value
    ) |>
    arrange(vykaz, polozka_index)

  # Seřaď sloupce podle logického pořadí (jako ve zdrojových xlsx)
  col_order <- c(
    "vykaz", "polozka_index",
    # Ze Summary
    "typ_promenne", "count_years", "count_mes", "frekvence_mereni",
    "na_ratio", "zeros_ratio", "is_negative",
    "count_uniques_diff", "min_diff", "max_diff", "diffmean_range", "outliers_ratio_diff",
    "first_year", "last_year",
    # Ze Strange behaviour
    "is_integer", "empty_variable", "without_na", "high_na_ratio", "one_unique_value",
    "significant_diffmean_range", "greater_outliers_ratio",
    "existing_label", "inconsistent_mereni", "mereni_ok_ratio",
    "count_serious_errors", "count_strange_behaviour",
    "exist"
  )

  # Vyber jen sloupce, které existují
  col_order <- col_order[col_order %in% names(result)]
  result |> select(all_of(col_order))
}


# ------------------------------------------------------------------------------
# write_pam_compare_xlsx()
# Uložení compare výsledků do Excel reportu
# INPUT: compare list (výstup porovnej_pam_reporty), path_out
# OUTPUT: .xlsx soubor se dvěma listy (Overview, Summary)
# ------------------------------------------------------------------------------

write_pam_compare_xlsx <- function(compare, path_out) {
  wb <- createWorkbook()

  # --- List 1: Overview (s vykaz sloupcem) ---
  addWorksheet(wb, sheetName = "Overview")

  overview_data <- compare$overview_compare
  if (nrow(overview_data) > 0) {
    overview_data <- overview_data |>
      mutate(
        display_value = case_when(
          is.na(value_control) ~ paste0("(nový) → ", value_test),
          is.na(value_test) ~ paste0(value_control, " → (chybí)"),
          TRUE ~ paste0(value_control, " → ", value_test)
        )
      ) |>
      select(vykaz, variable, display_value) |>
      rename(Metrika = variable, "Control → Test" = display_value)
  } else {
    overview_data <- tibble(vykaz = character(), Metrika = character(), "Control → Test" = character())
  }

  # Přidej missing_vykaz pokud existuje (jen výkazy začínající na "p1")
  if (!is.null(compare$missing_vykaz) && nrow(compare$missing_vykaz) > 0) {
    missing_display <- compare$missing_vykaz |>
      filter(str_detect(vykaz, "^p1")) |>
      mutate(Metrika = vykaz, "Control → Test" = status) |>
      select(vykaz, Metrika, "Control → Test")
    if (nrow(missing_display) > 0) {
      overview_data <- bind_rows(overview_data, missing_display)
    }
  }

  writeData(wb, "Overview", overview_data)

  # --- Listy 2-4: Summary dle výkazů ---
  summary_wide <- format_summary_compare_wide(compare$summary_compare)
  if (nrow(summary_wide) > 0) {
    # Vynechej výkazy, které nezačínají na "p1"
    summary_wide <- summary_wide |>
      filter(str_detect(vykaz, "^p1")) |>
      arrange(vykaz, polozka_index)
  } else {
    summary_wide <- tibble(vykaz = character(), polozka_index = character())
  }

  # Vytvoř 3 listy dle výkazů (jen když nejsou prázdné)
  vykaz_sheets <- list(
    list(pattern = "^p1_", sheet_name = "Summary P1-04"),
    list(pattern = "^p1a", sheet_name = "Summary P1a"),
    list(pattern = "^p1c", sheet_name = "Summary P1c")
  )

  created_summary_sheets <- list()

  for (sheet_config in vykaz_sheets) {
    sheet_data <- summary_wide |> filter(str_detect(vykaz, sheet_config$pattern))

    # Ulož list jen pokud není prázdný
    if (nrow(sheet_data) > 0) {
      # Vynechej prázdné sloupce (všechny NA)
      sheet_data <- sheet_data |>
        select(where(~!all(is.na(.x))))

      addWorksheet(wb, sheetName = sheet_config$sheet_name)
      writeData(wb, sheet_config$sheet_name, sheet_data)
      created_summary_sheets[[sheet_config$sheet_name]] <- list(
        n_rows = nrow(sheet_data),
        n_cols = ncol(sheet_data)
      )
    }
  }

  # --- Formátování ---
  header_style <- createStyle(textDecoration = "bold")
  wrap_style <- createStyle(wrapText = TRUE, valign = "top")

  # Overview formátování
  n_cols <- ncol(overview_data)
  if (n_cols > 0) {
    addStyle(wb, "Overview", header_style, rows = 1, cols = 1:n_cols, gridExpand = FALSE)
    addFilter(wb, "Overview", row = 1, cols = 1:n_cols)
  }
  setColWidths(wb, "Overview", cols = 1, widths = 15)
  setColWidths(wb, "Overview", cols = 2, widths = 30)
  setColWidths(wb, "Overview", cols = 3, widths = 40)

  # Summary listy formátování (jen pro vytvořené listy)
  for (sheet_name in names(created_summary_sheets)) {
    n_rows <- created_summary_sheets[[sheet_name]]$n_rows
    n_cols <- created_summary_sheets[[sheet_name]]$n_cols

    if (n_cols > 0 && n_rows > 0) {
      addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:n_cols, gridExpand = FALSE)
      addFilter(wb, sheet_name, row = 1, cols = 1:n_cols)

      if (n_rows > 1) {
        addStyle(wb, sheet_name, wrap_style, rows = 2:(n_rows + 1), cols = 1:n_cols,
                 gridExpand = TRUE)
      }
    }
    setColWidths(wb, sheet_name, cols = 1:3, widths = c(12, 20, 15))
  }

  saveWorkbook(wb, path_out, overwrite = TRUE)
}


validace_pam <- function(
    data,
    labels_clean       = NULL,
    path_out           = NULL,
    id_cols            = c("rok", "mes", "hosp_druh", "plat_rad", "druh_pam"),
    r_pattern          = "^r\\d{3}$",
    facility_id        = "ico",
    outlier_multiplier = 3,
    max_na_ratio       = 0.95,
    max_outliers_ratio = 0.01,
    max_rok            = as.integer(format(Sys.Date(), "%Y")) - 1L,
    verbose            = TRUE,
    vykaz              = NULL,
    oddil              = NULL,
    clean_zeros        = TRUE,    # T/F: čistit cyklické nuly (100% nula per měsíc)
    compare_data       = FALSE,   # T/F: srovnat tento report s kontrolní várkou
    path_control       = NULL     # složka s kontrolními (staršími) xlsx reporty
) {

  log <- function(...) if (verbose) message(...)

  # Compute effective_max_rok EARLY (needed for clean_cyclic_zeros)
  effective_max_rok <- NULL
  if (!is.null(max_rok)) {
    rok_int <- as.integer(data$rok)
    effective_max_rok <- if (all(rok_int < 100, na.rm = TRUE) && max_rok >= 100) {
      max_rok %% 100
    } else {
      max_rok
    }
  }

  # --- Čištění cyklických nul (měsíce 100% nula = neměřeny) ---
  cyclic_zeros_detected <- FALSE
  log("\n[DEBUG] Podmínka cyklických nul: clean_zeros=", clean_zeros, ", 'mes' v datech=", "mes" %in% names(data))
  if (clean_zeros && "mes" %in% names(data)) {
    clean_result <- clean_cyclic_zeros(data, r_pattern = r_pattern, max_rok = max_rok, effective_max_rok = effective_max_rok)
    data <- clean_result$data
    if (!is.null(clean_result$report) && nrow(clean_result$report) > 0) {
      cyclic_zeros_detected <- TRUE
      log("\n── Čištění cyklických nul ──")
      log("Konvertovány nuly na NA v měsících (100% nula = neměřeno):")
      print(clean_result$report)
    }
  } else {
    log("→ Čištění cyklických nul PŘESKOČENO (P1c bez měsíce)")
  }

  missing_cols <- setdiff(c(id_cols, "rok", "mes"), names(data))
  if (length(missing_cols) > 0)
    stop("V datech chybí sloupce: ", paste(missing_cols, collapse = ", "))

  if ("mes" %in% names(data))
    data <- data |> mutate(mes = str_pad(as.character(mes), width = 2, pad = "0"))

  r_cols <- names(data)[str_detect(names(data), r_pattern)]
  if (length(r_cols) == 0)
    stop("V datech nebyla nalezena žádná r-ková proměnná odpovídající vzoru: ", r_pattern)

  # Počet mes=NA PŘED filtrací roku (aby se zobrazil i když se řádky později filtrují)
  n_rows_na_mes_before_filter <- if ("mes" %in% names(data)) sum(is.na(data$mes)) else 0

  if (!is.null(max_rok)) {
    data <- data |> filter(as.integer(rok) <= effective_max_rok)
    log("  Filtrováno na roky ≤ ", effective_max_rok, " (", nrow(data), " řádků)")
  }

  log("\n── Krok 1: Kontrola struktury dat ──────────────────────────────")
  id_cols_full <- if (facility_id %in% names(data) && !facility_id %in% id_cols) {
    mes_pos <- which(id_cols == "mes")
    if (length(mes_pos) > 0)
      c(id_cols[seq_len(mes_pos)], facility_id, id_cols[-seq_len(mes_pos)])
    else
      c(id_cols, facility_id)
  } else id_cols
  overview <- check_pam_structure(data, id_cols = id_cols_full, r_pattern = r_pattern)
  # Přepis n_rows_with_na_mes na hodnotu PŘED filtrací roku
  overview$n_rows_with_na_mes <- n_rows_na_mes_before_filter
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
    outlier_multiplier = outlier_multiplier,
    vykaz              = vykaz,
    oddil              = oddil,
    facility_id        = facility_id
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
    r_pattern      = r_pattern,
    facility_id    = facility_id
  )
  log("  Příklady sestaveny: ", nrow(examples), " záznamů.")

  top_outlier_facilities <- if (facility_id %in% names(data) &&
                                "outlier_limit_upper" %in% names(summary_table)) {
    r_cols_all <- names(data)[str_detect(names(data), r_pattern)]
    data |>
      select(all_of(facility_id), all_of(r_cols_all)) |>
      pivot_longer(cols = all_of(r_cols_all),
                   names_to = "polozka_index", values_to = "v") |>
      mutate(v_num = suppressWarnings(as.numeric(v))) |>
      left_join(summary_table |> select(polozka_index, outlier_limit_upper),
                by = "polozka_index") |>
      filter(!is.na(outlier_limit_upper), !is.na(v_num), v_num > outlier_limit_upper) |>
      count(.data[[facility_id]], name = "n_outl") |>
      arrange(desc(n_outl)) |>
      filter(n_outl >= 5) |>
      slice_head(n = 20)
  } else {
    tibble(!!sym(facility_id) := character(), n_outl = integer())
  }

  n_empty_facility_id <- if (facility_id %in% names(data)) {
    fac <- data[[facility_id]]
    sum(is.na(fac) | fac == "", na.rm = TRUE)
  } else NA_integer_

  # Počet chybějících secondary facility_id
  # Pro rid: kontroluj ico i red_izo
  # Pro ico/red_izo: kontroluj druhý
  secondary_facilities <- if (facility_id == "rid") {
    list(ico = "ico", red_izo = "red_izo")
  } else if (facility_id == "red_izo") {
    list(ico = "ico")
  } else if (facility_id == "ico") {
    list(red_izo = "red_izo")
  } else {
    list()
  }

  secondary_empty_counts <- map_int(secondary_facilities, function(sec_id) {
    if (sec_id %in% names(data)) {
      fac_sec <- data[[sec_id]]
      sum(is.na(fac_sec) | fac_sec == "", na.rm = TRUE)
    } else {
      NA_integer_
    }
  })

  report <- list(
    overview               = overview,
    freq_table             = freq_table,
    coverage               = coverage,
    summary                = summary_table,
    strange_behaviour      = strange_behaviour,
    examples               = examples,
    top_outlier_facilities = top_outlier_facilities,
    max_rok_filter         = effective_max_rok,
    facility_id            = facility_id,
    n_empty_facility_id    = n_empty_facility_id,
    secondary_facilities   = secondary_facilities,
    secondary_empty_counts = secondary_empty_counts,
    cyclic_zeros_detected  = cyclic_zeros_detected
  )

  if (!is.null(path_out)) {
    log("\n── Krok 7: Export do .xlsx ──────────────────────────────────────")
    write_pam_validation_xlsx(report, path_out)
  }

  # ── Krok 8: Srovnání s kontrolní várkou (volitelné) ───────────────────────
  if (compare_data) {
    if (is.null(path_out)) {
      log("  ⚠ Srovnání vyžaduje uložený report (path_out). Přeskočeno.")
    } else if (is.null(path_control) || !dir.exists(path_control)) {
      log("  ⚠ path_control nezadán / neexistuje. Srovnání přeskočeno.")
    } else {
      # kontrolní report = stejný název bez koncovky _YYMMDD (jiné datum várky)
      stem <- sub("_\\d{6}\\.xlsx$", "", basename(path_out))
      kandidati <- list.files(path_control,
                              pattern = paste0("^", stem, ".*\\.xlsx$"),
                              full.names = TRUE, recursive = TRUE)
      kandidati <- setdiff(normalizePath(kandidati), normalizePath(path_out))  # ne sám sebe
      if (length(kandidati) == 0) {
        log("  ⚠ Kontrolní report pro '", stem, "' nenalezen v path_control. Srovnání přeskočeno.")
      } else {
        control_file <- kandidati[which.max(file.info(kandidati)$mtime)]  # nejnovější odpovídající
        log("\n── Krok 8: Srovnání s kontrolním reportem ───────────────────────")
        log("  Kontrolní report: ", basename(control_file))
        report$compare <- porovnej_pam_reporty(control_file, path_out)
        log("  Neshody → overview: ", nrow(report$compare$overview_compare),
            " | summary: ", nrow(report$compare$summary_compare))
      }
    }
  }

  log("\n✓ Validace dokončena.\n")
  invisible(report)
}
