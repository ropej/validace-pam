# Validace PAM

Automatizovaná validace r-kových proměnných z výkazů personalistiky a mezd (PaM) datového modelu školy MŠMT.

## 📖 Metodika

Kompletní dokumentace je dostupná zde: **https://ropej.github.io/validace-pam/**

Metodika obsahuje:
- Popis vstupních dat a struktur (P1-04, P1a, P1c)
- Kroky validace a detekci frekvence měření
- Popis výstupních XLSX reportů
- Příklady kódu pro jednotlivé výkazy
- Návod na srovnání dvou várek dat

## 🗂️ Obsah repozitáře

```
validace-pam/
├── README.md                                        # Tento soubor
├── validace PaM - metodika.qmd                      # Zdrojový Quarto dokument (metodika)
├── validace_PAM.R                                   # Hlavní skript pro spuštění validace
├── validace_PAM_funkce.R                            # Knihovna funkcí pro validaci
├── docs/
│   ├── index.html                                   # Vyrendrovaná metodika (GitHub Pages)
│   ├── frekvence_manualni_maping.yml                # Ruční mapování frekvencí (szablona)
│   ├── frekvence_manualni_maping_autogenerovana.yml # Automaticky vygenerované mapování
│   └── example_compare_p1c.png                      # Obrázek z metodiky
├── Subanalýzy/
│   └── kontrola_prazdnych_labelu.R                  # Pomocný skript pro analýzy
└── Výkazy-validace/                                 # Výstupní složka (se reporty dle data)
```

### Mapování frekvencí

- **`frekvence_manualni_maping.yml`** – ruční přepisy automatické detekce (když je potřeba opravit či upřesnit)
- **`frekvence_manualni_maping_autogenerovana.yml`** – návrh automaticky vygenerovaný z textových popisů v labelech; funkce `generate_auto_frequency_mapping()` detekuje měsíce měření (březen, červen, září, prosinec) a automaticky agreguje proměnné se stejnými parametry po oddílech (např. všechny r04xxx se stejnou frekvencí → agregace na r04)

## 🚀 Spuštění validace

### Příprava
Aktualizuj datum testované sady v `validace_PAM.R`:
```r
datum_test <- "260612"   # YYMMDD formát
```

### Spuštění
V RStudiu nebo R konzoli:
```r
source("validace_PAM.R")
```

Reporty se vygenerují do složky `Výkazy-validace/<datum_test>/`

## 📊 Výstupní reporty

Pro každý výkaz (P1-04, P1a, oddíly P1c) se vytvoří `.xlsx` soubor se čtyřmi listy:

1. **Report overview** – strukturální přehled dat
2. **Summary** – per-proměnné statistiky
3. **Strange behaviour** – flagy podezřelého chování
4. **Example of strange behaviour** – konkrétní příklady problémů

## 🔄 Srovnání dvou várek

Pro porovnání nové várky s předchozí:
```r
compare_all <- porovnej_pam_varky(
  path_test    = "Výkazy-validace/260612",
  path_control = "Výkazy-validace/260421"
)
```

Výstup: `Porovnani_260612_vs_260421.xlsx`

## 📋 Požadované balíčky

```r
library(arrow)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(purrr)
library(readxl)
library(openxlsx)
```

## 📧 Autor

Romana Pejcalová  
IPs DATA, MŠMT  
romana.pejcalova@msmt.gov.cz

## 📝 Poznámka

Validace PAM slouží jako opakovaně spustitelný nástroj pro kontrolu dat z výkazů personalistiky a mezd. Díky automatické detekci frekvence měření zachytí nestandardní chování i v sub-ročních časových řadách.
