rm(list = ls())

# Nastavit datum
datum_test <- "260612"

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
source("validace_PAM.R")