# Extended Mortality Analysis 2005-2023
# Author: Rodrigo Quiroga
# Repository: github.com/rquiroga7/suicidios_0-20-ARGENTINA
# This script generates extended annual plots using all yearly data from 2005-2023

library(pacman)
p_load(httr,readxl,dplyr,ggplot2,scales,ggrepel,viridis,padr,gganimate,wesanderson,readr,forcats,kableExtra,tidyr,mgcv,MASS,lubridate,ggborderline)
Sys.setlocale("LC_ALL","English")

# Import custom functions
source("funciones_analisis.R")

#####################
# LOAD ALL ANNUAL DATA 2005-2023 from defweb files
#####################

# Read all CSV files from the zip archive - directly from zip without extraction
defweb_files <- c("defweb05.csv", "defweb06.csv", "defweb07.csv", "defweb08.csv", "defweb09.csv",
                  "defweb10.csv", "defweb11.csv", "defweb12.csv", "defweb13.csv", "defweb14.csv",
                  "defweb15.csv", "defweb16.csv", "defweb17.csv", "defweb18.csv", "defweb19.csv",
                  "defweb20.csv", "defweb21.csv", "defweb22.csv", "defweb23.csv")

# Initialize empty list to store dataframes
datamort_list <- list()

# Read each file and add year column
for(i in seq_along(defweb_files)) {
  year <- 2004 + i  # 2005 for first file, 2006 for second, etc.

  # Determine separator: semicolon for 2020 onwards, comma for earlier
  sep_char <- if(year >= 2020) ";" else ","

  # Read from zip file directly without extraction
  temp_data <- read_delim(unz("defweb05-23.zip", defweb_files[i]),
                          delim = sep_char,
                          locale = readr::locale(encoding = "latin1"),
                          col_types = cols(
                            PROVRES = col_character(),
                            SEXO = col_character(),
                            CAUSA = col_character(),
                            MAT = col_character(),
                            GRUPEDAD = col_character(),
                            CUENTA = col_double()
                          ),
                          show_col_types = FALSE)

  # Add year column
  temp_data$anio <- year

  # Store in list
  datamort_list[[i]] <- temp_data
}

# Combine all years
datamort_05_23 <- bind_rows(datamort_list)

# Standardize age groups
datamort_05_23 <- datamort_05_23 %>%
  mutate(grupo_etario = case_when(
    grepl("^01_|^02_|^03_|^04_", GRUPEDAD) ~ "0-20",  # 01_Menor de 1 año, 02_1 a 9, 03_10 a 14, 04_15 a 19
    grepl("^05_|^06_|^07_|^08_", GRUPEDAD) ~ "20-39",  # 05_20 a 24, 06_25 a 29, 07_30 a 34, 08_35 a 39
    grepl("^09_|^10_|^11_|^12_", GRUPEDAD) ~ "40-59",  # 09_40 a 44, 10_45 a 49, 11_50 a 54, 12_55 a 59
    grepl("^13_|^14_|^15_|^16_|^17_", GRUPEDAD) ~ "60+",  # 13_60 a 64, 14_65 a 69, 15_70 a 74, 16_75 a 79, 17_80 y más
    TRUE ~ "Desconocido"
  ))

# Standardize sex codes (1=Varones, 2=Mujeres)
datamort_05_23 <- datamort_05_23 %>%
  mutate(sexo_nombre = case_when(
    SEXO == "1" ~ "Varones",
    SEXO == "2" ~ "Mujeres",
    TRUE ~ "Indeterminado"
  ))

# Map ICD-10 codes to cause categories
datamort_05_23 <- datamort_05_23 %>%
  mutate(grupo_causa_defuncion_CIE10 = case_when(
    # COVID-19 U07
    grepl("^U07", CAUSA) ~ "COVID",
    # Suicides X60-X84
    grepl("^X[6-7][0-9]$|^X8[0-4]$", CAUSA) ~ "SUICIDIO",
    # Infectious diseases A00-B99
    grepl("^[AB]", CAUSA) ~ "ENF INFECCIOSAS",
    # Malignant tumors C00-C97
    grepl("^C[0-9]", CAUSA) ~ "TUMORES MALIGNOS",
    # Benign tumors D00-D48
    grepl("^D[0-4]", CAUSA) ~ "TUMORES BENIGNOS",
    # Diabetes E10-E14
    grepl("^E1[0-4]$", CAUSA) ~ "DIABETES",
    # Nutritional deficiencies and anemias D50-D64, E40-E64
    grepl("^D5[0-9]$|^D6[0-4]$|^E[4-6][0-9]$", CAUSA) ~ "ANEMIA O DEF NUTRICIONAL",
    # Metabolic disorders E00-E39, E65-E90 (excluding diabetes and nutritional)
    grepl("^E[0-3]|^E[7-9]", CAUSA) ~ "TRAST METABOLICOS",
    # Meningitis G00-G03
    grepl("^G0[0-3]$", CAUSA) ~ "MENINGITIS",
    # Alzheimer G30
    grepl("^G30$", CAUSA) ~ "ALZHEIMER",
    # Mental disorders F00-F99
    grepl("^F", CAUSA) ~ "TRASTORNOS MENTALES",
    # Circulatory system I00-I99
    grepl("^I", CAUSA) ~ "ENF SIST CIRCULATORIO",
    # Respiratory system J00-J99
    grepl("^J", CAUSA) ~ "ENF SIST RESPIRATORIO",
    # Appendix, hernia, intestinal obstruction K35-K46, K55-K56
    grepl("^K3[5-9]$|^K4[0-6]$|^K5[56]$", CAUSA) ~ "APEND, HERNIA, OBST INSTEST",
    # Liver diseases K70-K77
    grepl("^K7[0-7]$", CAUSA) ~ "ENF SIST HEPATICO",
    # Urinary system N00-N39
    grepl("^N[0-3]", CAUSA) ~ "ENF SIST URINARIO",
    # Pregnancy and childbirth O00-O99
    grepl("^O", CAUSA) ~ "EMBARAZO Y PARTO",
    # Perinatal conditions P00-P96
    grepl("^P", CAUSA) ~ "AFECC PERINATALES",
    # Congenital malformations Q00-Q99
    grepl("^Q", CAUSA) ~ "MALFORMACIONES",
    # External causes V01-Y98 (excluding suicides)
    grepl("^[VWY]", CAUSA) & !grepl("^X[6-7][0-9]$|^X8[0-4]$", CAUSA) ~ "CAUSAS EXTERNAS",
    # Ill-defined causes R00-R99
    grepl("^R", CAUSA) ~ "CAUSAS MAL DEFINIDAS",
    # All other codes fall into "Other defined causes"
    TRUE ~ "DEMAS CAUSAS DEFINIDAS"
  )) %>%
  filter(!is.na(grupo_causa_defuncion_CIE10))  # Remove unmatched causes

# Aggregate to annual totals by age, cause, and sex
datamort_05_23_grupos <- datamort_05_23 %>%
  rename(anio_def = anio) %>%
  group_by(anio_def, grupo_etario, grupo_causa_defuncion_CIE10, sexo_nombre) %>%
  summarise(cantidad = sum(CUENTA, na.rm = TRUE), .groups = "drop")

datamort_05_23_causas <- datamort_05_23 %>%
  rename(anio_def = anio) %>%
  group_by(anio_def, grupo_etario, CAUSA, sexo_nombre) %>%
  summarise(cantidad = sum(CUENTA, na.rm = TRUE), .groups = "drop")

print(paste("Loaded 2005-2023 data:", nrow(datamort_05_23), "rows"))

######################################################
##########EXTENDED ANNUAL PLOTS 2005-2023############
######################################################

# Get unique age group levels (excluding "Desconocido")
etario_levels <- unique(datamort_05_23_grupos$grupo_etario)
etario_levels <- etario_levels[etario_levels != "Desconocido"]

# Generate extended annual plots for all years 2005-2023
for (i in seq_along(etario_levels)) {
 etario_level <- etario_levels[i]

 # Get data for this age group
 subset_data <- datamort_05_23_grupos[datamort_05_23_grupos$grupo_etario == etario_level, ]
 cantidad_anual <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + anio_def, data = subset_data, sum)

 # Filter out ALZHEIMER for young age groups
 if (etario_level == "0-20" || etario_level == "20-39") {
  cantidad_anual <- cantidad_anual %>% filter(grupo_causa_defuncion_CIE10 != "ALZHEIMER")
 }

 # Plot extended series 2005-2023
 plot_annual_simple(cantidad_anual,
                   paste0("plots/",etario_level,"_mortalidad_anual_causas_simple_2005_2023.png"),
                   col_group='grupo_causa_defuncion_CIE10',
                   title=paste0("Fallecidos anuales por Causa 2005-2023, edad = ", etario_level),
                   year_range=c(2005, 2023))

 print(paste("Generated extended plot for age group:", etario_level))
}

print("Extended analysis 2005-2023 completed!")

######################################################
##########PLOTS BY ORIGINAL AGE GROUPS################
######################################################

# Aggregate to annual totals by original GRUPEDAD
datamort_05_23_grupedad <- datamort_05_23 %>%
  rename(anio_def = anio) %>%
  group_by(anio_def, GRUPEDAD, grupo_causa_defuncion_CIE10, sexo_nombre) %>%
  summarise(cantidad = sum(CUENTA, na.rm = TRUE), .groups = "drop")

# Get unique GRUPEDAD levels (excluding unknown/null)
grupedad_levels <- unique(datamort_05_23_grupedad$GRUPEDAD)
grupedad_levels <- grupedad_levels[!is.na(grupedad_levels) & grupedad_levels != "NULL" & grupedad_levels != ""]

# Generate plots for each original age group
for (i in seq_along(grupedad_levels)) {
  grupedad_level <- grupedad_levels[i]
  
  # Get data for this age group
  subset_data <- datamort_05_23_grupedad[datamort_05_23_grupedad$GRUPEDAD == grupedad_level, ]
  cantidad_anual <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + anio_def, data = subset_data, sum)
  
  # Filter out ALZHEIMER for young age groups (01-08)
  if (grepl("^0[1-8]_", grupedad_level)) {
    cantidad_anual <- cantidad_anual %>% filter(grupo_causa_defuncion_CIE10 != "ALZHEIMER")
  }
  
  # Clean filename - extract just the number prefix for cleaner names
  filename_suffix <- gsub("[^0-9a-zA-Z_]", "_", grupedad_level)
  
  # Plot extended series 2005-2023
  plot_annual_simple(cantidad_anual,
                    paste0("plots/grupedad_", filename_suffix, "_mortalidad_anual_2005_2023.png"),
                    col_group='grupo_causa_defuncion_CIE10',
                    title=paste0("Fallecidos anuales por Causa 2005-2023, edad = ", grupedad_level),
                    year_range=c(2005, 2023))
  
  print(paste("Generated plot for GRUPEDAD:", grupedad_level))
}

print("Original age group plots completed!")


#Lets analyze the CAUSA variable, producing a table of the 10 most common yearly death causes for 2019-2023

#####################
top10 <- datamort_05_23_causas %>%
  group_by(anio_def, CAUSA) %>%
  summarise(total = sum(cantidad, na.rm=TRUE), .groups="drop") %>%
  arrange(anio_def, desc(total)) %>%
  group_by(anio_def) %>%
  slice_head(n=10) %>%
  ungroup()

  #Calculate total deaths per year for 2019-2023
total_deaths_yearly <- datamort_05_23_causas %>%
  filter(anio_def >= 2015) %>%
  group_by(anio_def) %>%
  summarise(total = sum(cantidad, na.rm=TRUE), .groups="drop")

#Count deaths per year for the CAUSA U07
top10_U07 <- datamort_05_23_causas %>%
  filter(CAUSA == "U07") %>%
  group_by(anio_def) %>%
  summarise(total = sum(cantidad, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(total))

#Identify causes with increasing trend within the ENF INFECCIOSAS group from 2020-2023
increasing_causes <- datamort_05_23_causas %>%
  filter(anio_def >= 2020) %>%
  filter(grepl("^[AB]", CAUSA)) %>%
  group_by(CAUSA, anio_def) %>%
  summarise(total = sum(cantidad, na.rm=TRUE), .groups="drop") %>%
  arrange(CAUSA, anio_def) %>%
  group_by(CAUSA) %>%
  summarise(trend = cor(anio_def, total), ratio = last(total) / first(total), .groups="drop", muertes_2023 = last(total)) %>%
  filter(ratio > 1.5 & trend > 0.5 & muertes_2023 >= 1000)