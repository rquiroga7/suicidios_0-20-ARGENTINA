# Extended Mortality Analysis 2005-2023
# Author: Rodrigo Quiroga
# Repository: github.com/rquiroga7/suicidios_0-20-ARGENTINA
# This script generates extended annual plots combining 2005-2014 and 2015-2023 data

library(pacman)
p_load(httr,readxl,dplyr,ggplot2,scales,ggrepel,viridis,padr,gganimate,wesanderson,readr,forcats,kableExtra,tidyr,mgcv,MASS,lubridate,ggborderline)
Sys.setlocale("LC_ALL","English")

# Import custom functions
source("funciones_analisis.R")

#####################
# LOAD DATA 2015-2023
#####################

datamort_15_22 <- read_csv("arg_def_m_15_22.zip",
                           skip=0,locale = readr::locale(encoding = "UTF-8") ,guess_max = 250,
)
datamort_23 <- read_csv("base_def_23_men.zip",
                           skip=0,locale = readr::locale(encoding = "UTF-8") ,guess_max = 250,
)
# Standardize grupo_etario to handle encoding differences (años vs anios)
datamort_23$grupo_etario <- gsub("años", "anios", datamort_23$grupo_etario)
datamort_23$grupo_etario <- gsub("más", "mas", datamort_23$grupo_etario)
# Unify age group format (remove extra space after "07.")
datamort_15_22$grupo_etario <- gsub("^07\\. de", "07.de", datamort_15_22$grupo_etario)
datamort_23$grupo_etario <- gsub("^07\\. de", "07.de", datamort_23$grupo_etario)

# Standardize sex column names (masculino/1.Varones -> Varones, femenino/2.Mujeres -> Mujeres)
datamort_15_22$sexo_nombre <- gsub("masculino", "Varones", datamort_15_22$sexo_nombre)
datamort_15_22$sexo_nombre <- gsub("femenino", "Mujeres", datamort_15_22$sexo_nombre)
datamort_23$sexo_nombre <- gsub("1.Varones", "Varones", datamort_23$sexo_nombre, fixed=TRUE)
datamort_23$sexo_nombre <- gsub("2.Mujeres", "Mujeres", datamort_23$sexo_nombre, fixed=TRUE)

#MERGE DATA
datamort_15_23 <- bind_rows(datamort_15_22,datamort_23)

# Standardize and simplify region names
datamort_15_23 <- datamort_15_23 %>%
  mutate(region = case_when(
    grepl("NOA|Noroeste", region, ignore.case = TRUE) ~ "NOA",
    grepl("NEA|Noreste", region, ignore.case = TRUE) ~ "NEA",
    grepl("Cuyo", region, ignore.case = TRUE) ~ "Cuyo",
    grepl("Centro", region, ignore.case = TRUE) ~ "Centro",
    grepl("Patag|Sur|Pat", region, ignore.case = TRUE) ~ "Patagonia",
    TRUE ~ region
  )) %>%
  filter(!grepl("no especificad|sin especificar|desconocid", region, ignore.case = TRUE))

#DEFINE SUICIDE CODES
suicidio<-c(paste0("X",60:84))

#FACTOR CAUSES
datamort_15_23 <- datamort_15_23 %>% mutate(grupo_causa_defuncion_CIE10 = ifelse(cod_causa_muerte_CIE10 %in% suicidio, "Suicidio", grupo_causa_defuncion_CIE10))
datamort_15_23$grupo_causa_defuncion_CIE10<-(as.factor(datamort_15_23$grupo_causa_defuncion_CIE10))
#CHANGE CAUSE NAMES
levels(datamort_15_23$grupo_causa_defuncion_CIE10)<-c("ENF INFECCIOSAS","TUMORES MALIGNOS","TUMORES BENIGNOS","DIABETES","ANEMIA O DEF NUTRICIONAL","TRAST METABOLICOS","MENINGITIS","ALZHEIMER","TRASTORNOS MENTALES","ENF SIST CIRCULATORIO","ENF SIST RESPIRATORIO","APEND, HERNIA, OBST INSTEST","ENF SIST HEPATICO","ENF SIST URINARIO","EMBARAZO Y PARTO","AFECC PERINATALES","MALFORMACIONES","CAUSAS EXTERNAS","DEMAS CAUSAS DEFINIDAS","CAUSAS MAL DEFINIDAS","SUICIDIO")
#FACTOR EDAD
datamort_15_23$grupo_etario<-as.factor(datamort_15_23$grupo_etario)
#CHANGE AGE NAMES
levels(datamort_15_23$grupo_etario)<- c("0-20","20-39","40-59","40-59","60+","60+","60+","Desconocido")

#####################
# LOAD ANNUAL DATA 2005-2014 from defweb files
#####################

# Read all CSV files from the zip archive - directly from zip without extraction
defweb_files <- c("defweb05.csv", "defweb06.csv", "defweb07.csv", "defweb08.csv", "defweb09.csv",
                  "defweb10.csv", "defweb11.csv", "defweb12.csv", "defweb13.csv", "defweb14.csv")

# Initialize empty list to store dataframes
datamort_list <- list()

# Read each file and add year column
for(i in seq_along(defweb_files)) {
  year <- 2004 + i  # 2005 for first file, 2006 for second, etc.
  
  # Read from zip file directly without extraction
  temp_data <- read_csv(unz("defweb05-14.zip", defweb_files[i]),
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
datamort_05_14 <- bind_rows(datamort_list)

# Standardize age groups to match 2015-2023 format
datamort_05_14 <- datamort_05_14 %>%
  mutate(grupo_etario = case_when(
    grepl("^01_|^02_|^03_|^04_", GRUPEDAD) ~ "0-20",  # 01_Menor de 1 año, 02_1 a 9, 03_10 a 14, 04_15 a 19
    grepl("^05_|^06_|^07_|^08_", GRUPEDAD) ~ "20-39",  # 05_20 a 24, 06_25 a 29, 07_30 a 34, 08_35 a 39
    grepl("^09_|^10_|^11_|^12_", GRUPEDAD) ~ "40-59",  # 09_40 a 44, 10_45 a 49, 11_50 a 54, 12_55 a 59
    grepl("^13_|^14_|^15_|^16_|^17_", GRUPEDAD) ~ "60+",  # 13_60 a 64, 14_65 a 69, 15_70 a 74, 16_75 a 79, 17_80 y más
    TRUE ~ "Desconocido"
  ))

# Standardize sex codes (1=Varones, 2=Mujeres)
datamort_05_14 <- datamort_05_14 %>%
  mutate(sexo_nombre = case_when(
    SEXO == "1" ~ "Varones",
    SEXO == "2" ~ "Mujeres",
    TRUE ~ "Indeterminado"
  ))

# Map ICD-10 codes to cause categories to match 2015-2023 structure
datamort_05_14 <- datamort_05_14 %>%
  mutate(grupo_causa_defuncion_CIE10 = case_when(
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
    # All other codes - exclude from analysis
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(grupo_causa_defuncion_CIE10))  # Remove unmatched causes

# Aggregate to annual totals by age, cause, and sex before final selection
datamort_05_14 <- datamort_05_14 %>%
  rename(anio_def = anio) %>%
  group_by(anio_def, grupo_etario, grupo_causa_defuncion_CIE10, sexo_nombre) %>%
  summarise(cantidad = sum(CUENTA, na.rm = TRUE), .groups = "drop")

print(paste("Loaded 2005-2014 data:", nrow(datamort_05_14), "rows"))

######################################################
##########EXTENDED ANNUAL PLOTS 2005-2023############
######################################################

# Get unique age group levels (excluding "Desconocido")
etario_levels <- levels(datamort_15_23$grupo_etario)
etario_levels <- etario_levels[-5]

# Generate extended annual plots combining 2005-2014 and 2015-2023 data
for (i in seq_along(etario_levels)) {
 etario_level <- etario_levels[i]
 
 # Get data from 2015-2023
 subset_data_15_23 <- datamort_15_23[datamort_15_23$grupo_etario == etario_level, ]
 cantidad_anual_15_23 <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + anio_def, data = subset_data_15_23, sum)
 # Remove DEMAS CAUSAS DEFINIDAS from 2015-2023 data
 cantidad_anual_15_23 <- cantidad_anual_15_23 %>% filter(grupo_causa_defuncion_CIE10 != "DEMAS CAUSAS DEFINIDAS")
 
 # Get data from 2005-2014
 subset_data_05_14 <- datamort_05_14[datamort_05_14$grupo_etario == etario_level, ]
 cantidad_anual_05_14 <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + anio_def, data = subset_data_05_14, sum)
 
 # Combine both periods
 cantidad_anual_05_23 <- bind_rows(cantidad_anual_05_14, cantidad_anual_15_23)
 
 # Filter out ALZHEIMER for young age groups
 if (etario_level == "0-20" | etario_level == "20-39") {
  cantidad_anual_05_23 <- cantidad_anual_05_23 %>% filter(grupo_causa_defuncion_CIE10 != "ALZHEIMER")
 }
 
 # Plot extended series 2005-2023
 plot_annual_simple(cantidad_anual_05_23, 
                   paste0("plots/",etario_level,"_mortalidad_anual_causas_simple_2005_2023.png"), 
                   col_group='grupo_causa_defuncion_CIE10', 
                   title=paste0("Fallecidos anuales por Causa 2005-2023, edad = ", etario_level),
                   year_range=c(2005, 2023))
 
 print(paste("Generated extended plot for age group:", etario_level))
}

print("Extended analysis 2005-2023 completed!")
