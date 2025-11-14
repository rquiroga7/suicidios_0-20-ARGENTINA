# Test script to load defweb 2005-2014 data
# Run this in RStudio to verify the data loading works correctly

library(pacman)
p_load(readr, dplyr)

# Set working directory
setwd("c:/Users/Usuario/Documents/GitHub/suicidios_0-20-ARGENTINA")

# Read all CSV files from the zip archive - directly without extraction
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
  
  print(paste("Loaded", defweb_files[i], "with", nrow(temp_data), "rows"))
}

# Combine all years
datamort_05_14 <- bind_rows(datamort_list)

# Standardize age groups to match 2015-2023 format
datamort_05_14 <- datamort_05_14 %>%
  mutate(grupo_etario = case_when(
    grepl("^01_|^02_|^03_|^04_", GRUPEDAD) ~ "0-20",
    grepl("^05_|^06_|^07_|^08_", GRUPEDAD) ~ "20-39",
    grepl("^09_|^10_|^11_|^12_", GRUPEDAD) ~ "40-59",
    grepl("^13_|^14_|^15_|^16_|^17_", GRUPEDAD) ~ "60+",
    TRUE ~ "Desconocido"
  ))

# Standardize sex codes
datamort_05_14 <- datamort_05_14 %>%
  mutate(sexo_nombre = case_when(
    SEXO == "1" ~ "Varones",
    SEXO == "2" ~ "Mujeres",
    TRUE ~ "Indeterminado"
  ))

# Map ICD-10 codes to cause categories
datamort_05_14 <- datamort_05_14 %>%
  mutate(grupo_causa_defuncion_CIE10 = case_when(
    grepl("^X[6-7][0-9]$|^X8[0-4]$", CAUSA) ~ "SUICIDIO",
    grepl("^[AB]", CAUSA) ~ "ENF INFECCIOSAS",
    grepl("^C[0-9]", CAUSA) ~ "TUMORES MALIGNOS",
    grepl("^D[0-4]", CAUSA) ~ "TUMORES BENIGNOS",
    grepl("^E1[0-4]$", CAUSA) ~ "DIABETES",
    grepl("^D5[0-3]$", CAUSA) ~ "ANEMIA O DEF NUTRICIONAL",
    grepl("^E", CAUSA) & !grepl("^E1[0-4]$", CAUSA) ~ "TRAST METABOLICOS",
    grepl("^G0[0-3]$", CAUSA) ~ "MENINGITIS",
    grepl("^G30$", CAUSA) ~ "ALZHEIMER",
    grepl("^F", CAUSA) ~ "TRASTORNOS MENTALES",
    grepl("^I", CAUSA) ~ "ENF SIST CIRCULATORIO",
    grepl("^J", CAUSA) ~ "ENF SIST RESPIRATORIO",
    grepl("^K[34589]|^K[0-2]", CAUSA) ~ "APEND, HERNIA, OBST INSTEST",
    grepl("^K7[0-7]$", CAUSA) ~ "ENF SIST HEPATICO",
    grepl("^N[0-3]", CAUSA) ~ "ENF SIST URINARIO",
    grepl("^O", CAUSA) ~ "EMBARAZO Y PARTO",
    grepl("^P", CAUSA) ~ "AFECC PERINATALES",
    grepl("^Q", CAUSA) ~ "MALFORMACIONES",
    grepl("^[VWY]", CAUSA) & !grepl("^X[6-7][0-9]$|^X8[0-4]$", CAUSA) ~ "CAUSAS EXTERNAS",
    grepl("^R", CAUSA) ~ "CAUSAS MAL DEFINIDAS",
    TRUE ~ "DEMAS CAUSAS DEFINIDAS"
  ))

print(paste("Total rows 2005-2014:", nrow(datamort_05_14)))
print("\nAge groups:")
print(table(datamort_05_14$grupo_etario))

print("\nYears available:")
print(table(datamort_05_14$anio))

print("\nSex categories:")
print(table(datamort_05_14$sexo_nombre))

print("\nCause categories:")
print(table(datamort_05_14$grupo_causa_defuncion_CIE10))

# Check for suicide codes
suicidios_05_14 <- datamort_05_14 %>%
  filter(grupo_causa_defuncion_CIE10 == "SUICIDIO")

print(paste("\nTotal suicide records 2005-2014:", nrow(suicidios_05_14)))
print("\nSuicides by year and age group:")
print(suicidios_05_14 %>% 
  group_by(anio, grupo_etario) %>% 
  summarise(total = sum(CUENTA, na.rm=TRUE), .groups="drop"))

