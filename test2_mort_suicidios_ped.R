
# Mortality Trend Analysis Script
# Author: Rodrigo Quiroga
# Repository: github.com/rquiroga7/suicidios_0-20-ARGENTINA

library(pacman)
p_load(httr,readxl,dplyr,ggplot2,scales,ggrepel,viridis,padr,gganimate,wesanderson,readr,forcats,kableExtra,tidyr,mgcv,MASS,lubridate,ggborderline)
Sys.setlocale("LC_ALL","English")

# Import custom functions
source("funciones_analisis.R")

#####################

#LOAD DATA 15-22
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

#Check grupo_etario levels
levels(as.factor(datamort_15_23$grupo_etario))

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
levels(datamort_15_23$grupo_etario)<- c("0-20","20-39","40-49","50-59","60-69","70-79","80-200","Desconocido")

######################################################
##########MENSUAL POR CAUSA Y GRUPO ETARIO###########
######################################################
# Get unique levels of grupo_etario
etario_levels <- levels(datamort_15_23$grupo_etario)
etario_levels<- etario_levels[1:2]

# Create empty list to store results
df_plot3_list <- list()
# Loop over each level of grupo_etario
for (i in seq_along(etario_levels)) {
 etario_level <- etario_levels[i]
 # Subset the data to the current level of grupo_etario
 subset_data <- datamort_15_23[datamort_15_23$grupo_etario == etario_level, ]
 # Calculate monthly counts
 cantidad_mensual <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + mes_def + anio_def, data = subset_data, sum)
 # For 0-20 age group, filter out ALZHEIMER
 if (etario_level == "0-20" | etario_level == "21-39" ) {
  cantidad_mensual <- cantidad_mensual %>% filter(grupo_causa_defuncion_CIE10 != "ALZHEIMER")
 }
 # Convert mes_def and anio_def to date format
 df <- cantidad_mensual %>%
  mutate(mes_anio = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))
 # Calculate trend using GAM
 df_plot2 <- calculate_trend_GAM(df, "grupo_causa_defuncion_CIE10")
 # Merge trend data with original data
 df_plot3 <- merge(df_plot2, df, by = c("grupo_causa_defuncion_CIE10", "anio_def", "mes_def"))
 # Add date column
 df_plot3 <- df_plot3 %>%
  mutate(fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))
 if (etario_level == "0-20" | etario_level == "20-39") {
                               plot_abs_data_line(df_plot3, paste0(etario_level,"_tendencias_mortalidad_mensual_causas_picos.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="yes",point_size=0.01,line_size=0.8)
                               plot_abs_data_line(df_plot3 %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO"), paste0(etario_level,"_tendencias_mortalidad_mensual_SUICIDIOS.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="no",date_breaks="3 months",point_size=3,line_size=2.5)
                               
                               # Generate separate suicide plots by sex
                               # Calculate monthly counts by sex for suicides, filter out undefined sex
                               cantidad_mensual_sex <- aggregate(cantidad ~ sexo_nombre + mes_def + anio_def, 
                                                                 data = subset_data %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO" & sexo_nombre %in% c("Varones", "Mujeres")), 
                                                                 sum)
                               df_sex <- cantidad_mensual_sex %>%
                                 mutate(mes_anio = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))
                               df_plot2_sex <- calculate_trend_GAM(df_sex, "sexo_nombre")
                               df_plot3_sex <- merge(df_plot2_sex, df_sex, by = c("sexo_nombre", "anio_def", "mes_def"))
                               df_plot3_sex <- df_plot3_sex %>%
                                 mutate(fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))
                               plot_abs_data_line(df_plot3_sex, paste0(etario_level,"_tendencias_mortalidad_mensual_SUICIDIOS_POR_SEXO.png"), col_group='sexo_nombre', col_x='fecha', title=paste0("Suicidios mensuales por Sexo, edad = ", etario_level), peaks="no",date_breaks="3 months",point_size=3,line_size=2.5,facet_ncol=1)
                              }
 else {plot_abs_data_line(df_plot3, paste0(etario_level,"_tendencias_mortalidad_mensual_causas_picos.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="yes")}
}

# Generate annual plots without GAM projections for all age groups
for (i in seq_along(etario_levels)) {
 etario_level <- etario_levels[i]
 subset_data <- datamort_15_23[datamort_15_23$grupo_etario == etario_level, ]
 cantidad_anual <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + anio_def, data = subset_data, sum)
 # Filter out ALZHEIMER for young age groups
 if (etario_level == "0-20" | etario_level == "20-39") {
  cantidad_anual <- cantidad_anual %>% filter(grupo_causa_defuncion_CIE10 != "ALZHEIMER")
 }
 plot_annual_simple(cantidad_anual, paste0(etario_level,"_mortalidad_anual_causas_simple.png"), col_group='grupo_causa_defuncion_CIE10', title=paste0("Fallecidos anuales por Causa, edad = ", etario_level))
 
 # Generate annual suicide plots by region
 cantidad_anual_suic_region <- aggregate(cantidad ~ region + anio_def, 
                                         data = subset_data %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO"), 
                                         sum)
 if(nrow(cantidad_anual_suic_region) > 0) {
  plot_annual_simple(cantidad_anual_suic_region, paste0(etario_level,"_mortalidad_anual_SUICIDIOS_POR_REGION.png"), col_group='region', title=paste0("Suicidios anuales por Región, edad = ", etario_level))
 }
 
 # Generate annual suicide plots by region and sex
 cantidad_anual_suic_region_sex <- aggregate(cantidad ~ region + anio_def + sexo_nombre, 
                                             data = subset_data %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO" & sexo_nombre %in% c("Varones", "Mujeres")), 
                                             sum)
 if(nrow(cantidad_anual_suic_region_sex) > 0) {
  plot_annual_simple(cantidad_anual_suic_region_sex, paste0(etario_level,"_mortalidad_anual_SUICIDIOS_POR_REGION_Y_SEXO.png"), col_group='sexo_nombre', title=paste0("Suicidios anuales por Región y Sexo, edad = ", etario_level), facet_var='region', manual_colors=c("Varones"="blue", "Mujeres"="red"))
 }
}


