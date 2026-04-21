# Mortality Trend Analysis Script
# Author: Rodrigo Quiroga
# Repository: github.com/rquiroga7/suicidios_0-20-ARGENTINA

# Robust package installation / loading: install missing packages then require()
required_pkgs <- c("httr","readxl","dplyr","ggplot2","scales","ggrepel","viridis","padr","gganimate","wesanderson","readr","forcats","kableExtra","tidyr","mgcv","MASS","lubridate","ggborderline")
installed_pkgs <- rownames(installed.packages())
to_install <- setdiff(required_pkgs, installed_pkgs)
if(length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_pkgs, function(p) {
  suppressPackageStartupMessages(require(p, character.only = TRUE, quietly = TRUE))
}))
Sys.setlocale("LC_ALL","English")

# Import custom functions
source("funciones_analisis.R")

#####################

#LOAD DATA 15-22
datamort_15_22 <- read_csv("arg_def_m_15_22.zip",
                           skip=0,locale = readr::locale(encoding = "UTF-8") ,guess_max = 250,
)
datamort_24 <- read_csv("base_def_24_men.zip",
                           skip=0,locale = readr::locale(encoding = "UTF-8") ,guess_max = 250,
)
datamort_23 <- read_csv("base_def_23_men.zip",
                           skip=0,locale = readr::locale(encoding = "UTF-8") ,guess_max = 250,
)
# Standardize grupo_etario to handle encoding differences (años vs anios)

# Combine 2023 and 2024 into a single object, then merge with 2015-2022
datamort_23_24 <- bind_rows(datamort_23, datamort_24)
# (Deferred) per-dataset normalization removed — will normalize once after merging

#MERGE DATA
datamort_15_24 <- bind_rows(datamort_15_22, datamort_23_24)

# Consolidated normalization: normalize text fields across the full merged dataset
datamort_15_24 <- datamort_15_24 %>%
  # start with any existing sexo_nombre as character
  mutate(sexo_nombre = as.character(sexo_nombre)) %>%
  # if present, prefer the `Sexo` column values for newer files
  { if ("Sexo" %in% names(.)) mutate(., sexo_nombre = ifelse(is.na(sexo_nombre) | sexo_nombre == "", as.character(Sexo), sexo_nombre)) else . } %>%
  { if ("sexo" %in% names(.)) mutate(., sexo_nombre = ifelse(is.na(sexo_nombre) | sexo_nombre == "", as.character(sexo), sexo_nombre)) else . } %>%
  { if ("sexo_id" %in% names(.)) mutate(., sexo_nombre = ifelse(is.na(sexo_nombre) | sexo_nombre == "", as.character(sexo_id), sexo_nombre)) else . } %>%
  # normalize sexo naming patterns (handle 2023: '1.Varones'/'2.Mujeres' and 2024: 'masculino'/'femenino')
  mutate(
    sexo_nombre = trimws(sexo_nombre),
    sexo_nombre = case_when(
      grepl("^1\\.?Varon|^1\\.?Varones|^1\\.$", sexo_nombre, ignore.case = TRUE) ~ "Varones",
      grepl("^2\\.?Mujer|^2\\.?Mujeres|^2\\.$", sexo_nombre, ignore.case = TRUE) ~ "Mujeres",
      grepl("mascul|^masculino|^masc$|varon|varones", sexo_nombre, ignore.case = TRUE) ~ "Varones",
      grepl("femen|^femenino|mujer|mujeres", sexo_nombre, ignore.case = TRUE) ~ "Mujeres",
      TRUE ~ sexo_nombre
    )
  ) %>%
  # normalize age-group labels once
  mutate(
    grupo_etario = as.character(grupo_etario),
    grupo_etario = gsub("^07\\. de", "07.de", grupo_etario),
    grupo_etario = gsub("años", "anios", grupo_etario),
    grupo_etario = gsub("más", "mas", grupo_etario),
    grupo_etario = as.factor(grupo_etario),
    sexo_nombre = as.factor(sexo_nombre)
  )

# Standardize and simplify region names
datamort_15_24 <- datamort_15_24 %>%
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
levels(as.factor(datamort_15_24$grupo_etario))
levels(as.factor(datamort_15_24$grupo_causa_defuncion_CIE10))

#DEFINE SUICIDE AND OTHER CODES
suicidio<-c(paste0("X",60:84),paste0("Y",87)) # + Y87
transito<-c(paste0("V",01:99),paste0("Y",85)) # + Y85

#FACTOR CAUSES: assign groups using `case_when()` and normalized codes
datamort_15_24 <- datamort_15_24 %>%
  mutate(cod_clean = toupper(trimws(as.character(cod_causa_muerte_CIE10))),
         # remove leading zeros after the letter (e.g. V01 -> V1, X00 -> X0)
         cod_clean = gsub("^([A-Z])0+([0-9]+)$", "\\1\\2", cod_clean)) %>%
  mutate(grupo_causa_defuncion_CIE10 = case_when(
    cod_clean %in% suicidio ~ "Suicidio",
    cod_clean %in% transito ~ "Accidentes de tránsito",
    TRUE ~ grupo_causa_defuncion_CIE10
  )) %>%
  mutate(grupo_causa_defuncion_CIE10 = as.factor(grupo_causa_defuncion_CIE10)) %>%
  dplyr::select(-cod_clean)

#CHANGE CAUSE NAMES
levels(datamort_15_24$grupo_causa_defuncion_CIE10)<-c("ENF INFECCIOSAS","TUMORES MALIGNOS","TUMORES BENIGNOS","DIABETES","ANEMIA O DEF NUTRICIONAL","TRAST METABOLICOS","MENINGITIS","ALZHEIMER","TRASTORNOS MENTALES","ENF SIST CIRCULATORIO","ENF SIST RESPIRATORIO","APEND, HERNIA, OBST INSTEST","ENF SIST HEPATICO","ENF SIST URINARIO","EMBARAZO Y PARTO","AFECC PERINATALES","MALFORMACIONES","CAUSAS EXTERNAS","DEMAS CAUSAS DEFINIDAS","CAUSAS MAL DEFINIDAS","ACCIDENTES DE TRÁNSITO","SUICIDIO")

#FACTOR EDAD
datamort_15_24$grupo_etario<-as.factor(datamort_15_24$grupo_etario)
#CHANGE AGE NAMES
levels(datamort_15_24$grupo_etario)<- c("0-19","20-39","40-59","40-59","60-79","60-79","80+","Desconocido")

## Ensure year is integer and causes are not NA
datamort_15_24$anio_def <- as.integer(as.character(datamort_15_24$anio_def))
datamort_15_24$grupo_causa_defuncion_CIE10 <- as.character(datamort_15_24$grupo_causa_defuncion_CIE10)
datamort_15_24$grupo_causa_defuncion_CIE10[is.na(datamort_15_24$grupo_causa_defuncion_CIE10) | datamort_15_24$grupo_causa_defuncion_CIE10==""] <- paste0("OTRA_", datamort_15_24$cod_causa_muerte_CIE10[is.na(datamort_15_24$grupo_causa_defuncion_CIE10) | datamort_15_24$grupo_causa_defuncion_CIE10==""])
datamort_15_24$grupo_causa_defuncion_CIE10 <- as.factor(datamort_15_24$grupo_causa_defuncion_CIE10)

## Compute top-20 causes per age group using totals across the full period (so facets show most important causes)
top_by_age_total <- datamort_15_24 %>%
  filter(!is.na(anio_def)) %>%
  group_by(grupo_etario, grupo_causa_defuncion_CIE10) %>%
  summarise(total = sum(cantidad, na.rm = TRUE), .groups = "drop") %>%
  arrange(grupo_etario, desc(total)) %>%
  group_by(grupo_etario) %>%
  slice_head(n = 20) %>%
  summarise(top = list(as.character(grupo_causa_defuncion_CIE10)), .groups = "drop")
top_list <- setNames(top_by_age_total$top, top_by_age_total$grupo_etario)

# Get unique levels of grupo_etario
etario_levels <- levels(datamort_15_24$grupo_etario)
etario_levels<- etario_levels[etario_levels != "Desconocido"] # Exclude "Desconocido" if present

######################################################
##########MENSUAL POR CAUSA Y GRUPO ETARIO###########
######################################################
# Import custom functions
source("funciones_analisis.R")

# Create empty list to store results
df_plot3_list <- list()

# Loop over each level of grupo_etario
for (i in seq_along(etario_levels)) {
  etario_level <- etario_levels[i]

  # Subset the data to the current level of grupo_etario
  subset_data <- datamort_15_24[datamort_15_24$grupo_etario == etario_level, ]
  if (nrow(subset_data) == 0) next

  # Calculate monthly counts
  cantidad_mensual <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + mes_def + anio_def, data = subset_data, sum)

  # Restrict to top-20 causes for this age group (based on totals across period)
  top_for_age <- top_list[[etario_level]]
  if (!is.null(top_for_age) && length(top_for_age) > 0) {
    cantidad_mensual <- cantidad_mensual %>% filter(grupo_causa_defuncion_CIE10 %in% top_for_age)
  }

  # Convert mes_def and anio_def to date format
  df <- cantidad_mensual %>%
    mutate(mes_anio = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))

  # Calculate trend using GAM
  df_plot2 <- calculate_trend_GAM(df, "grupo_causa_defuncion_CIE10")

  # Merge trend data with original data (keep all observed months in `df`)
  df_plot3 <- merge(df, df_plot2, by = c("grupo_causa_defuncion_CIE10", "anio_def", "mes_def"), all.x = TRUE)

  # Add date column
  df_plot3 <- df_plot3 %>%
    mutate(fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))

  # Ensure cause factor orders follow totals (so legend/facets are sorted)
  if (!is.null(top_for_age) && length(top_for_age) > 0) {
    df_plot3$grupo_causa_defuncion_CIE10 <- factor(df_plot3$grupo_causa_defuncion_CIE10, levels = top_for_age)
  }
    plot_abs_data_line(df_plot3, paste0("plots/", etario_level, "_tendencias_mortalidad_mensual_causas_picos.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="yes",point_size=0.01,line_size=0.8)
    # Also plot monthly excess (observed - expected) using same grouping and common y-axis
    plot_excess_monthly(df_plot3, paste0("plots/", etario_level, "_exceso_mortalidad_mensual_causas_picos.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Exceso mensual por Causa, edad = ", etario_level), peaks = "yes")

    plot_abs_data_line(df_plot3 %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO"), paste0("plots/",etario_level,"_tendencias_mortalidad_mensual_SUICIDIOS.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="no",date_breaks="3 months",point_size=3,line_size=2.5)

    # Generate separate suicide plots by sex
    # Calculate monthly counts by sex for suicides, filter out undefined sex
    cantidad_mensual_sex <- aggregate(cantidad ~ sexo_nombre + mes_def + anio_def, 
                                      data = subset_data %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO" & sexo_nombre %in% c("Varones", "Mujeres")), 
                                      sum)
    df_sex <- cantidad_mensual_sex %>%
      mutate(mes_anio = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))
    df_plot2_sex <- calculate_trend_GAM(df_sex, "sexo_nombre")
    df_plot3_sex <- merge(df_sex, df_plot2_sex, by = c("sexo_nombre", "anio_def", "mes_def"), all.x = TRUE)
    df_plot3_sex <- df_plot3_sex %>%
      mutate(fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"))
    plot_abs_data_line(df_plot3_sex, paste0("plots/",etario_level,"_tendencias_mortalidad_mensual_SUICIDIOS_POR_SEXO.png"), col_group='sexo_nombre', col_x='fecha', title=paste0("Suicidios mensuales por Sexo, edad = ", etario_level), peaks="no",date_breaks="3 months",point_size=3,line_size=2.5,facet_ncol=1)
}

# Generate annual plots without GAM projections for all age groups
for (i in seq_along(etario_levels)) {
 etario_level <- etario_levels[i]
 subset_data <- datamort_15_24[datamort_15_24$grupo_etario == etario_level, ]
 if(nrow(subset_data) == 0) next
 cantidad_anual <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + anio_def, data = subset_data, sum)
 # For each age group, keep top-20 causes in 2024 if available
 top_for_age <- top_list[[etario_level]]
 cantidad_anual_plot <- cantidad_anual
 if(!is.null(top_for_age) && length(top_for_age) > 0) {
   cantidad_anual_plot <- cantidad_anual %>% filter(grupo_causa_defuncion_CIE10 %in% top_for_age)
 }
 if(nrow(cantidad_anual_plot) > 0) {
   plot_annual_simple(cantidad_anual_plot, paste0("plots/",etario_level,"_mortalidad_anual_causas_simple.png"), col_group='grupo_causa_defuncion_CIE10', title=paste0("Fallecidos anuales por Causa, edad = ", etario_level))
 }
 
 # Generate annual suicide plots by region
 suic_region_src <- subset_data %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO")
 if(nrow(suic_region_src) > 0) {
   cantidad_anual_suic_region <- aggregate(cantidad ~ region + anio_def, data = suic_region_src, sum)
   if(nrow(cantidad_anual_suic_region) > 0) {
     plot_annual_simple(cantidad_anual_suic_region, paste0("plots/",etario_level,"_mortalidad_anual_SUICIDIOS_POR_REGION.png"), col_group='region', title=paste0("Suicidios anuales por Región, edad = ", etario_level))
   }
 }
 
 # Generate annual suicide plots by region and sex
 suic_region_sex_src <- subset_data %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO" & sexo_nombre %in% c("Varones", "Mujeres"))
 if(nrow(suic_region_sex_src) > 0) {
   cantidad_anual_suic_region_sex <- aggregate(cantidad ~ region + anio_def + sexo_nombre, data = suic_region_sex_src, sum)
   if(nrow(cantidad_anual_suic_region_sex) > 0) {
     plot_annual_simple(cantidad_anual_suic_region_sex, paste0("plots/",etario_level,"_mortalidad_anual_SUICIDIOS_POR_REGION_Y_SEXO.png"), col_group='sexo_nombre', title=paste0("Suicidios anuales por Región y Sexo, edad = ", etario_level), facet_var='region', manual_colors=c("Varones"="blue", "Mujeres"="red"))
   }
 }
}

## Annual suicide counts for selected jurisdictions (Córdoba, Santa Fe, CABA)
# select suicides in 0-20 age group and map jurisdiction names to canonical labels
cantidad_anual_jurisd <- datamort_15_24 %>%
  filter(grupo_etario == etario_levels[1] & grupo_causa_defuncion_CIE10 == "SUICIDIO") %>%
  mutate(jurisd_simple = case_when(
    grepl("Córdoba|Cordoba", jurisdiccion, ignore.case = TRUE) ~ "Córdoba",
    grepl("Santa Fe", jurisdiccion, ignore.case = TRUE) ~ "Santa Fe",
    grepl("Ciudad Autonoma|CABA|Capital Federal|Bs As|Buenos Aires", jurisdiccion, ignore.case = TRUE) ~ "CABA",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(jurisd_simple)) %>%
  group_by(jurisd_simple, anio_def) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE), .groups = "drop")

# Derive x-axis range from data
year_min <- suppressWarnings(min(as.integer(cantidad_anual_jurisd$anio_def), na.rm = TRUE))
year_max <- suppressWarnings(max(as.integer(cantidad_anual_jurisd$anio_def), na.rm = TRUE))
if (is.infinite(year_min) || is.na(year_min)) year_min <- 2015
if (is.infinite(year_max) || is.na(year_max)) year_max <- 2023

# Plot lines for the three jurisdictions with legend
ggplot(cantidad_anual_jurisd, aes(x = anio_def, y = cantidad, color = jurisd_simple, group = jurisd_simple)) +
  geom_borderline(size = 1.5) +
  geom_point(size = 3) +
  labs(x = "Año", y = "Suicidios anuales",
       title = "Suicidios anuales 0-19 años - Selección de jurisdicciones",
       color = "Jurisdicción",
       caption = "Datos del Ministerio de Salud Argentina - DEIS. Análisis por Rodrigo Quiroga.") +
  theme_bw(base_size = 18) +
  scale_x_continuous(breaks = seq(year_min, year_max, by = 1)) +
  scale_y_continuous(minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(vjust = 0.5, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 0),
        legend.position = "bottom")

ggsave("plots/0-19_suicidios_anuales_CORDOBA_SF_CABA.png", dpi = 400, width = 18, height = 10)

## Also create the same multi-jurisdiction annual plot for the 20-39 age group
if (length(etario_levels) >= 2) {
  edad2 <- etario_levels[2]
  cantidad_anual_jurisd_20_39 <- datamort_15_24 %>%
    filter(grupo_etario == edad2 & grupo_causa_defuncion_CIE10 == "SUICIDIO") %>%
    mutate(jurisd_simple = case_when(
      grepl("Córdoba|Cordoba", jurisdiccion, ignore.case = TRUE) ~ "Córdoba",
      grepl("Santa Fe", jurisdiccion, ignore.case = TRUE) ~ "Santa Fe",
      grepl("Ciudad Autonoma|CABA|Capital Federal|Bs As|Buenos Aires", jurisdiccion, ignore.case = TRUE) ~ "CABA",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(jurisd_simple)) %>%
    group_by(jurisd_simple, anio_def) %>%
    summarise(cantidad = sum(cantidad, na.rm = TRUE), .groups = "drop")

  if (nrow(cantidad_anual_jurisd_20_39) > 0) {
    year_min2 <- suppressWarnings(min(as.integer(cantidad_anual_jurisd_20_39$anio_def), na.rm = TRUE))
    year_max2 <- suppressWarnings(max(as.integer(cantidad_anual_jurisd_20_39$anio_def), na.rm = TRUE))
    if (is.infinite(year_min2) || is.na(year_min2)) year_min2 <- 2015
    if (is.infinite(year_max2) || is.na(year_max2)) year_max2 <- 2023

    ggplot(cantidad_anual_jurisd_20_39, aes(x = anio_def, y = cantidad, color = jurisd_simple, group = jurisd_simple)) +
      geom_borderline(size = 1.5) +
      geom_point(size = 3) +
      labs(x = "Año", y = "Suicidios anuales",
           title = paste0("Suicidios anuales ", edad2, " - Selección de jurisdicciones"),
           color = "Jurisdicción",
           caption = "Datos del Ministerio de Salud Argentina - DEIS. Análisis por Rodrigo Quiroga.") +
      theme_bw(base_size = 18) +
      scale_x_continuous(breaks = seq(year_min2, year_max2, by = 1)) +
      scale_y_continuous(minor_breaks = NULL) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            plot.title = element_text(vjust = 0.5, hjust = 0.5),
            plot.caption = element_text(size = 12, hjust = 0),
            legend.position = "bottom")

    ggsave(paste0("plots/", gsub("-", "_", edad2), "_suicidios_anuales_CORDOBA_SF_CABA.png"), dpi = 400, width = 18, height = 10)
  }
}