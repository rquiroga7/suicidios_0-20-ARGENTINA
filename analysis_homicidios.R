# Analysis of homicide deaths by sex, year and province
# Uses defweb05-23 data + defuncion2024.zip (DEIS - Argentina, full coverage)
# ICD-10 homicide codes: X85-X99, Y00-Y09, Y87.1

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

footnote <- paste0(
  "Datos del Ministerio de Salud Argentina - DEIS.\n",
  "Codigos CIE-10: homicidio X85-Y09, intencion indeterminada Y10-Y34, causa mal definida R99.\n",
  "Analisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
)

#  Province code mapping (INDEC) 
prov_map <- c(
  "02" = "CABA",
  "06" = "Buenos Aires",
  "10" = "Catamarca",
  "14" = "Cordoba",
  "18" = "Corrientes",
  "22" = "Chaco",
  "26" = "Chubut",
  "30" = "Entre Rios",
  "34" = "Formosa",
  "38" = "Jujuy",
  "42" = "La Pampa",
  "46" = "La Rioja",
  "50" = "Mendoza",
  "54" = "Misiones",
  "58" = "Neuquen",
  "62" = "Rio Negro",
  "66" = "Salta",
  "70" = "San Juan",
  "74" = "San Luis",
  "78" = "Santa Cruz",
  "82" = "Santa Fe",
  "86" = "Santiago del Estero",
  "90" = "Tucuman",
  "94" = "Tierra del Fuego"
)

#  Load all defweb files 
defweb_dir <- "defweb05-23"

read_defweb <- function(year) {
  fname <- file.path(defweb_dir, sprintf("defweb%02d.csv", year %% 100))
  # Files 2020+ use semicolons; 2005-2019 use commas
  sep <- if (year >= 2020) ";" else ","
  df <- read_delim(
    fname,
    delim     = sep,
    col_types = cols(
      PROVRES  = col_character(),
      SEXO     = col_character(),
      CAUSA    = col_character(),
      MAT      = col_character(),
      GRUPEDAD = col_character(),
      CUENTA   = col_double()
    ),
    locale        = locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    name_repair   = "minimal"
  )
  # Strip BOM from first column name if present
  names(df)[1] <- sub("^[^A-Za-z]+", "", names(df)[1])
  df$anio <- year
  df
}

raw <- bind_rows(lapply(2005:2023, read_defweb))

# -- Load 2024 data from ZIP (defuncion2024.zip, full national coverage) ------
# Schema: anio; jurisdiccion_de_residencia_id (int); jurisdicion_residencia_nombre;
#         cie10_causa_id; cie10_clasificacion; sexo_id; Sexo; ...; cantidad
raw24 <- read_delim(
  unz("defuncion2024.zip", "defuncion2024.csv"),
  delim = ";",
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  rename(CAUSA = cie10_causa_id, CUENTA_chr = cantidad) %>%
  mutate(
    CUENTA    = as.double(CUENTA_chr),
    anio      = 2024L,
    sexo      = case_when(
      grepl("mascul|varon", Sexo, ignore.case = TRUE) ~ "Varones",
      grepl("femen|mujer",  Sexo, ignore.case = TRUE) ~ "Mujeres",
      TRUE ~ "Indeterminado"
    ),
    prov_code = sprintf("%02d", suppressWarnings(as.integer(jurisdiccion_de_residencia_id))),
    provincia = coalesce(prov_map[prov_code], paste0("Prov.", prov_code))
  )

#  Filter homicides (X85-X99, Y00-Y09, Y87.1) 
is_homicide <- function(causa) {
  grepl("^X8[5-9]$|^X9[0-9]$|^Y0[0-9]$|^Y87\\.1$", causa, perl = TRUE)
}

homicidios_0523 <- raw %>%
  filter(is_homicide(CAUSA)) %>%
  mutate(
    sexo = case_when(
      SEXO == "1" ~ "Varones",
      SEXO == "2" ~ "Mujeres",
      TRUE        ~ "Indeterminado"
    ),
    prov_code = sprintf("%02s", trimws(PROVRES)),
    provincia = coalesce(prov_map[prov_code], paste0("Prov.", prov_code))
  ) %>%
  select(anio, sexo, provincia, CUENTA)

homicidios_24 <- raw24 %>%
  filter(is_homicide(CAUSA)) %>%
  select(anio, sexo, provincia, CUENTA)

homicidios <- bind_rows(homicidios_0523, homicidios_24)

#  1. Total deaths by year and sex 
by_year_sex <- homicidios %>%
  group_by(anio, sexo) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop")

cat("\n=== Homicidios por ao y sexo ===\n")
print(by_year_sex %>% pivot_wider(names_from = sexo, values_from = muertes, values_fill = 0))

p1 <- ggplot(
  by_year_sex %>% filter(sexo %in% c("Varones", "Mujeres")),
  aes(x = anio, y = muertes, colour = sexo, group = sexo)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2005:2024) +
  scale_colour_manual(values = c("Varones" = "#2166ac", "Mujeres" = "#d6604d")) +
  labs(
    title  = "Muertes por homicidio en Argentina (2005-2024)",
    x      = "Anio", y = "Numero de muertes",
    colour = "Sexo", caption = footnote
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 7))

ggsave("plots/homicidios_por_anio_sexo.png", p1, width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_por_anio_sexo.png\n")

#  2. Matrix: province  year (total, both sexes) 
matrix_prov_year <- homicidios %>%
  group_by(provincia, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
  arrange(provincia)

cat("\n=== Matriz homicidios: provincia  ao ===\n")
print(matrix_prov_year, n = Inf)

write_csv(matrix_prov_year, "outputs/homicidios_matriz_provincia_anio.csv")
cat("Guardado: outputs/homicidios_matriz_provincia_anio.csv\n")

#  3. Matrix: province  year for Varones 
matrix_varones <- homicidios %>%
  filter(sexo == "Varones") %>%
  group_by(provincia, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
  arrange(provincia)

cat("\n=== Matriz homicidios VARONES: provincia  ao ===\n")
print(matrix_varones, n = Inf)

write_csv(matrix_varones, "outputs/homicidios_matriz_varones_provincia_anio.csv")
cat("Guardado: outputs/homicidios_matriz_varones_provincia_anio.csv\n")

#  4. Matrix: province  year for Mujeres 
matrix_mujeres <- homicidios %>%
  filter(sexo == "Mujeres") %>%
  group_by(provincia, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
  arrange(provincia)

cat("\n=== Matriz homicidios MUJERES: provincia  ao ===\n")
print(matrix_mujeres, n = Inf)

write_csv(matrix_mujeres, "outputs/homicidios_matriz_mujeres_provincia_anio.csv")
cat("Guardado: outputs/homicidios_matriz_mujeres_provincia_anio.csv\n")

#  5. Heatmap: province  year (total) 
heatmap_data <- homicidios %>%
  group_by(provincia, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop")

# Order provinces by total homicides descending
prov_order <- heatmap_data %>%
  group_by(provincia) %>%
  summarise(total = sum(muertes)) %>%
  arrange(desc(total)) %>%
  pull(provincia)

p2 <- ggplot(
  heatmap_data %>% mutate(provincia = factor(provincia, levels = rev(prov_order))),
  aes(x = anio, y = provincia, fill = muertes)
) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = muertes), size = 2.5, colour = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Muertes") +
  scale_x_continuous(breaks = 2005:2024) +
  labs(
    title = "Homicidios por provincia y anio - Argentina (2005-2024)",
    x = "Año", y = NULL, caption = footnote
  ) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 7))

ggsave("plots/homicidios_heatmap_provincia_anio.png", p2, width = 14, height = 8, dpi = 150)
cat("Guardado: plots/homicidios_heatmap_provincia_anio.png\n")

#  6. Sex ratio (male/female) by year 
ratio_sex <- by_year_sex %>%
  filter(sexo %in% c("Varones", "Mujeres")) %>%
  pivot_wider(names_from = sexo, values_from = muertes) %>%
  mutate(ratio_M_F = round(Varones / Mujeres, 2))

cat("\n=== Razn Varones/Mujeres por ao ===\n")
print(ratio_sex)

write_csv(ratio_sex, "outputs/homicidios_ratio_sexo_por_anio.csv")
cat("Guardado: outputs/homicidios_ratio_sexo_por_anio.csv\n")

# ── 7. Variant A: homicides + Y10-Y34 (undetermined intent) ──────────────────
is_undetermined <- function(causa) {
  grepl("^Y([12][0-9]|3[0-4])$", causa, perl = TRUE)
}

# Build combined dataset from defweb (2005-2023)
hom_und_0523 <- raw %>%
  filter(is_homicide(CAUSA) | is_undetermined(CAUSA)) %>%
  mutate(
    sexo = case_when(
      SEXO == "1" ~ "Varones",
      SEXO == "2" ~ "Mujeres",
      TRUE        ~ "Indeterminado"
    ),
    prov_code = sprintf("%02s", trimws(PROVRES)),
    provincia = coalesce(prov_map[prov_code], paste0("Prov.", prov_code))
  ) %>%
  select(anio, sexo, provincia, CUENTA)

# 2024: homicides + undetermined
hom_und_24 <- raw24 %>%
  filter(is_homicide(CAUSA) | is_undetermined(CAUSA)) %>%
  select(anio, sexo, provincia, CUENTA)

by_year_sex_varA <- bind_rows(hom_und_0523, hom_und_24) %>%
  group_by(anio, sexo) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop")

cat("\n=== Variante A: homicidios + intencion indeterminada (Y10-Y34) ===\n")
print(by_year_sex_varA %>% filter(sexo %in% c("Varones","Mujeres")) %>%
        pivot_wider(names_from = sexo, values_from = muertes, values_fill = 0))

p_varA <- ggplot(
  by_year_sex_varA %>% filter(sexo %in% c("Varones", "Mujeres")),
  aes(x = anio, y = muertes, colour = sexo, group = sexo)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2005:2024) +
  scale_colour_manual(values = c("Varones" = "#2166ac", "Mujeres" = "#d6604d")) +
  labs(
    title    = "Homicidios + intencion indeterminada (X85-Y09 + Y10-Y34)",
    subtitle = "Argentina 2005-2024",
    x = "Año", y = "Numero de muertes", colour = "Sexo", caption = footnote
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 7))

ggsave("plots/homicidios_varA_mas_indeterminado.png", p_varA,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_varA_mas_indeterminado.png\n")

# ── 8. Variant B: homicides 2005-2024 (full observed, defuncion2024.zip) ─────
by_year_sex_varB <- by_year_sex %>%
  filter(sexo %in% c("Varones", "Mujeres"))

cat("\n=== Variante B: homicidios por ano y sexo (2024 completo) ===\n")
print(by_year_sex_varB %>% select(anio, sexo, muertes) %>%
        filter(anio >= 2022) %>%
        pivot_wider(names_from = sexo, values_from = muertes))

p_varB <- ggplot(by_year_sex_varB,
                 aes(x = anio, y = muertes, colour = sexo, group = sexo)) +
  geom_line(linewidth = 1) +
  geom_point(shape = 16, size = 2) +
  scale_x_continuous(breaks = 2005:2024) +
  scale_colour_manual(values = c("Varones" = "#2166ac", "Mujeres" = "#d6604d")) +
  labs(
    title    = "Homicidios en Argentina (2005-2024)",
    subtitle = "Datos observados - cobertura nacional completa",
    x = "Año", y = "Numero de muertes", colour = "Sexo", caption = footnote
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 7))

ggsave("plots/homicidios_varB_2024_completo.png", p_varB,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_varB_2024_completo.png\n")

# ── 9. Variant C: homicides + undetermined, 2024 fully observed ──────────────
by_year_sex_varC <- by_year_sex_varA %>%
  filter(sexo %in% c("Varones", "Mujeres"))

cat("\n=== Variante C: homicidios + indet (2024 completo) ===\n")
print(by_year_sex_varC %>% select(anio, sexo, muertes) %>%
        filter(anio >= 2022) %>%
        pivot_wider(names_from = sexo, values_from = muertes))

p_varC <- ggplot(by_year_sex_varC,
                 aes(x = anio, y = muertes, colour = sexo, group = sexo)) +
  geom_line(linewidth = 1) +
  geom_point(shape = 16, size = 2) +
  scale_x_continuous(breaks = 2005:2024) +
  scale_colour_manual(values = c("Varones" = "#2166ac", "Mujeres" = "#d6604d")) +
  labs(
    title    = "Homicidios + intencion indeterminada (X85-Y09 + Y10-Y34)",
    subtitle = "Argentina 2005-2024 - cobertura nacional completa",
    x = "Año", y = "Numero de muertes", colour = "Sexo", caption = footnote
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 7))

ggsave("plots/homicidios_varC_indeterminado.png", p_varC,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_varC_indeterminado.png\n")

# ── 9c. Stacked barcharts: homicidio vs indeterminado, by sex ────────────────
# Build cause-labelled datasets
hom_und_labelled_0523 <- raw %>%
  filter(is_homicide(CAUSA) | is_undetermined(CAUSA)) %>%
  mutate(
    sexo = case_when(
      SEXO == "1" ~ "Varones",
      SEXO == "2" ~ "Mujeres",
      TRUE        ~ "Indeterminado"
    ),
    tipo = if_else(is_homicide(CAUSA), "Homicidio", "Intencion indeterminada")
  ) %>%
  select(anio, sexo, tipo, CUENTA)

hom_und_labelled_24 <- raw24 %>%
  filter(is_homicide(CAUSA) | is_undetermined(CAUSA)) %>%
  mutate(tipo = if_else(is_homicide(CAUSA), "Homicidio", "Intencion indeterminada")) %>%
  select(anio, sexo, tipo, CUENTA)

hom_und_labelled <- bind_rows(hom_und_labelled_0523, hom_und_labelled_24) %>%
  filter(sexo %in% c("Varones", "Mujeres")) %>%
  group_by(anio, sexo, tipo) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  mutate(tipo = factor(tipo, levels = c("Homicidio", "Intencion indeterminada")))

tipo_colours_varones <- c("Homicidio" = "#1565c0", "Intencion indeterminada" = "#4dd0e1")
tipo_colours_mujeres <- c("Homicidio" = "#ad1457", "Intencion indeterminada" = "#f48fb1")

make_stacked_bar <- function(data, sexo_sel, title_suffix, colours) {
  d <- data %>% filter(sexo == sexo_sel) %>%
    arrange(anio, tipo) %>%
    group_by(anio) %>%
    mutate(label_y = cumsum(muertes) - muertes + muertes * 0.06 + 10) %>%
    ungroup()
  ggplot(d, aes(x = anio, y = muertes, fill = tipo)) +
    geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +
    geom_text(aes(y = label_y, label = muertes),
              angle = 90, hjust = 0, vjust = 0.5,
              fontface = "bold", colour = "white", size = 2.8) +
    scale_x_continuous(breaks = 2005:2024) +
    scale_fill_manual(values = colours) +
    labs(
      title = paste("Homicidio + intencion indeterminada -", title_suffix),
      subtitle = "X85-Y09 (homicidio) + Y10-Y34 (intencion indeterminada), Argentina 2005-2024",
      x = "Año", y = "Numero de muertes", fill = NULL, caption = footnote
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.caption = element_text(hjust = 0, size = 7)
    )
}

p_stack_varones <- make_stacked_bar(hom_und_labelled, "Varones", "Varones", tipo_colours_varones)
ggsave("plots/homicidios_stacked_varones.png", p_stack_varones,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_stacked_varones.png\n")

p_stack_mujeres <- make_stacked_bar(hom_und_labelled, "Mujeres", "Mujeres", tipo_colours_mujeres)
ggsave("plots/homicidios_stacked_mujeres.png", p_stack_mujeres,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_stacked_mujeres.png\n")

# ── 9b. Matrices: province × year for homicidio+indeterminado ────────────────
hom_und_all <- bind_rows(hom_und_0523, hom_und_24)

matrix_hom_und_varones <- hom_und_all %>%
  filter(sexo == "Varones") %>%
  group_by(provincia, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
  arrange(provincia)

write_csv(matrix_hom_und_varones, "outputs/hom_und_matriz_varones_provincia_anio.csv")
cat("Guardado: outputs/hom_und_matriz_varones_provincia_anio.csv\n")

matrix_hom_und_mujeres <- hom_und_all %>%
  filter(sexo == "Mujeres") %>%
  group_by(provincia, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
  arrange(provincia)

write_csv(matrix_hom_und_mujeres, "outputs/hom_und_matriz_mujeres_provincia_anio.csv")
cat("Guardado: outputs/hom_und_matriz_mujeres_provincia_anio.csv\n")

matrix_hom_und_total <- hom_und_all %>%
  group_by(provincia, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
  arrange(provincia)

write_csv(matrix_hom_und_total, "outputs/hom_und_matriz_total_provincia_anio.csv")
cat("Guardado: outputs/hom_und_matriz_total_provincia_anio.csv\n")

# ── 10. Variant C by province, women only ────────────────────────────────────
# Build province-level homicides + undetermined, women, 2005-2024
# Exclude codes 98 (unknown prov) and 99 (unidentified)
varC_prov_0523 <- hom_und_0523 %>%
  filter(sexo == "Mujeres", !provincia %in% c("Prov.98", "Prov.99")) %>%
  group_by(anio, provincia) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop")

varC_prov_24 <- hom_und_24 %>%
  filter(sexo == "Mujeres", !provincia %in% c("Prov.98", "Prov.99")) %>%
  group_by(anio, provincia) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop")

varC_prov_obs <- bind_rows(varC_prov_0523, varC_prov_24)

# Provinces present/missing in 2024
provs_with_2024    <- varC_prov_obs %>% filter(anio == 2024) %>% pull(provincia) %>% unique()
provs_missing_2024 <- setdiff(unique(varC_prov_obs$provincia), provs_with_2024)

# Impute 2024 for missing provinces: mean of 2021-2023 (consistent with varC logic)
imputed_2024_prov <- varC_prov_obs %>%
  filter(provincia %in% provs_missing_2024, anio %in% 2021:2023) %>%
  group_by(provincia) %>%
  summarise(muertes = round(mean(muertes, na.rm = TRUE)), .groups = "drop") %>%
  mutate(anio = 2024L)

# Dashed segment: 2023 observed -> 2024 imputed for missing provinces
dashed_missing_prov <- bind_rows(
  varC_prov_obs %>% filter(provincia %in% provs_missing_2024, anio == 2023),
  imputed_2024_prov
)

# Colour palette for 24 real provinces
all_provs <- sort(unique(varC_prov_obs$provincia))
n_prov <- length(all_provs)
prov_colours <- setNames(scales::hue_pal()(n_prov), all_provs)

p_varC_prov <- ggplot(mapping = aes(x = anio, y = muertes,
                                    colour = provincia, group = provincia)) +
  # dashed connector 2023->2024 for missing provinces
  geom_line(data = dashed_missing_prov, linetype = "dashed", linewidth = 0.7) +
  # solid observed lines (present provinces extend to 2024, missing to 2023)
  geom_line(data = varC_prov_obs, linewidth = 0.7) +
  geom_point(data = varC_prov_obs, shape = 16, size = 1.5) +
  # hollow circle at imputed 2024 for missing provinces
  geom_point(data = imputed_2024_prov, shape = 1, size = 2, stroke = 0.9) +
  scale_x_continuous(breaks = 2005:2024) +
  scale_y_log10(labels = scales::comma_format(accuracy = 1)) +
  scale_colour_manual(values = prov_colours) +
  labs(
    title    = "Homicidios + intencion indeterminada - Mujeres, por provincia",
    subtitle = "X85-Y09 + Y10-Y34  |  circulo hueco = 2024 imputado (media 2021-2023)",
    x = "Año", y = "Numero de muertes (escala log)", colour = "Provincia",
    caption = footnote
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.key.size = unit(0.5, "lines"),
    legend.text  = element_text(size = 8),
    plot.caption = element_text(hjust = 0, size = 7)
  )

ggsave("plots/homicidios_varC_mujeres_por_provincia.png", p_varC_prov,
       width = 14, height = 7, dpi = 150)
cat("Guardado: plots/homicidios_varC_mujeres_por_provincia.png\n")

# ── 11. R99 (causa mal definida) analysis ────────────────────────────────────
is_r99 <- function(causa) grepl("^R99$", causa, perl = TRUE)

r99_0523 <- raw %>%
  filter(is_r99(CAUSA)) %>%
  mutate(
    sexo = case_when(
      SEXO == "1" ~ "Varones",
      SEXO == "2" ~ "Mujeres",
      TRUE        ~ "Indeterminado"
    ),
    prov_code = sprintf("%02s", trimws(PROVRES)),
    provincia = coalesce(prov_map[prov_code], paste0("Prov.", prov_code))
  ) %>%
  select(anio, sexo, provincia, CUENTA)

r99_24 <- raw24 %>%
  filter(is_r99(CAUSA)) %>%
  select(anio, sexo, provincia, CUENTA)

r99_all <- bind_rows(r99_0523, r99_24)

# R99 province × year matrices
write_csv(
  r99_all %>%
    group_by(provincia, anio) %>%
    summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
    arrange(provincia),
  "outputs/r99_matriz_total_provincia_anio.csv"
)
cat("Guardado: outputs/r99_matriz_total_provincia_anio.csv\n")

write_csv(
  r99_all %>% filter(sexo == "Varones") %>%
    group_by(provincia, anio) %>%
    summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
    arrange(provincia),
  "outputs/r99_matriz_varones_provincia_anio.csv"
)
cat("Guardado: outputs/r99_matriz_varones_provincia_anio.csv\n")

write_csv(
  r99_all %>% filter(sexo == "Mujeres") %>%
    group_by(provincia, anio) %>%
    summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = anio, values_from = muertes, values_fill = 0) %>%
    arrange(provincia),
  "outputs/r99_matriz_mujeres_provincia_anio.csv"
)
cat("Guardado: outputs/r99_matriz_mujeres_provincia_anio.csv\n")

# R99 line plot by sex (like varA)
by_year_sex_r99 <- r99_all %>%
  filter(sexo %in% c("Varones", "Mujeres")) %>%
  group_by(anio, sexo) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop")

p_r99 <- ggplot(by_year_sex_r99,
                aes(x = anio, y = muertes, colour = sexo, group = sexo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2005:2024) +
  scale_colour_manual(values = c("Varones" = "#2166ac", "Mujeres" = "#d6604d")) +
  labs(
    title    = "Muertes por causa mal definida (R99)",
    subtitle = "Argentina 2005-2024",
    x = "Año", y = "Numero de muertes", colour = "Sexo", caption = footnote
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 7))

ggsave("plots/r99_por_anio_sexo.png", p_r99, width = 10, height = 5, dpi = 150)
cat("Guardado: plots/r99_por_anio_sexo.png\n")

# Stacked bars: homicidio + indeterminada + R99
r99_labelled_0523 <- raw %>%
  filter(is_r99(CAUSA)) %>%
  mutate(
    sexo = case_when(
      SEXO == "1" ~ "Varones",
      SEXO == "2" ~ "Mujeres",
      TRUE        ~ "Indeterminado"
    ),
    tipo = "R99 (causa mal definida)"
  ) %>%
  select(anio, sexo, tipo, CUENTA)

r99_labelled_24 <- raw24 %>%
  filter(is_r99(CAUSA)) %>%
  mutate(tipo = "R99 (causa mal definida)") %>%
  select(anio, sexo, tipo, CUENTA)

hom_und_r99_labelled <- bind_rows(hom_und_labelled_0523, hom_und_labelled_24,
                                   r99_labelled_0523, r99_labelled_24) %>%
  filter(sexo %in% c("Varones", "Mujeres")) %>%
  group_by(anio, sexo, tipo) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  mutate(tipo = factor(tipo, levels = c("Homicidio",
                                        "Intencion indeterminada",
                                        "R99 (causa mal definida)")))

tipo_colours_varones3 <- c("Homicidio"                = "#1565c0",
                            "Intencion indeterminada"  = "#4dd0e1",
                            "R99 (causa mal definida)" = "#546e7a")
tipo_colours_mujeres3 <- c("Homicidio"                = "#ad1457",
                            "Intencion indeterminada"  = "#f48fb1",
                            "R99 (causa mal definida)" = "#546e7a")

make_stacked_bar3 <- function(data, sexo_sel, title_suffix, colours) {
  d <- data %>% filter(sexo == sexo_sel) %>%
    arrange(anio, tipo)

  ymax <- max(d$muertes, na.rm = TRUE) * 4  # headroom for rotated labels

  ggplot(d, aes(x = factor(anio), y = muertes, fill = tipo)) +
    geom_col(width = 0.8, position = position_dodge(width = 0.85)) +
    geom_text(aes(label = muertes),
              position = position_dodge(width = 0.85),
              angle = 90, hjust = -0.1, vjust = 0.5,
              fontface = "bold", colour = "black", size = 2.8) +
    scale_y_log10(labels = scales::comma_format(accuracy = 1)) +
    coord_cartesian(ylim = c(50, ymax)) +
    scale_fill_manual(values = colours) +
    labs(
      title = paste("Homicidio + indeterminado + R99 -", title_suffix),
      subtitle = "X85-Y09 (homicidio) + Y10-Y34 (indeterminado) + R99 (causa mal definida), Argentina 2005-2024",
      x = "Año", y = "Numero de muertes (escala log)", fill = NULL, caption = footnote
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.caption = element_text(hjust = 0, size = 7)
    )
}

p_stack3_varones <- make_stacked_bar3(hom_und_r99_labelled, "Varones", "Varones", tipo_colours_varones3)
ggsave("plots/homicidios_r99_stacked_varones.png", p_stack3_varones,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_r99_stacked_varones.png\n")

p_stack3_mujeres <- make_stacked_bar3(hom_und_r99_labelled, "Mujeres", "Mujeres", tipo_colours_mujeres3)
ggsave("plots/homicidios_r99_stacked_mujeres.png", p_stack3_mujeres,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_r99_stacked_mujeres.png\n")

# ── 12. Femicidios comparison plots ──────────────────────────────────────────
femicidios_csjn <- tibble(
  anio    = c(2017L, 2018L, 2019L, 2020L, 2021L, 2022L, 2023L, 2024L),
  muertes = c(252,   257,   260,   254,   231,   226,   250,   228),
  tipo    = "Femicidios (CSJN)",
  sexo    = "Mujeres"
)

footnote_fem <- paste0(
  "Datos homicidio/indeterminado/R99: DEIS - Ministerio de Salud Argentina.\n",
  "Femicidios: Corte Suprema de Justicia de la Nacion (CSJN), Registro Nacional de Femicidios.\n",
  "Analisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
)

# 12a. Four-category dodged bar: hom+ind+R99+femicidios, mujeres, 2017-2024
hom_und_r99_fem <- bind_rows(
  hom_und_r99_labelled %>%
    filter(sexo == "Mujeres", anio >= 2017L) %>%
    mutate(tipo = as.character(tipo)),
  femicidios_csjn
) %>%
  mutate(tipo = factor(tipo, levels = c("Homicidio",
                                        "Intencion indeterminada",
                                        "R99 (causa mal definida)",
                                        "Femicidios (CSJN)")))

tipo_colours_mujeres4 <- c(
  "Homicidio"                = "#ad1457",
  "Intencion indeterminada"  = "#f48fb1",
  "R99 (causa mal definida)" = "#546e7a",
  "Femicidios (CSJN)"        = "#e65100"
)

ymax4 <- max(hom_und_r99_fem$muertes, na.rm = TRUE) * 4

p_fem4 <- ggplot(hom_und_r99_fem,
                 aes(x = factor(anio), y = muertes, fill = tipo)) +
  geom_col(width = 0.85, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = muertes),
            position = position_dodge(width = 0.9),
            angle = 90, hjust = -0.1, vjust = 0.5,
            fontface = "bold", colour = "black", size = 2.8) +
  scale_y_log10(labels = scales::comma_format(accuracy = 1)) +
  coord_cartesian(ylim = c(50, ymax4)) +
  scale_fill_manual(values = tipo_colours_mujeres4) +
  labs(
    title    = "Homicidio + indeterminado + R99 + Femicidios - Mujeres",
    subtitle = "Argentina 2017-2024 | Femicidios: CSJN; resto: DEIS CIE-10",
    x = "Año", y = "Numero de muertes (escala log)", fill = NULL,
    caption = footnote_fem
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.caption   = element_text(hjust = 0, size = 7)
  )

ggsave("plots/homicidios_r99_fem_mujeres_2017_2024.png", p_fem4,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_r99_fem_mujeres_2017_2024.png\n")

# 12b. Two-category comparison: Homicidio vs Femicidios, mujeres, 2017-2024
hom_vs_fem <- bind_rows(
  hom_und_r99_labelled %>%
    filter(sexo == "Mujeres", anio >= 2017L, tipo == "Homicidio") %>%
    mutate(tipo = as.character(tipo)),
  femicidios_csjn
) %>%
  mutate(tipo = factor(tipo, levels = c("Homicidio", "Femicidios (CSJN)")))

tipo_colours_hom_fem <- c(
  "Homicidio"         = "#ad1457",
  "Femicidios (CSJN)" = "#e65100"
)

p_hom_fem <- ggplot(hom_vs_fem,
                    aes(x = factor(anio), y = muertes, fill = tipo)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.8)) +
  geom_text(aes(label = muertes),
            position = position_dodge(width = 0.8),
            angle = 90, hjust = -0.1, vjust = 0.5,
            fontface = "bold", colour = "black", size = 3.2) +
  scale_fill_manual(values = tipo_colours_hom_fem) +
  coord_cartesian(ylim = c(0, max(hom_vs_fem$muertes) * 1.35)) +
  labs(
    title    = "Homicidios (DEIS) vs Femicidios (CSJN) - Mujeres, 2017-2024",
    subtitle = "Homicidio: CIE-10 X85-Y09 | Femicidios: Registro Nacional CSJN",
    x = "Año", y = "Numero de muertes", fill = NULL,
    caption = footnote_fem
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.caption   = element_text(hjust = 0, size = 7)
  )

ggsave("plots/homicidios_vs_femicidios_mujeres_2017_2024.png", p_hom_fem,
       width = 8, height = 5, dpi = 150)
cat("Guardado: plots/homicidios_vs_femicidios_mujeres_2017_2024.png\n")

# ── 13. Stacked bars: W (otros accidentes), X00-X59 (externas accidentales) ──
is_v_cause   <- function(causa) grepl("^V", causa, perl = TRUE)
is_w_cause   <- function(causa) grepl("^W", causa, perl = TRUE)
is_x0059     <- function(causa) grepl("^X([0-4][0-9]|5[0-9])$", causa, perl = TRUE)

vwx_labelled_0523 <- raw %>%
  filter(is_w_cause(CAUSA) | is_x0059(CAUSA)) %>%
  mutate(
    sexo = case_when(
      SEXO == "1" ~ "Varones",
      SEXO == "2" ~ "Mujeres",
      TRUE        ~ "Indeterminado"
    ),
    tipo = if_else(is_w_cause(CAUSA),
                   "Otros accidentes (W00-W99)",
                   "Causas externas accidentales (X00-X59)")
  ) %>%
  select(anio, sexo, tipo, CUENTA)

vwx_labelled_24 <- raw24 %>%
  filter(is_w_cause(CAUSA) | is_x0059(CAUSA)) %>%
  mutate(tipo = if_else(is_w_cause(CAUSA),
                        "Otros accidentes (W00-W99)",
                        "Causas externas accidentales (X00-X59)")) %>%
  select(anio, sexo, tipo, CUENTA)

vwx_labelled <- bind_rows(vwx_labelled_0523, vwx_labelled_24) %>%
  filter(sexo %in% c("Varones", "Mujeres")) %>%
  group_by(anio, sexo, tipo) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  mutate(tipo = factor(tipo, levels = c("Otros accidentes (W00-W99)",
                                        "Causas externas accidentales (X00-X59)")))

footnote_vwx <- paste0(
  "Datos del Ministerio de Salud Argentina - DEIS.\n",
  "Codigos CIE-10: W00-W99 (otros accidentes), X00-X59 (causas externas accidentales).\n",
  "Analisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
)

tipo_colours_vwx_varones <- c(
  "Otros accidentes (W00-W99)"              = "#1565c0",
  "Causas externas accidentales (X00-X59)"  = "#4dd0e1"
)
tipo_colours_vwx_mujeres <- c(
  "Otros accidentes (W00-W99)"              = "#ad1457",
  "Causas externas accidentales (X00-X59)"  = "#f48fb1"
)

make_stacked_bar_vwx <- function(data, sexo_sel, title_suffix, colours) {
  d <- data %>% filter(sexo == sexo_sel) %>%
    arrange(anio, tipo) %>%
    group_by(anio) %>%
    mutate(label_y = cumsum(muertes) - muertes + muertes * 0.06 + 10) %>%
    ungroup()
  ggplot(d, aes(x = anio, y = muertes, fill = tipo)) +
    geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +
    geom_text(aes(y = label_y, label = scales::comma(muertes)),
              angle = 90, hjust = 0, vjust = 0.5,
              fontface = "bold", colour = "white", size = 2.8) +
    scale_x_continuous(breaks = 2005:2024) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = colours) +
    labs(
      title    = paste("Causas externas accidentales (W, X00-X59) -", title_suffix),
      subtitle = "W00-W99 (otros accidentes) + X00-X59 (ext. accidentales), Argentina 2005-2024",
      x = "Año", y = "Numero de muertes", fill = NULL, caption = footnote_vwx
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x    = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      plot.caption   = element_text(hjust = 0, size = 7)
    )
}

p_vwx_varones <- make_stacked_bar_vwx(vwx_labelled, "Varones", "Varones", tipo_colours_vwx_varones)
ggsave("plots/causas_vwx_stacked_varones.png", p_vwx_varones,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/causas_vwx_stacked_varones.png\n")

p_vwx_mujeres <- make_stacked_bar_vwx(vwx_labelled, "Mujeres", "Mujeres", tipo_colours_vwx_mujeres)
ggsave("plots/causas_vwx_stacked_mujeres.png", p_vwx_mujeres,
       width = 10, height = 5, dpi = 150)
cat("Guardado: plots/causas_vwx_stacked_mujeres.png\n")

# ── 15. Top-5 W and X00-X59 causes of death 2021-2024 ────────────────────────
causa_nombres <- read_csv("causas_nombre.txt", show_col_types = FALSE) %>%
  rename(CAUSA = CODIGO, nombre = VALOR)

wx_top <- bind_rows(
  vwx_labelled_0523 %>% mutate(CAUSA_raw = NA),  # need raw CAUSA — rebuild from raw
  vwx_labelled_24   %>% mutate(CAUSA_raw = NA)
)

# Rebuild from raw data with cause-level detail
wx_detail_0523 <- raw %>%
  filter((is_w_cause(CAUSA) | is_x0059(CAUSA)), anio >= 2021L) %>%
  mutate(
    sexo = case_when(SEXO == "1" ~ "Varones", SEXO == "2" ~ "Mujeres", TRUE ~ "Indeterminado"),
    grupo = if_else(is_w_cause(CAUSA), "W (otros accidentes)", "X00-X59 (ext. accidentales)")
  ) %>%
  select(anio, sexo, grupo, CAUSA, CUENTA)

wx_detail_24 <- raw24 %>%
  filter((is_w_cause(CAUSA) | is_x0059(CAUSA)), anio >= 2021L) %>%
  mutate(grupo = if_else(is_w_cause(CAUSA), "W (otros accidentes)", "X00-X59 (ext. accidentales)")) %>%
  select(anio, sexo, grupo, CAUSA, CUENTA)

wx_detail <- bind_rows(wx_detail_0523, wx_detail_24) %>%
  filter(sexo %in% c("Varones", "Mujeres"))

wx_table <- wx_detail %>%
  group_by(grupo, CAUSA) %>%
  summarise(muertes_2021_2024 = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  left_join(causa_nombres, by = "CAUSA") %>%
  arrange(grupo, desc(muertes_2021_2024)) %>%
  group_by(grupo) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  select(grupo, CAUSA, nombre)

wx_yearly <- wx_detail %>%
  group_by(grupo, CAUSA, anio) %>%
  summarise(muertes = sum(CUENTA, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = muertes, values_fill = 0,
              names_prefix = "anio_")

wx_table <- wx_table %>%
  left_join(wx_yearly, by = c("grupo", "CAUSA")) %>%
  mutate(total_2021_2024 = rowSums(across(starts_with("anio_")), na.rm = TRUE)) %>%
  arrange(grupo, desc(total_2021_2024))

cat("\n=== Top 5 causas W y X00-X59 (2021-2024) ===\n")
print(wx_table, n = Inf)
write_csv(wx_table, "outputs/top5_wx_causas_2021_2024.csv")
cat("Guardado: outputs/top5_wx_causas_2021_2024.csv\n")

# ── Mujeres 2024 vs 2021-2023: por causa (sin desagregacion por edad) ─────────
wx_detail_mujeres <- bind_rows(
  raw %>%
    filter((is_w_cause(CAUSA) | is_x0059(CAUSA)), SEXO == "2", anio >= 2021L) %>%
    select(anio, CAUSA, CUENTA),
  raw24 %>%
    filter((is_w_cause(CAUSA) | is_x0059(CAUSA)), sexo == "Mujeres") %>%
    select(anio, CAUSA, CUENTA)
)

avg_2123_causa <- wx_detail_mujeres %>%
  filter(anio %in% 2021:2023) %>%
  group_by(anio, CAUSA) %>%
  summarise(muertes = sum(CUENTA, na.rm=TRUE), .groups="drop") %>%
  group_by(CAUSA) %>%
  summarise(avg_2021_2023 = round(mean(muertes, na.rm=TRUE), 1), .groups="drop")

# yearly totals per cause
wx_yearly_causa <- wx_detail_mujeres %>%
  group_by(anio, CAUSA) %>%
  summarise(muertes = sum(CUENTA, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from=anio, values_from=muertes, values_fill=0, names_prefix="anio_")

d2024_causa <- wx_detail_mujeres %>%
  filter(anio == 2024) %>%
  group_by(CAUSA) %>%
  summarise(muertes_2024 = sum(CUENTA, na.rm=TRUE), .groups="drop")

wx_causa_table <- avg_2123_causa %>%
  full_join(d2024_causa, by="CAUSA") %>%
  replace_na(list(avg_2021_2023=0, muertes_2024=0)) %>%
  mutate(
    delta      = round(muertes_2024 - avg_2021_2023, 1),
    pct_change = ifelse(avg_2021_2023 > 3, round(100*delta/avg_2021_2023), NA_real_)
  ) %>%
  left_join(causa_nombres, by="CAUSA") %>%
  left_join(wx_yearly_causa, by="CAUSA") %>%
  arrange(desc(delta)) %>%
  select(CAUSA, nombre, avg_2021_2023, any_of(c("anio_2021","anio_2022","anio_2023","anio_2024")), delta, pct_change)

cat("\n=== Mujeres W+X00-X59: 2024 vs promedio 2021-2023 (por causa) ===\n")
print(wx_causa_table, n=30)
write_csv(wx_causa_table, "outputs/wx_causas_mujeres_2024_vs_2123.csv")
cat("Guardado: outputs/wx_causas_mujeres_2024_vs_2123.csv\n")
