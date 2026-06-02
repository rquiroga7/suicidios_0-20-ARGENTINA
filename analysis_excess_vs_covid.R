# analysis_excess_vs_covid.R
# Estimate association between excess deaths and registered COVID + selected causes
# Produces coefficient tables and diagnostic plots by age-group

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(broom)
library(sandwich)
library(lmtest)
library(MASS)

# helper functions from repo
source("funciones_analisis.R")

# Read data (same sources used by main pipeline)
df1519 <- read_csv("arg_def_m_15_22.zip", guess_max = 500000, locale = readr::locale(encoding = "UTF-8"))
df23 <- read_csv("base_def_23_men.zip", guess_max = 500000, locale = readr::locale(encoding = "UTF-8"))
df24 <- read_csv("base_def_24_men.zip", guess_max = 500000, locale = readr::locale(encoding = "UTF-8"))

df <- bind_rows(df1519, df23, df24)

# Minimal normalization (match the pipeline expectations)
df <- df %>%
  mutate(
    mes_def = sprintf("%02d", as.integer(mes_def)),
    anio_def = as.integer(as.character(anio_def)),
    fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"),
    grupo_etario = as.character(grupo_etario),
    sexo_nombre = as.character(ifelse(!is.null(sexo_nombre), sexo_nombre, ifelse(!is.null(Sexo), Sexo, sexo)))
  )

# Normalize `sexo_nombre` and `grupo_etario` to match main pipeline conventions
# This avoids mismatched age-group labels across files (which caused 2023 counts to be grouped incorrectly)
df <- df %>%
  mutate(sexo_nombre = trimws(as.character(sexo_nombre))) %>%
  mutate(
    sexo_nombre = case_when(
      grepl("^1\\.?Varon|^1\\.?Varones|^1\\.$", sexo_nombre, ignore.case = TRUE) ~ "Varones",
      grepl("^2\\.?Mujer|^2\\.?Mujeres|^2\\.$", sexo_nombre, ignore.case = TRUE) ~ "Mujeres",
      grepl("mascul|^masculino|^masc$|varon|varones", sexo_nombre, ignore.case = TRUE) ~ "Varones",
      grepl("femen|^femenino|mujer|mujeres", sexo_nombre, ignore.case = TRUE) ~ "Mujeres",
      TRUE ~ sexo_nombre
    )
  ) %>%
  mutate(
    grupo_etario = as.character(grupo_etario),
    grupo_etario = gsub("^07\\. de", "07.de", grupo_etario),
    grupo_etario = gsub("años", "anios", grupo_etario),
    grupo_etario = gsub("más", "mas", grupo_etario)
  )


# Choose cause selectors as simple pattern matches on the grouping label when available
# Fallback to cod_causa_muerte_CIE10 when grouping is missing

df <- df %>%
  mutate(
    cod = toupper(trimws(as.character(cod_causa_muerte_CIE10))),
    grp_label = ifelse(!is.na(grupo_causa_defuncion_CIE10) & grupo_causa_defuncion_CIE10!="", as.character(grupo_causa_defuncion_CIE10), NA_character_),
    is_covid = grepl("^U07", cod) | grepl("COVID", grp_label, ignore.case = TRUE),
    is_resp = grepl("RESPIRATORIO|RESPIRATO", grp_label, ignore.case = TRUE) | grepl("^J", cod),
    is_circ = grepl("CIRCULATORIO", grp_label, ignore.case = TRUE) | grepl("^I", cod),
    is_diab = grepl("DIABETES", grp_label, ignore.case = TRUE) | grepl("^E10|^E11", cod),
    is_alz = grepl("ALZHEIM", grp_label, ignore.case = TRUE) | grepl("^G30", cod),
    is_met = grepl("TRAST METAB|METABOLIC", grp_label, ignore.case = TRUE) | grepl("^E7[0-9]|^E8[0-8]", cod),
    is_anem = grepl("ANEMIA|NUTRICIONAL", grp_label, ignore.case = TRUE) | grepl("^D5[0-9]", cod)
  )

# Exclude unknown age groups, create canonical age factor
age_levels <- df$grupo_etario %>% unique() %>% na.omit()
# Keep non-empty
age_levels <- age_levels[age_levels!="" & !is.na(age_levels)]

# Aggregate monthly counts for each age-group
monthly <- df %>%
  filter(!is.na(anio_def) & !is.na(mes_def)) %>%
  mutate(grupo_etario = ifelse(is.na(grupo_etario) | grupo_etario=="", "Desconocido", grupo_etario)) %>%
  group_by(grupo_etario, anio_def, mes_def) %>%
  summarise(
    obs = sum(as.numeric(cantidad), na.rm = TRUE),
    covid = sum(as.numeric(cantidad[is_covid]), na.rm = TRUE),
    resp = sum(as.numeric(cantidad[is_resp]), na.rm = TRUE),
    circ = sum(as.numeric(cantidad[is_circ]), na.rm = TRUE),
    diabetes = sum(as.numeric(cantidad[is_diab]), na.rm = TRUE),
    alzheimer = sum(as.numeric(cantidad[is_alz]), na.rm = TRUE),
    metabolic = sum(as.numeric(cantidad[is_met]), na.rm = TRUE),
    anemia = sum(as.numeric(cantidad[is_anem]), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format="%d/%m/%Y"),
         month = as.integer(mes_def))

# For each age-group, compute expected deaths (GAM trained on 2015-2019) using the helper
results <- list()
plots <- list()
coef_tables <- list()

for (age in unique(monthly$grupo_etario)) {
  if (age == "Desconocido") next
  message("Processing age: ", age)
  df_age <- monthly %>% filter(grupo_etario == age)
  # Prepare df for GAM: single "ALL" cause to model all-cause trend
  df_gam <- df_age %>% dplyr::select(anio_def, mes_def, cantidad = obs) %>% mutate(GROUP = "ALL")
  names(df_gam)[names(df_gam)=="GROUP"] <- "group"
  # calculate_trend_GAM expects a column name; call using 'group'
  # Compute expected deaths with a GAM trained on 2015-2019 (local implementation)
  all_years <- 2015:2019
  all_months <- sprintf("%02d", 1:12)
  full_data <- expand.grid(anio_def = all_years, mes_def = all_months)
  merged_data <- merge(full_data, df_gam, by = c("anio_def", "mes_def"), all.x = TRUE)
  merged_data$cantidad[is.na(merged_data$cantidad)] <- 0
  # fit GAM; fallback to simple lm if gam unavailable
  fit_ok <- FALSE
  try({
    mg <- mgcv::gam(cantidad ~ anio_def + mes_def, data = merged_data)
    fit_ok <<- TRUE
  }, silent = TRUE)
  if (!fit_ok) {
    mg <- lm(cantidad ~ anio_def + as.numeric(mes_def), data = merged_data)
  }
  # prediction range: years present in df_age
  min_year <- min(df_age$anio_def, na.rm = TRUE)
  max_year <- max(df_age$anio_def, na.rm = TRUE)
  new_data <- expand.grid(anio_def = seq(min_year, max_year), mes_def = all_months)
  preds <- predict(mg, newdata = new_data, se.fit = TRUE)
  df_trend2 <- new_data %>% mutate(pred = preds$fit, pred.lower = preds$fit - 1.96 * preds$se.fit, pred.upper = preds$fit + 1.96 * preds$se.fit)
  df_merge <- df_age %>% left_join(df_trend2, by = c("anio_def", "mes_def"))
  df_merge <- df_merge %>% mutate(pred = ifelse(is.na(pred), NA_real_, pred), excess = obs - pred)

  # Create period indicator for post-2020 (increase in sub-registration expected from 2021)
  df_merge <- df_merge %>% mutate(post2021 = as.integer(anio_def >= 2021))

  # Fit models with year-specific COVID effect: covid * factor(anio_def)
  # use only months where pred is available (pred not NA)
  df_model <- df_merge %>% filter(!is.na(pred))
  if (nrow(df_model) < 36) { next }

  # Linear model on excess with year interactions
  m_lm <- lm(excess ~ covid * factor(anio_def) + resp + circ + diabetes + alzheimer + metabolic + anemia + factor(month), data = df_model)
  vcov_lm <- sandwich::vcovHC(m_lm, type = "HC1")
  ct_lm <- lmtest::coeftest(m_lm, vcov. = vcov_lm)
  tidy_lm <- broom::tidy(ct_lm) %>% mutate(model = "lm", age_group = age)

  # Extract year-specific covid effects: base 'covid' + interactions covid:factor(anio_def) if present
  years <- sort(unique(df_model$anio_def))
  covid_effects <- lapply(years, function(y) {
    coef_base <- if ("covid" %in% names(coef(m_lm))) coef(m_lm)["covid"] else 0
    inter_name <- paste0("covid:factor(anio_def)", y)
    inter_val <- if (inter_name %in% names(coef(m_lm))) coef(m_lm)[inter_name] else 0
    effect <- coef_base + inter_val
    # compute robust se via delta method: var(covid) + var(inter) + 2cov
    vcovmat <- vcov_lm
    var_base <- if ("covid" %in% rownames(vcovmat)) vcovmat["covid","covid"] else NA_real_
    var_inter <- if (inter_name %in% rownames(vcovmat)) vcovmat[inter_name,inter_name] else 0
    covar <- if (inter_name %in% rownames(vcovmat)) vcovmat["covid", inter_name] else 0
    se <- sqrt(var_base + var_inter + 2*covar)
    data.frame(age_group = age, anio_def = y, coef = effect, se = se, model = "lm")
  }) %>% bind_rows()

  # Negative-binomial GLM (counts) with offset log(pred) as robustness; require pred>0
  df_glm <- df_model %>% filter(!is.na(pred) & pred > 0)
  glm_res <- NULL
  if (nrow(df_glm) >= 36) {
    try({
      m_nb <- MASS::glm.nb(obs ~ covid * factor(anio_def) + resp + circ + diabetes + alzheimer + metabolic + anemia + factor(month) + offset(log(pred)), data = df_glm)
      vcov_nb <- sandwich::vcovHC(m_nb, type = "HC1")
      ct_nb <- lmtest::coeftest(m_nb, vcov. = vcov_nb)
      tidy_nb <- broom::tidy(ct_nb) %>% mutate(model = "nb", age_group = age)
      glm_res <- list(model = m_nb, tidy = tidy_nb)
    }, silent = TRUE)
  }

  # Save outputs
  coef_tables[[paste0(age, "_lm")]] <- tidy_lm
  if (!is.null(glm_res)) coef_tables[[paste0(age, "_nb")]] <- glm_res$tidy
  results[[age]] <- df_merge

  # Save year-specific covid effects (lm)
  coef_tables[[paste0(age, "_covid_years")]] <- covid_effects

  # Plot: observed vs predicted + covid series (scaled)
  p <- ggplot(df_merge, aes(x = fecha)) +
    geom_line(aes(y = obs), color = "black") +
    geom_line(aes(y = pred), color = "blue") +
    geom_line(aes(y = covid * max(obs, na.rm=TRUE) / max(covid + 1, na.rm=TRUE)), color = "red", linetype = "dashed") +
    labs(title = paste0("Observed vs Predicted and COVID (scaled) - ", age), y = "Counts (obs=black, pred=blue, covid=red dashed)" ) +
    theme_minimal()
  ggsave(paste0("plots/excess_vs_covid_", gsub("[^A-Za-z0-9]", "_", age), ".png"), p, dpi = 300, width = 10, height = 5)
  plots[[age]] <- p
}

# Combine coefficient tables and write
coef_df <- bind_rows(coef_tables)
write.csv(coef_df, "results_excess_vs_covid_coefficients_by_age.csv", row.names = FALSE)

# Save merged monthly results per age as one CSV (stacked)
all_results <- bind_rows(results, .id = "age")
write.csv(all_results, "results_excess_monthly_by_age.csv", row.names = FALSE)

message("Done. Outputs saved:")
message(" - results_excess_vs_covid_coefficients_by_age.csv")
message(" - results_excess_monthly_by_age.csv")
message(" - plots/ (png files per age)")
