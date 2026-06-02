#!/usr/bin/env Rscript

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(broom)
library(sandwich)
library(lmtest)
library(mgcv)

# Normalize and compute expected (GAM trained on 2015-2019) per cause, then model excess_cause ~ covid

source("funciones_analisis.R")

# Read raw files
df1519 <- read_csv("arg_def_m_15_22.zip", guess_max=500000, locale = readr::locale(encoding = "UTF-8"))
df23 <- read_csv("base_def_23_men.zip", guess_max=500000, locale = readr::locale(encoding = "UTF-8"))
df24 <- read_csv("base_def_24_men.zip", guess_max=500000, locale = readr::locale(encoding = "UTF-8"))
df <- bind_rows(df1519, df23, df24)

# minimal normalization like earlier script
df <- df %>%
  mutate(
    mes_def = sprintf("%02d", as.integer(mes_def)),
    anio_def = as.integer(as.character(anio_def)),
    fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format = "%d/%m/%Y"),
    grupo_etario = as.character(grupo_etario),
    sexo_nombre = as.character(ifelse(!is.null(sexo_nombre), sexo_nombre, ifelse(!is.null(Sexo), Sexo, sexo)))
  ) %>%
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

# define cause flags (same heuristics as earlier)
df <- df %>% mutate(
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

causes <- c(covid = "is_covid", resp = "is_resp", circ = "is_circ", diabetes = "is_diab", alzheimer = "is_alz", metabolic = "is_met", anemia = "is_anem")

# aggregate monthly counts by age
monthly_base <- df %>%
  filter(!is.na(anio_def) & !is.na(mes_def)) %>%
  mutate(grupo_etario = ifelse(is.na(grupo_etario) | grupo_etario=="", "Desconocido", grupo_etario)) %>%
  group_by(grupo_etario, anio_def, mes_def) %>%
  summarize(
    obs_all = sum(as.numeric(cantidad), na.rm = TRUE),
    covid = sum(as.numeric(cantidad[is_covid]), na.rm = TRUE),
    resp = sum(as.numeric(cantidad[is_resp]), na.rm = TRUE),
    circ = sum(as.numeric(cantidad[is_circ]), na.rm = TRUE),
    diabetes = sum(as.numeric(cantidad[is_diab]), na.rm = TRUE),
    alzheimer = sum(as.numeric(cantidad[is_alz]), na.rm = TRUE),
    metabolic = sum(as.numeric(cantidad[is_met]), na.rm = TRUE),
    anemia = sum(as.numeric(cantidad[is_anem]), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(fecha = as.Date(paste0("01/", mes_def, "/", anio_def), format="%d/%m/%Y"), month = as.integer(mes_def))

results_list <- list()
models_coefs <- list()
cor_out <- list()

for (age in unique(monthly_base$grupo_etario)){
  if(age=="Desconocido") next
  message("Processing age: ", age)
  df_age <- monthly_base %>% filter(grupo_etario==age)
  min_year <- min(df_age$anio_def, na.rm=TRUE); max_year <- max(df_age$anio_def, na.rm=TRUE)

  for(cn in names(causes)){
    varflag <- causes[[cn]]
    # observed series for cause
    df_age_c <- df_age %>% mutate(obs_cause = .data[[cn]])

    # build full grid for 2015-2019 training
    train_years <- 2015:2019
    months <- sprintf('%02d', 1:12)
    full_data <- expand.grid(anio_def = train_years, mes_def = months)
    merged_data <- merge(full_data, df_age_c %>% dplyr::select(anio_def, mes_def, obs_cause), by = c('anio_def','mes_def'), all.x = TRUE)
    merged_data$obs_cause[is.na(merged_data$obs_cause)] <- 0
    merged_data$mes_def <- as.integer(merged_data$mes_def)

    # fit GAM (fallback to lm)
    fit_ok <- FALSE
    try({ mg <- mgcv::gam(obs_cause ~ anio_def + s(mes_def, k=6), data = merged_data); fit_ok <<- TRUE }, silent = TRUE)
    if(!fit_ok){ mg <- lm(obs_cause ~ anio_def + as.integer(mes_def), data = merged_data) }

    # prediction range across available years for this age
    new_data <- expand.grid(anio_def = seq(min_year, max_year), mes_def = as.integer(months))
    preds <- tryCatch(predict(mg, newdata = new_data, se.fit = TRUE), error = function(e) list(fit = rep(NA, nrow(new_data)), se.fit = rep(NA, nrow(new_data))))
    new_data$pred <- preds$fit
    new_data$pred.lower <- preds$fit - 1.96 * preds$se.fit
    new_data$pred.upper <- preds$fit + 1.96 * preds$se.fit
    new_data$mes_def <- sprintf('%02d', new_data$mes_def)

    # merge with observed df_age
    df_merge <- df_age %>% left_join(new_data, by = c('anio_def','mes_def')) %>%
      mutate(obs_cause = .data[[cn]], excess_cause = obs_cause - pred)

    # store
    results_list[[paste(age,cn,sep='__')]] <- df_merge

    # correlations per year between excess_cause and covid
    for(y in sort(unique(df_merge$anio_def))){
      sub <- df_merge %>% filter(anio_def==y)
      if(nrow(sub) < 3) next
      good <- which(!is.na(sub$excess_cause) & !is.na(sub$covid))
      if(length(good) < 3) next
      x <- sub$excess_cause[good]; yv <- sub$covid[good]
      p1 <- tryCatch(cor.test(x,yv, method='pearson'), error=function(e)NULL)
      p2 <- tryCatch(cor.test(x,yv, method='spearman'), error=function(e)NULL)
      cor_out[[length(cor_out)+1]] <- data.frame(age=age, anio_def=y, cause=cn, method='pearson', cor=if(!is.null(p1)) as.numeric(p1$estimate) else NA_real_, p.value=if(!is.null(p1)) p1$p.value else NA_real_, n=length(good))
      cor_out[[length(cor_out)+1]] <- data.frame(age=age, anio_def=y, cause=cn, method='spearman', cor=if(!is.null(p2)) as.numeric(p2$estimate) else NA_real_, p.value=if(!is.null(p2)) p2$p.value else NA_real_, n=length(good))
    }

    # model excess_cause ~ covid * factor(year) + month fixed effects (only months with pred available)
    df_model <- df_merge %>% filter(!is.na(pred)) %>% mutate(month = as.integer(mes_def))
    if(nrow(df_model) >= 36){
      try({
        m_lm <- lm(excess_cause ~ covid * factor(anio_def) + factor(month), data = df_model)
        vcov_lm <- sandwich::vcovHC(m_lm, type='HC1')
        ct <- lmtest::coeftest(m_lm, vcov.=vcov_lm)
        tidy_ct <- broom::tidy(ct) %>% mutate(age=age, cause=cn)
        models_coefs[[paste(age,cn,sep='__')]] <- tidy_ct

        # compute year-specific covid effects
        years <- sort(unique(df_model$anio_def))
        coefs <- coef(m_lm); vc <- vcov_lm
        for(y in years){
          base <- if('covid' %in% names(coefs)) coefs['covid'] else 0
          inter <- paste0('covid:factor(anio_def)', y)
          interv <- if(inter %in% names(coefs)) coefs[inter] else 0
          eff <- base + interv
          var_base <- if('covid' %in% rownames(vc)) vc['covid','covid'] else NA_real_
          var_inter <- if(inter %in% rownames(vc)) vc[inter,inter] else 0
          covar <- if(inter %in% rownames(vc)) vc['covid',inter] else 0
          se <- sqrt(var_base + var_inter + 2*covar)
          models_coefs[[paste(age,cn, 'covid_year', y, sep='__')]] <- data.frame(age=age, cause=cn, anio_def=y, coef=eff, se=se)
        }
      }, silent = TRUE)
    }
  }
}

# write outputs
if(length(cor_out)>0) write.csv(bind_rows(cor_out), "correlations_excess_vs_covid_excess_by_age_year.csv", row.names=FALSE)
if(length(models_coefs)>0) write.csv(bind_rows(models_coefs), "models_excess_vs_covid_coefs_by_age_cause.csv", row.names=FALSE)

message("Done. Outputs:")
message(" - correlations_excess_vs_covid_excess_by_age_year.csv")
message(" - models_excess_vs_covid_coefs_by_age_cause.csv")
