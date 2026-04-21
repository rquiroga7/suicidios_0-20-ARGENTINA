# funciones_analisis.R
# Functions for mortality trend analysis
# Author: Rodrigo Quiroga

###DEFINE HELPER FUNCTIONS###
`%notin%` <- Negate(`%in%`)
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}
is.Date <- function(x) inherits(x, 'Date')
colu <- function(df,colu=colu) {return(df %>% pull(colu))}
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy+accuracy}

###TREND CALCULATION FUNCTIONS###
calculate_trend_robust <- function(df, colname){
 # subset dataframe to only include years 2015-2019
 df_subset <- subset(df, anio_def >= 2015 & anio_def <= 2019 & anio_def != 2016)
 # fit linear regression model to subsetted data for each "colname" (jurisdiccion,causa)
 lm_models <- by(df_subset, df_subset[[colname]], function(x) rlm(cantidad ~ anio_def, data = x, maxit = 100))
 df_trend <- do.call(rbind, lapply(names(lm_models), function(name) {
  new_data <- data.frame(anio_def = 2015:2021)
  new_data[, as.character(colname)] <- name
  preds <- predict(lm_models[[name]], new_data, interval = "confidence", level = 0.95)
  new_data <- new_data %>%
   mutate(pred.lower = preds[, "lwr"], pred = preds[, "fit"], pred.upper = preds[, "upr"])
  return(new_data)
 }))
 return(merge(df_trend, df, by = c(colname, "anio_def")))
}

calculate_trend_GAM <- function(df, colname) {
 # Subset dataframe to only include years 2015-2019 and exclude month "00"
 df_subset <- subset(df, anio_def >= 2015 & anio_def <= 2019 & mes_def != "00")
 # Define a vector of all possible years (2015-2019)   # Define a vector of all possible months (01-12)
 all_years <- 2015:2019; all_months <- sprintf("%02d", 1:12)
 # Create a data frame with all possible combinations of anio_def and mes_def
 full_data <- expand.grid(anio_def = all_years, mes_def = all_months)

 # Fit GAM model to subsetted data for each "colname" (grupo_causa_defuncion_CIE10)
 gam_models <- by(df_subset, df_subset[[colname]], function(x) {
  # Merge the full_data data frame with the original data frame x to fill in any missing values with cantidad = 0
  merged_data <- merge(full_data, x, all.x = TRUE)
  merged_data$cantidad[is.na(merged_data$cantidad)] <- 0 # Replace NAs with 0
  tryCatch(
   gam(cantidad ~ anio_def + mes_def, data = merged_data),
   error = function(e) NULL
  )
 })
 # Predict using the GAM model and calculate confidence intervals
 # determine prediction year range from the input dataframe `df`
 # fall back to 2015:2023 if df has no valid years
 min_year <- suppressWarnings(min(as.integer(df$anio_def), na.rm = TRUE))
 max_year <- suppressWarnings(max(as.integer(df$anio_def), na.rm = TRUE))
  new_data <- expand.grid(
  colname = names(gam_models),
  anio_def = seq(min_year, max_year),
  mes_def = sprintf("%02d", 1:12) # all months from 01 to 12
 )
 names(new_data)[1]<-{{colname}}
 df_trend <- data.frame()
 for (name in levels(as.factor(df_subset[[colname]]))) {
  data <- new_data[new_data[[colname]] == name,]
  # Check if model exists and is not NULL
  if (!is.null(gam_models[[name]]) && inherits(gam_models[[name]], "gam")) {
   preds <- predict(gam_models[[name]], newdata = data, se.fit = TRUE)
   data <- cbind(
    data,
    pred.lower = preds$fit - 1.96 * preds$se.fit,
    pred = preds$fit,
    pred.upper = preds$fit + 1.96 * preds$se.fit
   )
   df_trend <- rbind(df_trend, data)
  }
 }
 return(df_trend)
}

calculate_trend_rGAM <- function(df, colname) {
 # Subset dataframe to only include years 2015-2019 and exclude month "00"
 df_subset <- subset(df, anio_def >= 2015 & anio_def <= 2019 & mes_def != "00")
 df_subset$mes_def<-as.numeric(df_subset$mes_def)
 # Define a vector of all possible years (2015-2019)   # Define a vector of all possible months (01-12)
 all_years <- 2015:2019; all_months <- sprintf("%02d", 1:12)
 # Create a data frame with all possible combinations of anio_def and mes_def
 full_data <- expand.grid(anio_def = as.numeric(all_years), mes_def = as.numeric(all_months))

 # Fit GAM model to subsetted data for each "colname" (grupo_causa_defuncion_CIE10)
 gam_models <- by(df_subset, df_subset[[colname]], function(x) {
  # Merge the full_data data frame with the original data frame x to fill in any missing values with cantidad = 0
  merged_data <- merge(full_data, x, all.x = TRUE,by=c("anio_def","mes_def"))
  name<-x[[colname]][1]
  merged_data$cantidad[is.na(merged_data$cantidad)] <- 0 # Replace NAs with 0
  merged_data$cantidad<-as.numeric(as.character(merged_data$cantidad))
  merged_data$grupo_causa_defuncion_CIE10[is.na(merged_data$grupo_causa_defuncion_CIE10)] <- name # Replace NAs with colname
  tryCatch(
   gam(cantidad ~ anio_def + s(mes_def), data = merged_data,method="REML"),
   error = function(e) NULL
  )
 })
 # Predict using the GAM model and calculate confidence intervals
 new_data <- expand.grid(
  colname = names(gam_models),
  anio_def = 2015:2021,
  mes_def = 1:12 # all months from 01 to 12
 )
 names(new_data)[1]<-{{colname}}
 df_trend <- data.frame()
 for (name in levels(as.factor(df_subset[[colname]]))) {
  data <- new_data[new_data[[colname]] == name,]
  # Check if model exists and is not NULL
  if (!is.null(gam_models[[name]]) && inherits(gam_models[[name]], "gam")) {
   preds <- predict(gam_models[[name]], newdata = data, se.fit = TRUE)
   data <- cbind(
    data,
    pred.lower = preds$fit - 1.96 * preds$se.fit,
    pred = preds$fit,
    pred.upper = preds$fit + 1.96 * preds$se.fit
   )
   df_trend <- rbind(df_trend, data)
  }
 }
 df_trend$mes_def<-sprintf("%02d", df_trend$mes_def)
 return(df_trend)
}

###DATA PROCESSING FUNCTIONS###
normalize_data <- function(df, group_col) {
 df_norm <- df %>%
  group_by(.data[[group_col]]) %>%
  mutate(cantidad_norm = cantidad/cantidad[anio_def == 2015] * 100,          pred_norm = pred/cantidad[anio_def == 2015] * 100,          pred.upper_norm = pred.upper/cantidad[anio_def == 2015] * 100,          pred.lower_norm = pred.lower/cantidad[anio_def == 2015] * 100) %>%
  ungroup()
 return(df_norm)
}

calculate_excess <- function(df, col_group = 'grupo_causa_defuncion_CIE10', col_x = 'anio_def') {
 if (inherits(df[[col_x]], "Date")) {
  df2 <- df %>% mutate(years = format(df[[col_x]], "%Y"))
 } else {
  df2 <- df %>% mutate(years = df[[col_x]])
 }
 df3 <- df2 %>% group_by({{ col_group }}, years) %>%
  mutate(exceso=cantidad-pred,exceso_porc=exceso/pred*100,exceso.lower=cantidad-pred.upper,exceso.upper=cantidad-pred.lower)
 test <- df3 %>% group_by(.data[[col_group]]) %>% filter(years < 2020) %>%
  mutate(p75 = quantile(cantidad, 0.75)) %>%
  dplyr::select({{ col_group }}, p75) %>% distinct()
 df4 <- merge(test, df3, by = {{ col_group }}, all.y = TRUE) %>%
  group_by({{ col_group }}, anio_def) %>%
  mutate(exceso_p75 = cantidad - p75, exceso_porc_p75 = exceso_p75 / p75 * 100)
 return(df4)
}

reorder_factors <- function(df, colname,cantidad) {
 #Calculate max values for each group
 max_vals <- df %>% group_by(.data[[colname]]) %>% summarize(max_cantidad = max(.data[[cantidad]]))
 # Arrange col_group factor levels in descending order of max_cantidad
 df[[colname]] <- factor(df[[colname]], levels = max_vals[[colname]][order(-max_vals$max_cantidad)])
 return(df)
}

# Shared helper to add peak (COVID wave) vertical lines consistently across plots.
peaks_layers <- function(peaks) {
  if (tolower(peaks) == "yes") {
    return(list(
      geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red", alpha = 0.6),
      geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red", alpha = 0.6),
      geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "darkorange", alpha = 0.6),
      geom_vline(xintercept = as.numeric(as.Date("2022-12-01")), color = "darkorange", alpha = 0.6),
      geom_vline(xintercept = as.numeric(as.Date("2024-03-01")), color = "darkorange", alpha = 0.6)
    ))
  } else {
    return(NULL)
  }
}

###PLOTTING FUNCTIONS###
plot_abs_data <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
  plot <- ggplot()
  # add shared peaks lines behind other layers when requested
  plot <- plot + peaks_layers(peaks) +
    geom_point(data = dataframe , aes(x = .data[[col_x]], y = cantidad)) +
    geom_line(data = dataframe , aes(x = .data[[col_x]], y = pred, group = .data[[col_group]], color = .data[[col_group]])) +
    geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = `pred.lower`, ymax = `pred.upper`, fill = .data[[col_group]]), alpha = 0.4) +
    facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width =18))) +
    {
      base_caption <- "L\u00ednea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ a\u00f1o + mes). Intervalo de confianza: 95%.\nDatos del Ministerio de Salud Argentina - DEIS.\nAn\u00e1lisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
      caption_text <- if(tolower(peaks) == "yes") paste0(base_caption, "\nLas l\u00edneas rojas verticales corresponden a las tres principales olas de COVID-19.") else base_caption
      labs(x = "A\u00f1o", y = "Fallecidos anuales", caption = caption_text)
    } +
    theme_bw(base_size=18) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))+
    scale_y_continuous(minor_breaks = NULL)+
    ggtitle(title)
  if (is.Date(dataframe[[col_x]])){
    data_min_raw <- suppressWarnings(min(dataframe[[col_x]], na.rm = TRUE))
    data_max_raw <- suppressWarnings(max(dataframe[[col_x]], na.rm = TRUE))
    if (is.na(data_min_raw)) data_min_raw <- as.Date("2015-01-01")
    if (is.na(data_max_raw)) data_max_raw <- data_min_raw
    # floor min to first of month, floor max to first of month then add one month
    min_month <- as.Date(paste0(format(data_min_raw, "%Y-%m"), "-01"))
    max_month <- as.Date(paste0(format(data_max_raw, "%Y-%m"), "-01"))
    max_next_month <- seq(max_month, by = "month", length.out = 2)[2]
    # create Jan-1 breaks from start year to end year+1
    start_year <- as.integer(format(min_month, "%Y"))
    end_year_next <- as.integer(format(max_month, "%Y")) + 1
    jan_breaks <- as.Date(paste0(seq(start_year, end_year_next), "-01-01"))
    plot <- plot + scale_x_date(minor_breaks = NULL, breaks = jan_breaks, date_labels = "%Y-%m", limits = c(min_month, max_next_month), expand = c(0,0))
  } else {
    plot <- plot + scale_x_continuous(minor_breaks = NULL, breaks = seq(2015, 2021, by = 1))
  }
 ggplot2::ggsave(fname, plot = plot, device = ragg::agg_png, dpi = 400, width = 18, height = 10)
 return(plot)
}

plot_annual_simple <- function(dataframe, fname, col_group='grupo_causa_defuncion_CIE10',title='Titulo',facet_var=NULL,manual_colors=NULL,year_range=NULL){
 # Determine year range for x-axis breaks (derive from data when year_range is NULL)
 if (is.null(year_range)) {
  if (is.Date(dataframe$anio_def)) {
   year_min <- as.integer(format(min(dataframe$anio_def, na.rm = TRUE), "%Y"))
   year_max <- as.integer(format(max(dataframe$anio_def, na.rm = TRUE), "%Y"))
  } else {
   year_min <- suppressWarnings(min(as.integer(dataframe$anio_def), na.rm = TRUE))
   year_max <- suppressWarnings(max(as.integer(dataframe$anio_def), na.rm = TRUE))
  }
  if (is.infinite(year_min) || is.na(year_min)) year_min <- 2015
  if (is.infinite(year_max) || is.na(year_max)) year_max <- 2023
 } else {
  year_min <- min(year_range)
  year_max <- max(year_range)
 }
 
 plot <- ggplot(data = dataframe, aes(x = anio_def, y = cantidad, color = .data[[col_group]])) +
  geom_borderline(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(x = "A\u00f1o", y = "Fallecidos anuales", caption = "Datos del Ministerio de Salud Argentina - DEIS.\nAn\u00e1lisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA") +
  theme_bw(base_size=18) +
  scale_y_continuous(minor_breaks = NULL)+
  scale_x_continuous(breaks = seq(year_min, year_max, by = 1))+
  ggtitle(title)
 
 # Add facet_wrap based on facet_var parameter
 if(!is.null(facet_var)){
  plot <- plot + facet_wrap(~.data[[facet_var]], scales="free_y", ncol=4, labeller = labeller(.rows = label_wrap_gen(width = 18))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "bottom", legend.title = element_text(size=16), legend.text = element_text(size=14), strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))
 } else {
  plot <- plot + facet_wrap(~.data[[col_group]], scales="free_y", ncol=4, labeller = labeller(.rows = label_wrap_gen(width = 18))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))
 }
 
 # Add manual colors if provided
 if(!is.null(manual_colors)){
  plot <- plot + scale_color_manual(values = manual_colors, name = "Sexo")
 }
 
  ggplot2::ggsave(fname, plot = plot, device = ragg::agg_png, dpi = 400, width = 18, height = 10)
 return(plot)
}

plot_abs_data_line <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO',date_breaks="1 year",point_size=1,line_size=1,facet_ncol=NULL){
 plot <- ggplot(data = dataframe)
 # add shared peaks lines behind other layers when requested
 plot <- plot + peaks_layers(peaks) +
  geom_ribbon(aes(x = .data[[col_x]], ymin = `pred.lower`, ymax = `pred.upper`, fill = .data[[col_group]]), alpha = 0.4) +
  geom_borderline(aes(x = .data[[col_x]], y = pred, group = .data[[col_group]], color = .data[[col_group]]),linewidth=line_size) +
  geom_borderline(aes(x = .data[[col_x]], y = cantidad),linewidth = line_size) +
  geom_point(aes(x = .data[[col_x]], y = cantidad),size=point_size) +
  facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width = 18)), ncol=facet_ncol) +
  {
    base_caption <- "L\u00ednea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ a\u00f1o + mes). Intervalo de confianza: 95%.\nDatos del Ministerio de Salud Argentina - DEIS.\nAn\u00e1lisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
    caption_text <- if(tolower(peaks) == "yes") paste0(base_caption, "\nLas l\u00edneas rojas verticales corresponden a las tres principales olas de COVID-19.") else base_caption
    labs(x = "A\u00f1o-Mes", y = "Fallecidos mensuales", caption = caption_text)
  } +
  theme_bw(base_size=18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if (is.Date(dataframe[[col_x]])) {
  data_min_raw <- suppressWarnings(min(dataframe[[col_x]], na.rm = TRUE))
  data_max_raw <- suppressWarnings(max(dataframe[[col_x]], na.rm = TRUE))
  if (is.na(data_min_raw)) data_min_raw <- as.Date("2015-01-01")
  if (is.na(data_max_raw)) data_max_raw <- data_min_raw
  min_month <- as.Date(paste0(format(data_min_raw, "%Y-%m"), "-01"))
  max_month <- as.Date(paste0(format(data_max_raw, "%Y-%m"), "-01"))
  max_next_month <- seq(max_month, by = "month", length.out = 2)[2]
  start_year <- as.integer(format(min_month, "%Y"))
  end_year_next <- as.integer(format(max_month, "%Y")) + 1
  jan_breaks <- as.Date(paste0(seq(start_year, end_year_next), "-01-01"))
  plot <- plot + scale_x_date(minor_breaks = NULL, breaks = jan_breaks, date_labels = "%Y-%m", limits = c(min_month, max_next_month), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL, breaks = seq(2015, 2023, by = 1))
 }
 ggsave(fname, plot = plot, device = ragg::agg_png, dpi = 400, width = 18, height = 10)
 return(plot)
}

plot_excess_monthly <- function(dataframe, fname, col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title='Titulo', date_breaks="1 month", facet_ncol=NULL, peaks='NO'){
  # dataframe is expected to contain: cantidad, pred, pred.lower, pred.upper, and the grouping column
  df <- dataframe %>%
    mutate(exceso = as.numeric(cantidad) - as.numeric(pred),
           exceso.lower = as.numeric(cantidad) - as.numeric(pred.upper),
           exceso.upper = as.numeric(cantidad) - as.numeric(pred.lower))

  # don't force common y-limits: allow free y-axis per facet

  # start plot and place vertical peak lines behind other layers when requested
  plot <- ggplot()
  plot <- plot + peaks_layers(peaks) + geom_hline(yintercept = 0, color = "black") +
    geom_ribbon(data = df, aes(x = .data[[col_x]], ymin = exceso.lower, ymax = exceso.upper, fill = .data[[col_group]]), alpha = 0.25) +
    geom_col(data = df, aes(x = .data[[col_x]], y = exceso, fill = .data[[col_group]]), position = 'identity', alpha = 0.75) +
    # add 3-month centered moving average to reduce noise
    geom_line(data = df %>% group_by(.data[[col_group]]) %>% arrange(.data[[col_x]]) %>% mutate(exceso_ma = as.numeric(stats::filter(exceso, rep(1/3,3), sides = 2))) %>% ungroup(),
              aes(x = .data[[col_x]], y = exceso_ma, color = .data[[col_group]], group = .data[[col_group]]), size = 0.9, na.rm = TRUE) +
    facet_wrap(~.data[[col_group]], scales = "free_y", ncol = facet_ncol, labeller = labeller(.rows = label_wrap_gen(width = 18))) +
    {
      base_caption <- "L\u00ednea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ a\u00f1o + mes). Intervalo de confianza: 95%.\nDatos del Ministerio de Salud Argentina - DEIS.\nAn\u00e1lisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
      caption_text <- if(tolower(peaks) == "yes") paste0(base_caption, "\nLas l\u00edneas rojas verticales corresponden a las tres principales olas de COVID-19.\nExceso = Observados - Esperados (GAM entrenado 2015-2019).") else paste0(base_caption, "\nExceso = Observados - Esperados (GAM entrenado 2015-2019).")
      labs(x = "A\u00f1o-Mes", y = "Exceso de fallecidos (cantidad - esperado)", caption = caption_text)
    } +
    theme_bw(base_size = 18) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none", strip.text = element_text(size = 14), plot.title = element_text(vjust=0.5,hjust=0.5), plot.caption = element_text(size=12,hjust=0)) +
    scale_y_continuous(minor_breaks = NULL)

  if (is.Date(df[[col_x]])) {
    data_min_raw <- suppressWarnings(min(df[[col_x]], na.rm = TRUE))
    data_max_raw <- suppressWarnings(max(df[[col_x]], na.rm = TRUE))
    if (is.na(data_min_raw)) data_min_raw <- as.Date("2015-01-01")
    if (is.na(data_max_raw)) data_max_raw <- data_min_raw
    min_month <- as.Date(paste0(format(data_min_raw, "%Y-%m"), "-01"))
    max_month <- as.Date(paste0(format(data_max_raw, "%Y-%m"), "-01"))
    max_next_month <- seq(max_month, by = "month", length.out = 2)[2]
    start_year <- as.integer(format(min_month, "%Y"))
    end_year_next <- as.integer(format(max_month, "%Y")) + 1
    jan_breaks <- as.Date(paste0(seq(start_year, end_year_next), "-01-01"))
    plot <- plot + scale_x_date(minor_breaks = NULL, breaks = jan_breaks, date_labels = "%Y-%m", limits = c(min_month, max_next_month), expand = c(0,0))
  }
  # continuous x (years) handling when not date
  if (!is.Date(df[[col_x]])) {
    yrs <- suppressWarnings(range(as.integer(df[[col_x]]), na.rm = TRUE))
    if (any(is.infinite(yrs))) yrs <- c(2015, 2023)
    plot <- plot + scale_x_continuous(minor_breaks = NULL, breaks = seq(yrs[1], yrs[2], by = 1))
  }
  # set provided title
  plot <- plot + ggtitle(title)
  ggplot2::ggsave(fname, plot = plot, device = ragg::agg_png, dpi = 400, width = 18, height = 10)
  return(plot)
}

plot_norm_data <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
 plot<- ggplot(data = dataframe %>% filter(cantidad_norm<400), aes(x = .data[[col_x]], y = cantidad_norm))
 # add shared peaks lines behind other layers when requested
 plot <- plot + peaks_layers(peaks) +
  geom_point() +
  geom_line(aes(group = .data[[col_group]], color = .data[[col_group]], y = pred_norm)) +
  geom_ribbon(aes(ymin = pred.lower_norm, ymax = pred.upper_norm, fill = .data[[col_group]]), alpha = 0.4) +
  facet_wrap(~ .data[[col_group]], labeller = labeller(.rows = label_wrap_gen(width = 18))) +
  {
    base_caption <- "L\u00ednea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ a\u00f1o + mes). Intervalo de confianza: 95%.\nDatos del Ministerio de Salud Argentina - DEIS.\nAn\u00e1lisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
    caption_text <- if(tolower(peaks) == "yes") paste0(base_caption, "\nLas l\u00edneas rojas verticales corresponden a las tres principales olas de COVID-19.") else base_caption
    labs(x = 'A\u00f1o', y = 'Fallecidos anuales (valor 2015=100)', color = "Jurisdiccion", fill = "Jurisdiccion", caption = caption_text)
  } +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 # peaks are handled by peaks_layers() earlier; no-op here
 if (is.Date(dataframe[[col_x]])) {
  data_min_raw <- suppressWarnings(min(dataframe[[col_x]], na.rm = TRUE))
  data_max_raw <- suppressWarnings(max(dataframe[[col_x]], na.rm = TRUE))
  if (is.na(data_min_raw)) data_min_raw <- as.Date("2015-01-01")
  if (is.na(data_max_raw)) data_max_raw <- data_min_raw
  min_month <- as.Date(paste0(format(data_min_raw, "%Y-%m"), "-01"))
  max_month <- as.Date(paste0(format(data_max_raw, "%Y-%m"), "-01"))
  max_next_month <- seq(max_month, by = "month", length.out = 2)[2]
  start_year <- as.integer(format(min_month, "%Y"))
  end_year_next <- as.integer(format(max_month, "%Y")) + 1
  jan_breaks <- as.Date(paste0(seq(start_year, end_year_next), "-01-01"))
  plot <- plot + scale_x_date(minor_breaks = NULL, breaks = jan_breaks, date_labels = "%Y", limits = c(min_month, max_next_month), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL, breaks = seq(2015, 2021, by = 1))
 }
  ggplot2::ggsave(fname, plot = plot, device = ragg::agg_png, dpi = 400, width = 18, height = 10)
 return(plot)
}

plot_abs_excess <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
  # ensure exceso columns exist
  if(!"exceso" %in% names(dataframe)){
    dataframe <- dataframe %>% mutate(exceso = as.numeric(cantidad) - as.numeric(pred),
                                      exceso.lower = as.numeric(cantidad) - as.numeric(pred.upper),
                                      exceso.upper = as.numeric(cantidad) - as.numeric(pred.lower))
  }

  base_caption <- "L\u00ednea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ a\u00f1o + mes). Intervalo de confianza: 95%.\nDatos del Ministerio de Salud Argentina - DEIS.\nAn\u00e1lisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA"
  caption_text <- if(tolower(peaks) == "yes") paste0(base_caption, "\nLas l\u00edneas rojas verticales corresponden a las tres principales olas de COVID-19.\nExceso = Observados - Esperados (GAM entrenado 2015-2019).") else paste0(base_caption, "\nExceso = Observados - Esperados (GAM entrenado 2015-2019).")

  plot <- ggplot() +
    peaks_layers(peaks) +
    geom_hline(yintercept = 0, color = "black") +
    geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = exceso.lower, ymax = exceso.upper, fill = .data[[col_group]]), alpha = 0.4) +
    geom_col(data = dataframe, aes(x = .data[[col_x]], y = exceso, fill = .data[[col_group]]), position = 'identity', alpha = 0.8) +
    facet_wrap(~.data[[col_group]], scales = "free_y", labeller = labeller(.rows = label_wrap_gen(width = 18))) +
    labs(x = "A\u00f1o-Mes", y = "Exceso de fallecidos", caption = caption_text) +
    theme_bw(base_size=18) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none", strip.text = element_text(size = 14), plot.title = element_text(vjust=0.5,hjust=0.5), plot.caption = element_text(size=12,hjust=0)) +
    scale_y_continuous(minor_breaks = NULL) +
    ggtitle(title)

  if(is.Date(dataframe[[col_x]])){
    data_min_raw <- suppressWarnings(min(dataframe[[col_x]], na.rm = TRUE))
    data_max_raw <- suppressWarnings(max(dataframe[[col_x]], na.rm = TRUE))
    if (is.na(data_min_raw)) data_min_raw <- as.Date("2015-01-01")
    if (is.na(data_max_raw)) data_max_raw <- data_min_raw
    min_month <- as.Date(paste0(format(data_min_raw, "%Y-%m"), "-01"))
    max_month <- as.Date(paste0(format(data_max_raw, "%Y-%m"), "-01"))
    max_next_month <- seq(max_month, by = "month", length.out = 2)[2]
    start_year <- as.integer(format(min_month, "%Y"))
    end_year_next <- as.integer(format(max_month, "%Y")) + 1
    jan_breaks <- as.Date(paste0(seq(start_year, end_year_next), "-01-01"))
    plot <- plot + scale_x_date(minor_breaks = NULL, breaks = jan_breaks, date_labels = "%Y-%m", limits = c(min_month, max_next_month), expand = c(0,0))
  } else {
    plot <- plot + scale_x_continuous(minor_breaks = NULL, breaks = seq(2015, 2021, by = 1))
  }

  ggplot2::ggsave(fname, plot = plot, device = ragg::agg_png, dpi = 400, width = 18, height = 10)
  return(plot)
}

###TABLE CREATION FUNCTIONS###
create_and_save_kable <- function(df_n, output_file) {
 tbl <- kable(df_n,
              format="latex",
              booktabs = T,
              align = c("l",rep("r",14)),
              col.names = c("Cause of Death",rep(c("Estimated\nExcess","95%CI"),7)),
              escape=FALSE) %>%
  kable_classic(full_width = FALSE,
                font_size = 10) %>%
  kable_styling(latex_options=c("scale_down","striped"),
                position = "center",
                fixed_thead = TRUE
  ) %>%
  add_header_above(c("", "2015" = 2, "2016" = 2, "2017" = 2, "2018" = 2, "2019" = 2, "2020" = 2, "2021" = 2),bold = TRUE) %>%
  column_spec(1, bold = TRUE, color = "#3c3c3c", border_right = TRUE) %>%
  column_spec(c(1,3,5,7,9,11,13,15), border_right = TRUE) %>%
  row_spec(seq(0, nrow(df_n), 2), background = "#f5f5f5")

 save_kable(tbl, output_file)
}
