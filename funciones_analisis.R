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
 new_data <- expand.grid(
  colname = names(gam_models),
  anio_def = 2015:2023,
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

###PLOTTING FUNCTIONS###
plot_abs_data <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
 plot <- ggplot() +
  geom_point(data = dataframe , aes(x = .data[[col_x]], y = cantidad)) +
  geom_line(data = dataframe , aes(x = .data[[col_x]], y = pred, group = .data[[col_group]], color = .data[[col_group]])) +
  geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = `pred.lower`, ymax = `pred.upper`, fill = .data[[col_group]]), alpha = 0.4) +
  facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width = 18))) +
  labs(x = "Año", y = "Fallecidos anuales", caption = "Línea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ año + mes). Intervalo de confianza: 95%. Las líneas rojas verticales corresponden a las tres principales olas de COVID-19.\nDatos del Ministerio de Salud Argentina - DEIS. Análisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA") +
  theme_bw(base_size=18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){
  plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = date_breaks, date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2024-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2021, by = 1))
 }
 ggsave(fname, plot, dpi = 400, width = 18, height = 10)
 return(plot)
}

plot_abs_data_line <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO',date_breaks="1 year",point_size=1,line_size=1,facet_ncol=NULL){
 plot <- ggplot() +
  geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = `pred.lower`, ymax = `pred.upper`, fill = .data[[col_group]]), alpha = 0.4) +
  geom_borderline(data = dataframe , aes(x = .data[[col_x]], y = pred, group = .data[[col_group]], color = .data[[col_group]]),size=line_size) +
  geom_borderline(data = dataframe , aes(x = .data[[col_x]], y = cantidad),size = line_size) +
  geom_point(data = dataframe , aes(x = .data[[col_x]], y = cantidad),size=point_size) +
  facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width = 18)), ncol=facet_ncol) +
  labs(x = "Año-Mes", y = "Fallecidos mensuales", caption = "Línea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ año + mes). Intervalo de confianza: 95%. Las líneas rojas verticales corresponden a las tres principales olas de COVID-19.\nDatos del Ministerio de Salud Argentina - DEIS. Análisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA") +
  theme_bw(base_size=18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){
  plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = date_breaks, date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2024-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2023, by = 1))
 }
 ggsave(fname, plot, dpi = 400, width = 18, height = 10)
 return(plot)
}

plot_norm_data <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
 plot<- ggplot(data = dataframe %>% filter(cantidad_norm<400), aes(x = .data[[col_x]], y = cantidad_norm)) +
  geom_point() +
  geom_line(aes(group = .data[[col_group]], color = .data[[col_group]], y = pred_norm)) +
  geom_ribbon(aes(ymin = pred.lower_norm, ymax = pred.upper_norm, fill = .data[[col_group]]), alpha = 0.4) +
  facet_wrap(~ .data[[col_group]], labeller = labeller(.rows = label_wrap_gen(width = 18))) +
  labs(x = 'Año', y = 'Fallecidos anuales (valor 2015=100)', color = "Jurisdiccion", fill = "Jurisdiccion", caption = "Línea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ año + mes). Intervalo de confianza: 95%. Las líneas rojas verticales corresponden a las tres principales olas de COVID-19.\nDatos del Ministerio de Salud Argentina - DEIS. Análisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){ plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2022-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2021, by = 1))
 }
 ggsave(fname, dpi = 400, width = 18, height = 10)
 return(plot)
}

plot_abs_excess <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
 plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black")+
  geom_line(data = dataframe , aes(x = .data[[col_x]], y = exceso)) +
  geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = `exceso.lower`, ymax = `exceso.upper`, fill = grupo_causa_defuncion_CIE10), alpha = 0.4) +
  facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width = 18))) +
  labs(x = "Año", y = "Exceso de fallecidos", caption = "Línea de tendencia: Modelo Aditivo Generalizado (GAM) calculado con datos de 2015-2019 (cantidad ~ año + mes). Intervalo de confianza: 95%. Las líneas rojas verticales corresponden a las tres principales olas de COVID-19.\nDatos del Ministerio de Salud Argentina - DEIS. Análisis por Rodrigo Quiroga. Ver github.com/rquiroga7/suicidios_0-20-ARGENTINA") +
  theme_bw(base_size=18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5),plot.caption=element_text(size=12,hjust=0))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){
  plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2022-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2021, by = 1))
 }
 ggsave(fname, plot, dpi = 400, width = 18, height = 10)
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
