
library(pacman)
p_load(httr,readxl,dplyr,ggplot2,scales,ggrepel,viridis,padr,gganimate,wesanderson,readr,forcats,kableExtra,tidyr,mgcv,MASS,lubridate,ggborderline)
Sys.setlocale("LC_ALL","English")
`%notin%` <- Negate(`%in%`)
ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}
is.Date <- function(x) inherits(x, 'Date')
colu <- function(df,colu=colu) {return(df %>% pull(colu))}
###IMPORTANTE###
setwd("C:/Users/Usuario/Documents/GitHub/COVID19/mortalidad_arg")
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy+accuracy}

###DEFINE FUNCTIONS###
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
normalize_data <- function(df, group_col) {
 df_norm <- df %>%
  group_by(.data[[group_col]]) %>%
  mutate(cantidad_norm = cantidad/cantidad[anio_def == 2015] * 100,          pred_norm = pred/cantidad[anio_def == 2015] * 100,          pred.upper_norm = pred.upper/cantidad[anio_def == 2015] * 100,          pred.lower_norm = pred.lower/cantidad[anio_def == 2015] * 100) %>%
  ungroup()
 return(df_norm)
}
plot_abs_data <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
 plot <- ggplot() +
  geom_point(data = dataframe , aes(x = .data[[col_x]], y = cantidad)) +
  geom_line(data = dataframe , aes(x = .data[[col_x]], y = pred, group = .data[[col_group]], color = .data[[col_group]])) +
  geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = `pred.lower`, ymax = `pred.upper`, fill = .data[[col_group]]), alpha = 0.4) +
  facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width = 18))) +
  labs(x = "Año", y = "Fallecidos anuales") +
  theme_bw(base_size=18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){
  plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2022-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2021, by = 1))
 }
 ggsave(fname, plot, dpi = 400, width = 18, height = 10)
 return(plot)
}
plot_abs_data_line <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO',date_breaks="1 year",point_size=1,line_size=1){
 plot <- ggplot() +
  geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = `pred.lower`, ymax = `pred.upper`, fill = .data[[col_group]]), alpha = 0.4) +
  geom_borderline(data = dataframe , aes(x = .data[[col_x]], y = pred, group = .data[[col_group]], color = .data[[col_group]]),size=line_size) +
  geom_borderline(data = dataframe , aes(x = .data[[col_x]], y = cantidad),size = line_size) +
  geom_point(data = dataframe , aes(x = .data[[col_x]], y = cantidad),size=point_size) +
  facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width = 18))) +
  labs(x = "Año-Mes", y = "Fallecidos mensuales") +
  theme_bw(base_size=18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){
  plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = date_breaks, date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2022-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2021, by = 1))
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
  labs(x = 'Año', y = 'Fallecidos anuales (valor 2015=100)', color = "Jurisdiccion", fill = "Jurisdiccion") +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){
  plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2022-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2021, by = 1))
 }
 ggsave(fname, dpi = 400, width = 18, height = 10)
 return(plot)
}
calculate_excess <- function(df, col_group = 'grupo_causa_defuncion_CIE10', col_x = 'anio_def') {
 if (inherits(df[[col_x]], "Date")) {
  df2 <- df %>% mutate(years = format(df[[col_x]], "%Y"))
 } else {
  df2 <- df %>% mutate(years = df[[col_x]])
 }
 df3 <- df2 %>% group_by({{ col_group }}, years) %>%
  mutate(exceso=cantidad-pred,exceso_porc=exceso/pred*100,exceso.lower=cantidad-pred.upper,exceso.upper=cantidad-pred.lower)
 #mutate(exceso = cantidad - pred, exceso_porc = exceso / pred * 100)
 test <- df3 %>% group_by(.data[[col_group]]) %>% filter(years < 2020) %>%
  mutate(p75 = quantile(cantidad, 0.75)) %>%
  dplyr::select({{ col_group }}, p75) %>% distinct()
 df4 <- merge(test, df3, by = {{ col_group }}, all.y = TRUE) %>%
  group_by({{ col_group }}, anio_def) %>%
  mutate(exceso_p75 = cantidad - p75, exceso_porc_p75 = exceso_p75 / p75 * 100)
 return(df4)
}
plot_abs_excess <- function(dataframe, fname, col_group='jurisdiccion',col_x='anio_def',title='Titulo',peaks='NO'){
 plot <- ggplot() +
  geom_hline(yintercept = 0, color = "black")+
  geom_line(data = dataframe , aes(x = .data[[col_x]], y = exceso)) +
  geom_ribbon(data = dataframe, aes(x = .data[[col_x]], ymin = `exceso.lower`, ymax = `exceso.upper`, fill = grupo_causa_defuncion_CIE10), alpha = 0.4) +
  facet_wrap(~.data[[col_group]], scales="free_y", labeller = labeller(.rows = label_wrap_gen(width = 18))) +
  labs(x = "Año", y = "Exceso de fallecidos") +
  theme_bw(base_size=18) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = "none", strip.text = element_text(size = 14),plot.title=element_text(vjust=0.5,hjust=0.5))+
  scale_y_continuous(minor_breaks = NULL)+
  ggtitle(title)
 if(tolower(peaks) == "yes") {
  plot <- plot + geom_vline(xintercept = as.numeric(as.Date("2020-09-01")), color = "red") + geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red")
 }
 if(is.Date(dataframe[[col_x]])){
  plot <- plot + scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%m-%Y", limits = c(as.Date("2015-01-01"), as.Date("2022-01-01")), expand = c(0,0))
 } else {
  plot <- plot + scale_x_continuous(minor_breaks = NULL,breaks = seq(2015, 2021, by = 1))
 }
 ggsave(fname, plot, dpi = 400, width = 18, height = 10)
 return(plot)
}
calculate_trend_GAM <- function(df, colname) {
 # Subset dataframe to only include years 2015-2019 and exclude month "00"
 df_subset <- subset(df, anio_def >= 2015 & anio_def <= 2019 & mes_def != "00")
 # Define a vector of all possible years (2015-2019)   # Define a vector of all possible months (01-12)
 all_years <- 2015:2019; all_months <- sprintf("%02d", 1:12)
 # Create a data frame with all possible combinations of anio_def and mes_def
 full_data <- expand.grid(anio_def = all_years, mes_def = all_months)
 #TEST
 #df_subset1 <- subset(df_subset, df_subset[[colname]]==levels(df_subset[[colname]])[5])
 #merged_data <- merge(full_data, df_subset1, all.x = TRUE)

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
 #gam_models <- by(df_subset, df_subset[[colname]], function(x) gam(cantidad ~ anio_def + mes_def, data = x))
 # Predict using the GAM model and calculate confidence intervals
 new_data <- expand.grid(
  colname = names(gam_models),
  anio_def = 2015:2021,
  mes_def = sprintf("%02d", 1:12) # all months from 01 to 12
 )
 names(new_data)[1]<-{{colname}}
 df_trend <- data.frame()
 for (name in levels(as.factor(df_subset[[colname]]))) {
  #TEST
  #name<-levels(as.factor(df_subset[[colname]]))[i]
  data <- new_data[new_data[[colname]] == name,]
  if (!is.null(gam_models[[name]])) {
   preds <- predict(gam_models[[name]], newdata = data, se.fit = TRUE)
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
 #TEST
 #df_subset1 <- subset(df_subset, df_subset[[colname]]==levels(df_subset[[colname]])[5])
 #merged_data <- merge(full_data, df_subset1, all.x = TRUE)

 # Fit GAM model to subsetted data for each "colname" (grupo_causa_defuncion_CIE10)
 gam_models <- by(df_subset, df_subset[[colname]], function(x) {
  # Merge the full_data data frame with the original data frame x to fill in any missing values with cantidad = 0
  #print(x)
  merged_data <- merge(full_data, x, all.x = TRUE,by=c("anio_def","mes_def"))
  #print(merged_data)
  #TEST
  #merged_data <- merge(full_data, df_subset, all.x = TRUE,by=c("anio_def","mes_def"))
  ###
  name<-x[[colname]][1]
  merged_data$cantidad[is.na(merged_data$cantidad)] <- 0 # Replace NAs with 0
  merged_data$cantidad<-as.numeric(as.character(merged_data$cantidad))
  merged_data$grupo_causa_defuncion_CIE10[is.na(merged_data$grupo_causa_defuncion_CIE10)] <- name # Replace NAs with colname
  #print(merged_data)
  tryCatch(
   gam(cantidad ~ anio_def + s(mes_def), data = merged_data,method="REML"),
   error = function(e) NULL
  )
 })
 #gam_models <- by(df_subset, df_subset[[colname]], function(x) gam(cantidad ~ anio_def + mes_def, data = x))
 # Predict using the GAM model and calculate confidence intervals
 new_data <- expand.grid(
  colname = names(gam_models),
  anio_def = 2015:2021,
  mes_def = 1:12 # all months from 01 to 12
 )
 names(new_data)[1]<-{{colname}}
 df_trend <- data.frame()
 for (name in levels(as.factor(df_subset[[colname]]))) {
  #TEST
  #name<-levels(as.factor(df_subset[[colname]]))[i]
  data <- new_data[new_data[[colname]] == name,]
  if (!is.null(gam_models[[name]])) {
   preds <- predict(gam_models[[name]], newdata = data, se.fit = TRUE)
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
 #return(gam_models)

}
reorder_factors <- function(df, colname,cantidad) {
 #Calculate max values for each group
 max_vals <- df %>% group_by(.data[[colname]]) %>% summarize(max_cantidad = max(.data[[cantidad]]))
 # Arrange col_group factor levels in descending order of max_cantidad
 df[[colname]] <- factor(df[[colname]], levels = max_vals[[colname]][order(-max_vals$max_cantidad)])
 return(df)
}
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


#####################

#LOAD DATA
datamort2 <- read_csv("arg_def_15_21.zip",
                      skip=0,locale = readr::locale(encoding = "latin1") ,guess_max = 250,
)
#FACTOR CAUSES
suicidio<-c(paste0("X",60:84))
datamort2 <- datamort2 %>% mutate(grupo_causa_defuncion_CIE10 = ifelse(cod_causa_muerte_CIE10 %in% suicidio, "Suicidio", grupo_causa_defuncion_CIE10))
datamort2$grupo_causa_defuncion_CIE10<-(as.factor(datamort2$grupo_causa_defuncion_CIE10))
#CHANGE CAUSE NAMES
levels(datamort2$grupo_causa_defuncion_CIE10)<-c("ENF INFECCIOSAS","TUMORES MALIGNOS","TUMORES BENIGNOS","DIABETES","ANEMIA O DEF NUTRICIONAL","TRAST METABOLICOS","MENINGITIS","ALZHEIMER","TRASTORNOS MENTALES","ENF SIST CIRCULATORIO","ENF SIST RESPIRATORIO","APEND, HERNIA, OBST INSTEST","ENF SIST HEPATICO","ENF SIST URINARIO","EMBARAZO Y PARTO","AFECC PERINATALES","MALFORMACIONES","CAUSAS EXTERNAS","DEMAS CAUSAS DEFINIDAS","CAUSAS MAL DEFINIDAS","SUICIDIO")
#FACTOR EDAD
datamort2$grupo_etario<-as.factor(datamort2$grupo_etario)
#CHANGE AGE NAMES
levels(datamort2$grupo_etario)<- c("0-20","20-39","40-49","50-59","60-69","70-79","80-200","Desconocido")

######################################################
##########MENSUAL POR CAUSA Y GRUPO ETARIO###########
######################################################
# Get unique levels of grupo_etario
etario_levels <- levels(datamort2$grupo_etario)
etario_levels<- etario_levels[1]

# Create empty list to store results
df_plot3_list <- list()
# Loop over each level of grupo_etario
for (i in seq_along(etario_levels)) {
 etario_level <- etario_levels[i]
 # Subset the data to the current level of grupo_etario
 subset_data <- datamort2[datamort2$grupo_etario == etario_level, ]
 # Calculate monthly counts
 cantidad_mensual <- aggregate(cantidad ~ grupo_causa_defuncion_CIE10 + mes_def + anio_def, data = subset_data, sum)
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
 if (etario_level == "0-20") {
                               plot_abs_data_line(df_plot3 %>% filter(grupo_causa_defuncion_CIE10 != "ALZHEIMER"), paste0(etario_level,"_tendencias_mortalidad_mensual_causas_picos.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="yes",point_size=0.01,line_size=0.8)
                               plot_abs_data_line(df_plot3 %>% filter(grupo_causa_defuncion_CIE10=="SUICIDIO"), paste0(etario_level,"_tendencias_mortalidad_mensual_SUICIDIOS.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="no",date_breaks="3 months",point_size=3,line_size=2.5)
                              }
 else {plot_abs_data_line(df_plot3, paste0(etario_level,"_tendencias_mortalidad_mensual_causas_picos.png"), col_group='grupo_causa_defuncion_CIE10', col_x='fecha', title=paste0("Fallecidos mensuales por Causa, edad = ", etario_level), peaks="yes")}
}


