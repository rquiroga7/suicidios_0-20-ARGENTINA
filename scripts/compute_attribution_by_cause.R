# compute_attribution_by_cause.R
library(tidyverse)

res_month <- read_csv("results_excess_monthly_by_age.csv")
coefs <- read_csv("models_excess_vs_covid_coefs_by_age_cause.csv")

# causes to include
causes <- c("resp","circ","diabetes","alzheimer","metabolic","anemia")

# baseline proportions per age x cause (2015-2019)
baseline <- res_month %>% filter(anio_def >= 2015 & anio_def <= 2019) %>%
  group_by(age) %>%
  summarise(across(all_of(causes), ~sum(.x, na.rm=TRUE)),
            total_all = sum(obs, na.rm=TRUE)) %>%
  rowwise() %>%
  mutate(across(all_of(causes), ~ ifelse(total_all>0, .x/total_all, 0), .names = "prop_{col}")) %>%
  select(age, starts_with("prop_"))

# allocate total predicted into cause-specific predicted using baseline proportions
res_alloc <- res_month %>%
  left_join(baseline, by = "age") %>%
  rowwise() %>%
  mutate(across(all_of(causes), ~ as.numeric(.x), .names = "obs_{col}")) %>%
  {.
    df <- .
    for(c in causes){
      prop_col <- paste0("prop_",c)
      pred_c <- df$pred * df[[prop_col]]
      df[[paste0("pred_",c)]] <- pred_c
      df[[paste0("excess_",c)]] <- df[[c]] - pred_c
    }
    df
  }

# Check: sum of cause-level pred should equal total pred (within rounding)
# compute yearly aggregates per age x cause
monthly_with_age_cause <- res_alloc %>%
  mutate(month_int = as.integer(month))

yearly <- monthly_with_age_cause %>%
  group_by(age, anio_def) %>%
  summarise(across(starts_with("excess_"), ~sum(.x, na.rm=TRUE)),
            across(starts_with("pred_"), ~sum(.x, na.rm=TRUE)),
            covid_sum = sum(covid, na.rm=TRUE),
            covid_sq_sum = sum(covid^2, na.rm=TRUE),
            .groups = "drop")

# join coefficients (coefs file expected to have columns: age, cause, anio_def, coef, se)
coefs_sel <- coefs %>% select(age, cause, anio_def, coef, se) %>% rename(age=age)

# reshape yearly to long cause rows
## removed intermediate incorrect reshape that referenced 'total_excess_' / 'sum_pred_' prefixes
## proceed to reshape using the actual 'excess_' and 'pred_' column prefixes produced above

# Above pivot produced duplicated cause names with prefix; fix by computing pred_sum directly
yearly_long <- yearly %>% pivot_longer(cols = starts_with("excess_"), names_to = "tmp", values_to = "excess_total") %>%
  mutate(cause = str_remove(tmp, "excess_")) %>% select(-tmp) %>%
  left_join(yearly %>% pivot_longer(cols = starts_with("pred_"), names_to = "tp", values_to = "pred_sum") %>% mutate(cause = str_remove(tp, "pred_")) %>% select(-tp), by = c("age","anio_def","cause"))

# join covid sums
yearly_long <- yearly_long %>% left_join(yearly %>% select(age, anio_def, covid_sum, covid_sq_sum), by = c("age","anio_def"))

# join coefficients (some combos may be missing -> NA)
yearly_long <- yearly_long %>% left_join(coefs_sel, by = c("age","cause","anio_def"))

# compute attributable and delta-method SE and CI (if coef available)
yearly_long <- yearly_long %>%
  mutate(attributable = ifelse(!is.na(coef), coef * covid_sum, NA_real_),
         se_attrib = ifelse(!is.na(se), se * sqrt(covid_sq_sum), NA_real_),
         lower95 = attributable - 1.96 * se_attrib,
         upper95 = attributable + 1.96 * se_attrib)

# Save CSV
dir.create("outputs", showWarnings = FALSE)
write_csv(yearly_long, "outputs/yearly_excess_by_cause_allocated.csv")
write_csv(yearly_long %>% select(age, anio_def, cause, excess_total, attributable, se_attrib, lower95, upper95), "outputs/yearly_excess_attributable_by_cause.csv")

# create formatted per-year tables
years <- sort(unique(yearly_long$anio_def))
for(y in years){
  tab <- yearly_long %>% filter(anio_def==y) %>%
    mutate(cell = sprintf("%0.0f (%0.0f ± %0.0f)", excess_total, ifelse(is.na(attributable), 0, attributable), ifelse(is.na(se_attrib), 0, 1.96 * se_attrib))) %>%
    select(age, cause, cell) %>%
    pivot_wider(names_from = cause, values_from = cell)
  write_csv(tab, paste0("outputs/table_age_by_cause_", y, ".csv"))
}

message("Done: outputs/yearly_excess_attributable_by_cause.csv and per-year tables in outputs/")
