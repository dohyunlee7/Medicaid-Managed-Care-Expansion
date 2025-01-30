#'
#' did_analysis.R -- Implement difference-in-difference model to extended data
#'

library(dplyr)
library(haven)
library(datasets)
library(readxl)
library(pubtheme)
library(fastDummies)
library(did)
library(lfe)
library(tidyverse)


### 1. Load data, preprocess ###

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel_inflation_adj.rds"))

jumps <- readRDS(paste0(path, "/Temp/jumps.rds"))

# Reformat state names
new_merged_data$state <- tolower(new_merged_data$state)
jumps$state <- tolower(jumps$state)

# Remove Puerto Rico
new_merged_data <- new_merged_data %>%
  filter(state != "Puerto Rico") %>%
  filter(year %in% 1995:2022)

# Merge 1991-2022 panel with mandate data
main_data <- left_join(new_merged_data, jumps, by = "state")

# Categorize states into treatment groups
never_treated_states <- c("alabama", "alaska", "maine",
                          "montana", "south dakota", "wyoming")

clean_treated_states <- c("arizona", "connecticut", "delaware", "hawaii",
                          "new jersey", "north carolina", "new mexico",
                          "oregon", "rhode island", "tennessee", "georgia",
                          "iowa", "nebraska", "new hampshire", "utah",
                          "washington")

gradually_treated_states <- c("arkansas", "idaho", "indiana", "maryland", 
                              "michigan", "oklahoma", "massachusetts",
                              "florida", "illinois", "kentucky", "louisiana",
                              "mississippi", "north dakota", "california",
                              "colorado", "district of columbia", "kansas",
                              "minnesota", "missouri", "nevada", "new york",
                              "ohio", "pennsylvania", "south carolina",
                              "texas", "virginia", "west virginia", "wisconsin",
                              "vermont")

# Create variable for treatment categories
main_data <- main_data %>%
  mutate(category = ifelse(state %in% clean_treated_states,
                           "clean treated",
                           ifelse(state %in% gradually_treated_states,
                                  "gradually treated",
                                  "never treated")))

# Create variable for medicaid spending per capita and fill treatment years 
# for never treated states with Inf
main_data <- main_data %>%
  mutate(all_medicaid_spending_per_cap = `total medicaid (mt + at)` / total_med_enr) %>%
  mutate(treatment_year = ifelse(category == "never treated",
                                 Inf,
                                 treatment_year))

# Create variable for center time for the treated groups
# Create binary treatment variable
dat <- main_data %>%
  mutate(center_time = ifelse(category %in% c("clean treated", "gradually treated"),
                              year - treatment_year,
                              0)) %>%
  dummy_cols(select_columns = "center_time") %>%
  mutate(treated = case_when(
    state %in% c(clean_treated_states, gradually_treated_states) & 
      year >= treatment_year ~ 1,
    state %in% c(clean_treated_states, gradually_treated_states) & 
      year < treatment_year ~ 0,
    state %in% never_treated_states ~ 0
  ))

keepvars <- c(paste0("`center_time_-", 20:1, "`"), 
              "center_time_0", 
              paste0("center_time_", 1:20))

prep_es_10 <- function(mod){
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se
  )
  
  es <- mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-10:-2, 0:10)) %>% 
    select(t, estimate, se)
  es <- rbind(es, c(-1, 0, 0))
  es <- es %>% mutate(lower = estimate - 1.96 * se,
                      upper = estimate + 1.96 * se)
  return(es)
}

prep_es_20 <- function(mod){
  
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se
  )
  
  es <- mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-20:-2, 0:20)) %>% 
    select(t, estimate, se)
  es <- rbind(es, c(-1, 0, 0))
  es <- es %>% mutate(lower = estimate - 1.96 * se,
                      upper = estimate + 1.96 * se)
  return(es)
}


### DiD - 10 periods

mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ 
                     `center_time_-10` + `center_time_-9` + `center_time_-8` + `center_time_-7` +
                     `center_time_-6` + `center_time_-5` + `center_time_-4` + `center_time_-3` +
                     `center_time_-2` + `center_time_-1` + 
                     `center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` +
                     `center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + 
                     `center_time_9` + `center_time_10`  |
                     as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -10 & center_time <= 10),
                   exactDOF = TRUE)

es_d <- prep_es_10(mod_d)

es_d <- es_d %>% 
  mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))

mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ treated | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -10 & center_time <= 10), 
                   exactDOF = TRUE)

summary(mod_d)
coef <- round(mod_d$coefficients[1],3)

star <- ifelse(mod_d$pval[1] <= 0.01, "***",
               ifelse(mod_d$pval[1] <= 0.05, "**",
                      ifelse(mod_d$pval[1] <= 0.1, "*", "")))

se <- round(mod_d$se[1],3)
comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")

# Event study plot
p1 <- ggplot(data = es_d, aes(x = t, y = estimate, group = 1))+
  geom_point(size= 3, color = "gray26")+
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width=.1, 
                linewidth = 0.8, 
                color = "gray26") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             linewidth = 1,
             color = "firebrick4")+
  scale_x_continuous(breaks = round(seq(-10, 10, by = 1),1),
                     limits = c(-10.5, 10.5))+
  scale_y_continuous(breaks = round(seq(-0.5, 0.5, by = 0.1), 1),
                     limits = c(-0.5, 0.5))+
  labs(x = "Relative Time", y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position="none")+
  annotate("text", x = 8, y = -0.4, label = comb_est, size = 5)
print(p1)









