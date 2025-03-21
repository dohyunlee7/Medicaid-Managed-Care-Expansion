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

### ------------------------------ FUNCTIONS ------------------------------- ###

# For 10 year period DiD
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

# For 20 year period DiD
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

### ------------------------------------------------------------------------ ###


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
  filter(state != "puerto rico")

# Merge 1991-2022 panel with treatment year data
main_data <- left_join(new_merged_data, jumps, by = "state")

# Categorize states into treatment groups
always_treated <- c("utah", "maryland",
                    "michigan" ,"minnesota", "illinois",
                    "ohio", "wisconsin")

never_treated_states <- c("alabama", "alaska", "maine",
                          "montana", "south dakota", "wyoming")

clean_treated_states <- c("arizona", "connecticut", "delaware", "hawaii",
                          "new jersey", "north carolina", "new mexico",
                          "oregon", "rhode island", "tennessee", "georgia",
                          "iowa", "nebraska", "new hampshire", "nevada",
                          "washington")

gradually_treated_states <- c("arkansas", "idaho", "indiana", 
                              "oklahoma", "massachusetts",
                              "florida", "illinois", "kentucky", "louisiana",
                              "mississippi", "north dakota", "california",
                              "colorado", "district of columbia", "kansas",
                               "missouri", "new york",
                               "pennsylvania", "south carolina",
                              "texas", "virginia", "west virginia", 
                              "vermont")

main_data <- main_data %>%
  filter(!state %in% c(always_treated, "connecticut", "oklahoma", "vermont"))

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


### DiD - 10 periods ###

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
  geom_point(size = 3, color = "gray26")+
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = .1, 
                linewidth = 0.8, 
                color = "gray26") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             linewidth = 0.7,
             color = "firebrick4")+
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = 0.7,
             color = "green4") +
  scale_x_continuous(breaks = round(seq(-10, 10, by = 1),1),
                     limits = c(-10.5, 10.5))+
  # scale_y_continuous(breaks = seq(-0.15, 0.30, by = 0.1),
  #                    limits = c(-0.15, 0.30))+
  labs(title = "Log Medicaid Spending Per Capita",
       subtitle = "10-Year Period",
       x = "Relative Time", 
       y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none")+
  annotate("text", x = 8, y = -0.2, label = comb_est, size = 5) +
  theme_pub()

print(p1)

ggsave(paste0(path, "/Output/did_10yr.png"), 
       plot = p1, 
       width = 12, 
       height = 10, 
       dpi = 300)

### DiD - 20 periods ###

mod_d <- lfe::felm(pct_in_comp_mco ~ 
                     `center_time_-20` + `center_time_-19` + `center_time_-18` + `center_time_-17` +
                     `center_time_-16` + `center_time_-15` + `center_time_-14` + `center_time_-13` +
                     `center_time_-12` + `center_time_-11` + 
                     `center_time_-10` + `center_time_-9` + `center_time_-8` + `center_time_-7` +
                     `center_time_-6` + `center_time_-5` + `center_time_-4` + `center_time_-3` +
                     `center_time_-2` + `center_time_-1` + 
                     `center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` +
                     `center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + 
                     `center_time_9` + `center_time_10` + `center_time_11` + `center_time_12` +
                     `center_time_13` + `center_time_14` + `center_time_15` + `center_time_16` +
                     `center_time_17` + `center_time_18` + `center_time_19` + `center_time_20`  |
                     as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -20 & center_time <= 20),
                   exactDOF = TRUE)

es_d <- prep_es_20(mod_d)

es_d <- es_d %>% 
  mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))

mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ treated | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -20 & center_time <= 20), 
                   exactDOF = TRUE)

summary(mod_d)
coef <- round(mod_d$coefficients[1],3)

star <- ifelse(mod_d$pval[1] <= 0.01, "***",
               ifelse(mod_d$pval[1] <= 0.05, "**",
                      ifelse(mod_d$pval[1] <= 0.1, "*", "")))

se <- round(mod_d$se[1],3)
comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")

# Event study plot
p2 <- ggplot(data = es_d, aes(x = t, y = estimate, group = 1))+
  geom_point(size = 3, color = "gray26")+
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width=.1, 
                linewidth = 0.8, 
                color = "gray26") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             linewidth = 0.7,
             color = "firebrick4")+
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = 0.7,
             color = "green4") +
  scale_x_continuous(breaks = round(seq(-20, 20, by = 1),1),
                     limits = c(-20.5, 20.5))+
  scale_y_continuous(breaks = seq(-0.20, 0.50, by = 0.1),
                     limits = c(-0.20, 0.50))+
  labs(title = "Log Medicaid Spending Per Capita",
       subtitle = "20-Year Period",
       x = "Relative Time", 
       y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position="none")+
  annotate("text", x = 17, y = -0.2, label = comb_est, size = 5) +
  theme_pub()

print(p2)


ggsave(paste0(path, "/Output/did_20yr.png"), 
       plot = p2, 
       width = 12, 
       height = 10, 
       dpi = 300)

### ----------------------- Replicating by cohort -------------------------- ###

treat_data <- dat %>%
  filter(year %in% 1992:2022) %>%
  group_by(state) %>%
  filter(any(treatment_year == 2012) | all(treated == 0))

mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ 
                     `center_time_-10` + `center_time_-9` + `center_time_-8` +
                     `center_time_-7` + `center_time_-6` +
                     `center_time_-5` + `center_time_-4` + `center_time_-3` +
                     `center_time_-2` + `center_time_-1` + 
                     `center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` +
                     `center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + 
                     `center_time_9` + `center_time_10`  |
                     as.factor(state) + as.factor(year),
                   data = subset(treat_data, center_time >= -10 & center_time <= 10),
                   exactDOF = TRUE)

es_d <- prep_es_10(mod_d)

es_d <- es_d %>% 
  mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))

mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ treated | as.factor(state) + as.factor(year),
                   data = subset(treat_data, center_time >= -10 & center_time <= 10), 
                   exactDOF = TRUE)

summary(mod_d)
coef <- round(mod_d$coefficients[1],3)

star <- ifelse(mod_d$pval[1] <= 0.01, "***",
               ifelse(mod_d$pval[1] <= 0.05, "**",
                      ifelse(mod_d$pval[1] <= 0.1, "*", "")))

se <- round(mod_d$se[1],3)
comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")

# Event study plot
p <- ggplot(data = es_d, aes(x = t, y = estimate, group = 1))+
  geom_point(size = 3, color = "gray26")+
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = .1, 
                linewidth = 0.8, 
                color = "gray26") + 
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             linewidth = 0.7,
             color = "firebrick4")+
  geom_vline(xintercept = 0,
             linetype = "dashed",
             linewidth = 0.7,
             color = "green4") +
  scale_x_continuous(breaks = round(seq(-10, 10, by = 1),1),
                     limits = c(-10.5, 10.5))+
  scale_y_continuous(breaks = seq(-1, 1, by = 0.2),
                     limits = c(-1, 1))+
  labs(title = "Log Medicaid Spending Per Capita",
       subtitle = "10-Year Period, Treatment Year = 2012",
       x = "Relative Time", 
       y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none")+
  annotate("text", x = 8, y = -0.8, label = comb_est, size = 5) +
  theme_pub()

print(p)

ggsave(paste0(path, "/Output/did_10yr_2012.png"), 
       plot = p, 
       width = 12, 
       height = 10, 
       dpi = 300)









