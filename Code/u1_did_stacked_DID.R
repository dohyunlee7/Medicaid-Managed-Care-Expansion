#'
#' did_analysis.R -- Implement difference-in-difference model to extended data
#'
rm(list = ls())

packages <- c("ggplot2", "estimatr", "readxl", "readr", "dplyr",
              "stringr", "ivreg", "tidyverse", "texreg", "ggrepel", 
              "lfe", "fastDummies", "ggpubr", "RColorBrewer", "ggthemes",
              "stargazer", "grid", "foreign", "kableExtra", "lubridate",
              "haven", "fuzzyjoin", "stringi", "zoo", "viridis", "scales")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

options(scipen = 100)
theme_set(theme_clean() + theme(plot.background = element_blank()))
set.seed(2023)

### 1. Load data, preprocess ###

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")
# path <- "C:/Users/ruochens/Dropbox (Penn)/Research project/MCO/Medicaid_privatization_exp"
# setwd("C:/Users/ruochens/Dropbox (Penn)/Research project/MCO/Medicaid_privatization_exp")

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel_inflation_adj.rds"))

jumps <- readRDS(paste0(path, "/Temp/jumps.rds"))
jumps <- jumps %>%
  rename(pct_in_comp_mco_jump = pct_in_comp_mco)

# Reformat state names
new_merged_data$state <- tolower(new_merged_data$state)
jumps$state <- tolower(jumps$state)

# Remove Puerto Rico
new_merged_data <- new_merged_data %>%
  filter(state != "puerto rico") %>%
  filter(year %in% 1991:2022)

# Merge 1991-2022 panel with mandate data
main_data <- left_join(new_merged_data, jumps, by = "state")

# Categorize states into treatment groups
never_treated_states <- c("alabama", "alaska", "maine",
                          "montana", "south dakota", "wyoming",
                          "arkansas", "idaho")

clean_treated_states <- c("arizona", "connecticut", "delaware", "hawaii",
                          "new jersey", "north carolina", "new mexico",
                          "oregon", "rhode island", "tennessee", "georgia",
                          "iowa", "nebraska", "new hampshire", "nevada",
                          "washington")

gradually_treated_states <- c("indiana", "oklahoma", "massachusetts",
                              "florida", "kentucky", "louisiana",
                              "mississippi", "north dakota", "california",
                              "colorado", "district of columbia", "kansas",
                              "missouri", "new york",
                              "pennsylvania", "south carolina",
                              "texas", "virginia", "west virginia",
                              "vermont")
always_treated <- c("utah", "maryland", "michigan",
                    "illinois", "minnesota", "ohio",
                    "wisconsin")

# Create variable for treatment categories
main_data <- main_data %>%
  mutate(category = ifelse(state %in% clean_treated_states,
                           "clean treated",
                           ifelse(state %in% gradually_treated_states,
                                  "gradually treated",
                                  ifelse(state %in% always_treated,
                                         "always treated", 
                                         "never treated"))))

# Create variable for medicaid spending per capita and fill treatment years 
# for never treated states with Inf
main_data <- main_data %>%
  mutate(all_medicaid_spending_per_cap = `total medicaid (mt + at)` / total_med_enr,
         log_all_medicaid_spending_per_cap = log(all_medicaid_spending_per_cap)) %>%
  mutate(treatment_year = case_when(
    category == "clean treated" ~ treatment_year,
    category == "gradually treated" ~ treatment_year,
    category == "never treated" ~ Inf,
    category == "always treated" ~ 1991
  ))

# re-define treatment year
# Standard: the first year a state's Comp MMC is >= 10\% of total enrollment 
main_data <- main_data %>%
  mutate(treatment_year = case_when(
    ## never treated states
    state == "alabama" ~ Inf,
    state == "alaska" ~ Inf,
    state == "maine" ~ Inf,
    state == "montana" ~ Inf,
    state == "south dakota" ~ Inf,
    state == "wyoming" ~ Inf,
    state == "arkansas" ~ Inf,
    state == "idaho" ~ Inf,
    
    ## clean treated states
    state == "arizona" ~ 1996,
    state == "connecticut" ~ 1996, # turn-off in 2012
    state == "delaware" ~ 1996,
    state == "hawaii" ~ 1995,
    state == "new jersey" ~ 1995, 
    state == "north carolina" ~ 2021,
    state == "new mexico" ~ 1997,
    state == "oregon" ~ 1994,
    state == "rhode island" ~ 1995,
    state == "tennessee" ~ 1994,
    state == "georgia" ~ 2006,
    state == "iowa" ~ 2016, # weird; need check
    state == "nebraska" ~ 1996,
    state == "new hampshire" ~ 2014,
    state == "washington" ~ 1994,
    state == "nevada" ~ 1998,
    
    ## gradually treated states
    state == "indiana" ~ 1995, # or 1996
    state == "oklahoma" ~ 1996, # turn off in 2004
    state == "massachusetts" ~ 1992,
    state == "florida" ~ 1992,
    state == "kentucky" ~ 1998,
    state == "louisiana" ~ 2012,
    state == "mississippi" ~ 2011,
    state == "north dakota" ~ 2014,
    state == "california" ~ 1996, # could be 1996 or 1997
    state == "colorado" ~ 1996, # weird
    state == "district of columbia" ~ 1995,
    state == "kansas" ~ 1997,
    state == "missouri" ~ 1996,
    state == "new york" ~ 1995,
    state == "pennsylvania" ~ 1995,
    state == "south carolina" ~ 2007,
    state == "texas" ~ 1997,
    state == "virginia" ~ 1996,
    state == "west virginia" ~ 1997,
    state == "vermont" ~ 1997, # turn off in 2001
    
    ## alwayas treated
    state == "utah" ~ 1991, # weird
    state == "maryland" ~ 1991,
    state == "michigan" ~ 1991,
    state == "illinois" ~ 1991, ## weird
    state == "minnesota" ~ 1991,
    state == "ohio" ~ 1991,
    state == "wisconsin" ~ 1991
    
  ))

# remove always treated
# Create variable for center time for the treated groups
# Create binary treatment variable
dat <- main_data %>%
  filter(category != "always treated") %>% 
  filter(!(state %in% c("connecticut", "oklahoma", "vermont"))) %>% 
  mutate(center_time = ifelse(category %in% c("clean treated", "gradually treated"),
                              year - treatment_year,
                              0)) %>%
  dummy_cols(select_columns = "center_time") %>%
  mutate(treated = case_when(
    state %in% c(clean_treated_states, gradually_treated_states) & 
      year >= treatment_year ~ 1,
    state %in% c(clean_treated_states, gradually_treated_states) & 
      year < treatment_year ~ 0,
    state %in% never_treated_states ~ 0,
  ))

# prep functions 
keepvars <- c(paste0("`center_time_-", 20:2, "`"),  
              paste0("center_time_", 0:20))

prep_es_10 <- function(mod){
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se,
    pval = mod$pval
  )
  
  es <- mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-10:-2, 0:10)) %>% 
    select(t, estimate, se, pval)
  es <- rbind(es, c(-1, 0, 0, 0))
  es <- es %>% mutate(lower = estimate - 1.96 * se,
                      upper = estimate + 1.96 * se)
  return(es)
}

prep_es_20 <- function(mod){
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se,
    pval = mod$pval
  )
  
  es <- mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-20:-2, 0:20)) %>% 
    select(t, estimate, se, pval)
  es <- rbind(es, c(-1, 0, 0, 0))
  es <- es %>% mutate(lower = estimate - 1.96 * se,
                      upper = estimate + 1.96 * se)
  return(es)
}

prep_es_trend_10 <- function(mod){
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se,
    pval = mod$pval
  )
  
  es <- mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(0:10)) %>% 
    select(t, estimate, se, pval)
  es <- rbind(es, c(-1, 0, 0, 0))
  es <- es %>% 
    mutate(lower = estimate - 1.96 * se,
           upper = estimate + 1.96 * se,
           star = ifelse(pval <= 0.001, "***", 
                         ifelse(pval<=0.01,"**", 
                                ifelse(pval<=0.05, "*", ""))),
           coef_star = paste0(round(estimate,2), star, sep =""))
  return(es)
}

prep_es_trend_20 <- function(mod){
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se,
    pval = mod$pval
  )
  
  es <- mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(0:20)) %>% 
    select(t, estimate, se, pval)
  es <- rbind(es, c(-1, 0, 0, 0))
  es <- es %>% 
    mutate(lower = estimate - 1.96 * se,
           upper = estimate + 1.96 * se,
           star = ifelse(pval <= 0.001, "***", 
                         ifelse(pval<=0.01,"**", 
                                ifelse(pval<=0.05, "*", ""))),
           coef_star = paste0(round(estimate,2), star, sep =""))
  return(es)
}

did_graph_10 <- function(outcome, outcome_name){
  formula_str1 <- paste0(
    outcome, " ~ ",
    "`center_time_-10` + `center_time_-9` + ",
    "`center_time_-8` + `center_time_-7` + `center_time_-6` + `center_time_-5` + ",
    "`center_time_-4` + `center_time_-3` + `center_time_-2` + `center_time_0` + ",
    "`center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` + ",
    "`center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + ",
    "`center_time_9` + `center_time_10` | ",
    "as.factor(state) + as.character(year) ",
    sep = ""
  )
  formula1 <- as.formula(formula_str1)
  mod_d <- lfe::felm(formula1, 
                     data = subset(dat, center_time >= -10 & center_time <= 10),
                     exactDOF = TRUE)
  es_d <- prep_es_10(mod_d)
  
  es_d <- es_d %>% 
    mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))
  
  formula_str2 <- paste0(
    outcome, " ~ ",
    "treated | as.factor(state) + as.factor(year)",
    sep = ""
  )
  formula2 <- as.formula(formula_str2)
  agg_mod <- lfe::felm(formula2,
                       data = subset(dat, center_time >= -10 & center_time <= 10),
                       exactDOF = TRUE)

  coef <- round(agg_mod$coefficients[1],3)
  star <- ifelse(agg_mod$pval[1]<=0.01, "***",
                 ifelse(agg_mod$pval[1]<=0.05, "**",
                        ifelse(agg_mod$pval[1]<=0.1, "*", "")))
  se <- round(agg_mod$se[1],3)
  comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")
  text_pos <- min(es_d$lower)*9/10
  
  # Event study plot
  p <- ggplot(data = es_d, aes(x = t, y = estimate, group = 1))+
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
    # scale_y_continuous(breaks = round(seq(-0.2, 0.2, by = 0.1), 1),
    #                  limits = c(-0.2, 0.2))+
    labs(x = "Relative Time", y = "Estimate",
         title = outcome_name)+
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position="none")+
    annotate("text", x = 8, y = text_pos, label = comb_est, size = 5)
  return(p)
}

did_graph_20 <- function(outcome, outcome_name){
  formula_str1 <- paste0(
    outcome, " ~ ",
    "`center_time_-20` + `center_time_-19` + `center_time_-18` + `center_time_-17` + `center_time_-16` + ",
    "`center_time_-15` + `center_time_-14` + `center_time_-13` + `center_time_-12` + `center_time_-11` + ",
    "`center_time_-10` + `center_time_-9` + ",
    "`center_time_-8` + `center_time_-7` + `center_time_-6` + `center_time_-5` + ",
    "`center_time_-4` + `center_time_-3` + `center_time_-2` + `center_time_0` + ",
    "`center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` + ",
    "`center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + ",
    "`center_time_9` + `center_time_10` + ",
    "`center_time_11` + `center_time_12` + `center_time_13` + `center_time_14` + `center_time_15` + ",
    "`center_time_16` + `center_time_17` + `center_time_18` + `center_time_19` + `center_time_20` | ",
    "as.factor(state) + as.character(year) ",
    sep = ""
  )
  formula1 <- as.formula(formula_str1)
  mod_d <- lfe::felm(formula1, 
                     data = subset(dat, center_time >= -20 & center_time <= 20),
                     exactDOF = TRUE)
  es_d <- prep_es_20(mod_d)
  
  es_d <- es_d %>% 
    mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))
  
  formula_str2 <- paste0(
    outcome, " ~ ",
    "treated | as.factor(state) + ",
    "as.factor(year)",
    sep = ""
  )
  formula2 <- as.formula(formula_str2)
  agg_mod <- lfe::felm(formula2,
                       data = subset(dat, center_time >= -20 & center_time <= 20), 
                       exactDOF = TRUE)
  
  coef <- round(agg_mod$coefficients[1],3)
  star <- ifelse(agg_mod$pval[1]<=0.01, "***",
                 ifelse(agg_mod$pval[1]<=0.05, "**",
                        ifelse(agg_mod$pval[1]<=0.1, "*", "")))
  se <- round(agg_mod$se[1],3)
  comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")
  text_pos <- min(es_d$lower)*9/10
  
  # Event study plot
  p <- ggplot(data = es_d, aes(x = t, y = estimate, group = 1))+
    geom_point(size= 3, color = "gray26")+
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  width=.1, 
                  linewidth = 0.8, 
                  color = "gray26") +
    geom_hline(yintercept = 0, 
               linetype = "dashed", 
               linewidth = 1,
               color = "firebrick4")+
    scale_x_continuous(breaks = round(seq(-20, 20, by = 1),1),
                       limits = c(-20.5, 20.5))+
    # scale_y_continuous(breaks = round(seq(-0.2, 0.2, by = 0.1), 1),
    #                  limits = c(-0.2, 0.2))+
    labs(x = "Relative Time", y = "Estimate",
         title = outcome_name)+
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position="none")+
    annotate("text", x = 15, y = text_pos, label = comb_est, size = 5)
  return(p)
}

did_graph_trend_10 <- function(outcome, outcome_name){
  p<-list()
  ## normal event study 
  formula_str1 <- paste0(
    outcome, " ~ ",
    "`center_time_-10` + `center_time_-9` + ",
    "`center_time_-8` + `center_time_-7` + `center_time_-6` + `center_time_-5` + ",
    "`center_time_-4` + `center_time_-3` + `center_time_-2` + `center_time_0` + ",
    "`center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` + ",
    "`center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + ",
    "`center_time_9` + `center_time_10` | ",
    "as.factor(state) + as.character(year) ",
    sep = ""
  )
  formula1 <- as.formula(formula_str1)
  mod_d <- lfe::felm(formula1, 
                     data = subset(dat, center_time >= -10 & center_time <= 10),
                     exactDOF = TRUE)
  es_d <- prep_es_10(mod_d)
  
  es_d <- es_d %>% 
    mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))
  
  ## pre-trend line
  pre_trend <- es_d %>% filter(t < 0) 
  pre_trend_fit <- lm(estimate ~ t, data = pre_trend)
  predicted_df <- data.frame(
    t = seq(-10, 10, by = 0.1),
    estimate = predict(pre_trend_fit, newdata = data.frame(t = seq(-10, 10, by = 0.1)))
  )
  
  ## estimate difference
  formula_str2 <- paste0(
    outcome, " ~ ",
    "`center_time_0` + ",
    "`center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` + ",
    "`center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + ",
    "`center_time_9` + `center_time_10` + ",
    "center_time | ",
    "as.factor(state) + as.character(year) ",
    sep = ""
  )
  formula2 <- as.formula(formula_str2)
  mod_d <- lfe::felm(formula2, 
                     data = subset(dat, center_time >= -10 & center_time <= 10),
                     exactDOF = TRUE)
  es_d2 <- prep_es_trend_10(mod_d)
  
  es_d2 <- es_d2 %>% 
    mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))
  
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
    # scale_y_continuous(breaks = round(seq(-0.2, 0.2, by = 0.1), 1),
    #                  limits = c(-0.2, 0.2))+
    labs(x = "Relative Time", y = "Estimate",
         title = outcome_name)+
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position="none")+
    geom_text(data = es_d2 %>% filter(t >= 0 & t <= 10), 
              aes(x = t, y = 0.5*max(es_d2$upper), label = coef_star, angle = 45),
              color = "blue", size = 4, vjust = -0.3)+
    geom_line(data = predicted_df, aes(x = t, y = estimate), 
              color = "blue", linetype = "dashed", linewidth = 1.2)
  # annotate("text", x = 8, y = text_pos, label = comb_est, size = 5)
  
  p2<-ggplot(data = es_d2, aes(x = t, y = estimate)) +
    geom_point(size = 3, color = "black") +  # Black dots for estimates
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  width = 0.2, 
                  linewidth = 0.8, 
                  color = "black") +  # Error bars
    geom_line(color = "black", linewidth = 1) +  # Line connecting estimates
    geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", linewidth = 1) +  # Reference line at y = 0
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    # scale_y_continuous(breaks = seq(-0.5,1,0.1),
    #                    limits = c(-0.5, 1.1))+
    labs(x = "Relative Time", y = "Estimate", title = outcome_name) +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position="none")
  p2
  
  p[[1]] <- p1
  p[[2]] <- p2
  return(p)
}

did_graph_trend_20 <- function(outcome, outcome_name){
  p<-list()
  ## normal event study 
  formula_str1 <- paste0(
    outcome, " ~ ",
    "`center_time_-20` + `center_time_-19` + `center_time_-18` + `center_time_-17` + `center_time_-16` + ",
    "`center_time_-15` + `center_time_-14` + `center_time_-13` + `center_time_-12` + `center_time_-11` + ",
    "`center_time_-10` + `center_time_-9` + ",
    "`center_time_-8` + `center_time_-7` + `center_time_-6` + `center_time_-5` + ",
    "`center_time_-4` + `center_time_-3` + `center_time_-2` + `center_time_0` + ",
    "`center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` + ",
    "`center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + ",
    "`center_time_9` + `center_time_10` + ",
    "`center_time_11` + `center_time_12` + `center_time_13` + `center_time_14` + `center_time_15` + ",
    "`center_time_16` + `center_time_17` + `center_time_18` + `center_time_19` + `center_time_20` | ",
    "as.factor(state) + as.character(year) ",
    sep = ""
  )
  formula1 <- as.formula(formula_str1)
  mod_d <- lfe::felm(formula1, 
                     data = subset(dat, center_time >= -20 & center_time <= 20),
                     exactDOF = TRUE)
  es_d <- prep_es_20(mod_d)
  
  es_d <- es_d %>% 
    mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))
  
  ## pre-trend line
  pre_trend <- es_d %>% filter(t < 0) 
  pre_trend_fit <- lm(estimate ~ t, data = pre_trend)
  predicted_df <- data.frame(
    t = seq(-20, 20, by = 0.1),
    estimate = predict(pre_trend_fit, newdata = data.frame(t = seq(-20, 20, by = 0.1)))
  )
  
  ## estimate difference
  formula_str2 <- paste0(
    outcome, " ~ ",
    "`center_time_0` + ",
    "`center_time_1` + `center_time_2` + `center_time_3` + `center_time_4` + ",
    "`center_time_5` + `center_time_6` + `center_time_7` + `center_time_8` + ",
    "`center_time_9` + `center_time_10` + ",
    "`center_time_11` + `center_time_12` + `center_time_13` + `center_time_14` + `center_time_15` + ",
    "`center_time_16` + `center_time_17` + `center_time_18` + `center_time_19` + `center_time_20` + ",
    "as.numeric(center_time) | ",
    "as.factor(state) + as.character(year) ",
    sep = ""
  )
  formula2 <- as.formula(formula_str2)
  mod_d <- lfe::felm(formula2, 
                     data = subset(dat, center_time >= -20 & center_time <= 20),
                     exactDOF = TRUE)
  es_d2 <- prep_es_trend_20(mod_d)
  
  es_d2 <- es_d2 %>% 
    mutate(sig = ifelse((lower > 0 | upper < 0), 1, 0))
  
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
    scale_x_continuous(breaks = round(seq(-20, 20, by = 1),1),
                       limits = c(-20.5, 20.5))+
    # scale_y_continuous(breaks = round(seq(-0.2, 0.2, by = 0.1), 1),
    #                  limits = c(-0.2, 0.2))+
    labs(x = "Relative Time", y = "Estimate",
         title = outcome_name)+
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position="none")+
    geom_text(data = es_d2 %>% filter(t >= 0 & t <= 20), 
              aes(x = t, y = 0.5*max(es_d2$upper), label = coef_star),
              color = "blue", size = 4, vjust = -0.3, angle = 45)+
    geom_line(data = predicted_df, aes(x = t, y = estimate), 
              color = "blue", linetype = "dashed", linewidth = 1.2)
  # annotate("text", x = 8, y = text_pos, label = comb_est, size = 5)
  
  p2<-ggplot(data = es_d2, aes(x = t, y = estimate)) +
    geom_point(size = 3, color = "black") +  # Black dots for estimates
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  width = 0.2, 
                  linewidth = 0.8, 
                  color = "black") +  # Error bars
    geom_line(color = "black", linewidth = 1) +  # Line connecting estimates
    geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", linewidth = 1) +  # Reference line at y = 0
    scale_x_continuous(breaks = seq(-20, 20, by = 1)) +
    # scale_y_continuous(
    #   breaks = seq(-3, 2.5, 0.2),
    #   limits = c(-3, 2.5),
    #   labels = label_number(accuracy = 0.1)  # Force rounding to 1 decimal place
    # ) +
    labs(x = "Relative Time", y = "Estimate", title = outcome_name) +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.position="none")
  p2
  p[[1]] <- p1
  p[[2]] <- p2
  return(p)
}

# construct stacked data
# for each treatment year, construct control without treatment within 10/20 years
collect_10 <- list()
collect_20 <- list()

for(i in unique(dat$treatment_year[dat$treatment_year != Inf])){
  treated_state <- dat %>%
    filter(treatment_year == i) %>% 
    mutate(treatment_year_group = i) %>% 
    select(state, year, center_time, treated, `total medicaid (mt + at)`,
           log_all_medicaid_spending_per_cap, all_medicaid_spending_per_cap,
           pct_in_comp_mco, treatment_year, treatment_year_group)
  max_year <- max(treated_state$year)
  
  control_state_10 <- dat %>% 
    filter(treatment_year > i + 10,
           year <= i + 10, year >= i - 10) %>% 
    mutate(treatment_year_group = i,
           center_time = 0,
           treated = 0) %>% 
    select(state, year, center_time, treated, `total medicaid (mt + at)`,
           log_all_medicaid_spending_per_cap, all_medicaid_spending_per_cap,
           pct_in_comp_mco,
           treatment_year, treatment_year_group)
  
  control_state_20 <- dat %>% 
    filter(treatment_year > i + 20, 
           year <= i + 20, year >= i - 20) %>% 
    mutate(treatment_year_group = i,
           center_time = 0,
           treated = 0) %>%
    select(state, year, center_time, treated, `total medicaid (mt + at)`,
           log_all_medicaid_spending_per_cap, all_medicaid_spending_per_cap,
           pct_in_comp_mco,
           treatment_year, treatment_year_group)
  
  collect_10[[length(collect_10)+1]] <- rbind(treated_state, control_state_10)
  collect_20[[length(collect_20)+1]] <- rbind(treated_state, control_state_20)
}
dat_10 <- do.call(rbind, collect_10)
dat_20 <- do.call(rbind, collect_20)

dat_10 <- dat_10 %>% dummy_cols(select_columns = "center_time")
dat_20 <- dat_20 %>% dummy_cols(select_columns = "center_time")

# DID - 10 period
dat <- dat_10
dat <- dat %>%
  rename(total_medicaid_spending = `total medicaid (mt + at)`) %>%
  mutate(log_total_medicaid_spending = log(total_medicaid_spending))

p1 <- did_graph_10("pct_in_comp_mco", "Prop. Comp MCO Enroll")
p2 <- did_graph_10("log_total_medicaid_spending", "Log Total Medicaid Spending")
p3 <- did_graph_10("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")
p4 <- did_graph_trend_10("pct_in_comp_mco", "Prop. Comp MCO Enroll")[1]
p5 <- did_graph_trend_10("all_medicaid_spending_per_cap", "Medicaid Spending Per Capita")[1]
p6 <- did_graph_trend_10("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")[1]
p7 <- did_graph_trend_10("pct_in_comp_mco", "Prop. Comp MCO Enroll")[2]
p8 <- did_graph_trend_10("all_medicaid_spending_per_cap", "Medicaid Spending Per Capita")[2]
p9 <- did_graph_trend_10("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")[2]

png(filename = "Output/did_stack/DID_10_MCO.png",
    width=1300, height=800, res = 150)
print(p1)
dev.off()

png(filename = "Output/did_stack/DID_10_sp_percap.png",
    width=1300, height=800, res = 150)
print(p2)
dev.off()

png(filename = "Output/did_stack/DID_10_log_sp_percap.png",
    width=1300, height=800, res = 150)
print(p3)
dev.off()

png(filename = "Output/did_stack/DID_10_MCO_diff.png",
    width=1300, height=800, res = 150)
print(p4)
dev.off()

png(filename = "Output/did_stack/DID_10_sp_percap_diff.png",
    width=1300, height=800, res = 150)
print(p5)
dev.off()

png(filename = "Output/did_stack/DID_10_log_sp_percap_diff.png",
    width=1300, height=800, res = 150)
print(p6)
dev.off()

png(filename = "Output/did_stack/DID_10_MCO_diff2.png",
    width=1300, height=800, res = 150)
print(p7)
dev.off()

png(filename = "Output/did_stack/DID_10_sp_percap_diff2.png",
    width=1300, height=800, res = 150)
print(p8)
dev.off()

png(filename = "Output/did_stack/DID_10_log_sp_percap_diff2.png",
    width=1300, height=800, res = 150)
print(p9)
dev.off()


for(i in unique(dat_10$treatment_year[dat_10$treatment_year != Inf])){
  dat <- dat_10 %>% 
    filter(treatment_year_group == i)
  p1 <- did_graph_10("pct_in_comp_mco", "Prop. Comp MCO Enroll")
  p3 <- did_graph_10("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")
  p5 <- did_graph_trend_10("log_all_medicaid_spending_per_cap", "Medicaid Spending Per Capita")[2]
  
  png(filename = paste0("Output/did_stack/treat_",i,"_log_per_cap.png", sep = ""),
      width=1300, height=800, res = 150)
  print(p3)
  dev.off()
  
  png(filename = paste0("Output/did_stack/treat_",i,"_log_per_cap_trend.png", sep = ""),
      width=1300, height=800, res = 150)
  print(p5)
  dev.off()
}


# DID - 20 period
dat <- dat_20
p1 <- did_graph_20("pct_in_comp_mco", "Prop. Comp MCO Enroll")
p2 <- did_graph_20("all_medicaid_spending_per_cap", "Medicaid Spending Per Capita")
p2 <- did_graph_20("log_total_medicaid_spending", "Log Total Medicaid Spending")
p3 <- did_graph_20("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")
p4 <- did_graph_trend_20("pct_in_comp_mco", "Prop. Comp MCO Enroll")[1]
p5 <- did_graph_trend_20("all_medicaid_spending_per_cap", "Medicaid Spending Per Capita")[1]
p6 <- did_graph_trend_20("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")[1]
p7 <- did_graph_trend_20("pct_in_comp_mco", "Prop. Comp MCO Enroll")[2]
p8 <- did_graph_trend_20("all_medicaid_spending_per_cap", "Medicaid Spending Per Capita")[2]
p9 <- did_graph_trend_20("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")[2]

png(filename = "Output/did_stack/DID_20_MCO.png",
    width=1300, height=800, res = 150)
print(p1)
dev.off()

png(filename = "Output/did_stack/DID_20_sp_percap.png",
    width=1300, height=800, res = 150)
print(p2)
dev.off()

png(filename = "Output/did_stack/DID_20_log_sp_percap.png",
    width=1300, height=800, res = 150)
print(p3)
dev.off()

png(filename = "Output/did_stack/DID_20_MCO_diff.png",
    width=1300, height=800, res = 150)
print(p4)
dev.off()

png(filename = "Output/did_stack/DID_20_sp_percap_diff.png",
    width=1300, height=800, res = 150)
print(p5)
dev.off()

png(filename = "Output/did_stack/DID_20_log_sp_percap_diff.png",
    width=1300, height=800, res = 150)
print(p6)
dev.off()

png(filename = "Output/did_stack/DID_20_MCO_diff2.png",
    width=1300, height=800, res = 150)
print(p7)
dev.off()

png(filename = "Output/did_stack/DID_20_sp_percap_diff2.png",
    width=1300, height=800, res = 150)
print(p8)
dev.off()

png(filename = "Output/did_stack/DID_20_log_sp_percap_diff2.png",
    width=1300, height=800, res = 150)
print(p9)
dev.off()


for(i in unique(dat_20$treatment_year[dat_20$treatment_year != Inf])){
  dat <- dat_20 %>% 
    filter(treatment_year_group == i)
  p1 <- did_graph_20("pct_in_comp_mco", "Prop. Comp MCO Enroll")
  p3 <- did_graph_20("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")
  p5 <- did_graph_trend_20("log_all_medicaid_spending_per_cap", "Log Medicaid Spending Per Capita")[2]
  
  png(filename = paste0("Output/did_stack/treat_",i,"_log_per_cap_20yrs.png", sep = ""),
      width=1300, height=800, res = 150)
  print(p3)
  dev.off()
  
  png(filename = paste0("Output/did_stack/treat_",i,"_log_per_cap_20yrs_trend.png", sep = ""),
      width=1300, height=800, res = 150)
  print(p5)
  dev.off()
}













