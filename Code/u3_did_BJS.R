#'
#' did_analysis.R -- Implement difference-in-difference model to extended data
#'
rm(list = ls())

packages <- c("ggplot2", "estimatr", "readxl", "readr", "dplyr",
              "stringr", "ivreg", "tidyverse", "texreg", "ggrepel", 
              "lfe", "fastDummies", "ggpubr", "RColorBrewer", "ggthemes",
              "stargazer", "grid", "foreign", "kableExtra", "lubridate",
              "haven", "fuzzyjoin", "stringi", "zoo", "viridis", "did",
              "didimputation")

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

# path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
#                   "medicaid_privatization_exp")
path <- "C:/Users/ruochens/Dropbox (Penn)/Research project/MCO/Medicaid_privatization_exp"
setwd("C:/Users/ruochens/Dropbox (Penn)/Research project/MCO/Medicaid_privatization_exp")

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel_inflation_adj.rds"))

jumps <- readRDS(paste0(path, "/Temp/jumps.rds"))

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
table(main_data$treatment_year)

# describe treatment units in each year
main_data %>% 
  select(state, treatment_year) %>% distinct() %>%
  group_by(treatment_year) %>% 
  summarise(n_state = n(),
            states = paste(state, collapse = ";"))

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
  ),
  state_id = as.numeric(as.factor(state)))

write.csv(dat, file = "Temp/data_ready_BJS.csv")

dat <- dat %>% 
  filter(center_time <= 20, center_time >= -20)

## log spending per capita
did_imputation(data =dat, 
               yname = "log_all_medicaid_spending_per_cap", 
               gname = "treatment_year",
               tname = "year",
               idname = "state_id",
               horizon=TRUE)

did_results <- did_imputation(data =dat, 
               yname = "log_all_medicaid_spending_per_cap", 
               gname = "treatment_year",
               tname = "year",
               idname = "state_id",
               horizon=TRUE
               )

png(filename = "Output/did_bjs/did_log_per_cap_20yrs.png", 
    width=1300, height=800, res = 150)
ggplot(did_results, aes(x = as.numeric(term), y = estimate)) +
  geom_point(size = 3, color = "black") +  # Plot point estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, 
                linewidth = 0.8, 
                color = "black") +  # Error bars
  geom_line(color = "black", linewidth = 1) +  # Line connecting estimates
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", linewidth = 1) +  # Reference line at y = 0
  labs(
    x = "Years Since Treatment",
    y = "Estimates",
    title = "Log Medicaid Spending Per Capita"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position="none")
dev.off()

## spending per capita
did_imputation(data =dat, 
               yname = "all_medicaid_spending_per_cap", 
               gname = "treatment_year",
               tname = "year",
               idname = "state_id",
               horizon=TRUE)

did_results <- did_imputation(data =dat, 
                              yname = "all_medicaid_spending_per_cap", 
                              gname = "treatment_year",
                              tname = "year",
                              idname = "state_id",
                              horizon=TRUE
)

png(filename = "Output/did_bjs/did_per_cap_20yrs.png", 
    width=1300, height=800, res = 150)
ggplot(did_results, aes(x = as.numeric(term), y = estimate)) +
  geom_point(size = 3, color = "black") +  # Plot point estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, 
                linewidth = 0.8, 
                color = "black") +  # Error bars
  geom_line(color = "black", linewidth = 1) +  # Line connecting estimates
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", linewidth = 1) +  # Reference line at y = 0
  labs(
    x = "Years Since Treatment",
    y = "Estimates",
    title = "Medicaid Spending Per Capita"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position="none")
dev.off()

## MCo enrollment
did_results <- did_imputation(data =dat, 
                              yname = "pct_in_comp_mco", 
                              gname = "treatment_year",
                              tname = "year",
                              idname = "state_id",
                              horizon=TRUE
)

png(filename = "Output/did_bjs/did_enroll_20yrs.png", 
    width=1300, height=800, res = 150)
ggplot(did_results, aes(x = as.numeric(term), y = estimate)) +
  geom_point(size = 3, color = "black") +  # Plot point estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, 
                linewidth = 0.8, 
                color = "black") +  # Error bars
  geom_line(color = "black", linewidth = 1) +  # Line connecting estimates
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", linewidth = 1) +  # Reference line at y = 0
  labs(
    x = "Years Since Treatment",
    y = "Estimates",
    title = "Medicaid Spending Per Capita"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position="none")
dev.off()

# pre_treatment_data <- subset(dat, year < treatment_year & category != "never treated" & !is.na(treatment_year))
# 
# did_imputation(
#   data = pre_treatment_data,  # Restrict to pre-treatment periods
#   yname = "log_all_medicaid_spending_per_cap", 
#   gname = "treatment_year",
#   tname = "year",
#   idname = "state_id"
#   )



