#'
#' synth_did_analysis.R -- Implement synthetic difference-in-difference model to extended data
#'
#'

rm(list = ls())

packages <- c("ggplot2", "estimatr", "readxl", "readr", "dplyr",
              "stringr", "ivreg", "tidyverse", "texreg", "ggrepel", 
              "lfe", "fastDummies", "ggpubr", "RColorBrewer", "ggthemes",
              "stargazer", "grid", "foreign", "kableExtra", "lubridate",
              "haven", "fuzzyjoin", "stringi", "zoo", "viridis", "scales",
              "synthdid", "pubtheme")

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

### ------------------- Example from `synthdid` documentation -------------- ###
# Estimate the effect of California Proposition 99 on cigarette consumption
data('california_prop99')
setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)

### ------------------------------------------------------------------------ ###

### 1. Load data, preprocess ###

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

save_path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                       "medicaid_privatization_exp", "Output")

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

# Initialize results dataframe
results <- data.frame(state = character(), year = numeric(), gap = numeric())

treated_states <- c(clean_treated_states, gradually_treated_states)

dat <- dat %>%
  mutate(state_id = as.numeric(factor(state)))

dat <- dat %>%
  mutate(log_total_spend = log(`total medicaid (mt + at)`))

data <- dat %>%
  select(state, 
         year,
         pct_in_comp_mco,
         `total medicaid (mt + at)`,
         log_total_spend,
         all_medicaid_spending_per_cap,
         log_all_medicaid_spending_per_cap, 
         treatment_year, 
         treated)

cohorts <- unique(dat$treatment_year)
cohorts <- cohorts[!is.infinite(cohorts)]

# Initialize a list to store results
results <- list()

# Loop through each cohort
for (cohort_year in cohorts) {
  
  tryCatch({
    
  })
  
  # Subset data for the current cohort
  cohort_data <- data %>%
    filter(treatment_year == cohort_year | is.infinite(treatment_year))
  
  # Reshape the cohort data to wide format
  Y <- cohort_data %>%
    select(state, year, all_medicaid_spending_per_cap) %>%
    pivot_wider(names_from = year, 
                values_from =  all_medicaid_spending_per_cap) %>%
    column_to_rownames("state") %>%
    as.matrix()
  
  # Number of control units (N0)
  N0 <- sum(treatment_timing == Inf)
  
  # Number of pre-treatment periods (T0)
  T0 <- cohort_year - min(cohort_data$year)
  
  if (nrow(Y) == 0 || ncol(Y) == 0) {
    stop("Y has invalid dimensions. Skipping cohort year ", cohort_year)
  }
  
  # Estimate the treatment effect using synthdid
  tau.hat <- synthdid_estimate(Y, N0, T0)
  
  estimators <- list(did = did_estimate,
                    sc = sc_estimate,
                    sdid = synthdid_estimate)
  
  estimates <- lapply(estimators, function(estimator) {estimator(Y, N0, T0)})
  

  synthdid_plot(estimates[1:3],
                control.name = 'control',
                treated.name = 'treated',
                facet.vertical=FALSE,
                lambda.comparable=TRUE,
                se.method = 'none',
                trajectory.linetype = 1,
                line.width=.75,
                effect.curvature=-.4,
                trajectory.alpha=.7,
                effect.alpha=.7,
                diagram.alpha=1,
                onset.alpha=.7
                ) + 
    labs(title = "Estimators - Total Medicaid Spending per Capita",
         subtitle = paste0("Treatment Year = ", cohort_year),
         x = "Year",
         y = "Total Medicaid Spending per Capita")
}  
 
# Loop through each cohort
for (cohort_year in cohorts) {
  
  tryCatch({
    # Subset data for the current cohort
    cohort_data <- data %>%
      filter(treatment_year == cohort_year | is.infinite(treatment_year))
  
    # Reshape the cohort data to wide format
    Y <- cohort_data %>%
      select(state, year, all_medicaid_spending_per_cap) %>%
      pivot_wider(names_from = year, 
                  values_from = all_medicaid_spending_per_cap) %>%
      column_to_rownames("state") %>%
      as.matrix()
    
    # Extract treatment timing for the cohort
    treatment_timing <- cohort_data %>%
      group_by(state) %>%
      summarize(treatment_year = unique(treatment_year)) %>%
      pull(treatment_year)
    
    # Number of control units (N0)
    N0 <- sum(treatment_timing == Inf)
    
    # Number of pre-treatment periods (T0)
    T0 <- cohort_year - min(cohort_data$year)
    
    # Check if Y has valid dimensions
    if (nrow(Y) == 0 || ncol(Y) == 0) {
      stop("Y has invalid dimensions. Skipping cohort year ", cohort_year)
    }
    
    # Estimate the treatment effect using synthdid
    tau.hat <- synthdid_estimate(Y, N0, T0)
    
    # Define estimators
    estimators <- list(did = did_estimate,
                       sc = sc_estimate,
                       sdid = synthdid_estimate)
    
    # Apply estimators
    estimates <- lapply(estimators, function(estimator) {estimator(Y, N0, T0)})
    
    # Generate the plot
    plot <- synthdid_plot(estimates[1:3],
                          control.name = 'control',
                          treated.name = "treated",  # Use the treated unit's name
                          facet.vertical = FALSE,
                          lambda.comparable = TRUE,
                          se.method = 'none',
                          trajectory.linetype = 1,
                          line.width = .75,
                          effect.curvature = -.4,
                          trajectory.alpha = .7,
                          effect.alpha = .7,
                          diagram.alpha = 1,
                          onset.alpha = .7) +
      labs(title = "Estimators - Total Medicaid Spending per Capita",
           subtitle = paste0("Treatment Year = ", cohort_year),
           x = "Year",
           y = "Total Medicaid Spending per Capita")
    
    # Save the plot
    ggsave(filename = paste0(save_path, "/synthdid_plot_", cohort_year, ".png"),
           plot = plot,
           width = 8,
           height = 6,
           dpi = 300)
    
    # Print a success message
    message("Plot saved for cohort year ", cohort_year)
    
  }, error = function(e) {
    # Print an error message and skip the cohort year
    message("Error in cohort year ", cohort_year, ": ", e$message)
  })
}
