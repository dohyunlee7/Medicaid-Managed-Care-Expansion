#'
#' did_analysis.R -- Implement difference-in-difference (Callaway - Santanna) to extended data
#'

library(dplyr)
library(haven)
library(datasets)
library(readxl)
library(pubtheme)
library(did)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel3.rds"))

# Load enrollment jump data
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

# Create variable names for treatment year, relative time, and treatment binary
# indicator
main_data <- main_data %>%
  mutate(treatment_year = ifelse(state %in% never_treated_states,
                                 NA,
                                 treatment_year),
         relative_time = year - treatment_year,
         treated = ifelse(!is.na(treatment_year) & year >= treatment_year, 1, 0))

main_data <- main_data %>%
  rename(total_medicaid_spending = `total medicaid (mt + at)`,
         total_mco_spending = `m-medicaid - mco`)

# Assign Inf to states with NA in their treatment year (these would be never treated states)
# Calculate Medicaid spending per capita
# Create variable for state_id number
main_data <- main_data %>%
  mutate(treatment_year = ifelse(is.na(treatment_year), Inf, treatment_year),
         all_medicaid_spending_per_cap = `total medicaid (mt + at)` / total_med_enr,
         state_id = as.numeric(as.factor(state)))

# Get average treatment effect
att_results <- att_gt(
  yname = "all_medicaid_spending_per_cap",
  gname = "treatment_year",
  tname = "year",
  idname = "state_id",
  data = main_data,
  control_group = "notyettreated",
  est_method = "reg"
)

# Looking at results, effects, etc
summary(att_results)

agg_effects <- aggte(att_results, type = "dynamic")
summary(agg_effects)

# Plot
ggdid(agg_effects)














