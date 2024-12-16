#'
#' clean_comp_mco_kff.R -- Clean older raw data from KFF
#'

### ------------------------------- FUNCTIONS ------------------------------ ###

#' Joins Medicaid total enrollment to bigger list of dataframes and
#' calculates the proportion of comprehensive risk-based MMC enrollees
#' at state level
#' @param data_list list of dataframes that contains MCO enrollment
#'                  numbers 
#' @param df annual dataframe that contains denominator scraped from MMC
#'           enrollment report
#' @param year input for target year
#' @return updated sublist for desired year
join_and_fill <- function(data_list, df, year) {
  
  # Select just state and total state Medicaid enrollment
  df <- df %>%
    select(state, total_med_enr) %>%
    mutate(total_med_enr = as.numeric(total_med_enr))
  
  # Join denominator to sublist
  data_list[[year]] <- left_join(data_list[[year]], df, by = "state")
  
  # Calculate proportion of CRB MMC enrollees per state
  data_list[[year]] <- data_list[[year]] %>%
    mutate(med_enr_pct = crb_mc_enrollees / total_med_enr)
  
  return(data_list[[year]])
}


### -------------- Process raw comp. MCO data from 2003 - 2015  ------------ ###

library(readr)
library(dplyr)
library(tidyverse)

setwd(file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                "medicaid_privatization_exp", "Input_Data", 
                "Medicaid_managedcare_enrollment_report", "raw_comp_mco_data"))

# Define years
years <- setdiff(2003:2015, 2012)

# Get all file names with the years attached
file_names <- paste0("raw_data_", years, ".csv")

# Apply read_csv to the file names
data_list <- lapply(file_names, read_csv)

# Assign years as names of sublists
names(data_list) <- years

# Shorten name of object within sublist
new_name <- "total_mmc_enr"

data_list <- lapply(years, function(year) {
  sublist <- data_list[[as.character(year)]]
  names(sublist) <- new_name
  return(sublist)
})

names(data_list) <- years

# All data is stored in one single column and all elements are separated
# by commas
data_split <- lapply(data_list, function(data) {
  data.frame(do.call('rbind', strsplit(as.character(data$total_mmc_enr), 
                                       ',', 
                                       fixed = TRUE)))
})

for (year in names(data_split)) {
  
  # Add variable for year
  data_split[[year]]$year <- as.numeric(year)
  
  # Omit unneeded rows
  data_split[[year]] <- data_split[[year]][-c(1:2, 
                                              56:nrow(data_split[[year]])), ]
}

# Unlist and rowbind
data_comb <- do.call(rbind, data_split)

# Omit duplicate column for state name
data_comb <- data_comb[, -4]

# Reassign variable names
names(data_comb) <- c("state", 
                      "crb_mc_enrollees",
                      "med_enr_pct",
                      "year")

# Replace all instances of "N/A" as NA
data_comb[data_comb == "N/A"] <- NA

# saveRDS(data_comb, file = "comp_mco_03_15.rds")

### --------------- Process raw comp. MCO data from 2016 - 2021 ------------ ###

# Define years
years <- 2016:2021

# Get all file names with the years attached
file_names <- paste0("raw_data_", years, ".csv")

# Apply read_csv to the file names
data_split <- lapply(file_names, read_csv)

# Assign years as names of sublists
names(data_split) <- years

for (year in names(data_split)) {
  
  # Add variable for year
  data_split[[year]]$year <- as.numeric(year)
  
  # Omit unneeded rows
  data_split[[year]] <- data_split[[year]][-c(1:3, 
                                              56:nrow(data_split[[year]])), ]
}

data_table <- do.call(rbind, data_split)

data_table <- data_table[, -4]

# Reassign variable names
names(data_table) <- c("state", 
                      "crb_mc_enrollees",
                      "med_enr_pct",
                      "year")

# Replace all instances of "N/A" as NA
data_table[data_table == "N/A"] <- NA

# Combine 2003-2015 and 2016-2021 tables
full_data_table <- rbind(data_comb, data_table)

full_data_table$crb_mc_enrollees <- as.numeric(full_data_table$crb_mc_enrollees)
full_data_table$med_enr_pct <- as.numeric(full_data_table$med_enr_pct)

save_path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                       "medicaid_privatization_exp", "temp")

saveRDS(full_data_table, paste0(save_path, "/full_mco_table.rds"))


### ------------------- Integrate Proportion of Comp. MCO ------------------ ###

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                  "medicaid_privatization_exp", "Input_Data", 
                  "Medicaid_managedcare_enrollment_report", 
                  "by_program_pop_from_report")

# Group each year as its own sublist for easier joining
grouped_table <- full_data_table %>%
  group_by(year) %>%
  group_split() %>%
  lapply(as.data.frame)

# Name lists as the year
names(grouped_table) <- setdiff(2003:2021, 2012)

# Define years
years <- setdiff(2008:2021, 2012)

# Get all file names with the years attached
file_names <- paste0(path, "/data_", years, ".csv")

# Apply read_csv to the file names
files_list <- lapply(file_names, read_csv)
names(files_list) <- years

# Rename the variable in each dataframe within the list
files_list <- lapply(files_list, function(df) {
  if ("total_medicaid_enrollees" %in% names(df)) {
    names(df)[names(df) == "total_medicaid_enrollees"] <- "total_med_enr"
  }
  return(df)
})

# Apply joining and filling function to all sublists
grouped_table[as.character(years)] <- lapply(years, function(year) {
  join_and_fill(grouped_table, 
                files_list[[as.character(year)]], 
                as.character(year))
})

# Unlist into dataframe
gt <- bind_rows(grouped_table)

# Fill in missing total Medicaid enrollees 2016-2021 (Not in KFF raw data)
gt <- gt %>%
  mutate(total_med_enr = ifelse(is.na(total_med_enr),
                                crb_mc_enrollees / med_enr_pct,
                                total_med_enr))

saveRDS(gt, file = paste0(path, "/full_table_with_proportions.rds"))



