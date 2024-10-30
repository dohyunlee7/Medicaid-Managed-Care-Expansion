#'
#' clean_cms_64_97_01.R -- Clean 1997-2001 Medicaid Financial Management Reports
#'                         for all states
#'

library(dplyr)
library(readxl)
library(tidyverse)

setwd(file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                "medicaid_privatization_exp"))
source("Code/clean_cms_64_functions.R")

file_path <- file.path(getwd(), "Input_Data", "CMS_64_report", 
                       "financial-management-report-fy1997-2001")

# Extract sheets from the 1997-2001 reports
fmr <- multiple_sheets(paste0(file_path, "/FMR1997through2001.xlsx"))

# Apply cleaner function to the list of all FMR sheets
fmr_list <- lapply(fmr, fmr_cleaner_97_01)

# Initialize lists to store the two types of data
map_list <- list()
adp_list <- list()

for (page in names(fmr_list)) {
  for (state in names(fmr_list[[page]])) {
    
    if (nrow(fmr_list[[page]][[state]][["map"]]) > 0) {
      
      # Create variable for tag for type of data and state name for MAP data
      fmr_list[[page]][[state]][["map"]]$tag <- paste0("MAP - ", state)
      
      # Create variable for year for MAP data
      fmr_list[[page]][[state]][["map"]]$year <- as.numeric(gsub("FMR", "", page))
      
      # Append dataframe to list
      map_list <- append(map_list, list(fmr_list[[page]][[state]][["map"]]))
    }
    
    if (nrow(fmr_list[[page]][[state]][["adp"]]) > 0) {
      
      # Create variable for tag for type of data and state name for TAG data
      fmr_list[[page]][[state]][["adp"]]$tag <- paste0("ADP - ", state)
      
      # Create variable for year for ADP data
      fmr_list[[page]][[state]][["adp"]]$year <- as.numeric(gsub("FMR", "", page))
      
      # Append dataframe to list
      adp_list <- append(adp_list, list(fmr_list[[page]][[state]][["adp"]]))
    }
  }
}

# Unlist and convert to singular dataframe
map_df <- do.call(rbind, map_list)
adp_df <- do.call(rbind, adp_list)

# Restructure variable names
names(map_df) <- tolower(names(map_df))
names(map_df) <- gsub(" ", "_", names(map_df))

names(adp_df) <- tolower(names(adp_df))
names(adp_df) <- gsub(" ", "_", names(adp_df))

# Omit leftover NAs from chunking
adp_df <- adp_df %>%
  filter(!is.na(service_category))

# Pivot MAP table longer
map_long <- map_df %>%
  pivot_longer(
    cols = c(total_computable, federal_share, state_share),
    names_to = "term",
    values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# Pivot long MAP table wider
map_wider <- map_long %>%
  pivot_wider(
    names_from = service_category,
    values_from = value,
    values_fn = list(value = ~ .[[1]])
  ) %>%
  arrange(tag, term, year)

# Pivot ADP table longer
adp_long <- adp_df %>%
  pivot_longer(
    cols = c(total_computable, federal_share, state_share),
    names_to = "term",
    values_to = "value"
  ) %>%
  mutate(value = as.numeric(value))

# Pivot long ADP table wider
adp_wider <- adp_long %>%
  pivot_wider(
    names_from = service_category,
    values_from = value,
    values_fn = list(value = ~ .[[1]])
  ) %>%
  arrange(tag, term, year)


saveRDS(map_wider, file = "Temp/map_wide_97_01.rds")
saveRDS(adp_wider, file = "Temp/adp_wide_97_01.rds")




























