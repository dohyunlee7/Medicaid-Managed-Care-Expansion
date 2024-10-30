#'
#' clean_cms_64_12.R -- Clean CMS-64 FMR for 2012
#'

library(dplyr)
library(readxl)
library(tidyverse)

setwd(file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                "medicaid_privatization_exp"))
source("Code/clean_cms_64_functions.R")

path <- file.path(getwd(), "Input_Data", "CMS_64_report", 
                  "financial-management-report-fy2012-13")

### ------------------------------ 2012 FMR -------------------------------- ###

fmr <- multiple_sheets(paste0(path, "/FMR Net Expenditures FY12.xlsx"))

# Apply 2012 cleaner function to Excel sheets
fmr_list <- lapply(names(fmr), function(state) {
  fmr_cleaner_2012(fmr[[state]], state)
})

# Assign list names
names(fmr_list) <- names(fmr)

map_list <- list()
adp_list <- list()

for (state in names(fmr_list)) {
  if (nrow(fmr_list[[state]][["map"]]) > 0) {
    
    # Create variable for tag for type of data and state name for MAP data
    fmr_list[[state]][["map"]]$tag <- paste0("MAP - ", state)
    
    # Create variable for year for MAP data
    fmr_list[[state]][["map"]]$year <- as.numeric(2012)
    
    # Append dataframe to list
    map_list <- append(map_list, list(fmr_list[[state]][["map"]]))
  }
  
  if (nrow(fmr_list[[state]][["adp"]]) > 0) {
    
    # Create variable for tag for type of data and state name for TAG data
    fmr_list[[state]][["adp"]]$tag <- paste0("ADP - ", state)
    
    # Create variable for year for ADP data
    fmr_list[[state]][["adp"]]$year <- as.numeric(2012)
    
    # Append dataframe to list
    adp_list <- append(adp_list, list(fmr_list[[state]][["adp"]]))
  }
}

# Convert to singular dataframe
map_df <- do.call(rbind, map_list)
adp_df <- do.call(rbind, adp_list)

# Restructure variable names
names(map_df) <- tolower(names(map_df))
names(map_df) <- gsub(" ", "_", names(map_df))

names(adp_df) <- tolower(names(adp_df))
names(adp_df) <- gsub(" ", "_", names(adp_df))

# Pivot MAP table longer
map_long <- map_df %>%
  pivot_longer(
    cols = c(total_computable, 
             federal_share, 
             federal_share_medicaid,
             federal_share_arra,
             federal_share_bipp,
             state_share),
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
    cols = c(total_computable, 
             federal_share, 
             state_share),
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

saveRDS(map_wider, file = "Temp/map_wide_2012.rds")
saveRDS(adp_wider, file = "Temp/adp_wide_2012.rds")















