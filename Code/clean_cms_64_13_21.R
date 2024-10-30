#'
#' clean_cms_64_13_21.R -- Clean 2013-2021 Medicaid Financial Management Reports
#'                         for all states
#'

### ------------------------------ FUNCTIONS ------------------------------- ###

#' Clean the MAP data for all 50 states and territories
#' @param fmr financial management reports read in multiple sheets
#' @param year the year the FMR was created
#' @return wide-pivoted panel dataframe 
clean_map <- function(fmr, year) {
  
  # Keep lists with the MAP tag
  map_list <- fmr[grepl("^MAP", names(fmr))]
  
  for (page in names(map_list)) {
    
    # Get rid of rows that have more than 4 NAs -- unneeded rows
    map_list[[page]] <- map_list[[page]][rowSums(is.na(map_list[[page]])) <= 4, ]
    
    # Set first row as column names
    names(map_list[[page]]) <- as.character(map_list[[page]][1, ])
    
    # Remove that first row
    map_list[[page]] <- map_list[[page]][-1, ]
    
    # Lower-case the column names
    names(map_list[[page]]) <- tolower(names(map_list[[page]]))
    
    # Remove double spacing
    names(map_list[[page]]) <- str_squish(names(map_list[[page]]))
    
    # Replace spaces with underscore in column names
    names(map_list[[page]]) <- gsub(" ", "_", names(map_list[[page]]))
    
    # Add tag and year
    map_list[[page]][["tag"]] <- page
    map_list[[page]][["year"]] <- year
  }
  
  # Row bind list elements
  map_df <- do.call(rbind, map_list)
  
  # Determine the columns that exist in the dataset
  available_cols <- intersect(c("total_computable", 
                                "federal_share", 
                                "federal_share_medicaid",
                                "federal_share_bipp",
                                "federal_share_arra",
                                "federal_share_covid",
                                "state_share"),
                              names(map_df))
  
  # Pivot MAP table longer
  map_long <- map_df %>%
    pivot_longer(
      cols = all_of(available_cols),
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
  
  return(map_wider)
}

#' Clean the ADM data for all 50 states and territories
#' @param fmr financial management reports read in multiple sheets
#' @param year the year the FMR was created
#' @return wide-pivoted panel dataframe 
clean_adm <- function(fmr, year) {
  
  # Keep lists with the ADM tag
  adm_list <- fmr[grepl("^ADM", names(fmr))]
  
  for (page in names(adm_list)) {
    
    # Get rid of rows that have more than 4 NAs -- unneeded rows
    adm_list[[page]] <- adm_list[[page]][rowSums(is.na(adm_list[[page]])) <= 2, ]
    
    # Set first row as column names
    names(adm_list[[page]]) <- as.character(adm_list[[page]][1, ])
    
    # Remove that first row
    adm_list[[page]] <- adm_list[[page]][-1, ]
    
    # Lower-case the column names
    names(adm_list[[page]]) <- tolower(names(adm_list[[page]]))
    
    # Remove double spacing
    names(adm_list[[page]]) <- str_squish(names(adm_list[[page]]))
    
    # Replace spaces with underscore in column names
    names(adm_list[[page]]) <- gsub(" ", "_", names(adm_list[[page]]))
    
    # Add tag and year
    adm_list[[page]][["tag"]] <- page
    adm_list[[page]][["year"]] <- year
  }
  
  # Row bind list elements
  adm_df <- do.call(rbind, adm_list)
  
  # Pivot ADP table longer
  adm_long <- adm_df %>%
    pivot_longer(
      cols = c(total_computable, federal_share, state_share),
      names_to = "term",
      values_to = "value"
    ) %>%
    mutate(value = as.numeric(value))
  
  # Pivot long ADP table wider
  adm_wider <- adm_long %>%
    pivot_wider(
      names_from = service_category,
      values_from = value,
      values_fn = list(value = ~ .[[1]])
    ) %>%
    arrange(tag, term, year)
  
  return(adm_wider)
}

### ------------------------------------------------------------------------ ###

library(dplyr)
library(readxl)
library(tidyverse)

# Set directories
setwd(file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                "medicaid_privatization_exp"))
source("Code/clean_cms_64_functions.R")

path <- file.path(getwd(), "Input_Data", "CMS_64_report")

# Create vector of years
years <- c("2012-13", as.character(2014:2023))

# Initialize lists to store MAP and ADM data
map_list <- list()
adm_list <- list()

# Iterating reading different file names for FMR years 2013-2023
for (year in years) {
  folder_name <- paste0("financial-management-report-fy", year)
  
  if (year == "2012-13") {
    file_name <- "FMR Net Expenditures FY13.xlsx"
    input_year <- "2013"
  } else if (year == "2014") {
    file_name <- "FMR Net Expenditures FY14.xlsx"
    input_year <- "2014"
  } else if (year == "2015") {
    file_name <- "FY 2015 NET EXPENDITURES.xlsx"
    input_year <- "2015"
  } else if(as.numeric(year) >= 2016 & as.numeric(year) <= 2023) {
    file_name <- paste0("FY ", substr(year, 1, 4), " FMR NET EXPENDITURES.xlsx")
    input_year <- substr(year, 1, 4)
  } else {
    file_name <- paste0("FMR Net Expenditures FY", substr(year, 1, 4), ".xlsx")
    input_year <- substr(year, 1, 4)
  }
  
  # Get exact file name
  fmr_file_path <- file.path(path, folder_name, file_name)
  
  # Get all the sheets in the Excel file and store in list called fmr
  fmr <- multiple_sheets(fmr_file_path)
  
  # Apply cleaner functions to data into panel format
  map_list[[year]] <- clean_map(fmr, input_year)
  adm_list[[year]] <- clean_adm(fmr, input_year)
  }

# Combine all the dataframes
map_df <- bind_rows(map_list)
adm_df <- bind_rows(adm_list)

# Save
saveRDS(map_df, file = "Temp/map_wide_13_23.rds")
saveRDS(adm_df, file = "Temp/adm_wide_13_23.rds")



