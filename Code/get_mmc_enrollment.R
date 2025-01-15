#'
#' get_mmc_enrollment.R -- Clean and aggregate MMC summary reports
#'                         Key column: Enrollment in Managed Care/any MC
#'
### ----------------------------- FUNCTIONS -------------------------------- ###

tbl_cleaner <- function(df) {
  names(df) <- tolower(names(df))
  names(df) <- gsub(" ", "_", names(df))
  
  df <- df %>%
    select(state,
           total_medicaid_enrollment_in_any_type_of_managed_care,
           year) %>%
    rename(managed_care_enrollment = total_medicaid_enrollment_in_any_type_of_managed_care) %>%
    mutate(managed_care_enrollment = gsub(",", "", managed_care_enrollment),
           managed_care_enrollment = as.numeric(managed_care_enrollment))
  
  df$state <- gsub("[0-9]", "", df$state)
  
  df <- df %>%
    filter(!state %in% territories)
  
  df$managed_care_enrollment <- as.numeric(df$managed_care_enrollment)
  
  return(df)
}

### ------------------------------------------------------------------------ ###

library(dplyr)
library(readxl)
library(pdftools)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                  "medicaid_privatization_exp", "Input_Data",
                  "Medicaid_managedcare_enrollment_report", 
                  "program_summary_from_report")

save_path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                       "medicaid_privatization_exp", "Temp")

d_11 <- read_excel(paste0(path, "/data_2011.xlsx"))
d_13 <- read_excel(paste0(path, "/data_2013.xlsx"))
d_14 <- read_excel(paste0(path, "/data_2014.xlsx"))
d_15 <- read_excel(paste0(path, "/data_2015.xlsx"))

# Clean 2011 table
names(d_11) <- tolower(names(d_11))
names(d_11) <- gsub(" ", "_", names(d_11))

d_11 <- d_11 %>%
  select(state, managed_care_enrollment)

d_11$state <- str_to_title(d_11$state)
d_11$state <- gsub("District Of Columbia", "District of Columbia", d_11$state)
d_11 <- d_11[!is.na(d_11$state), ]
d_11$managed_care_enrollment <- as.numeric(d_11$managed_care_enrollment)
d_11$year <- 2011

# Clean 2013 table
names(d_13) <- tolower(names(d_13))
names(d_13) <- gsub(" ", "_", names(d_13))

d_13 <- d_13 %>%
  select(state_or_territory, all_medicaid_managed_care1) %>%
  rename(state = state_or_territory,
        managed_care_enrollment = all_medicaid_managed_care1)

territories <- c("Guam", "American Samoa3", "Northern Mariana Islands",
                 "Puerto Rico", "Virgin Islands", "TOTALS")

d_13 <- d_13 %>%
  filter(!state %in% territories)

d_13 <- d_13[!is.na(d_13$state), ]

d_13$state <- gsub("[0-9]", "", d_13$state)

d_13$managed_care_enrollment <- as.numeric(d_13$managed_care_enrollment)

d_13$year <- 2013

# Clean 2014 table
names(d_14) <- tolower(names(d_14))
names(d_14) <- gsub(" ", "_", names(d_14))

d_14 <- d_14 %>%
  select(state_or_territory, 
         `total_medicaid_enrollment_in_any_type_of\r\nmanaged_care2`) %>%
  rename(state = state_or_territory,
         managed_care_enrollment = `total_medicaid_enrollment_in_any_type_of\r\nmanaged_care2`)

d_14$state <- gsub("[0-9]", "", d_14$state)

d_14 <- d_14 %>%
  filter(!state %in% territories)

d_14$managed_care_enrollment <- as.numeric(d_14$managed_care_enrollment)

d_14$year <- 2014

# Clean 2015 table
names(d_15) <- tolower(names(d_15))
names(d_15) <- gsub(" ", "_", names(d_15))

d_15 <- d_15 %>%
  select(state_or_territory, 
         total_medicaid_enrollment_in_any_type_of_managed_care2) %>%
  rename(state = state_or_territory,
         managed_care_enrollment = total_medicaid_enrollment_in_any_type_of_managed_care2)

d_15$state <- gsub("[0-9]", "", d_15$state)

d_15 <- d_15 %>%
  filter(!state %in% territories)

d_15$managed_care_enrollment <- as.numeric(d_15$managed_care_enrollment)

d_15$year <- 2015

### Read in years 2016-2022 ###

years <- 2016:2022

# Get vector of file names and use apply function to read in files in one line
file_names <- paste0(path, "/data_", years, ".csv")

data_list <- lapply(file_names, read_csv)

names(data_list) <- years

# Apply the standardization
new_data_list <- lapply(data_list, tbl_cleaner)

dfs <- do.call(rbind, new_data_list)

full_dfs <- bind_rows(d_11, d_13, d_14, d_15, dfs)

write_csv(full_dfs, file = paste0(save_path, "/any_mmc_2015_2022.csv"))



