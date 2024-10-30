#'
#' clean_mmc_enr_reports.R -- Clean MMC enrollment reports into CSV files
#'                            clean up more later -- functionalize everything
#'

### ----------------------------- FUNCTIONS -------------------------------- ###
#' Converts pdf text to list
#' @param text page of text as a string
#' @return list with each line as its own sublist
pdf_to_list <- function(text) {
  
  # Splits string text
  lines <- strsplit(text, "\n")[[1]]
  
  # To the filtered lines format the spacing for the columns
  list_data <- lapply(lines, function(line) {
    columns <- strsplit(line, "\\s{2,}|\t")[[1]]
    return(columns)
  })
  return(list_data)
}

### --------------------------- 2010 MMC Reports --------------------------- ###

library(dplyr)
library(pdftools)
library(readxl)
library(tidyverse)

# Change path depending on file
pdf_path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                      "medicaid_privatization_exp", "Input_Data",
                      "Medicaid_managedcare_enrollment_report",
                      "MMC Enrollment Report 2010 Complete Program Summary.pdf")

# Get pages from pdf
pages <- pdf_text(pdf_path)

# Filter for just the MMC enrollment numbers by state
pages <- pages[56:57]

# Organize page text into lists
list_data <- lapply(pages, pdf_to_list)

# Consolidate list text into data frame
mmc_enr <- as.data.frame(do.call(rbind, list_data[[1]]))

# Remove first two columns and first couple and last rows
mmc_enr <- mmc_enr[-c(1:3, 57:61), -c(1,2)]

# Rename columns
names(mmc_enr) <- c("state", 
                    "total_med_enr",
                    "managed_care_enrollment",
                    "pct_in_managed_care")

# Change state names to title case
mmc_enr$state <- str_to_title(mmc_enr$state)

# Clean up number formatting
mmc_enr <- mmc_enr %>%
  mutate(total_med_enr = gsub(",", "", total_med_enr),
         managed_care_enrollment = gsub(",", "", managed_care_enrollment),
         pct_in_managed_care = gsub("%", "", pct_in_managed_care)) %>%
  mutate(total_med_enr = as.numeric(total_med_enr),
         managed_care_enrollment = as.numeric(managed_care_enrollment),
         pct_in_managed_care = as.numeric(pct_in_managed_care) / 100)

# Consolidate list text into data frame
mmc_entity_enr <- as.data.frame(do.call(rbind, list_data[[2]]))

# Remove unneeded rows/columns
mmc_entity_enr <- mmc_entity_enr[-c(1:3, 57:61), -10]

# Rename column names
names(mmc_entity_enr) <- c("state",
                           "hio",
                           "commercial_mco",
                           "medicaid_only_mco",
                           "pccm",
                           "pihp",
                           "pahp",
                           "pace",
                           "other")

# Omit all commas
mmc_entity_enr <- as.data.frame(sapply(mmc_entity_enr, function(x) {
  x <- gsub(",", "", x)
}))

# Convert columns 2-9 to numeric
mmc_entity_enr <- mmc_entity_enr %>%
  mutate(across(2:9, ~as.numeric(.)))

# Change naming for DC
mmc_entity_enr$state <- str_to_title(mmc_entity_enr$state)

# Join MMC enrollment data and MMC entity enrollment data
mmc_2010 <- left_join(mmc_enr, mmc_entity_enr, by = "state")

mmc_2010$state <- gsub("District Of Columbia", 
                       "District of Columbia",
                       mmc_2010$state)

save_path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                       "medicaid_privatization_exp", "Input_Data",
                       "Medicaid_managedcare_enrollment_report",
                       "by_program_pop_from_report")

write_csv(mmc_2010, paste0(save_path, "/data_2010.csv"))

### -------------------------- 2008 MMC enrollment ------------------------- ###

# Change path depending on file
setwd(file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                "medicaid_privatization_exp", "Input_Data",
                "Medicaid_managedcare_enrollment_report"))

mmc_2008 <- pdf_text("2008_medicaid_managed_care_reports/2008_mmc_enrollment.pdf")

# Organize page text into lists
mmc_2008_list <- pdf_to_list(mmc_2008)

# Consolidate list text into data frame
mmc_2008_df <- as.data.frame(do.call(rbind, mmc_2008_list))

mmc_2008_df <- mmc_2008_df[-c(1:3, 61:64), -c(1:2)]

mmc_2008_df <- mmc_2008_df[rowSums(mmc_2008_df == "") == 0, ]

names(mmc_2008_df) <- c("state",
                        "total_med_enr",
                        "managed_care_enrollment",
                        "pct_in_managed_care")

mmc_2008_df <- mmc_2008_df %>%
  mutate(total_med_enr = gsub(",", "", total_med_enr),
         managed_care_enrollment = gsub(",", "", managed_care_enrollment),
         pct_in_managed_care = gsub("%", "", pct_in_managed_care)) %>%
  mutate(total_med_enr = as.numeric(total_med_enr),
         managed_care_enrollment = as.numeric(managed_care_enrollment),
         pct_in_managed_care = as.numeric(pct_in_managed_care) / 100)

mmc_2008_df$state <- str_to_title(mmc_2008_df$state)

mmc_entity_2008 <- pdf_text("2008_medicaid_managed_care_reports/2008_mmc_entity_enrollment.pdf")

mmc_entity_2008_list <- pdf_to_list(mmc_entity_2008)

mmc_entity_2008_df <- as.data.frame(do.call(rbind, mmc_entity_2008_list))

mmc_entity_2008_df <- mmc_entity_2008_df[-c(1:3, 60:63), -10]

names(mmc_entity_2008_df) <- c("state",
                               "hio",
                               "commercial_mco",
                               "medicaid_only_mco",
                               "pccm",
                               "pihp",
                               "pahp",
                               "pace",
                               "other")

# Omit all commas
mmc_entity_2008_df <- as.data.frame(sapply(mmc_entity_2008_df, function(x) {
  x <- gsub(",", "", x)
}))

# Extracting data for Missouri somehow gets messed up -- manually handle
mmc_entity_2008_df <- mmc_entity_2008_df[-grep("Missour|^$", 
                                               mmc_entity_2008_df$state), ]

mmc_entity_2008_df$state <- str_to_title(mmc_2008_df$state)

mmc_2008_full <- left_join(mmc_2008_df, mmc_entity_2008_df, by = "state")

missouri <- data.frame(
  state = "Missouri",
  total_med_enr = 833112,
  managed_care_enrollment = 810828,
  pct_in_managed_care = 0.9733,
  hio = 0,
  commercial_mco = 0,
  medicaid_only_mco = 345868,
  pccm = 0,
  pihp = 0,
  pahp = 464764,
  pace = 196,
  other = 0
)

# Append manually handled dataframe for Missouri onto the main working table
mmc_2008_full <- rbind(mmc_entity_2008_df, missouri)

mmc_2008_full <- mmc_2008_full[order(mmc_2008_full$state), ]

mmc_2008_full$state <- gsub("Dist. Of Columbia",
                            "District of Columbia",
                            mmc_2008_full$state)
df3[-1] <- lapply(df3[-1], function(x) as.numeric(gsub(",", "", x)))

write_csv(mmc_2008_full, paste0(save_path, "/data_2008.csv"))

### ------------------------- 2009 MMC Enrollment -------------------------- ###

mmc_2009 <- pdf_text("2009_medicaid_managed_care_reports/2009_mmc_enrollment.pdf")

# Organize page text into lists
mmc_2009_list <- pdf_to_list(mmc_2009)

# Consolidate list text into data frame
mmc_2009_df <- as.data.frame(do.call(rbind, mmc_2009_list))

# Remove unneeded rows
mmc_2009_df <- mmc_2009_df[-c(1:3, 57:61), -c(1,2)]

# Rename variables
names(mmc_2009_df) <- c("state",
                        "total_med_enr",
                        "managed_care_enrollment",
                        "pct_in_managed_care")

# Clean up number formatting 
mmc_2009_df <- mmc_2009_df %>%
  mutate(total_med_enr = gsub(",", "", total_med_enr),
         managed_care_enrollment = gsub(",", "", managed_care_enrollment),
         pct_in_managed_care = gsub("%", "", pct_in_managed_care)) %>%
  mutate(total_med_enr = as.numeric(total_med_enr),
         managed_care_enrollment = as.numeric(managed_care_enrollment),
         pct_in_managed_care = as.numeric(pct_in_managed_care) / 100)

# Apply string formatting to state names
mmc_2009_df$state <- str_to_title(mmc_2009_df$state)

# Get pdf text table for MMC entity enrollment
mmc_entity_2009 <- pdf_text("2009_medicaid_managed_care_reports/2009_mmc_entity_enrollment.pdf")

# Convert pdf text to a list where each line of text is a sublist
mmc_entity_2009_list <- pdf_to_list(mmc_entity_2009)

# Convert to dataframe
mmc_entity_2009_df <- as.data.frame(do.call(rbind, mmc_entity_2009_list))

# Tidy up formatting by omitting unneeded rows and column
mmc_entity_2009_df <- mmc_entity_2009_df[-c(1:3, 57:61), -10]

# Give columns the appropriate names
names(mmc_entity_2009_df) <- c("state",
                               "hio",
                               "commercial_mco",
                               "medicaid_only_mco",
                               "pccm",
                               "pihp",
                               "pahp",
                               "pace",
                               "other")

# Omit all commas
mmc_entity_2009_df <- as.data.frame(sapply(mmc_entity_2009_df, function(x) {
  x <- gsub(",", "", x)
}))

# Neatly format state names
mmc_entity_2009_df$state <- str_to_title(mmc_entity_2009_df$state)

# Join the two dataframes by state name
mmc_2009_full <- left_join(mmc_2009_df, mmc_entity_2009_df, by = "state")

# Change name for DC
mmc_2009_full$state <- gsub("District Of Columbia", 
                            "District of Columbia",
                            mmc_2009_full$state)

# Save data for 2009
write_csv(mmc_2009_full, paste0(save_path, "/data_2009.csv"))



