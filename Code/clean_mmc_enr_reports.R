#'
#' clean_mmc_enr_reports.R -- Clean MMC enrollment reports into CSV files
#'                            Not functionalized due to reports for different
#'                            years being formatted differently. PDF tables
#'                            have similar structure, but get read in differently
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

#' Function to process and clean the PDF enrollment tables for total Medicaid
#' enrollment and Managed Care enrollment
#' @param pdf_text PDF text
#' @param row_remove specific rows to remove (numeric)
#' @param col_remove specific columns to remove (numeric)
#' @param new_col_names new column names for table
#' @param comma_columns column with commas to remove in the values
#' @param percent_columns columns with percent signs to remove in the values
process_mmc_data <- function(pdf_text, 
                             row_remove = NULL, 
                             col_remove = NULL, 
                             new_col_names = NULL, 
                             comma_columns = NULL, 
                             percent_columns = NULL) {
  # Convert PDF to list of text lines
  mmc_list <- pdf_to_list(pdf_text)
  
  # Consolidate list text into data frame
  mmc_df <- as.data.frame(do.call(rbind, mmc_list))
  
  # Remove specified rows and columns if provided
  if (!is.null(row_remove)) {
    mmc_df <- mmc_df[-row_remove, ]
  }
  if (!is.null(col_remove)) {
    mmc_df <- mmc_df[, -col_remove]
  }
  
  # Rename columns if new names are provided
  if (!is.null(new_col_names)) {
    names(mmc_df) <- new_col_names
  }
  
  # Clean up number formatting
  mmc_df <- mmc_df %>%
    mutate(across(all_of(comma_columns), ~ as.numeric(gsub(",", "", .)))) %>%
    mutate(across(all_of(percent_columns), ~ as.numeric(gsub("%", "", .)) / 100))
  
  # Title case state names
  mmc_df$state <- str_to_title(mmc_df$state)
  
  return(mmc_df)
}

#' Function to process and clean the PDF enrollment tables for each Medicaid 
#' entity
#' @param pdf_text PDF text
#' @param row_remove specific rows to remove (numeric)
#' @param col_remove specific columns to remove (numeric)
#' @param new_col_names new column names for table
#' @param cleanup_commas 
process_mmc_entity_data <- function(pdf_text, 
                                    row_remove = NULL, 
                                    col_remove = NULL, 
                                    new_col_names = NULL, 
                                    cleanup_commas = TRUE) {
  # Convert PDF to list of text lines
  mmc_list <- pdf_to_list(pdf_text)
  
  # Convert list to a data frame
  mmc_df <- as.data.frame(do.call(rbind, mmc_list))
  
  # Remove specified rows and columns if provided
  if (!is.null(row_remove)) {
    mmc_df <- mmc_df[-row_remove, ]
  }
  if (!is.null(col_remove)) {
    mmc_df <- mmc_df[, -col_remove]
  }
  
  # Rename columns if new names are provided
  if (!is.null(new_col_names)) {
    names(mmc_df) <- new_col_names
  }
  
  # Clean up number formatting by removing commas
  if (cleanup_commas) {
    mmc_df <- as.data.frame(sapply(mmc_df, function(x) 
      gsub(",", "", x)), stringsAsFactors = FALSE)
  }
  
  # Title case state names
  mmc_df$state <- str_to_title(mmc_df$state)
  
  return(mmc_df)
}


### --------------------------- 2010 MMC Reports --------------------------- ###

library(dplyr)
library(pdftools)
library(readxl)
library(tidyverse)

# Change path depending on file
setwd(file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                "medicaid_privatization_exp", "Input_Data",
                "Medicaid_managedcare_enrollment_report", "reports"))

# Define save path
save_path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                       "medicaid_privatization_exp", "Input_Data",
                       "Medicaid_managedcare_enrollment_report",
                       "by_program_pop_from_report")

mmc_2010 <- pdf_text("MMC Enrollment Report 2010 Complete Program Summary.pdf")

# Get page where table for total Medicaid enrollment table lives
mmc_2010_text <- mmc_2010[56]

# Process total Medicaid and managed care enrollment data
mmc_2010_df <- process_mmc_data(
  pdf_text = mmc_2010_text,
  row_remove = c(1:3, 57:61),
  col_remove = c(1, 2),
  new_col_names = c("state", 
                    "total_med_enr", 
                    "managed_care_enrollment", 
                    "pct_in_managed_care"),
  comma_columns = c("total_med_enr", 
                    "managed_care_enrollment"),
  percent_columns = c("pct_in_managed_care")
)

# Get page where table for Medicaid enrollment by entity type table lives
mmc_entity_2010_text <- mmc_2010[57]

# Process Medicaid enrollment by entity type
mmc_entity_2010_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_2010_text,
  row_remove = c(1:3, 57:61),
  col_remove = 10,
  new_col_names = c("state", "hio", "commercial_mco", "medicaid_only_mco", 
                    "pccm", "pihp", "pahp", "pace", "other")
)

# Convert columns 2-9 to numeric
mmc_entity_2010_df <- mmc_entity_2010_df %>%
  mutate(across(2:9, ~as.numeric(.)))

mmc_entity_2010_df$state <- gsub("Dist Of Columbia",
                                 "District Of Columbia",
                                 mmc_entity_2010_df$state)

# Join MMC enrollment data and MMC entity enrollment data
mmc_2010 <- left_join(mmc_2010_df, mmc_entity_2010_df, by = "state")

mmc_2010$state <- gsub("District Of Columbia", 
                       "District of Columbia",
                       mmc_2010$state)

write_csv(mmc_2010, paste0(save_path, "/data_2010.csv"))

### ------------------------- 2009 MMC Enrollment -------------------------- ###

mmc_2009 <- pdf_text("2009_medicaid_managed_care_reports/2009_mmc_enrollment.pdf")

mmc_2009_df <- process_mmc_data(
  pdf_text = mmc_2009,
  row_remove = c(1:3, 57:61),
  col_remove = c(1, 2),
  new_col_names = c("state", "total_med_enr", "managed_care_enrollment", "pct_in_managed_care"),
  comma_columns = c("total_med_enr", "managed_care_enrollment"),
  percent_columns = c("pct_in_managed_care")
)

# Get pdf text table for MMC entity enrollment
mmc_entity_2009 <- pdf_text("2009_medicaid_managed_care_reports/2009_mmc_entity_enrollment.pdf")

# Process MMC entity enrollment
mmc_entity_2009_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_2009,
  row_remove = c(1:3, 57:61),
  col_remove = 10,
  new_col_names = c("state", "hio", "commercial_mco", "medicaid_only_mco", 
                    "pccm", "pihp", "pahp", "pace", "other")
)

# Join the two dataframes by state name
mmc_2009_full <- left_join(mmc_2009_df, mmc_entity_2009_df, by = "state")

# Change name for DC
mmc_2009_full$state <- gsub("District Of Columbia", 
                            "District of Columbia",
                            mmc_2009_full$state)

# Save data for 2009
write_csv(mmc_2009_full, paste0(save_path, "/data_2009.csv"))

### -------------------------- 2008 MMC enrollment ------------------------- ###

mmc_2008 <- pdf_text("2008_medicaid_managed_care_reports/2008_mmc_enrollment.pdf")

# Process total Medicaid and managed care enrollment data
mmc_2008_df <- process_mmc_data(
  pdf_text = mmc_2008,
  row_remove = c(1:3, 29:33, 61:64),
  col_remove = c(1, 2),
  new_col_names = c("state", 
                    "total_med_enr", 
                    "managed_care_enrollment", 
                    "pct_in_managed_care"),
  comma_columns = c("total_med_enr", 
                    "managed_care_enrollment"),
  percent_columns = c("pct_in_managed_care")
)

# Get pdf table for Medicaid enrollment by entity
mmc_entity_2008 <- pdf_text("2008_medicaid_managed_care_reports/2008_mmc_entity_enrollment.pdf")

# Get page where the table lives
mmc_entity_2008_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_2008,
  row_remove = c(1:3, 60:63),
  col_remove = 10,
  new_col_names = c("state", "hio", "commercial_mco", "medicaid_only_mco", 
                    "pccm", "pihp", "pahp", "pace", "other")
)

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
mmc_2008_full <- rbind(mmc_2008_full, missouri)

mmc_2008_full <- mmc_2008_full[order(mmc_2008_full$state), ]

mmc_2008_full$state <- gsub("Dist. Of Columbia",
                            "District of Columbia",
                            mmc_2008_full$state)

write_csv(mmc_2008_full, paste0(save_path, "/data_2008.csv"))


### -------------------------- 2006 MMC Enrollment ------------------------- ###

# Get PDF text table for Medicaid enrollment
mmc_2006 <- pdf_text("2006-medicaid-managed-care-enrollment-report.pdf")

# Get page where the table lives
mmc_2006_text <- mmc_2006[7]

mmc_2006_df <- process_mmc_data(
  pdf_text = mmc_2006_text,
  row_remove = c(1:3, 57:61),
  col_remove = c(1, 2),
  new_col_names = c("state", 
                    "total_med_enr", 
                    "managed_care_enrollment", 
                    "pct_in_managed_care"),
  comma_columns = c("total_med_enr", 
                    "managed_care_enrollment"),
  percent_columns = c("pct_in_managed_care")
)

# Get PDF text table for MMC entity enrollment
mmc_entity_2006_text <- mmc_2006[8]

mmc_entity_2006_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_2006_text,
  row_remove = c(1:3, 57:61),
  col_remove = 10,
  new_col_names = c("state", "hio", "commercial_mco", "medicaid_only_mco", 
                    "pccm", "pihp", "pahp", "pace", "other")
)

# Join the two dataframes by state name
mmc_2006_full <- left_join(mmc_2006_df, mmc_entity_2006_df, by = "state")

# Change name for DC
mmc_2006_full$state <- gsub("District Of Columbia", 
                            "District of Columbia",
                            mmc_2006_full$state)
# Save data for 2006
write_csv(mmc_2006_full, paste0(save_path, "/data_2006.csv"))















