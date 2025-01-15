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
library(datasets)
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

### -------------------------- 2002 MMC Enrollment ------------------------- ###

# Get PDF text table for Medicaid enrollment
mmc_2002 <- pdf_text("2002-medicaid-managed-care-enrollment-report.pdf")

# Get page where the table lives
mmc_entity_2002_text <- mmc_2002[9]

mmc_entity_2002_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_2002_text,
  row_remove = c(1:3, 57:61),
  col_remove = 10,
  new_col_names = c("", "row_num", "state", "hio", "commercial_mco", 
                    "medicaid_only_mco", "pccm", "php", "other")
)

mmc_entity_2002_df <- mmc_entity_2002_df %>%
  select(-V1, -row_num) %>%
  mutate(across(-state, ~ as.numeric(.)))

mmc_entity_2002_df$state <- gsub("Dist Of Columbia", 
                                 "District of Columbia", 
                                 mmc_entity_2002_df$state)

mmc_entity_2002_df <- mmc_entity_2002_df %>%
  mutate(crb_mc_enrollees = commercial_mco + medicaid_only_mco)

# Save data for 2006
write_csv(mmc_entity_2002_df, paste0(save_path, "/data_2002.csv"))

### -------------------------- 2001 MMC Enrollment ------------------------- ###

# Get PDF text table for Medicaid enrollment
mmc_2001 <- pdf_text("2001-medicaid-managed-care-enrollment-report.pdf")

# Get page where the table lives
mmc_entity_2001_text <- mmc_2001[9]

mmc_entity_2001_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_2001_text,
  row_remove = c(1:4, 97:103),
  col_remove = NULL,
  new_col_names = c("row_num", "state", "hio", "commercial_mco", 
                    "medicaid_only_mco", "pccm", "php", "other", "")
)

# Shift misaligned rows one to the left
for (i in 1:nrow(mmc_entity_2001_df)) {
  if (mmc_entity_2001_df$V9[i] == 0) {
    mmc_entity_2001_df[i, 1:8] <- c(mmc_entity_2001_df[i, 2:9], NA)
  }
}

# Remove rows where there is a number in the state column
mmc_entity_2001_df <- mmc_entity_2001_df[!grepl("[0-9]", 
                                                mmc_entity_2001_df$state), ]

mmc_entity_2001_df <- mmc_entity_2001_df %>%
  select(-row_num, -V9)

mmc_entity_2001_df <- mmc_entity_2001_df %>%
  mutate(
    across(2:7, ~ gsub(" ", "", .)),
    across(2:7, ~ as.numeric(.))
  )

mmc_entity_2001_df$state <- gsub("Dist Of Columbia", 
                                 "District of Columbia",
                                 mmc_entity_2001_df$state)

mmc_entity_2001_df <- mmc_entity_2001_df %>%
  mutate(crb_mc_enrollees = commercial_mco + medicaid_only_mco)

# Save data for 2001
write_csv(mmc_entity_2001_df, paste0(save_path, "/data_2001.csv"))

### -------------------------- 2000 MMC Enrollment ------------------------- ###

# Get PDF text table for Medicaid enrollment
mmc_2000 <- pdf_text("2000-medicaid-managed-care-enrollment-report.pdf")

# Get page where the table lives
mmc_entity_2000_text <- mmc_2000[9]

mmc_entity_2000_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_2001_text,
  row_remove = c(1:4, 97:103),
  col_remove = NULL,
  new_col_names = c("row_num", "state", "hio", "commercial_mco", 
                    "medicaid_only_mco", "pccm", "php", "other", "")
)

# Shift misaligned rows one to the left
for (i in 1:nrow(mmc_entity_2000_df)) {
  if (mmc_entity_2000_df$V9[i] == 0) {
    mmc_entity_2000_df[i, 1:8] <- c(mmc_entity_2000_df[i, 2:9], NA)
  }
}

# Remove rows where there is a number in the state column
mmc_entity_2000_df <- mmc_entity_2000_df[!grepl("[0-9]", 
                                                mmc_entity_2000_df$state), ]

mmc_entity_2000_df <- mmc_entity_2000_df %>%
  select(-row_num, -V9)

mmc_entity_2000_df <- mmc_entity_2000_df %>%
  mutate(
    across(2:7, ~ gsub(" ", "", .)),
    across(2:7, ~ as.numeric(.))
  )

mmc_entity_2000_df$state <- gsub("Dist Of Columbia", 
                                 "District of Columbia",
                                 mmc_entity_2000_df$state)

mmc_entity_2000_df <- mmc_entity_2000_df %>%
  mutate(crb_mc_enrollees = commercial_mco + medicaid_only_mco)

# Save data for 2000
write_csv(mmc_entity_2000_df, paste0(save_path, "/data_2000.csv"))

### -------------------------- 1999 MMC Enrollment ------------------------- ###

# Get PDF text table for Medicaid enrollment
mmc_1999 <- pdf_text("1999-medicaid-managed-care-enrollment-report.pdf")

# Get page where the table lives
mmc_entity_1999_text1 <- mmc_1999[7]
mmc_entity_1999_text2 <- mmc_1999[8]

mmc_entity_1999_df <- process_mmc_entity_data(
  pdf_text = mmc_entity_1999_text1,
  row_remove = c(1:9, 14, 19, 33, 35, 38),
  col_remove = 8,
  new_col_names = c("state", "hio", "commercial_mco", 
                    "medicaid_only_mco", "pccm", "php", "other")
)

# Edit messed up values for CA
mmc_entity_1999_df[4, "hio"] <- 365498
mmc_entity_1999_df[4, "php"] <- 333160
mmc_entity_1999_df[4, "other"] <- 2279

# Same with MI
mmc_entity_1999_df[22, "pccm"] <- 0
mmc_entity_1999_df[22, "php"] <- 1130608
mmc_entity_1999_df[22, "other"] <- 1124

mmc_entity_1999_df2 <- process_mmc_entity_data(
  pdf_text = mmc_entity_1999_text2,
  row_remove = c(6, 11, 13, 18, 21, 23, 25, 27, 33, 35, 37:41),
  col_remove = 8,
  new_col_names = c("state", "hio", "commercial_mco", 
                    "medicaid_only_mco", "pccm", "php", "other")
)

# Fix state names
mmc_entity_1999_df2[5, "state"] <- "New York"
mmc_entity_1999_df2[9, "state"] <- "North Carolina"
mmc_entity_1999_df2[10, "state"] <- "North Dakota"
mmc_entity_1999_df2[14, "state"] <- "Pennsylvania"
mmc_entity_1999_df2[16, "state"] <- "Rhode Island"
mmc_entity_1999_df2[17, "state"] <- "South Carolina"
mmc_entity_1999_df2[18, "state"] <- "South Dakota"
mmc_entity_1999_df2[25, "state"] <- "West Virginia"

# Tweak values for South Carolina
mmc_entity_1999_df2[17, "php"] <- 0
mmc_entity_1999_df2[17, "other"] <- 15695

# Tweak values for Tennessee
mmc_entity_1999_df2[19, "pccm"] <- 0
mmc_entity_1999_df2[19, "php"] <- 1312969

# Tweak values for Utah
mmc_entity_1999_df2[21, "pccm"] <- 0
mmc_entity_1999_df2[21, "php"] <- 118601
mmc_entity_1999_df2[21, "other"] <- 0

# Tweak values for Washington
mmc_entity_1999_df2[24, "pccm"] <- 3805
mmc_entity_1999_df2[24, "php"] <- 1413447
mmc_entity_1999_df2[24, "other"] <- 0

mmc_entity_1999_df <- rbind(mmc_entity_1999_df,
                            mmc_entity_1999_df2)

mmc_entity_1999_df <- mmc_entity_1999_df %>%
  mutate(
    across(2:7, ~ as.numeric(.))
  )

mmc_entity_1999_df <- mmc_entity_1999_df %>%
  mutate(crb_mc_enrollees = commercial_mco + medicaid_only_mco)

# Save data for 1999
write_csv(mmc_entity_1999_df, paste0(save_path, "/data_1999.csv"))

### -------------------------- 1998 MMC Enrollment ------------------------- ###

# Extend state.abb and state.name with DC name
state.abb <- c(state.abb, "DC")
state.name <- c(state.name, "District of Columbia")

# Get table by program type 
tbl_1998 <- read_excel("1998_tables.xlsx")

# Filter for risk-based managed care types (comprehensive and medicaid-only)
tbl_1998_filtered <- tbl_1998 %>%
  filter(plan_type %in% c("Comp-MCO", "Mcaid-MCO")) %>%
  filter(payment_arrangements == "FUL") %>%
  mutate(number_of_enrollees = gsub(",", "", number_of_enrollees),
         number_of_enrollees = as.numeric(number_of_enrollees),
         number_of_enrollees = ifelse(is.na(number_of_enrollees), 0, number_of_enrollees))

# Get sum of comp. risk-based enrollees per state
crb_1998 <- tbl_1998_filtered %>%
  group_by(state) %>%
  summarise(crb_mc_enrollees = sum(number_of_enrollees)) %>%
  mutate(year = 1998) %>%
  filter(state != "PR")
  
# Match state abbreviations to state names 
crb_1998$state <- state.name[match(crb_1998$state, state.abb)]

# Assign the rest of states with 0s
crb_1998 <- crb_1998 %>%
  complete(state = state.name, year, fill = list(crb_mc_enrollees = 0))

# Save data for 1998
write_csv(crb_1998, paste0(save_path, "/data_1998.csv"))

### -------------------------- 1997 MMC Enrollment ------------------------- ###

# Get table by program type 
tbl_1997 <- read_excel("1997_tables.xlsx")

tbl_1997_filtered <- tbl_1997 %>%
  filter(payment_arrangements == "FUL") %>%
  filter(plan_type %in% c("HMO/SPD", "HMO/FQ")) %>%
  mutate(number_of_enrollees = gsub(",", "", number_of_enrollees),
         number_of_enrollees = as.numeric(number_of_enrollees),
         number_of_enrollees = ifelse(is.na(number_of_enrollees), 0, number_of_enrollees))

# Get sum of comp. risk-based enrollees per state
crb_1997 <- tbl_1997_filtered %>%
  group_by(state) %>%
  summarise(crb_mc_enrollees = sum(number_of_enrollees)) %>%
  mutate(year = 1997) %>%
  filter(state != "PR")

# Match state abbreviations to state names 
crb_1997$state <- state.name[match(crb_1997$state, state.abb)]

# Assign the rest of states with 0s
crb_1997 <- crb_1997 %>%
  complete(state = state.name, year, fill = list(crb_mc_enrollees = 0))

# Save data for 1997
write_csv(crb_1997, paste0(save_path, "/data_1997.csv"))

### -------------------------- 1996 MMC Enrollment ------------------------- ###

# Get table by program type 
tbl_1996 <- read_excel("1996_tables.xlsx")

tbl_1996_filtered <- tbl_1996 %>%
  filter(payment_arrangement == "FUL") %>%
  filter(plan_type %in% c("HMO/SPD", "HMO/FQ")) %>%
  mutate(number_of_enrollees = gsub("[,-]", "", number_of_enrollees),
         number_of_enrollees = as.numeric(number_of_enrollees),
         number_of_enrollees = ifelse(is.na(number_of_enrollees), 0, number_of_enrollees))

# Get sum of comp. risk-based enrollees per state
crb_1996 <- tbl_1996_filtered %>%
  group_by(state) %>%
  summarise(crb_mc_enrollees = sum(number_of_enrollees)) %>%
  mutate(year = 1996) %>%
  filter(state != "PR")

# Match state abbreviations to state names 
crb_1996$state <- state.name[match(crb_1996$state, state.abb)]

# Assign the rest of states with 0s
crb_1996 <- crb_1996 %>%
  complete(state = state.name, year, fill = list(crb_mc_enrollees = 0))

# Save data for 1996
write_csv(crb_1996, paste0(save_path, "/data_1996.csv"))

### -------------------------- 1995 MMC Enrollment ------------------------- ###

# Get table by program type 
tbl_1995 <- read_excel("1995_tables.xlsx")

tbl_1995_filtered <- tbl_1995 %>%
  filter(payment_arrangement == "FUL") %>%
  filter(program_type %in% c("HMO/SPD", "HMO/FQ")) %>%
  mutate(number_of_enrollees = gsub("[,-]", "", number_of_enrollees),
         number_of_enrollees = as.numeric(number_of_enrollees),
         number_of_enrollees = ifelse(is.na(number_of_enrollees), 0, number_of_enrollees))

# Get sum of comp. risk-based enrollees per state
crb_1995 <- tbl_1995_filtered %>%
  group_by(state) %>%
  summarise(crb_mc_enrollees = sum(number_of_enrollees)) %>%
  mutate(year = 1995) %>%
  filter(state != "PR")

# Match state abbreviations to state names 
crb_1995$state <- state.name[match(crb_1995$state, state.abb)]

# Assign the rest of states with 0s
crb_1995 <- crb_1995 %>%
  complete(state = state.name, year, fill = list(crb_mc_enrollees = 0))

# Save data for 1995
write_csv(crb_1995, paste0(save_path, "/data_1995.csv"))






