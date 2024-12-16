#'
#' clean_mco_state_enrollment.R -- Clean state managed care enrollment table,
#'                                 get denominators for Medicaid state
#'                                 enrollment and calculate percentages for
#'                                 Comprehensive MCO
#'

### ------------------------------- FUNCTIONS ------------------------------ ###

#' Converts pdf to list
#' @param text page of text as a string
#' @return list with each line as its own sublist
pdf_to_list <- function(text) {
  
  # Split the string into lines
  lines <- strsplit(text, "\n")[[1]]
  
  # Process each line
  list_data <- lapply(lines, function(line) {
    
    # Split the cleaned line by 2 or more spaces or tabs
    columns <- strsplit(line, "\\s{2,}|\t")[[1]]
    
    return(columns)
  })
  return(list_data)
}

#' Shifts columns to match the PDF table structure
#' @param data text data organized as line by line text in a list
#' @return dataframe with values in the correct place
shift_columns <- function(data, expected_cols = 10) {
  
  shifted_data <- lapply(data, function(row) {
    # Step 1: Handle footnotes in the state name (column 1 and 2)
    if (nchar(row[2]) == 1 & row[2] %in% as.character(0:9)) {
      # Combine the state name with the footnote
      row[1] <- paste0(row[1], row[2])
      # Shift all subsequent values one position to the left
      row <- c(row[1], row[3:length(row)], "")
    }
    
    # Step 2: Adjust the number of columns to match the expected format
    # If too many columns, trim the extra ones
    if (length(row) > expected_cols) {
      row <- row[1:expected_cols]
    }
    
    # If too few columns, pad with empty strings
    if (length(row) < expected_cols) {
      row <- c(row, rep("", expected_cols - length(row)))
    }
    
    return(row)
  })
  
  shifted_df <- as.data.frame(do.call(rbind, shifted_data), stringsAsFactors = F)
  return(shifted_df)
}

#' Clean lines and split by commas
#' @param file_name text file
#' @return cleaned dataframe
clean_and_split <- function(file_name) {
  # Read the file as raw lines
  lines <- readLines(file_name)
  
  # Filter lines that contain a valid state name
  filtered_lines <- lines[sapply(lines, function(line) {
    any(sapply(state_names, grepl, line))
  })]
  
  # Split the remaining lines by commas
  cleaned_data <- do.call(rbind, lapply(filtered_lines, function(line) {
    unlist(strsplit(line, ","))
  }))
  
  # Convert to data frame
  df <- as.data.frame(cleaned_data, stringsAsFactors = FALSE)
  
  # Extract year from file name and add as a new column
  year <- as.numeric(gsub(".*_(\\d{4})\\.csv$", "\\1", file_name))
  df$year <- year
  
  return(df)
}

### -------------------------------- 2015 ---------------------------------- ###

library(dplyr)
library(readxl)
library(pdftools)
library(tidyverse)

setwd(file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                "medicaid_privatization_exp"))

pdf_path <- file.path(getwd(), "Input_Data", 
                      "Medicaid_managedcare_enrollment_report",
                      "reports",
                      "2015 medicaid managed care enrollment report.pdf")
save_path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun", 
                       "medicaid_privatization_exp", "Input_Data",
                       "Medicaid_managedcare_enrollment_report",
                       "by_program_pop_from_report") 

# Extract tables from pdf
pages <- pdf_text(pdf_path)[c(19, 20)]

# Store text in a list of lists
list_data <- lapply(pages, pdf_to_list)

# Align columns 
df <- shift_columns(list_data[[1]])
df2 <- shift_columns(list_data[[2]])

# Filter out empty strings and remove the first two rows (df) and first row (df2)
df <- df %>%
  filter(across(everything(), ~ . != "")) %>%
  slice(-c(1:2))

df2 <- df2 %>%
  filter(across(everything(), ~ . != "")) %>%
  slice(-1)

# Combine dataframes
df3 <- rbind(df, df2)

# Change column names
names(df3) <- c("state", "total_med_enr","comp_mco", "pccm", "mltss",
                "bho", "dental", "transportation", "pace", "other")

# Remove footnote numbers that follow state names
df3$state <- gsub("[0-9]+$", "", df3$state)

# Replace '--' and 'n/a' with NA
df3[df3 == "--" | df3 == "n/a"] <- NA

# Convert character numbers to numeric for all columns
df3[-1] <- lapply(df3[-1], function(x) as.numeric(gsub(",", "", x)))

write_csv(df3, file = paste0(save_path, "/data_2015.csv"))

### --------------------------------- 2014 --------------------------------- ###

pdf_path <- file.path(getwd(), "Input_Data", 
                      "Medicaid_managedcare_enrollment_report",
                      "reports",
                      "2014-medicaid-managed-care-enrollment-report_0.pdf")

# Extract pages with the tables
pages <- pdf_text(pdf_path)[c(20, 21)]
list_data <- lapply(pages, pdf_to_list)

# Transform as dataframe
df <- as.data.frame(do.call(rbind, list_data[[1]]))
df2 <- as.data.frame(do.call(rbind, list_data[[2]]))

# Omit unnecessary rows
df <- df[-c(1:5, 46), ]
df2 <- df2[-c(1:4, 21:30), ]

# Combine the two tables
df3 <- rbind(df, df2)

# Change variable names
names(df3) <- c("state", "total_med_enr","comp_mco", "pccm", "mltss",
                "bho", "dental", "transportation", "pace", "other")

# Replace '--' and 'n/a' with NA
df3[df3 == "--" | df3 == "n/a"] <- NA

# Convert character numbers to numeric for all columns
df3[-1] <- lapply(df3[-1], function(x) as.numeric(gsub(",", "", x)))

# Omit footnote numbers that follow the state name
df3$state <- gsub("[0-9]", "", df3$state)

# Save
write_csv(df3, file = paste0(save_path, "/data_2014.csv"))


### --------------------------------- 2013 --------------------------------- ###

pdf_path <- file.path(getwd(), "Input_Data", 
                      "Medicaid_managedcare_enrollment_report",
                      "reports",
                      "2013 Medicaid Report.pdf")

# Extract tables from pdf
pages <- pdf_text(pdf_path)[c(9, 10)]
list_data <- lapply(pages, pdf_to_list)

# Transform into dataframe
df <- as.data.frame(do.call(rbind, list_data[[1]]))
df2 <- as.data.frame(do.call(rbind, list_data[[2]]))

# Omit unnecessary rows
df <- df[-c(1:4, 47), ]
df2 <- df2[-c(1:4, 19:26), ]

# Combine dfs
df3 <- rbind(df, df2)

# Change variable names
names(df3) <- c("state", "total_med_enr","comp_mco", "pccm", "mltss",
                "bho", "dental", "transportation", "pace", "other")

# Replace '--' and 'n/a' with NA
df3[df3 == "--" | df3 == "n/a"] <- NA

# Convert character numbers to numeric for all columns
df3[-1] <- lapply(df3[-1], function(x) as.numeric(gsub(",", "", x)))

df3 <- df3 %>%
  select(-comp_mco)

comp_mco_2013 <- clean_and_split(paste0(path, file.path("/Input_Data",
                                                 "Medicaid_managedcare_enrollment_report", 
                                                 "raw_comp_mco_data",
                                                 "raw_data_2013.csv")))

comp_mco_2013 <- comp_mco_2013 %>%
  select(-V3) %>%
  rename(state = V1,
         comprehensive_mco_enr = V2) %>%
  mutate(state = gsub('"', '', state),
         comprehensive_mco_enr = gsub("[^0-9.]", "", comprehensive_mco_enr),
         comprehensive_mco_enr = as.numeric(comprehensive_mco_enr))

df3 <- df3 %>%
  left_join(comp_mco_2013, by = "state")

write_csv(df3, file = paste0(save_path, "/data_2013.csv"))

### --------------------------------- 2011 --------------------------------- ###

pdf_path <- file.path(getwd(), "Input_Data", 
                      "Medicaid_managedcare_enrollment_report",
                      "reports",
                      "2011-medicaid-mc-enrollment-report.pdf")

# Extract tables from pdf
pages <- pdf_text(pdf_path)[c(7, 8)]
list_data <- lapply(pages, pdf_to_list)

# Transform into dataframe
df <- as.data.frame(do.call(rbind, list_data[[1]]))
df2 <- as.data.frame(do.call(rbind, list_data[[2]]))

# Omit unnecessary rows and change variable names
df <- df %>%
  select(V3, V4) %>%
  slice(-c(1:3), -c(57:61)) %>%
  rename(state = V3,
         total_med_enr = V4)

# Apply string formatting
df$state <- str_to_title(df$state)
df$state <- gsub("District Of Columbia", "Dist of Columbia", df$state)
df$total_med_enr <- gsub(",", "", df$total_med_enr)
df$total_med_enr <- as.numeric(df$total_med_enr)

# Realign table
df2 <- df2[-c(1:3, 57:61), -10]

# Change variable names
names(df2) <- c("state", "hio","commercial_mco", "med_only_mco", "pccm",
                "pihp", "pahp", "pace", "other")

# For numeric columns, remove commas and transform into numeric
df2[-1] <- lapply(df2[-1], function(x) as.numeric(gsub(",", "", x)))

# Merge
df3 <- left_join(df2, df, by = "state")

write_csv(df3, file = paste0(save_path, "/data_2011.csv"))





