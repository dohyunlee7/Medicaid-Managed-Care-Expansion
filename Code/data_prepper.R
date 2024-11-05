#'
#' data_prepper.R -- Consolidate all the CMS-64 dataframes and Medicaid 
#'                   enrollment into one panel dataset
#'

### ----------------------------- FUNCTIONS -------------------------------- ###

#' Standardize variable naming
#' @param df dataframe
#' @param mapping list of name mapping
#' @return a dataframe with all versions of the same name standardized
standardize_columns <- function(df, mapping) {
  for (desired_name in names(mapping)) {
    current_name <- intersect(names(df), mapping[[desired_name]])
    if (length(current_name) > 0) {
      names(df)[names(df) == current_name] <- desired_name
    }
  }
  return(df)
}

#' Get enrollment numbers and select for the specific enrollment numbers we want
#' @param df dataframe
#' @return the dataframe with just state, total Medicaid, Comprehensive MCO,
#'         Any MCO (Comp. + PCCM + PCCM entity (if available) + MLTSS + Other)
#'         enrollment
enr_getter <- function(df) {
  
  # Turn state abbreviations to the state name
  df$state <- state.name[match(df$state, state.abb)]
  
  # Sum to get total MC enrollment by state
  df <- df %>%
    rename(total_med_enr = num_bene_id_any_el_days,
           comprehensive_mco_enr = num_bene_id_cmcp,
           mltss = num_bene_id_ltcm,
           dental = num_bene_id_dmcp,
           pccm = num_bene_id_pccm,
           bho = num_bene_id_bmcp,
           pace = num_bene_id_aice)
  
  # df <- df %>%
    # mutate(imputed_any_mco_enr = rowSums(df[, c("num_bene_id_cmcp",
    #                                             "num_bene_id_ltcm",
    #                                             "num_bene_id_pccm")], 
    #                                      na.rm = TRUE))
  
  # Rename variable for total Medicaid enrollment
  # Select relevant columns
  df <- df %>%
    select(state, 
           total_med_enr,
           comprehensive_mco_enr,
           mltss,
           dental,
           pccm,
           bho,
           pace)
  
  return(df)
}

#' Clean raw Comp. MCO files
#' @param data raw data from KFF csv
#' @return cleaned dataframe
process_comp_mco_list <- function(data) {
  
  # Filter data to keep only rows that contain a state
  filtered_data <- data[sapply(strsplit(data, ","), function(x) 
    x[1] %in% states)]
  
  # Split the filtered data by commas
  split_data <- strsplit(filtered_data, ",")
  
  # Convert the list of split data into a dataframe
  df <- do.call(rbind, lapply(split_data, function(x) 
    as.data.frame(t(x),stringsAsFactors = FALSE)))
  
  return(df)
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

#' Fill in missing values in panel with other dataframes
#' @param main_data the main panel
#' @param join_data smaller dataset used to join
#' @param by_cols vector of column names to join by (should be state and year)
#' @param coalesce_cols vector of column names to be joined into panel
#' @param reorder_cols vector of column names to order columns
#' @return panel data with needed values filled in
coalesce_join <- function(main_data, 
                          join_data, 
                          by_cols, 
                          coalesce_cols, 
                          reorder_cols = NULL) {
  
  # Join and coalesce specified columns
  main_data <- main_data %>%
    left_join(join_data, by = by_cols)
  
  # Coalesce columns 
  for (col in coalesce_cols) {
    main_data <- main_data %>%
      mutate(!!col := coalesce(.[[paste0(col, ".x")]], .[[paste0(col, ".y")]]))
  }
  
  # Drop both copies of the variable value
  main_data <- main_data %>%
    select(-matches("\\.x$"), -matches("\\.y$"))
    
  # Reorder columns if a specific order is provided
  if (!is.null(reorder_cols)) {
    main_data <- main_data %>%
      select(any_of(reorder_cols), everything())
  }
  
  # Arrange by state and year
  main_data <- main_data %>%
    arrange(across(all_of(by_cols)))
  
  return(main_data)
}




### ---------------------------- READ IN DATA ------------------------------ ###

library(dplyr)
library(haven)
library(datasets)
library(readxl)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

# Read in MAP data for 1997-2021
map_wide_97_01 <- readRDS(paste0(path, "/Temp/map_wide_97_01.rds"))
map_wide_02_11 <- readRDS(paste0(path, "/Temp/map_wide_02_11.rds"))
map_wide_2012 <- readRDS(paste0(path, "/Temp/map_wide_2012.rds"))
map_wide_13_23 <- readRDS(paste0(path, "/Temp/map_wide_13_23.rds"))
map_wide_13_23 <- map_wide_13_23 %>%
  mutate(year = as.numeric(year))

# Combine all the sub-datasets
big_map_wide <- bind_rows(map_wide_97_01, 
                          map_wide_02_11,
                          map_wide_2012,
                          map_wide_13_23)

# Retain rows with tag `total_computable`, remove term variable, reformat state
# names
big_map_wide <- big_map_wide %>%
  filter(term == "total_computable") %>%
  select(-term) %>%
  mutate(tag = gsub("MAP - ", "", tag))

# Read in ADM data for 1997-2021
adp_wide_97_01 <- readRDS(paste0(path, "/Temp/adp_wide_97_01.rds"))
adp_wide_02_11 <- readRDS(paste0(path, "/Temp/adp_wide_02_11.rds"))
adp_wide_2012 <- readRDS(paste0(path, "/Temp/adp_wide_2012.rds"))
adm_wide_13_23 <- readRDS(paste0(path, "/Temp/adm_wide_13_23.rds"))

adm_wide_13_23 <- adm_wide_13_23 %>%
  mutate(year = as.numeric(year))

# Combine all the sub-datasets
big_adp_wide <- bind_rows(adp_wide_97_01, 
                          adp_wide_02_11,
                          adp_wide_2012,
                          adm_wide_13_23)

# Retain rows with tag `total_computable`, remove term variable, reformat state
# names
big_adp_wide <- big_adp_wide %>%
  filter(term == "total_computable") %>%
  select(-term) %>%
  mutate(tag = gsub("\\s*(ADP - |ADM - )\\s*", "", tag))

# Join MAP and ADP dataframes by the state name (tag) and year
df <- left_join(big_map_wide, big_adp_wide, by = c("tag", "year"))

df <- df %>%
  filter(!tag %in% c("Amer. Samoa", 
                     "Guam", 
                     "Mass. Blind", 
                     "N. Mariana Islands",
                     "Virgin Islands",
                     "All States",
                     "National Totals"))

df$tag <- gsub("Dist. Of Col.", "District of Columbia", df$tag)

saveRDS(df, file = paste0(path, "/Temp/fmr_1997_2023.rds"))

### Read 1997-2023 data from here ###
temp <- readRDS(paste0(path, "/Temp/fmr_1997_2023.rds"))

### ---------------- MERGE ENROLLMENT DATA ONTO SPENDING DATA -------------- ###

# Read in files with MMC population by state
dir_path <- paste0(path, 
                   file.path("/Input_Data", 
                             "Medicaid_managedcare_enrollment_report", 
                             "by_program_pop_from_report"))

# Get vector of years we have for MC enrollment from report PDFs
years <- setdiff(2006:2022, c(2007, 2012))

# Get vector of file names and use apply function to read in files in one line
file_names <- paste0(dir_path, "/data_", years, ".csv")

data_list <- lapply(file_names, read_csv)

names(data_list) <- years

# Define standardized name for different spellings of the same variables
column_mapping <- list(
  total_med_enr = c("total_med_enr", 
                    "total_medicaid_enrollment", 
                    "total_medicaid_enrollees"),
  comprehensive_mco_enr = c("comp_mco",
                            "comprehensive_mco_with_or_without_mltss"),
  medicaid_only_mco = c("medicaid_only_mco",
                        "med_only_mco")
)

# Apply the standardization
new_data_list <- lapply(data_list, function(df) {
  df <- standardize_columns(df, column_mapping)
  df %>%
    select(state, everything())
})

# Define territories in a character vector (for omission)
territories <- c("Virgin Islands", 
                 "Puerto Rico", 
                 "American Samoa",
                 "American Samoa3",
                 "Guam",
                 "Northern Mariana Islands",
                 "TOTALS")

# Omit territories in every sublist
clean_new_data_list <- lapply(new_data_list, function(df) {
  df %>%
    filter(!state %in% territories)
})

# Remove dupes in 2017 sublist
data_list[["2017"]] <- data_list[["2017"]] %>%
  distinct(state, .keep_all = TRUE)

# Clean up formatting with regex
clean_new_data_list <- lapply(clean_new_data_list, function(df) {
  df %>%
    mutate(state = gsub("[0-9]+$", "", state)) %>%
    
    # Remove commas and convert to numeric
    mutate(across(-state, ~ as.numeric(gsub(",", "", .)))) %>%
    
    # Replace "--" with NA
    mutate(across(-state, ~ ifelse(. == "--", NA, .))) 
})

# Add empty column for Comprehensive MCO for 2008-2011 for merging
clean_new_data_list <- lapply(names(clean_new_data_list), function(year) {
  
  df <- clean_new_data_list[[year]]
  
  # Add 'comprehensive_mco_enr' column for years 2008-2011
  if (as.numeric(year) %in% setdiff(2006:2011, 2007)) {
    df$comprehensive_mco_enr <- NA  # Add empty column
  }
  
  return(df)
})

# Create variable Any MCO Enrollment:
# Any MCO = Comprehensive MCO + PCCM + PCCM Entity (if exists) + MLTSS + Other
clean_new_data_list <- lapply(clean_new_data_list, function(df) {
  df %>%
    mutate(imputed_any_mco_enr = rowSums(select(.,
                                                comprehensive_mco_enr,
                                                pccm,
                                                dplyr::contains("pccm_entity"),
                                                any_of(c("mltss", "mltss_only")),
                                                other),
                                         na.rm = TRUE))
})

# Reassign list names
names(clean_new_data_list) <- years

# Add year variable for each sublist before collapsing
clean_new_data_list <- lapply(names(clean_new_data_list), function(year) {
  clean_new_data_list[[year]] %>%
    mutate(year = as.numeric(year))
})

# Rename lists
names(clean_new_data_list) <- years

# Select for variables we want for each sublist
# clean_new_data_list <- lapply(clean_new_data_list, function(df) {
#   df %>%
#     select(state, year, total_med_enr, comprehensive_mco_enr, any_mco_enr)
# })

# Collapse list of dataframes into one big dataframe
data_df <- do.call(bind_rows, clean_new_data_list)

# Standardize DC naming scheme
data_df$state <- gsub("(District of Columbia|Dist\\. of Columbia)", 
                      "District of Columbia", 
                      data_df$state, 
                      ignore.case = TRUE)

# Merge on enrollment data onto spending data
data <- left_join(temp, data_df, by = c("tag" = "state", "year"))

# Lower case variable names
names(data) <- tolower(names(data))

# Change column name for state
new_data <- data %>%
  rename(state = tag)

### ------ Cleaning panel column names for merging with Tamara's data ------ ###

# Read in Tamara's 1991-2005 data
data_91_05 <- readRDS(paste0(path, "/Temp/data_1991_2005.rds"))

# Lowercase the column names 
colnames(data_91_05) <- tolower(colnames(data_91_05))

# Some column renaming
data_91_05 <- data_91_05 %>%
  select(-c(`state code`, territory)) %>%
  rename(state = `state name`,
         year = `fiscal year`,
         total_med_enr = `medicaid enrollment, as of june 30`,
         managed_care_enrollment = `managed care enrollment, as of june 30`,
         pct_in_managed_care = `percent enrolled in managed care`)

start_col <- which(names(new_data) == "inpatient hospital - reg. payments")
end_col <- which(names(new_data) == "other care services")

# Add the "m-" tag in front of the first set of spending variables in my panel
# Rename balance, collections, total net expenditures to match Tamara's data
new_data <- new_data %>%
  rename_with(~ if_else(. %in% names(new_data)[start_col:end_col], 
                        paste0("m-", .),
                        .)) %>%
  rename("mb-balance" = "balance.x",
         "mc-collections" = "collections.x",
         "mt-total net expenditures" = "total net expenditures.x")

# Get the column numbers
start_col2 <- which(names(new_data) == "family planning")
end_col2 <- which(names(new_data) == "other financial participation")

# Add the "a-" tag in front of the first set of spending variables in my panel
new_data <- new_data %>%
  rename_with(~ if_else(. %in% names(new_data)[start_col2:end_col2], 
                        paste0("a-", .),
                        .)) %>%
  rename("ab-balance" = "balance.y",
         "ac-collections" = "collections.y",
         "at-total net expenditures" = "total net expenditures.y")

# Get the column numbers
start_col3 <- which(names(new_data) == "premiums - up to 150%: gross premiums paid")
end_col3 <- which(names(new_data) == "administration")

# Add the "a-" tag in front of the first set of spending variables in my panel
new_data <- new_data %>%
  rename_with(~ if_else(. %in% names(new_data)[start_col3:end_col3], 
                        paste0("s-", .),
                        .))

new_data <- new_data %>%
  rename("sc-collections" = "less: collections",
         "st-total net expenditures" = "total") %>%
  mutate(`sb-balance` = `st-total net expenditures` - `sc-collections`)

# Rename some columns for the "c-" variables
new_data <- new_data %>%
  rename("cb-balance" = "c-balance",
         "cc-collections" = "c-collections",
         "ct-total net expenditures" = "c-total net")

# Remove ".x" and ".y" from column names
names(new_data) <- gsub("\\.x$|\\.y$", "", names(new_data))

### -------------------- Merging panel and Tamara's data ------------------- ###

# Row bind my dataset and Tamara's data
# Should now have a state-year panel from 1991-2021
merged_data <- bind_rows(data_91_05, new_data)

merged_data <- merged_data %>%
  select(state, 
         year, 
         total_med_enr, 
         managed_care_enrollment,
         pct_in_managed_care,
         comprehensive_mco_enr,
         hio,
         commercial_mco,
         medicaid_only_mco,
         pccm,
         pccm_entity,
         bho,
         pihp,
         pahp,
         bho_pihp_andor_pahp,
         mltss,
         mltss_only,
         dental,
         transportation,
         imputed_any_mco_enr,
         everything()) %>%
  select(-notes) %>%
  arrange(state, year) %>%
  distinct(state, year, .keep_all = TRUE)


saveRDS(merged_data, file = paste0(path, "/Temp/merged_panel.rds"))

### ------------- Integrate Comp. MCO data from KFF Tracker ---------------- ###
# Read in files with MMC population by state
comp_path <- paste0(path, 
                    file.path("/Input_Data",
                              "Medicaid_managedcare_enrollment_report", 
                              "raw_comp_mco_data"))

# Get vector of years we need for Comp. MCO
years <- 2003:2011

# Get vector of file names and use apply function to read in files in one line
comp_file_names <- paste0(comp_path, "/raw_data_", years, ".csv")

# Get names of states
state_names <- c(state.name, "District of Columbia")

comp_mco_list <- lapply(comp_file_names, clean_and_split)

names(comp_mco_list) <- years

# Clean variables in each column, rename things, convert variable types
comp_mco_list <- lapply(comp_mco_list, function(df) {
  df <- df %>%
    select(-V3) %>%
    rename(state = V1,
           comprehensive_mco_enr = V2) %>%
    mutate(state = gsub('"', '', state),
           comprehensive_mco_enr = gsub("[^0-9.]", "", comprehensive_mco_enr),
           comprehensive_mco_enr = as.numeric(comprehensive_mco_enr))
})

# Collapse list of lists down to one dataframe
comp_mco_df <- do.call(bind_rows, comp_mco_list)

# Fill in missing values in merged panel
new_merged_data <- merged_data %>%
  left_join(comp_mco_df, by = c("state", "year"), suffix = c("", "_new")) %>%
  mutate(comprehensive_mco_enr = coalesce(comprehensive_mco_enr, 
                                          comprehensive_mco_enr_new)) %>%
  select(-comprehensive_mco_enr_new)

### ------- Integrating 2006-07 Managed Care Enrollment from Claims -------- ###

# Read in 2006 and 2007 enrollment data from claims
df_06 <- read_csv(paste0(path,
                             file.path("/Input_Data",
                                       "Medicaid_managedcare_enrollment_report",
                                       "claims",
                                       "enrollment_2006_v2.csv")))

df_07 <- read_csv(paste0(path,
                             file.path("/Input_Data",
                                       "Medicaid_managedcare_enrollment_report",
                                       "claims",
                                       "enrollment_2007_v2.csv")))

# Lowercase variable names
names(df_06) <- tolower(names(df_06))
names(df_07) <- tolower(names(df_07))

# Apply enrollment getter function to both 2006 and 2007
# Add year variable
mc_enr_06 <- enr_getter(df_06)
mc_enr_06$year <- 2006

mc_enr_07 <- enr_getter(df_07)
mc_enr_07$year <- 2007

# Row bind the two datasets
mc_enr_bind <- rbind(mc_enr_06, mc_enr_07)

# Vector of column names in desired order
reorder_cols <- c("state", "year", "total_med_enr", "managed_care_enrollment", 
                  "pct_in_managed_care", "comprehensive_mco_enr", "hio", 
                  "commercial_mco", "medicaid_only_mco", "pccm", "pccm_entity", 
                  "bho", "pihp", "pahp", "bho_pihp_andor_pahp", "mltss", 
                  "mltss_only", "dental", "transportation", 
                  "imputed_any_mco_enr")

# Coalesce 2006 and 2007 values
new_merged_data <- coalesce_join(
  main_data = new_merged_data,
  join_data = mc_enr_bind,
  by_cols = c("state", "year"),
  coalesce_cols = c("total_med_enr", "mltss", "dental", "pccm", "bho", "pace"),
  reorder_cols = reorder_cols
)

saveRDS(new_merged_data, file = paste0(path, "/Temp/new_merged_panel.rds"))

### ----------------- Adding 2012 claims data into panel ------------------- ###

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel.rds"))

enr_2012 <- read_csv(paste0(path,
                            file.path("/Input_Data",
                                      "Medicaid_managedcare_enrollment_report", 
                                      "claims",
                                      "enrollment_2012.csv")))

# Lower case variable names
names(enr_2012) <- tolower(names(enr_2012))

# Get enrollment figures from 2012 claims
mc_enr_12 <- enr_getter(enr_2012)
mc_enr_12$year <- 2012

# Coalesce 2012 values
new_merged_data <- coalesce_join(
  main_data = new_merged_data,
  join_data = mc_enr_12,
  by_cols = c("state", "year"),
  coalesce_cols = c("total_med_enr", "mltss", "dental", "pccm", "bho", "pace"),
  reorder_cols = reorder_cols
)

saveRDS(new_merged_data, file = paste0(path, "/Temp/new_merged_panel.rds"))

### ---------------- Get Total Medicaid and SCHIP Spending ----------------- ###

# Fill in total Medicaid and SCHIP spending for missing values
new_merged_data <- new_merged_data %>%
  mutate(`total medicaid (mt + at)` = ifelse(is.na(`total medicaid (mt + at)`),
                                             `mt-total net expenditures` + 
                                               `at-total net expenditures`,
                                             `total medicaid (mt + at)`)) %>%
  
  mutate(`total schip (ct + st)` = ifelse(is.na(`total schip (ct + st)`),
                                          `ct-total net expenditures` + 
                                            `st-total net expenditures`,
                                          `total schip (ct + st)`))

saveRDS(new_merged_data, file = paste0(path, "/Temp/new_merged_panel.rds"))

### -------------- Append enrollment numbers for 1991 - 1995 --------------- ###

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel.rds"))

data_91_95 <- read_dta(paste0(path, file.path("/Input_Data",
                                              "Medicaid_managedcare_enrollment_report",
                                              "external",
                                              "fymcdben.dta")))

# Extend state.abb and state.name with DC information
state.abb <- c(state.abb, "DC")
state.name <- c(state.name, "District of Columbia")

# Match state abbreviations to state names 
data_91_95$state <- state.name[match(data_91_95$state, state.abb)]

# Keep years 1991 - 1995, rename variable name for enrollment
data_91_95 <- data_91_95 %>%
  filter(year %in% 1991:1995) %>%
  rename(total_med_enr = fymcdben)

# Append total Medicaid enrollment from 1991-1995 to panel
new_merged_data <- coalesce_join(
  main_data = new_merged_data,
  join_data = data_91_95,
  by_cols = c("state", "year"),
  coalesce_cols = c("total_med_enr"),
  reorder_cols = reorder_cols
)

###--------------------- Adjust spending to 2023 dollars ------------------- ###

# Read in CPI to adjust for inflation
bls <- read_xlsx(paste0(path, "/Input_Data/Inflation_BLS/SeriesReport-20240326212745_31976f.xlsx"))

# Omit rows with more than 2 NAs per row
bls <- bls[rowSums(is.na(bls)) <= 2, ]

# Set first row as variable names 
names(bls) <- bls[1, ]

# Lower case variable names
names(bls) <- tolower(names(bls))

# Omit the first row after naming
bls <- bls[-1, ]

# Clean year character formatting
bls$year <- gsub("\\.0$", "", bls$year)


















