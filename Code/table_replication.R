
#'
#' table_replication.R -- Replicate tables 2, 4, 5 in D&H
#'

library(dplyr)
library(kableExtra)
library(haven)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

# Read in Tamara's 1991-2005 data
data_91_05 <- readRDS(paste0(path, "/Temp/data_1991_2005.rds"))

# Read managed care enrollment from 1991-1995
mc91_05 <- read_dta(paste0(path, file.path("/Input_Data",
                                           "Medicaid_managedcare_enrollment_report",
                                           "external",
                                           "mc91_05.dta")))

d1 <- read_dta(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/external/fymcdben.dta"))

mandate <- read_dta(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/external/uimmc.dta"))

data <- readRDS(paste0(path, "/Temp/new_merged_panel.rds"))


data <- data %>%
  filter(state != "Puerto Rico")

data_agg <- data %>%
  group_by(year) %>%
  summarise(
    total_med_enr = sum(total_med_enr, na.rm = TRUE),
    total_mmc_enr = sum(managed_care_enrollment, na.rm = TRUE),
    pct_in_mmc = scales::percent(total_mmc_enr / total_med_enr, accuracy = 0.1)
  ) %>%
  mutate(total_med_enr_in_millions = round(total_med_enr / 1e6, 1)) %>%
  select(year, total_med_enr_in_millions, pct_in_mmc) 




### ------------------------------- Table 1 -------------------------------- ###

df_agg <- data %>%
  group_by(year) %>%
  summarise(
    total_med_enr = sum(total_med_enr, na.rm = TRUE),
    total_mmc_enr = sum(managed_care_enrollment, na.rm = TRUE),
    pct_in_mmc = scales::percent(total_mmc_enr / total_med_enr, accuracy = 0.1)
  )

df_agg


### ------------------------------- Table 2 -------------------------------- ###

pct_91 <- data %>%
  filter(year == 1991) %>%
  select(state, year, pct_in_managed_care)

pct_03 <- data %>%
  filter(year == 2003) %>%
  select(state, year, pct_in_managed_care)

pct_09 <- data %>%
  filter(year == 2009) %>%
  select(state, year, pct_in_managed_care)
  


### ------------------------------------------------------------------------ ###

mcd <- read_dta(paste0(path, file.path("/Input_Data",
                                       "Medicaid_managedcare_enrollment_report",
                                       "external",
                                       "mc91_05.dta")))

mcd <- mcd %>%
  filter(state != "XX")

data <- data %>%
  filter(state != "Puerto Rico")

data_agg <- data %>%
  group_by(year) %>%
  summarise(
    total_med_enr = sum(total_med_enr, na.rm = TRUE),
    total_mmc_enr = sum(managed_care_enrollment, na.rm = TRUE),
    pct_in_managed_care = total_mmc_enr / total_med_enr
  )






