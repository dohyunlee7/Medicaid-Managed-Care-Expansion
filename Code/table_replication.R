
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

d1 <- read_dta(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/external/fymcdben.dta"))

mandate <- read_dta(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/external/uimmc.dta"))

data <- readRDS(paste0(path, "/Temp/new_merged_panel.rds"))

data <- data %>%
  filter(year %in% 1991:2009) %>%
  filter(state != "Puerto Rico")



### ------------------------------- Table 1 -------------------------------- ###

df_agg <- data %>%
  group_by(year) %>%
  summarise(
    total_med_enr = sum(total_med_enr),
    total_mmc_enr = sum(managed_care_enrollment),
    pct_in_mmc = scales::percent(total_mmc_enr / total_med_enr, accuracy = 0.1)
  )

df_agg


### ------------------------------- Table 2 -------------------------------- ###

filled_data_91_05 <- filled_data_91_05 %>%
  filter(!`Fiscal year` %in% c(2004, 2005))

pct_03 <- filled_data_91_05 %>%
  filter(`Fiscal year` == 2003) %>%
  select(`State name`, `Fiscal year`, `Percent enrolled in managed care`)
  


### ------------------------------ Table 4 --------------------------------- ###











