
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

d1 <- d1 %>%
  filter(state != "XX") %>%
  filter(year %in% 1991:1995)

# Extend state.abb and state.name with DC information
state.abb <- c(state.abb, "DC")
state.name <- c(state.name, "District of Columbia")

# Match state abbreviations to state names 
d1$state <- state.name[match(d1$state, state.abb)]

filled_data_91_05 <- data_91_05 %>%
  left_join(d1, by = c(c(`State name` = "state"), c(`Fiscal year` = "year"))) %>%
  mutate(`Medicaid enrollment, as of June 30` = coalesce(`Medicaid enrollment, as of June 30`,
                                                         fymcdben))
filled_data_91_05 <- filled_data_91_05 %>%
  select(-fymcdben)


### ------------------------------- Table 1 -------------------------------- ###

df_agg <- filled_data_91_05 %>%
  group_by(`Fiscal year`) %>%
  summarise(
    total_med_enr = sum(`Medicaid enrollment, as of June 30`),
    total_mmc_enr = sum(`Managed care enrollment, as of June 30`),
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











