
#'
#' table_replication.R -- Replicate tables in D&H
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






### ------------------------------- Table 1 -------------------------------- ###

data_agg <- data %>%
  group_by(year) %>%
  summarise(
    total_med_enr = sum(total_med_enr, na.rm = TRUE),
    total_mmc_enr = sum(managed_care_enrollment, na.rm = TRUE),
    pct_in_mmc = scales::percent(total_mmc_enr / total_med_enr, accuracy = 0.1)
  ) %>%
  mutate(total_med_enr_in_millions = round(total_med_enr / 1e6, 1)) %>%
  select(year, total_med_enr_in_millions, pct_in_mmc) 


### ------------------------------- Table 2 -------------------------------- ###

pct_91 <- data %>%
  filter(year == 1991) %>%
  select(state, pct_in_managed_care) %>%
  rename(`1991` = pct_in_managed_care)

pct_03 <- data %>%
  filter(year == 2003) %>%
  select(state, pct_in_managed_care) %>%
  rename(`2003` = pct_in_managed_care)

pct_09 <- data %>%
  filter(year == 2009) %>%
  select(state, pct_in_managed_care) %>%
  rename(`2009` = pct_in_managed_care)

tbl2 <- left_join(pct_91, pct_03, by = "state")
tbl2 <- left_join(tbl2, pct_09, by = "state")

tbl2 <- tbl2 %>%
  mutate(across(c(`1991`, `2003`, `2009`), ~ scales::percent(.x, accuracy = 0.1)))

tbl2 <- tbl2 %>%
  mutate(across(everything(), ~ replace_na(.x, "0.0%")))

### ----------------------------- Table 4 ---------------------------------- ###
tbl4_data <- data %>%
  filter(year %in% c(1991, 2003, 2009)) %>%
  select(state, year, total_med_enr, `total medicaid (mt + at)`)


library(fredr)

# Get API Key from Fed. Reserve
fredr_set_key("8664fb88934dc0a2a037b8c6b153e4e5")

# Fetch annual CPI data for 'CPIAUCSL' 
# (Consumer Price Index for All Urban Consumers, All Items)
cpi_data <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2023-12-31"),
  frequency = "a"
)

# Calculate year-over-year percentage change (inflation rate) in CPI
cpi_data <- cpi_data %>%
  arrange(date) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  rename(cpi = value) %>%
  select(year, cpi)


# Join annual CPI to panel
data_adj <- left_join(tbl4_data, cpi_data, by = "year")

cpi_2010 <- cpi_data[cpi_data$year == 2010, ]$cpi

data_adj <- data_adj %>%
  mutate(adj_factor = cpi_2010 / cpi)

data_adj <- data_adj %>%
  mutate(across(28:456, ~. * adj_factor))

data_adj <- data_adj %>%
  mutate(spending_per_recip = `total medicaid (mt + at)` / total_med_enr)

tbl4 <- data_adj %>%
  select(state, year, spending_per_recip)

tbl4 <- tbl4 %>%
  pivot_wider(
    names_from = year,
    values_from = spending_per_recip
  )

tbl4 <- tbl4 %>%
  mutate(across(c(`1991`, `2003`, `2009`), ~ scales::dollar(round(.x))))

### ------------------------------------------------------------------------ ###







