library(dplyr)
library(haven)
library(datasets)
library(readxl)
library(tigris)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

d <- read_dta(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/external/cps_00002.dta.gz"))

d_medicaid <- d %>%
  filter(himcaidly == 2) %>%
  filter(year %in% 1991:2022)

d_medicaid_summary <- d_medicaid %>%
  mutate(
    age_group = case_when(
      age <= 14 ~ "children",
      age > 64 ~ "elderly",
      TRUE ~ "other"
    ),
    disabled = ifelse(disabwrk == 2, 1, 0)
  ) %>%
  group_by(year, statefip) %>%
  summarise(
    asec_total_medicaid_population = sum(asecwt, na.rm = TRUE),
    children_count = sum(asecwt[age_group == "children"], na.rm = TRUE),
    elderly_count = sum(asecwt[age_group == "elderly"], na.rm = TRUE),
    disabled_count = sum(asecwt[disabled == 1], na.rm = TRUE),
    children_prop = children_count / asec_total_medicaid_population,
    elderly_prop = elderly_count / asec_total_medicaid_population,
    disabled_prop = disabled_count / asec_total_medicaid_population,
    .groups = "drop"
  )

d_medicaid_summary$statefip <- sapply(d_medicaid_summary$statefip, 
                                      function(x) sprintf("%02d", x))

d_medicaid_summary$statefip <- as.character(d_medicaid_summary$statefip)

st_fips <- tigris::fips_codes

d_medicaid_summary$statefip <- st_fips$state_name[match(d_medicaid_summary$statefip,
                                                        st_fips$state_code)]

saveRDS(d_medicaid_summary, file = paste0(path, "/Temp/demographic_controls.rds"))






  
  
