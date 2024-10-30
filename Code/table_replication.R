
#'
#' table_replication.R -- Replicate tables 2, 4, 5 in D&H
#'

library(dplyr)
library(kableExtra)
library(tidyverse)



### ------------------------------- Table 2 -------------------------------- ###
new_merged_data <- readRDS(file = paste0(path, "/Temp/new_merged_panel.rds"))

d1 <- new_merged_data %>%
  mutate(pct_in_comp_mco = comprehensive_mco_enr / total_med_enr)

filtered_d1 <- d1 %>%
  filter(year %in% c(2003, 2009, 2015, 2021)) %>%
  distinct(state, year, pct_in_comp_mco)

tbl1_data <- filtered_d1 %>%
  pivot_wider(names_from = year, 
              values_from = pct_in_comp_mco)

tbl1_data <- tbl1_data %>%
  mutate(across(starts_with("20"), ~ scales::percent(., accuracy = 0.1)))

  
tbl1_data %>%
  knitr::kable("latex", 
               col.names = c("state", "2003", "2009", "2015", "2021"),
               caption = "Comprehensive MCO Enrollment Percentage by State") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%         # Bold the "State" column for emphasis
  add_header_above(c(" " = 1, "Comprehensive MCO Enrollment Percentage by Year" = 4)) # Add header over years

### ------------------------------ Table 4 --------------------------------- ###

d2 <- new_merged_data %>%
  mutate(spending_per_recip = `total medicaid (mt + at)` / total_med_enr)
  
filtered_d2 <- d2 %>%
  filter(year %in% c(1997, 2003, 2009, 2015, 2021)) %>%
  distinct(state, year, spending_per_recip) %>%
  mutate(spending_per_recip = round(spending_per_recip, 0))
  
tbl2_data <- filtered_d2 %>%
  pivot_wider(names_from = year, 
              values_from = spending_per_recip)
  
tbl2_data <- tbl2_data %>%
  mutate(across(matches("^(19|20)"), ~ scales::dollar(.)))

tbl2_data %>%
  knitr::kable("latex", 
               col.names = c("state", "1997", "2003", "2009", "2015", "2021"),
               caption = "State Medicaid Spending per Recipient (Unadjusted)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE)










