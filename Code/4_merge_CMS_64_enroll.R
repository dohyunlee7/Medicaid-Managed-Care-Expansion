# Goal: Merge data
# Data input: (1) CMS 64 report (13-20) (2) MCO enrollment
# 2024.1

# 0. load packages ----------------------------------------------------
rm(list = ls())

packages <- c("ggplot2", "estimatr", "readxl", "readr", "dplyr",
              "stringr", "ivreg", "tidyverse", "texreg", "ggrepel", 
              "lfe", "fastDummies", "ggpubr", "RColorBrewer", "ggthemes",
              "stargazer", "grid", "foreign", "kableExtra", "lubridate",
              "haven", "fuzzyjoin", "stringi", "zoo", "viridis")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

setwd("D:/Groups/YSPH-HPM-Ndumele/Networks/Dohyun/medicaid_privatization_exp")

load("Temp/map_wide_13_22.Rda")
load("Temp/adp_wide_13_22.Rda")
load("Temp/medicaid_enroll_report_sum.Rda")

# 1. data prep ----------------------------------------------------------------
colnames(map_wide)
colnames(adp_wide)

## select outcome vars
map_var_list <- c("`MCO - National Agreement`", "`MCO - State Sidebar Agreement`",
                  "`Medicaid - MCO`", "`Medicaid MCO - Evaluation and Management`",
                  "`Medicaid MCO - Vaccine codes`", "`Medicaid MCO - Community First Choice`",
                  "`Medicaid MCO - Preventive Services Grade A OR B, ACIP Vaccines and their Admin`",
                  "Inpatient Hospital - Reg. Payments", "Inpatient Hospital - DSH",
                  "Inpatient Hospital - Sup. Payments", "Inpatient Hospital - GME Payments",
                  "Mental Health Facility Services - Reg. Payments", "Mental Health Facility - DSH",
                  "Nursing Facility Services - Reg. Payments", "Nursing Facility Services - Sup. Payments",
                  "Intermediate Care Facility - Public")

map_wide <- map_wide %>% 
  mutate(inpatient = `Inpatient Hospital - Reg. Payments` + `Inpatient Hospital - DSH`+
                     `Inpatient Hospital - Sup. Payments` + `Inpatient Hospital - GME Payments`,
         mental_health = `Mental Health Facility Services - Reg. Payments` + `Mental Health Facility - DSH`,
         nursing = `Nursing Facility Services - Reg. Payments` + `Nursing Facility Services - Sup. Payments`,
         intermediate = `Intermediate Care Facility - Public` + `Intermediate Care - Private` + `Intermediate Care Facility - Mentally Retarded: Supplemental Payments`,
         physician_surgical = `Physician & Surgical Services - Reg. Payments` + `Physician & Surgical Services - Sup. Payments` +
                              `Physician & Surgical Services - Evaluation and Management` + `Physician & Surgical Services - Vaccine codes`,
         outpatient = `Outpatient Hospital Services - Reg. Payments` + `Outpatient Hospital Services - Sup. Payments`,
         drug = `Prescribed Drugs`)

## clean state variable 
map_wide <- map_wide %>% mutate(state_f = gsub("MAP - ", "", tag))
unique(map_wide$state)

## code treatment variable
map_wide <- map_wide %>% 
  mutate(state = case_when(
    state_f == "Alabama" ~ "AL",
    state_f == "Alaska" ~ "AK",
    state_f == "Arizona" ~ "AZ",
    state_f == "Arkansas" ~ "AR",
    state_f == "California" ~ "CA",
    state_f == "Colorado" ~ "CO",
    state_f == "Connecticut" ~ "CT",
    state_f == "Delaware" ~ "DE",
    state_f == "Florida" ~ "FL",
    state_f == "Georgia" ~ "GA",
    state_f == "Hawaii" ~ "HI",
    state_f == "Idaho" ~ "ID",
    state_f == "Illinois" ~ "IL",
    state_f == "Indiana" ~ "IN",
    state_f == "Iowa" ~ "IA",
    state_f == "Kansas" ~ "KS",
    state_f == "Kentucky" ~ "KY",
    state_f == "Louisiana" ~ "LA",
    state_f == "Maine" ~ "ME",
    state_f == "Maryland" ~ "MD",
    state_f == "Massachusetts" ~ "MA",
    state_f == "Michigan" ~ "MI",
    state_f == "Minnesota" ~ "MN",
    state_f == "Mississippi" ~ "MS",
    state_f == "Missouri" ~ "MO",
    state_f == "Montana" ~ "MT",
    state_f == "Nebraska" ~ "NE",
    state_f == "Nevada" ~ "NV",
    state_f == "New Hampshire" ~ "NH",
    state_f == "New Jersey" ~ "NJ",
    state_f == "New Mexico" ~ "NM",
    state_f == "New York" ~ "NY",
    state_f == "North Carolina" ~ "NC",
    state_f == "North Dakota" ~ "ND",
    state_f == "Ohio" ~ "OH",
    state_f == "Oklahoma" ~ "OK",
    state_f == "Oregon" ~ "OR",
    state_f == "Pennsylvania" ~ "PA",
    state_f == "Rhode Island" ~ "RI",
    state_f == "South Carolina" ~ "SC",
    state_f == "South Dakota" ~ "SD",
    state_f == "Tennessee" ~ "TN",
    state_f == "Texas" ~ "TX",
    state_f == "Utah" ~ "UT",
    state_f == "Vermont" ~ "VT",
    state_f == "Virginia" ~ "VA",
    state_f == "Washington" ~ "WA",
    state_f == "West Virginia" ~ "WV",
    state_f == "Wisconsin" ~ "WI",
    state_f == "Wyoming" ~ "WY"
  )) %>% 
  filter(!is.na(state)) 
  ## Amer. Samoa, DC, Guam, Mariana Islands, Puerto Rico and Virgin Islands are dropped

## medicaid spending per capita
colnames(medicaid_enroll_report)[1] <- "state_f"

map_wide <- left_join(map_wide, medicaid_enroll_report, by = c("state_f", "year"))

map_wide <- map_wide %>% mutate(medicaid_mco_capita = `Medicaid - MCO`/total_medicaid_comp_mco)

capital_full <- map_wide %>% filter(!is.na(medicaid_mco_capita))

## ACA Medicaid expansion
map_wide <- map_wide %>% 
  mutate(state_expansion = case_when(
    state_f == "Alabama" ~ "No",
    state_f == "Alaska" ~ "Yes",
    state_f == "Arizona" ~ "Yes",
    state_f == "Arkansas" ~ "Yes",
    state_f == "California" ~ "Yes",
    state_f == "Colorado" ~ "Yes",
    state_f == "Connecticut" ~ "Yes",
    state_f == "Delaware" ~ "Yes",
    state_f == "Florida" ~ "No",
    state_f == "Georgia" ~ "No",
    state_f == "Hawaii" ~ "Yes",
    state_f == "Idaho" ~ "Yes",
    state_f == "Illinois" ~ "Yes",
    state_f == "Indiana" ~ "Yes",
    state_f == "Iowa" ~ "Yes",
    state_f == "Kansas" ~ "No",
    state_f == "Kentucky" ~ "Yes",
    state_f == "Louisiana" ~ "Yes",
    state_f == "Maine" ~ "Yes",
    state_f == "Maryland" ~ "Yes",
    state_f == "Massachusetts" ~ "Yes",
    state_f == "Michigan" ~ "Yes",
    state_f == "Minnesota" ~ "Yes",
    state_f == "Mississippi" ~ "No",
    state_f == "Missouri" ~ "Yes",
    state_f == "Montana" ~ "Yes",
    state_f == "Nebraska" ~ "Yes",
    state_f == "Nevada" ~ "Yes",
    state_f == "New Hampshire" ~ "Yes",
    state_f == "New Jersey" ~ "Yes",
    state_f == "New Mexico" ~ "Yes",
    state_f == "New York" ~ "Yes",
    state_f == "North Carolina" ~ "No",
    state_f == "North Dakota" ~ "Yes",
    state_f == "Ohio" ~ "Yes",
    state_f == "Oklahoma" ~ "Yes",
    state_f == "Oregon" ~ "Yes",
    state_f == "Pennsylvania" ~ "Yes",
    state_f == "Rhode Island" ~ "Yes",
    state_f == "South Carolina" ~ "No",
    state_f == "South Dakota" ~ "Yes",
    state_f == "Tennessee" ~ "No",
    state_f == "Texas" ~ "No",
    state_f == "Utah" ~ "Yes",
    state_f == "Vermont" ~ "Yes",
    state_f == "Virginia" ~ "Yes",
    state_f == "Washington" ~ "Yes",
    state_f == "West Virginia" ~ "Yes",
    state_f == "Wisconsin" ~ "No",
    state_f == "Wyoming" ~ "No"
))

map_wide$`Medicaid - MCO`[map_wide$state_f == "Oklahoma" & map_wide$`Medicaid - MCO` < 0] <- NA
map_wide$`Medicaid - MCO`[map_wide$`Medicaid - MCO` < 0] <- NA # accounting issue


# 2. save data ---------------------
save(map_wide, file = "Temp/map_wide.Rda")

