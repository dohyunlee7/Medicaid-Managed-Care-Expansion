# Mediciad Capitation Rate
# Goal: clean Medicaid enrollment
# Input: Medicaid and CHIP enrollment report
# 2024.1

##### 0. load packages #####
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

options(scipen = 100)
theme_set(theme_clean() + theme(plot.background = element_blank()))
set.seed(2023)

## path
setwd("D:/Groups/YSPH-HPM-Ndumele/Networks/Dohyun/medicaid_privatization_exp")

##### 1. clean total Medicaid enroll data #####
## load data
medicaid_enroll_report <- read_csv("Input_Data/Medicaid_CHIP_enroll/data.csv") ## CMS total Medicaid enrollment

## process
medicaid_enroll_total <- medicaid_enroll_report %>% 
  filter(final_report=="Y") %>%
  mutate(year = substr(report_date,7,10),
         month = substr(report_date,1,2))

medicaid_enroll_total <- medicaid_enroll_total %>% 
  group_by(state_abbreviation, state_name, year) %>% 
  summarise(avg_total_medicaid_and_chip_enrollment = mean(total_medicaid_and_chip_enrollment)) %>% 
  mutate(year = as.numeric(year))

##### 2. Medicaid managed care data (directly from KFF) #####
## load data
# raw_data_2021 <- read_csv("raw_data_2021.csv", skip = 2)
# raw_data_2020 <- read_csv("raw_data_2020.csv", skip = 2)
# raw_data_2019 <- read_csv("raw_data_2019.csv", skip = 2)
# raw_data_2018 <- read_csv("raw_data_2018.csv", skip = 2)
# raw_data_2017 <- read_csv("raw_data_2017.csv", skip = 2)
# raw_data_2016 <- read_csv("raw_data_2016.csv", skip = 2)
# raw_data_2015 <- read_csv("raw_data_2015.csv", skip = 2)
# raw_data_2014 <- read_csv("raw_data_2014.csv", skip = 2)
# raw_data_2013 <- read_csv("raw_data_2013.csv", skip = 2)
# 
# ## process
# raw_data_2021 <- raw_data_2021[1:52,1:3]
# raw_data_2021 <- raw_data_2021 %>% mutate(year = 2021)
# raw_data_2020 <- raw_data_2020[1:52,1:3]
# raw_data_2020 <- raw_data_2020 %>% mutate(year = 2020)
# raw_data_2019 <- raw_data_2019[1:52,1:3]
# raw_data_2019 <- raw_data_2019 %>% mutate(year = 2019)
# raw_data_2018 <- raw_data_2018[1:52,1:3]
# raw_data_2018 <- raw_data_2018 %>% mutate(year = 2018)
# raw_data_2017 <- raw_data_2017[1:52,1:3]
# raw_data_2017 <- raw_data_2017 %>% mutate(year = 2017)
# raw_data_2016 <- raw_data_2016[1:52,1:3]
# raw_data_2016 <- raw_data_2016 %>% mutate(year = 2016)
# raw_data_2015 <- raw_data_2015[1:52,1:3]
# raw_data_2015 <- raw_data_2015 %>% mutate(year = 2015)
# raw_data_2014 <- raw_data_2014[1:52,1:3]
# raw_data_2014 <- raw_data_2014 %>% mutate(year = 2014)
# raw_data_2013 <- raw_data_2013[1:52,1:3]
# raw_data_2013 <- raw_data_2013 %>% mutate(year = 2013)
# 
# medicaid_enroll_cp_mco <- rbind(raw_data_2013, raw_data_2014, raw_data_2015, 
#                          raw_data_2016, raw_data_2017,
#                          raw_data_2018,raw_data_2019,raw_data_2020,
#                          raw_data_2021)

##### 3. Medicaid managed care TOTAL enrollment from report #####
## Goal: total Medicaid enrollee, ANY managed care enrollee, comprehensive MCO

## digitized data since 2016
data_2021 <- read_csv("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2021.csv")
data_2020 <- read_csv("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2020.csv")
data_2019 <- read_csv("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2019.csv")
data_2018 <- read_csv("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2018.csv")
data_2017 <- read_csv("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2017.csv")
data_2016 <- read_csv("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2016.csv")

for(i in 2016:2021){
  dat <- get(paste("data_", i, sep = ""))
  dat <- dat %>% 
    select(state, total_medicaid_enrollees, 
           total_medicaid_enrollment_in_any_type_of_managed_care,
           medicaid_enrollment_in_comprehensive_managed_care, year)
  colnames(dat) <- c("state", "total_medicaid_enrollees", 
                     "total_medicaid_managed_care", 
                     "total_medicaid_comp_mco", "year")
  assign(paste("data_", i, sep = ""), dat)
}

## before 2016: from pdf reports -- need extra cleaning
data_2015 <- read_excel("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2015.xlsx")
data_2014 <- read_excel("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2014.xlsx")
data_2013 <- read_excel("Input_Data/Medicaid_managedcare_enrollment_report/program_summary_from_report/data_2013.xlsx")

data_2015 <- data_2015 %>% 
  select(`State or Territory`, `Total Medicaid Enrollees1`, 
         `Total Medicaid Enrollment in Any Type of Managed Care2`,
         `Medicaid Enrollment in Comprehensive Managed Care3`) %>% 
  mutate(year = 2015)
colnames(data_2015) <- c("state", "total_medicaid_enrollees", 
                         "total_medicaid_managed_care", 
                         "total_medicaid_comp_mco", "year")

data_2014 <- data_2014 %>% 
  select(1, 6, 28, 50) %>% 
  mutate(year = 2014)
colnames(data_2014) <- c("state", "total_medicaid_enrollees", 
                         "total_medicaid_managed_care", 
                         "total_medicaid_comp_mco", "year")

data_2013 <- data_2013[-1, ] %>%
  select(1, 7, 21, 49) %>% 
  mutate(year = 2013)
colnames(data_2013) <- c("state", "total_medicaid_enrollees", 
                         "total_medicaid_managed_care", 
                         "total_medicaid_comp_mco", "year")

medicaid_enroll_report <- rbind(data_2013, data_2014, data_2015,
                                data_2016, data_2017, data_2018,
                                data_2019, data_2020, data_2021)
table(medicaid_enroll_report$year)

## clean
medicaid_enroll_report$total_medicaid_enrollees <- gsub(",", "", medicaid_enroll_report$total_medicaid_enrollees)
medicaid_enroll_report$total_medicaid_managed_care <- gsub(",", "", medicaid_enroll_report$total_medicaid_managed_care)
medicaid_enroll_report$total_medicaid_comp_mco <- gsub(",", "", medicaid_enroll_report$total_medicaid_comp_mco)

medicaid_enroll_report$total_medicaid_enrollees <- as.numeric(medicaid_enroll_report$total_medicaid_enrollees)
medicaid_enroll_report$total_medicaid_managed_care <- as.numeric(medicaid_enroll_report$total_medicaid_managed_care)
medicaid_enroll_report$total_medicaid_comp_mco <- as.numeric(medicaid_enroll_report$total_medicaid_comp_mco)

for(i in 0:9){
  medicaid_enroll_report$state <- gsub(i, "", medicaid_enroll_report$state)
}

medicaid_enroll_report <- medicaid_enroll_report %>% 
  filter(!(state %in% c("American Samoa", "Guam", "Northern Mariana Islands",
                        "Puerto Rico", "TOTALS", "Virgin Islands", "District of Columbia")))


##### 4. save data #####
save(medicaid_enroll_total, file = "Temp/medicaid_enroll_total.Rda")
save(medicaid_enroll_report, file = "Temp/medicaid_enroll_report_sum.Rda")



