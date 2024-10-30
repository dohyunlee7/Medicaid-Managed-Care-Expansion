# ----------------------------------------------------
# CMS-64 report process
# 2023.2.16
# ----------------------------------------------------

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

options(scipen = 100)
theme_set(theme_clean() + theme(plot.background = element_blank()))
set.seed(2023)

setwd("D:/Groups/YSPH-HPM-Ndumele/Networks/Dohyun/medicaid_privatization_exp/Input_Data/CMS_64_report")


# 1. import data -------------------------------------------------------
multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, skip = 6, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

# path <- "financial-management-report-fy2012-13/FMR Net Expenditures FY12.xlsx"
# dat_12 <- multiplesheets(path)

path <- "financial-management-report-fy2012-13/FMR Net Expenditures FY13.xlsx"
dat_13 <- multiplesheets(path)

path <- "financial-management-report-fy2014/FMR Net Expenditures FY14.xlsx"
dat_14 <- multiplesheets(path)

path <- "financial-management-report-fy2015/FY 2015 NET EXPENDITURES.xlsx"
dat_15 <- multiplesheets(path)

path <- "financial-management-report-fy2016/FY 2016 FMR NET EXPENDITURES.xlsx"
dat_16 <- multiplesheets(path)

path <- "financial-management-report-fy2017/FY 2017 FMR NET EXPENDITURES.xlsx"
dat_17 <- multiplesheets(path)

path <- "financial-management-report-fy2018/FY 2018 FMR NET EXPENDITURES.xlsx"
dat_18 <- multiplesheets(path)

path <- "financial-management-report-fy2019/FY 2019 FMR NET EXPENDITURES.xlsx"
dat_19 <- multiplesheets(path)

path <- "financial-management-report-fy2020/FY 2020 FMR NET EXPENDITURES.xlsx"
dat_20 <- multiplesheets(path)

path <- "financial-management-report-fy2021/FY 2021 FMR NET EXPENDITURES.xlsx"
dat_21 <- multiplesheets(path)

path <- "financial-management-report-fy20/FY 2022 FMR NET EXPENDITURES.xlsx"
dat_22 <- multiplesheets(path)


col_name <- c("service", "total_computable", "federal_share", "federal_share_medicaid",
              "federal_share_arra", "federal_share_bipp", "state_share")
col_name2 <- c("service", "total_computable", "federal_share", "state_share")


dat_process1 <- function(dat_yr){
  dat_open <- data.frame(dat_yr[1])
  colnames(dat_open) <- col_name 
  dat_open$tag <- names(dat_yr)[1]
  dat_open <- dat_open %>% filter(!(grepl("Created On:", service, fixed = T)),
                                !is.na(service))

for(i in 2:which(names(dat_yr)=="MAP - National Totals")){
    dat <- data.frame(dat_yr[i])
    colnames(dat) <- col_name
    dat <- dat %>% mutate(tag = names(dat_yr)[i])
    dat <- dat %>% filter(!(grepl("Created On:", service, fixed = T)),
                                    !is.na(service))
    
    dat_open <- rbind(dat_open, dat)
}
return(dat_open)
}

dat_process2 <- function(dat_yr){
dat_adm <- data.frame(dat_yr[(which(names(dat_yr)=="MAP - National Totals")+1)])
colnames(dat_adm) <- col_name2
dat_adm$tag <- names(dat_yr)[(which(names(dat_yr)=="MAP - National Totals")+1)]
dat_adm <- dat_adm %>% filter(!(grepl("Created On:", service, fixed = T)),
                                !is.na(service))

for(i in (which(names(dat_yr)=="MAP - National Totals")+1):length(dat_yr)){
  dat <- data.frame(dat_yr[i])
  colnames(dat) <- col_name2
  dat <- dat %>% mutate(tag = names(dat_yr)[i])
  dat <- dat %>% filter(!(grepl("Created On:", service, fixed = T)),
                        !is.na(service))
  
  dat_adm <- rbind(dat_adm, dat)
}
return(dat_adm)
}

dat_13_map <- dat_process1(dat_13)
dat_13_adp <- dat_process2(dat_13)
  dat_13_map$year <- 2013
  dat_13_adp$year <- 2013
dat_14_map <- dat_process1(dat_14)
dat_14_adp <- dat_process2(dat_14)
  dat_14_map$year <- 2014
  dat_14_adp$year <- 2014
dat_15_map <- dat_process1(dat_15)
dat_15_adp <- dat_process2(dat_15)
  dat_15_map$year <- 2015
  dat_15_adp$year <- 2015
dat_16_map <- dat_process1(dat_16)
dat_16_adp <- dat_process2(dat_16)
  dat_16_map$year <- 2016
  dat_16_adp$year <- 2016
dat_17_map <- dat_process1(dat_17)
dat_17_adp <- dat_process2(dat_17)
  dat_17_map$year <- 2017
  dat_17_adp$year <- 2017
dat_18_map <- dat_process1(dat_18)
dat_18_adp <- dat_process2(dat_18)
  dat_18_map$year <- 2018
  dat_18_adp$year <- 2018
dat_19_map <- dat_process1(dat_19)
dat_19_adp <- dat_process2(dat_19)
  dat_19_map$year <- 2019
  dat_19_adp$year <- 2019
dat_20_map <- dat_process1(dat_20)
dat_20_adp <- dat_process2(dat_20)
  dat_20_map$year <- 2020
  dat_20_adp$year <- 2020
dat_21_map <- dat_process1(dat_21)
dat_21_adp <- dat_process2(dat_21)
  dat_21_map$year <- 2021
  dat_21_adp$year <- 2021
dat_22_map <- dat_process1(dat_22)
dat_22_adp <- dat_process2(dat_22)
  dat_22_map$year <- 2022
  dat_22_adp$year <- 2022
  

# 2. merge ----------------------------------------------------------

dat_map <- rbind(dat_13_map,dat_14_map,dat_15_map,
                dat_16_map,dat_17_map,dat_18_map,dat_19_map,dat_20_map,
                dat_21_map, dat_22_map)
  
dat_adp <- rbind(dat_13_adp,dat_14_adp,dat_15_adp,
                dat_16_adp,dat_17_adp,dat_18_adp,dat_19_adp,dat_20_adp,
                dat_21_adp, dat_22_adp)

# dat_map <- dat_map %>% gather(key="term", value ="value", 2:7)
# dat_adp <- dat_adp %>% gather(key="term", value ="value", 2:4)

## drop notes
dat_map <- dat_map %>% filter(!grepl("NOTES:", service, fixed = T))
dat_adp <- dat_adp %>% filter(!grepl("NOTES:", service, fixed = T))
dat_adp <- dat_adp %>% 
  filter(!(grepl("CMS has not completed its review", service, fixed = T)))

## drop a few exemptions
#1. service with **
dat_map <- dat_map %>% 
  filter(!(grepl("MCO PIHP - Evaluation and Management", service, fixed = T)),
         !(grepl("MCO PAHP - Evaluation and Management", service, fixed = T)))

t <- dat_map %>% filter(grepl("Medicaid - MCO  **", service, fixed = T))
  # MAP - Oklahoma, 2015
t <- dat_map %>% filter(grepl("MCO PIHP - Evaluation and Management  **", service, fixed = T))
  # MAP - Iowa, 2015; MAP - National Totals, 2015
t <- dat_map %>% filter(grepl("MCO PAHP - Evaluation and Management   **", service, fixed = T))
  # MAP - Oklahoma, 2015
t <- dat_map %>% filter(grepl("MCO PAHP - Vaccine codes  **", service, fixed = T))
  # MAP - Oklahoma, 2015
t <- dat_map %>% filter(grepl("MCO PAHP - Evaluation and Management **", service, fixed = T))
  # MAP - National Totals, 2015

dat_map <- dat_map %>% filter(!(grepl("Medicaid - MCO  **", service, fixed = T)),
                   !(grepl("MCO PIHP - Evaluation and Management  **", service, fixed = T)),
                   !(grepl("MCO PAHP - Evaluation and Management   **", service, fixed = T)),
                   !(grepl("MCO PAHP - Vaccine codes  **", service, fixed = T)),
                   !(grepl("MCO PAHP - Evaluation and Management **", service, fixed = T)),
                   !(grepl("**", service, fixed = T)))

dat_map <- dat_map %>% filter(tag != "MAP - National Totals")


## which variable as the outcome?
## MAP
# dat_map_mco <- dat_map %>% filter(grepl("MCO", service, fixed = T))
dat_map_mco <- dat_map %>% gather(key="term", value ="value", 2:7)
    
list_dat_map_service <- unique(dat_map_mco$service)
tab_map_mco <- dat_map_mco %>% group_by(tag, term, year) %>% count()
map_wide <- data.frame(matrix(ncol = (3+length(list_dat_map_service)), nrow = nrow(tab_map_mco)))
colnames(map_wide)[1:3] <- c("tag", "term", "year")
colnames(map_wide)[4: (3+length(list_dat_map_service))] <- list_dat_map_service

for(i in 1:nrow(tab_map_mco)){
  map_wide[i, "term"] <- tab_map_mco[i, "term"]
  map_wide[i, "tag"] <- tab_map_mco[i, "tag"]
  map_wide[i, "year"] <- tab_map_mco[i, "year"]
  
  sub <- dat_map_mco %>% filter(tag ==  map_wide[i, "tag"],
                                term == map_wide[i, "term"],
                                year ==  map_wide[i, "year"])
  var_list <- unique(sub$service)
  
  for(m in 1:length(var_list)){
    map_wide[[var_list[m]]][i] <- 
      ifelse(!is.na(subset(sub, sub$service == var_list[m])$value),
             subset(sub, sub$service == var_list[m])$value,
             NA)
  }
}

## ADP
dat_adp <- dat_adp %>% distinct()
dat_adp_mco <- dat_adp %>% filter(!(grepl("NOTES", service, fixed = T)),
                              !(grepl("CMS has not", service, fixed = T)))
dat_adp_mco <- dat_adp_mco %>% gather(key="term", value ="value", 2:4)

list_dat_adp_service <- unique(dat_adp$service)
tab_adp_mco <- dat_adp_mco %>% group_by(tag, term, year) %>% count()
adp_wide <- data.frame(matrix(ncol = (3+length(list_dat_adp_service)), nrow = nrow(tab_adp_mco)))
colnames(adp_wide)[1:3] <- c("tag", "term", "year")
colnames(adp_wide)[4: (3+length(list_dat_adp_service))] <- list_dat_adp_service

for(i in 1:nrow(tab_adp_mco)){
  adp_wide[i, "term"] <- tab_adp_mco[i, "term"]
  adp_wide[i, "tag"] <- tab_adp_mco[i, "tag"]
  adp_wide[i, "year"] <- tab_adp_mco[i, "year"]
  
  sub <- dat_adp_mco %>% filter(tag == adp_wide[i, "tag"],
                                term == adp_wide[i, "term"],
                                year == adp_wide[i, "year"])
  var_list <- unique(sub$service)
  
  for(m in 1:length(var_list)){   
    adp_wide[[var_list[m]]][i] <- 
      ifelse(!is.na(subset(sub, sub$service ==  var_list[m])$value),
             subset(sub, sub$service ==  var_list[m])$value,
             NA)
  }
}

save(map_wide, file = "C:/Users/ruochens/Dropbox (Penn)/Research project/MCO/Medicaid_privatization_exp/Temp/map_wide_13_22.Rda")
save(adp_wide, file = "C:/Users/ruochens/Dropbox (Penn)/Research project/MCO/Medicaid_privatization_exp/Temp/adp_wide_13_22.Rda")

load("Temp/map_wide_13_22.Rda")
load("Temp/map_wide.Rda")
load("Temp/medicaid_enroll_report_sum.Rda")
