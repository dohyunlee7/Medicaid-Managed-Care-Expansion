# Mediciad Capitation Rate
# Goal: Describe MMC enrollment at state level
# Input: Medicaid and CHIP enrollment report
# 2024.5

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

## load data
load("Temp/medicaid_enroll_total.Rda")
load("Temp/medicaid_enroll_report_sum.Rda")

##### 1. compare MMC enrollment and comprehensive MMC enrollment #####
## generate prop of mmc
medicaid_enroll_report <- medicaid_enroll_report %>% 
  mutate(prop_mmc = total_medicaid_managed_care/total_medicaid_enrollees,
         prop_comp_mco = total_medicaid_comp_mco/total_medicaid_enrollees)

## use NC as an example
eg_nc <- medicaid_enroll_report %>% 
  filter(state == "North Carolina")

eg_nc %>% select(-total_medicaid_enrollees, -total_medicaid_managed_care, -total_medicaid_comp_mco) %>%
  kbl(caption="Example: NC",
      format="latex",
      col.names = c("State", "Year", "Prop. of Managed Care Enrollment", 
                    "Prop. of Comprehensive MCO Enrollment"),
      align="c",booktabs = T)

##### 2. data-drive treatment (comprehensive MMC) #####
## treat == 1 when prop_comp_mco is 50% larger in one year
state_comp_mco <- medicaid_enroll_report %>% 
  select(state, year, prop_comp_mco) %>%
  spread(year, prop_comp_mco)

state_comp_mco <- state_comp_mco %>% 
  mutate(treat_2014 = ifelse((`2014`-`2013`)>=0.5, 1, 0),
         treat_2015 = ifelse((`2015`-`2014`)>=0.5, 1, 0),
         treat_2016 = ifelse((`2016`-`2015`)>=0.5, 1, 0),
         treat_2017 = ifelse((`2017`-`2016`)>=0.5, 1, 0),
         treat_2018 = ifelse((`2018`-`2017`)>=0.5, 1, 0),
         treat_2019 = ifelse((`2019`-`2018`)>=0.5, 1, 0),
         treat_2020 = ifelse((`2020`-`2019`)>=0.5, 1, 0),
         treat_2021 = ifelse((`2021`-`2020`)>=0.5, 1, 0)) 

state_comp_mco_treated <- state_comp_mco %>% 
  filter(treat_2014 ==1 | treat_2015 ==1 | treat_2016 ==1 | 
         treat_2017 ==1 | treat_2018 ==1 | treat_2019 ==1 |
         treat_2020 ==1 | treat_2021 ==1) %>% 
  mutate(category = "Treated")

state_comp_mco_nevertreated <- state_comp_mco %>% 
  filter(`2013` < 0.1, `2014` < 0.1, `2015` < 0.1,
         `2016` < 0.1, `2017` < 0.1, `2018` < 0.1,
         `2019` < 0.1, `2020` < 0.1, `2021` < 0.1) %>%
  mutate(category = "Never treated")

state_comp_mco_alwaystreated <- state_comp_mco %>% 
  filter(`2013` >=0.8, `2014` >=0.8, `2015` >=0.8,
         `2016` >=0.8, `2017` >=0.8, `2018` >=0.8,
         `2019` >=0.8, `2020` >=0.8, `2021` >=0.8) %>%
  mutate(category = "Always treated")

state_comp_mco_exp <- rbind(state_comp_mco_treated, state_comp_mco_nevertreated,
                            state_comp_mco_alwaystreated)

save(state_comp_mco_exp, file = "Temp/define_comp_mco_treatment.Rda")

##### 3. plot - data-drive treatment (comprehensive MMC) #####
state_comp_mco_exp <- state_comp_mco_exp %>% select(1:10, "category") %>%
  gather(key = "Year", value = "prop_comp_mco", 2:10)

png(filename = "Output/define_comp_mco_treatment.png", width=1800, height=900, res = 150)
ggplot(data = state_comp_mco_exp, 
       aes(x = Year, y = prop_comp_mco, group = state, color = state))+
  geom_point()+
  geom_line()+
  facet_wrap(~category)+
  geom_label_repel(data = subset(state_comp_mco_exp, Year == 2017 & category == "Treated"),
                   aes(label = state),
                   box.padding   = 0.5, 
                   # point.padding = 0.3,
                   segment.color = 'grey50',
                   segment.size  = 0.2)+
  labs(x = "Year", y = "Proportion of Comprehensive MCO Enrollment",
       color = "State")+
  theme(legend.position="bottom",
        axis.text=element_text(size=9),
        axis.title=element_text(size=12, face="bold"),
        plot.title = element_text(size = 10, face="bold"),
        strip.text = element_text(size=15)) 
dev.off()









