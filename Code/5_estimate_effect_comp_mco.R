# Mediciad Capitation Rate
# Goal: Estimate effect of privatization (comp MCO)
# Input: Medicaid and CHIP enrollment report; CMS 64 report
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
load("Temp/define_comp_mco_treatment.Rda")
load("Temp/map_wide.Rda")
load("Temp/medicaid_enroll_report_sum.Rda")

##### 1. merge treatment in CMS 64 report #####
## adjust inflation rate
inflation <- read_excel("Input_Data/Inflation_BLS/SeriesReport-20240326212745_31976f.xlsx", skip = 10)
inflation <- inflation %>% select(Year, Annual) %>% 
  filter(Year >= 2013, Year <= 2022)

i_2014 <- inflation$Annual[inflation$Year==2014]*0.01
i_2015 <- inflation$Annual[inflation$Year==2015]*0.01
i_2016 <- inflation$Annual[inflation$Year==2016]*0.01
i_2017 <- inflation$Annual[inflation$Year==2017]*0.01
i_2018 <- inflation$Annual[inflation$Year==2018]*0.01
i_2019 <- inflation$Annual[inflation$Year==2019]*0.01
i_2020 <- inflation$Annual[inflation$Year==2020]*0.01
i_2021 <- inflation$Annual[inflation$Year==2021]*0.01
i_2022 <- inflation$Annual[inflation$Year==2022]*0.01

## all adjusted to 2013 value
map_wide  <- map_wide %>% 
  mutate(inflated_Medicaid_MCO = case_when(
    year == 2013 ~ `Medicaid - MCO`,
    year == 2014 ~ `Medicaid - MCO`/(1+i_2014),
    year == 2015 ~ `Medicaid - MCO`/(1+i_2014+i_2015),
    year == 2016 ~ `Medicaid - MCO`/(1+i_2014+i_2015+i_2016),
    year == 2017 ~ `Medicaid - MCO`/(1+i_2014+i_2015+i_2016+i_2017),
    year == 2018 ~ `Medicaid - MCO`/(1+i_2014+i_2015+i_2016+i_2017+i_2018),
    year == 2019 ~ `Medicaid - MCO`/(1+i_2014+i_2015+i_2016+i_2017+i_2018+i_2019),
    year == 2020 ~ `Medicaid - MCO`/(1+i_2014+i_2015+i_2016+i_2017+i_2018+i_2019+i_2020),
    year == 2021 ~ `Medicaid - MCO`/(1+i_2014+i_2015+i_2016+i_2017+i_2018+i_2019+i_2020+i_2021),
    year == 2022 ~ `Medicaid - MCO`/(1+i_2014+i_2015+i_2016+i_2017+i_2018+i_2019+i_2020+i_2021+i_2022)
  ),
  `Medicaid - MCO` = inflated_Medicaid_MCO) %>% 
  select(-inflated_Medicaid_MCO)

## select variable in spending data 
map_wide <- map_wide %>% 
  filter(term == "total_computable") %>%
  select(state, state_f, tag, term, year, 
         Balance, `Total Net Expenditures`, `Medicaid - MCO`,
         inpatient, mental_health, nursing, intermediate, physician_surgical, outpatient, drug,
         medicaid_mco_capita) ## medicaid_mco_capita is the comp mco spending
colnames(map_wide)[which(colnames(map_wide) == "state")] <- "state_brief"
colnames(map_wide)[which(colnames(map_wide) == "state_f")] <- "state"

## treatment def
treated_state <- unique(state_comp_mco_exp$state[state_comp_mco_exp$category == "Treated"])
always_treated_state <- unique(state_comp_mco_exp$state[state_comp_mco_exp$category == "Always treated"])
never_state <- unique(state_comp_mco_exp$state[state_comp_mco_exp$category == "Never treated"])

## select treated/always treated/never treated state spending data
state_comp_mco_exp2 <- state_comp_mco_exp %>% 
  select(1:10, "category") %>%
  gather(key = "year", value = "prop_comp_mco", 2:10)
state_comp_mco_exp2$year <- as.numeric(state_comp_mco_exp2$year)

dat <- left_join(map_wide, state_comp_mco_exp2, by = c("state", "year"))

state_comp_mco_exp <- state_comp_mco_exp %>% 
  select(state, 11:18) %>% 
  gather(key = "treat_year", value = "if_treated", 2:9) %>% 
  mutate(comp_mco_treat_year = as.numeric(substr(treat_year,7, 10))) %>% 
  select(-treat_year) %>% 
  filter(if_treated == 1)

dat <- left_join(dat, state_comp_mco_exp[,c("state", "comp_mco_treat_year")], by = "state")

## calcualte spending per capita
medicaid_enroll_report <- medicaid_enroll_report[,c(1,2,5)]
colnames(medicaid_enroll_report) <- c("state", "total_medicaid_enroll", "year")

dat <- left_join(dat, medicaid_enroll_report, by = c("state", "year"))

dat <- dat %>% 
  mutate(all_medicaid_spending_per_cap = `Total Net Expenditures`/total_medicaid_enroll)

##### 2. total spending change - raw data describe #####
dat %>% 
  filter(state %in% unique(state_comp_mco_exp$state)) %>% 
  ggplot(aes(x = year, y = Balance, group = state))+
  geom_point()+
  geom_line()+
  geom_vline(aes(xintercept = comp_mco_treat_year), linetype = "dashed", color = "firebrick", size = 1)+
  facet_wrap(~state)+
  scale_x_continuous(breaks = seq(2013, 2022, by = 1),
                     limits = c(2013, 2022))

##### 2. total spending change - DID #####
## function
keepvars <- c("`center_time_-6`", "`center_time_-5`", "`center_time_-4`", "`center_time_-3`",
              "`center_time_-2`", "`center_time_-1`","center_time_0",
              "center_time_1", "center_time_2", "center_time_3", 
              "center_time_4", "center_time_5", "center_time_6")

prep_es_3 <- function(mod){
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se
  )
  
  es <-
    mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-3:-2, 0:3)) %>% 
    select(t, estimate, se)
  es <- rbind(es, c(-1, 0, 0))
  es <- es %>% mutate(lower = estimate - 1.96 * se,
                      upper = estimate + 1.96 * se)
  return(es)
}

prep_es_5 <- function(mod){
  mod_2 <- tibble(
    estimate = mod$coefficients,
    term1 = rownames(mod$coefficients),
    se = mod$se
  )
  
  es <-
    mod_2 %>% 
    filter(term1 %in% keepvars) %>% 
    mutate(t = c(-5:-2, 0:5)) %>% 
    select(t, estimate, se)
  es <- rbind(es, c(-1, 0, 0))
  es <- es %>% mutate(lower = estimate - 1.96 * se,
                      upper = estimate + 1.96 * se)
  return(es)
}

## prep data 
dat <- dat %>% 
  mutate(treat_group = case_when(
    state %in% treated_state ~ "Treated",
    state %in% always_treated_state ~ "Always Treated",
    state %in% never_state ~ "Never Treated"
  )) %>% 
  filter(treat_group %in% c("Treated", "Never Treated")) %>% 
  mutate(center_time = ifelse(treat_group == "Treated", 
                              year - comp_mco_treat_year, 0)) %>%
  dummy_cols(select_columns = "center_time") %>% 
  mutate(treat = case_when(
    state %in% treated_state & year >= comp_mco_treat_year ~ 1,
    state %in% treated_state & year < comp_mco_treat_year ~ 0,
    state %in% never_state ~ 0
  ))


## DID - 3 periods
mod_d <- lfe::felm(log(Balance) ~ `center_time_-3` +
                     `center_time_-2` + `center_time_0`+
                     `center_time_1` + `center_time_2` + `center_time_3` | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -3 & center_time <= 3), 
                   exactDOF = TRUE)
es_d <- prep_es_3(mod_d)
es_d <- es_d %>% mutate(sig = ifelse((lower >0 | upper <0), 1, 0))

mod_d <- lfe::felm(log(Balance) ~ treat | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -3 & center_time <= 3), exactDOF = TRUE)
summary(mod_d)
coef <- round(mod_d$coefficients[1],3)
star <- ifelse(mod_d$pval[1]<=0.01, "***",
               ifelse(mod_d$pval[1]<=0.05, "**",
                      ifelse(mod_d$pval[1]<=0.1, "*", "")))
se <- round(mod_d$se[1],3)
comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")

png(filename = "Output/DID_3yr.png", width=1500, height=900, res = 150)
p1 <- ggplot(data = es_d, aes(x=t, y=estimate, group = 1))+
  geom_point(size= 3, color = "gray26")+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, size = 0.8, color = "gray26") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", size = 1)+
  scale_x_continuous(breaks = round(seq(-3, 3, by =1),1),
                     limits = c(-3.2,3.2))+
  scale_y_continuous(breaks = round(seq(-0.4, 0.4, by =0.1), 1),
                     limits = c(-0.4, 0.4))+
  labs(x = "Relative Time", y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.position="none")+
  annotate("text", x=2, y=-0.3, label= comb_est, size = 5)
print(p1)
dev.off()

## DID - 5 periods
mod_d <- lfe::felm(log(Balance) ~ `center_time_-5` + `center_time_-4` + `center_time_-3` +
                     `center_time_-2` + `center_time_0`+
                     `center_time_1` + `center_time_2` + `center_time_3`+
                     `center_time_4` + `center_time_5`| as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -5 & center_time <= 5), 
                   exactDOF = TRUE)
es_d <- prep_es_5(mod_d)
es_d <- es_d %>% mutate(sig = ifelse((lower >0 | upper <0), 1, 0))

mod_d <- lfe::felm(log(Balance) ~ treat | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -5 & center_time <= 5), exactDOF = TRUE)
summary(mod_d)
coef <- round(mod_d$coefficients[1],3)
star <- ifelse(mod_d$pval[1]<=0.01, "***",
               ifelse(mod_d$pval[1]<=0.05, "**",
                      ifelse(mod_d$pval[1]<=0.1, "*", "")))
se <- round(mod_d$se[1],3)
comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")
text_pos <- min(es_d$lower)*9/10

png(filename = "Output/DID_5yr.png", width=1500, height=900, res = 150)
p2 <- ggplot(data = es_d, aes(x=t, y=estimate, group = 1))+
  geom_point(size= 3, color = "gray26")+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, size = 0.8, color = "gray26") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", size = 1)+
  scale_x_continuous(breaks = round(seq(-5, 5, by =1),1),
                     limits = c(-5.2,5.2))+
  scale_y_continuous(breaks = round(seq(-0.4, 0.4, by =0.1), 1),
                     limits = c(-0.4, 0.4))+
  labs(x = "Relative Time", y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.position="none")+
  annotate("text", x=3, y=-0.3, label= comb_est, size = 5)
print(p2)
dev.off()

png(filename = "Output/DID.png", width=2200, height=800, res = 150)
ggarrange(p1,p2,
          labels = c("", ""),
          ncol = 2, nrow = 1)
dev.off()


##### 3. total spending per capita change - DID #####
## DID - 3 periods
mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ `center_time_-3` +
                     `center_time_-2` + `center_time_0`+
                     `center_time_1` + `center_time_2` + `center_time_3` | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -3 & center_time <= 3), 
                   exactDOF = TRUE)
es_d <- prep_es_3(mod_d)
es_d <- es_d %>% mutate(sig = ifelse((lower >0 | upper <0), 1, 0))

mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ treat | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -3 & center_time <= 3), exactDOF = TRUE)
summary(mod_d)
coef <- round(mod_d$coefficients[1],3)
star <- ifelse(mod_d$pval[1]<=0.01, "***",
               ifelse(mod_d$pval[1]<=0.05, "**",
                      ifelse(mod_d$pval[1]<=0.1, "*", "")))
se <- round(mod_d$se[1],3)
comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")

png(filename = "Output/DID_spending_percap_3yr.png", width=1500, height=900, res = 150)
p1 <- ggplot(data = es_d, aes(x=t, y=estimate, group = 1))+
  geom_point(size= 3, color = "gray26")+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, size = 0.8, color = "gray26") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", size = 1)+
  scale_x_continuous(breaks = round(seq(-3, 3, by =1),1),
                     limits = c(-3.2,3.2))+
  scale_y_continuous(breaks = round(seq(-0.4, 0.4, by =0.1), 1),
                     limits = c(-0.4, 0.4))+
  labs(x = "Relative Time", y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.position="none")+
  annotate("text", x=2, y=-0.3, label= comb_est, size = 5)
print(p1)
dev.off()

## DID - 5 periods
mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ `center_time_-5` + `center_time_-4` + `center_time_-3` +
                     `center_time_-2` + `center_time_0`+
                     `center_time_1` + `center_time_2` + `center_time_3`+
                     `center_time_4` + `center_time_5`| as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -5 & center_time <= 5), 
                   exactDOF = TRUE)
es_d <- prep_es_5(mod_d)
es_d <- es_d %>% mutate(sig = ifelse((lower >0 | upper <0), 1, 0))

mod_d <- lfe::felm(log(all_medicaid_spending_per_cap) ~ treat | as.factor(state) + as.factor(year),
                   data = subset(dat, center_time >= -5 & center_time <= 5), exactDOF = TRUE)
summary(mod_d)
coef <- round(mod_d$coefficients[1],3)
star <- ifelse(mod_d$pval[1]<=0.01, "***",
               ifelse(mod_d$pval[1]<=0.05, "**",
                      ifelse(mod_d$pval[1]<=0.1, "*", "")))
se <- round(mod_d$se[1],3)
comb_est <- paste("TWFE: ",coef, star, " (", se, ")", sep = "")
text_pos <- min(es_d$lower)*9/10

png(filename = "Output/DID_spending_percap_5yr.png", width=1500, height=900, res = 150)
p2 <- ggplot(data = es_d, aes(x=t, y=estimate, group = 1))+
  geom_point(size= 3, color = "gray26")+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, size = 0.8, color = "gray26") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick4", size = 1)+
  scale_x_continuous(breaks = round(seq(-5, 5, by =1),1),
                     limits = c(-5.2,5.2))+
  scale_y_continuous(breaks = round(seq(-0.4, 0.4, by =0.1), 1),
                     limits = c(-0.4, 0.4))+
  labs(x = "Relative Time", y = "Estimate")+
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.position="none")+
  annotate("text", x=3, y=-0.3, label= comb_est, size = 5)
print(p2)
dev.off()

png(filename = "Output/DID_spending_percap.png", width=2200, height=800, res = 150)
ggarrange(p1,p2,
          labels = c("", ""),
          ncol = 2, nrow = 1)
dev.off()





