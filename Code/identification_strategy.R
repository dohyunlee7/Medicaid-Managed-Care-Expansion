#'
#' identification_strategy.R -- Define treatment years for each state
#'

library(dplyr)
library(haven)
library(datasets)
library(readxl)
library(fredr)
library(pubtheme)
library(AER)
library(stargazer)
library(xtable)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel3.rds"))

d <- read_dta(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/external/fmr90-05_2.dta")) %>%
  arrange(year) %>%
  filter(payshare == 3)

new_merged_data <- new_merged_data %>%
  mutate(`total schip (ct + st)` = ifelse(is.na(`total schip (ct + st)`),
                                                0,
                                                `total schip (ct + st)`),
         `s-dental services` = ifelse(is.na(`s-dental services`),
                                          0,
                                          `s-dental services`),
         `c-dental services` = ifelse(is.na(`c-dental services`),
                                      0,
                                      `c-dental services`)) %>%
  mutate(allspend = `total medicaid (mt + at)` + `total schip (ct + st)`,
         allspendnod = allspend - `m-dental services` - `s-dental services` - `c-dental services`,
         pcapallspendnod = allspendnod / total_med_enr)

controls <- readRDS(paste0(path, "/Temp/demographic_controls.rds"))

mandates <- readRDS(paste0(path, "/Temp/mandate_pcts_by_st_yr_expanded.rds"))

# Reformat state names to title case
mandates$stname <- str_to_title(mandates$stname)
new_merged_data$state <- str_to_title(new_merged_data$state)
controls$statefip <- str_to_title(controls$statefip)

# Get the 663 observations (for just specification replication)
new_merged_data <- new_merged_data %>%
  filter(state != "Puerto Rico",
         year <= 2003)

# Merge 1991-2003 panel with mandate data
main_data <- left_join(new_merged_data, mandates, by = c("state" = "stname",
                                                         "year"))

main_data <- left_join(main_data, controls, by = c("state" = "statefip",
                                                   "year"))

main_data <- main_data %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))

cpi_data <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2010-12-31"),
  frequency = "a"
)

# Calculate year-over-year percentage change (inflation rate) in CPI
cpi_data <- cpi_data %>%
  arrange(date) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  rename(cpi = value) %>%
  select(year, cpi)


# Join annual CPI to panel
data_adj <- left_join(main_data, cpi_data, by = "year")

cpi_2010 <- cpi_data[cpi_data$year == 2010, ]$cpi

data_adj <- data_adj %>%
  mutate(adj_factor = cpi_2010 / cpi, 3)

data_adj <- data_adj %>%
  mutate(across(30:458, ~. * adj_factor))

main_data <- data_adj

### Figure 1 ###
fig1_data <- main_data %>%
  filter(state %in% c("Massachusetts",
                      "Florida",
                      "California",
                      "Nebraska",
                      "Illinois"))

fig1 <- ggplot(fig1_data, aes(x = year, y = pct_with_mandate, color = state)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1991, 2003, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.20),
                     limits = c(0, 1),
                     labels = scales::percent) +
  labs(title = "Figure 1",
       subtitle = "Percent of Medicaid recipients with MMC mandate in 5 selected states",
       x = "Year",
       y = "Percent") +
  theme_pub() +
  theme(
    plot.title = element_text(size = 22),       # Increase title font size
    plot.subtitle = element_text(size = 20),   # Increase subtitle font size
    axis.title.x = element_text(size = 18),    # Increase x-axis title font size
    axis.title.y = element_text(size = 18),    # Increase y-axis title font size
    axis.text.x = element_text(size = 16),     # Increase x-axis text font size
    axis.text.y = element_text(size = 16),     # Increase y-axis text font size
    legend.text = element_text(size = 18),     # Increase legend text font size
    legend.title = element_text(size = 18)     # Increase legend title font size
  )

ggsave(paste0(path, "/Output/dh_fig1.png"),
       plot = fig1,
       width = 18,
       height = 15,
       dpi = 300)


### EFFECT OF STATE AND LOCAL MANDATES ON MMC ENROLLMENT ###
### Replication of Tables and Models from D&H ###

# Column 0 (mu, sigma)
column0 <- main_data %>%
  summarise(
    # Mandatory MMC county (D&H definition)
    mmc_mean = mean(pct_with_mandate, na.rm = TRUE),
    mmc_sd = sd(pct_with_mandate, na.rm = TRUE),
    
    # Mandatory MMC county (Refined definition excluding PCCM)
    crb_mc_mean = mean(pct_with_crb_mandate, na.rm = TRUE),
    crb_mc_sd = sd(pct_with_crb_mandate, na.rm = TRUE),
    
    # Mandatory PCCM county
    pccm_mean = mean(pct_with_pccm_only, na.rm = TRUE),
    pccm_sd = sd(pct_with_pccm_only, na.rm = TRUE),
    
    # Mixed mandatory county
    mixed_mean = mean(pct_with_mixedmand, na.rm = TRUE),
    mixed_sd = sd(pct_with_mixedmand, na.rm = TRUE),
    
    # Mandatory HMO county
    hmo_mean = mean(pct_with_mandhmo, na.rm = TRUE),
    hmo_sd = sd(pct_with_mandhmo, na.rm = TRUE)
  )

main_data <- main_data %>%
  mutate(pct_in_managed_care = managed_care_enrollment / total_med_enr)

### Table 6: The Impact of State and Local MMC Mandates on MMC Enrollment
# Specification 1: 
tbl6_spec1 <- lm(pct_in_managed_care ~ pct_with_mandate + as.factor(state) + 
              as.factor(year), data = main_data)
summary(tbl6_spec1)

tbl6_spec2 <- lm(pct_in_managed_care ~ pct_with_mandate + children_prop + 
                   elderly_prop + disabled_prop + as.factor(state) + 
                   as.factor(year), data = main_data)
summary(tbl6_spec2)

tbl6_spec3 <- lm(pct_in_managed_care ~ pct_with_mandate + children_prop + 
                   elderly_prop + disabled_prop + as.factor(state) + 
                   as.factor(year) + state * year, data = main_data)
summary(tbl6_spec3)

tbl6_spec4 <- lm(pct_in_managed_care ~ pct_with_pccm_only + pct_with_mixedmand +
                   pct_with_mandhmo + children_prop + elderly_prop + 
                   disabled_prop + as.factor(state) + 
                   as.factor(year) + state * year, data = main_data)
summary(tbl6_spec4)

tbl6_spec5 <- lm(pct_in_pccm ~ pct_with_pccm_only + pct_with_mixedmand +
                   pct_with_mandhmo + children_prop + elderly_prop + 
                   disabled_prop + as.factor(state) + 
                   as.factor(year) + state * year, data = main_data)
summary(tbl6_spec5)

main_data <- main_data %>%
  mutate(pct_in_hmo = hmo / total_med_enr)

tbl6_spec6 <- lm(pct_in_comp_mco ~ pct_with_pccm_only + pct_with_mixedmand +
                   pct_with_mandhmo + children_prop + elderly_prop + 
                   disabled_prop + as.factor(state) + 
                   as.factor(year) + state * year, data = main_data)
summary(tbl6_spec6)

# Refined definition of managed care (no PCCM)
tbl6_spec1_crb <- lm(pct_in_comp_mco ~ pct_with_crb_mandate + factor(state) + 
                 factor(year), data = main_data)
summary(tbl6_spec1_crb)


# Just for PCCM
tbl6_spec1_pccm <- lm(pct_in_pccm ~ pct_with_pccm_only + factor(state) + 
                 factor(year), data = main_data)
summary(tbl6_spec1_pccm)


### Table 8: Impact of MMC Mandates and Enrollment on State Medicaid Expenditures

# Duggan and Hayford's definition of MMC, n = 663 (original tables)
# Sp. 1 RF
tbl8_spec1 <- lm(log(`total medicaid (mt + at)`) ~ 
                   pct_with_mandate +
                   log(total_med_enr) +
                   children_prop +
                   elderly_prop +
                   disabled_prop +
                   factor(state) + 
                   factor(year) +
                   state:year,
                 data = main_data)
summary(tbl8_spec1)

# Sp. 2 IV
iv_model2 <- ivreg(log(`total medicaid (mt + at)`) ~ 
                    pct_in_managed_care + 
                    log(total_med_enr) + 
                    children_prop +
                    elderly_prop +
                    disabled_prop +
                    factor(state) +
                    factor(year) + 
                    state:year |
                    pct_with_mandate +
                    log(total_med_enr) + 
                    children_prop +
                    elderly_prop +
                    disabled_prop +
                    factor(state) + 
                    factor(year) + 
                    state:year,
               data = main_data)
summary(iv_model2)

# Sp. 3 RF
tbl8_spec3 <- lm(log(allspendnod) ~ 
                   pct_with_mandate + 
                   pct_with_mandhmo +
                   log(total_med_enr) + 
                   children_prop +
                   elderly_prop +
                   disabled_prop +
                   factor(state) + 
                   factor(year) +
                   state:year,
                 data = main_data)
summary(tbl8_spec3)

main_data <- main_data %>%
  mutate(pct_in_mandhmo = pop_with_mandhmo / total_med_enr)

# Sp. 4 IV
iv_model4 <- ivreg(log(allspendnod) ~ 
                     pct_in_managed_care + 
                     pct_in_mandhmo +
                     log(total_med_enr) + 
                     children_prop +
                     elderly_prop +
                     disabled_prop +
                     factor(state) +
                     factor(year) + 
                     state:year |
                     pct_with_mandate +
                     pct_with_mandhmo +
                     log(total_med_enr) + 
                     children_prop +
                     elderly_prop +
                     disabled_prop +
                     factor(state) + 
                     factor(year) + 
                     state:year,
                   data = main_data)

summary(iv_model4)

stargazer(tbl8_spec1, iv_model2, tbl8_spec3, iv_model4,
          type = "latex",
          title = "",
          column.labels = c("RF", "IV", "RF", "IV"),
          model.numbers = F,
          dep.var.labels = "Dependent Variable",
          covariate.labels = c(
            "% of state pop'n in mand. MMC county",
            "% of Medicaid recip. in MMC",
            "% of state pop'n in mand. HMO county",
            "% of Medicaid recip. in HMOs",
            "Log(Medicaid Recips)",
            "% Medicaid (ages 0-14)",
            "% Medicaid (ages 65+)",
            "% Medicaid (disabled)"
          ),
          omit = c("Constant", "state", "year", "state:year"),
          align = T,
          font.size = "small",
          column.sep.width = "1pt",
          single.row = T,
          table.placement = "!htbp",
          longtable = T)

# Our definition of MMC (Full Risk), n = 663
# Sp. 1 RF
tbl8_spec1 <- lm(log(allspendnod) ~ 
                   pct_with_crb_mandate +
                   log(total_med_enr) +
                   children_prop +
                   elderly_prop +
                   disabled_prop +
                   as.factor(state) + 
                   as.factor(year) +
                   state:year,
                 data = main_data)
summary(tbl8_spec1)

# Sp. 2 IV
iv_model2 <- ivreg(log(allspendnod) ~ 
                     pct_in_comp_mco + 
                     log(total_med_enr) + 
                     children_prop +
                     elderly_prop +
                     disabled_prop +
                     as.factor(state) +
                     as.factor(year) + 
                     state:year |
                     pct_with_crb_mandate + 
                     log(total_med_enr) + 
                     # children_prop +
                     # elderly_prop +
                     # disabled_prop +
                     as.factor(state) + 
                     as.factor(year) +
                     state:year,
                   data = main_data)

summary(iv_model2)

# Sp. 3 RF
tbl8_spec3 <- lm(log(allspendnod) ~ 
                   pct_with_mandate + 
                   pct_with_mandhmo +
                   log(total_med_enr) + 
                   children_prop +
                   elderly_prop +
                   disabled_prop +
                   as.factor(state) + 
                   as.factor(year) +
                   state:year,
                 data = main_data)
summary(tbl8_spec3)

# Sp. 4 IV
iv_model4 <- ivreg(log(allspendnod) ~ 
                     pct_in_comp_mco + 
                     pct_in_mandhmo +
                     log(total_med_enr) + 
                     # children_prop +
                     # elderly_prop +
                     # disabled_prop +
                     factor(state) +
                     factor(year) + 
                     state:year |
                     pct_with_crb_mandate +
                     pct_with_mandhmo +
                     log(total_med_enr) + 
                     # children_prop +
                     # elderly_prop +
                     # disabled_prop +
                     factor(state) + 
                     factor(year) + 
                     state:year,
                   data = main_data)

stargazer(tbl8_spec1, iv_model2, tbl8_spec3, iv_model4,
          type = "latex",
          title = "",
          column.labels = c("RF", "IV", "RF", "IV"),
          model.numbers = F,
          dep.var.labels = "Dependent Variable",
          covariate.labels = c(
            "% of state pop'n in mand. MMC county",
            "% of Medicaid recip. in MMC",
            "% of state pop'n in mand. HMO county",
            "% of Medicaid recip. in HMOs",
            "Log(Medicaid Recips)",
            "% Medicaid (ages 0-14)",
            "% Medicaid (ages 65+)",
            "% Medicaid (disabled)"
          ),
          omit = c("Constant", "state", "year", "state:year"),
          align = T,
          font.size = "small",
          column.sep.width = "1pt",
          single.row = T,
          table.placement = "!htbp",
          longtable = T)

### --- Replications with state subset, extension, and extension subset ---- ###
excluded_states <- c("California", "Colorado", "Kansas", 
                     "Minnesota", "New York", "Pennsylvania",
                     "South Carolina", "Utah", "Virginia",
                     "West Virginia", "Wisonsin", "Mississippi",
                     "Nebraska")

main_data <- main_data %>%
  mutate(pct_with_mandate = ifelse(pct_with_mandate > 1, 
                                      1, 
                                   pct_with_mandate),
         pct_with_crb_mandate = ifelse(pct_with_crb_mandate > 1, 
                                  1, 
                                  pct_with_crb_mandate),
         pct_with_pccm_only = ifelse(pct_with_pccm_only > 1, 
                              1, 
                              pct_with_pccm_only),
         pct_in_pccm = replace_na(pct_in_pccm, 0))

main_data_subset <- main_data %>%
  filter(!state %in% excluded_states)

column0 <- main_data_subset %>%
  summarise(
    # Mandatory MMC county (D&H definition)
    mmc_mean = mean(pct_with_mandate, na.rm = TRUE),
    mmc_sd = sd(pct_with_mandate, na.rm = TRUE),
    
    # Mandatory MMC county (Refined definition excluding PCCM)
    crb_mc_mean = mean(pct_with_crb_mandate, na.rm = TRUE),
    crb_mc_sd = sd(pct_with_crb_mandate, na.rm = TRUE),
    
    # Mandatory PCCM county
    pccm_mean = mean(pct_with_pccm_only, na.rm = TRUE),
    pccm_sd = sd(pct_with_pccm_only, na.rm = TRUE),
    
    # Mixed mandatory county
    mixed_mean = mean(pct_with_mixedmand, na.rm = TRUE),
    mixed_sd = sd(pct_with_mixedmand, na.rm = TRUE),
    
    # Mandatory HMO county
    hmo_mean = mean(pct_with_mandhmo, na.rm = TRUE),
    hmo_sd = sd(pct_with_mandhmo, na.rm = TRUE)
  )

# Specification 1: 
tbl6_spec1 <- lm(pct_in_managed_care ~ pct_with_mandate + factor(state) + 
                   factor(year), data = main_data_subset)
summary(tbl6_spec1)

# Refined definition of managed care (no PCCM)
tbl6_spec1_crb <- lm(pct_in_comp_mco ~ pct_with_crb_mandate + factor(state) + 
                       factor(year), data = main_data_subset)
summary(tbl6_spec1_crb)


# Just for PCCM
tbl6_spec1_pccm <- lm(pct_in_pccm ~ pct_with_pccm_only + factor(state) + 
                        factor(year), data = main_data_subset)
summary(tbl6_spec1_pccm)


### ----------------------- Jump in Prop. of MMC --------------------------- ###

new_merged_data <- new_merged_data %>%
  mutate(pct_in_comp_mco = ifelse(pct_in_comp_mco > 1, 
                              1, 
                              pct_in_comp_mco)) # *Change metric

unique_states <- unique(new_merged_data$state)
output_dir <- file.path(path, "Output", "state_plots")


# Calculate the where there is a first instance of comp. mco enrollment > 10%
jumps <- new_merged_data %>%
  group_by(state) %>%
  arrange(year, .by_group = TRUE) %>%  # Ensure data is sorted by year within each state
  summarise(
    treatment_year = year[which(pct_in_comp_mco > 0.10)[1]],
    pct_in_comp_mco = pct_in_comp_mco[which(pct_in_comp_mco > 0.10)[1]]# First year where pct_in_comp_mco > 10%
  ) %>%
  ungroup()

jumps[jumps$state == "District Of Columbia", ]$treatment_year <- 1995
jumps[jumps$state == "District Of Columbia", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "District Of Columbia" & new_merged_data$year == "1995", ]$pct_in_comp_mco

jumps[jumps$state == "Indiana", ]$treatment_year <- 1995
jumps[jumps$state == "Indiana", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "Indiana" & new_merged_data$year == "1995", ]$pct_in_comp_mco

jumps[jumps$state == "Iowa", ]$treatment_year <- 2016
jumps[jumps$state == "Iowa", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "Iowa" & new_merged_data$year == "2016", ]$pct_in_comp_mco

jumps[jumps$state == "Kansas", ]$treatment_year <- 1997
jumps[jumps$state == "Kansas", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "Kansas" & new_merged_data$year == "1997", ]$pct_in_comp_mco

jumps[jumps$state == "New York", ]$treatment_year <- 1995
jumps[jumps$state == "New York", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "New York" & new_merged_data$year == "1995", ]$pct_in_comp_mco

jumps[jumps$state == "Texas", ]$treatment_year <- 1997
jumps[jumps$state == "Texas", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "Texas" & new_merged_data$year == "1997", ]$pct_in_comp_mco

jumps[jumps$state == "Mississippi", ]$treatment_year <- 2011
jumps[jumps$state == "Mississippi", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "Mississippi" & new_merged_data$year == "2011", ]$pct_in_comp_mco

jumps[jumps$state == "South Carolina", ]$treatment_year <- 2007
jumps[jumps$state == "South Carolina", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "South Carolina" & new_merged_data$year == "2007", ]$pct_in_comp_mco

jumps[jumps$state == "New Hampshire", ]$treatment_year <- 2014
jumps[jumps$state == "New Hampshire", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "New Hampshire" & new_merged_data$year == "2014", ]$pct_in_comp_mco

jumps[jumps$state == "Pennsylvania", ]$treatment_year <- 1995
jumps[jumps$state == "Pennsylvania", ]$pct_in_comp_mco <- 
  new_merged_data[new_merged_data$state == "Pennsylvania" & new_merged_data$year == "1995", ]$pct_in_comp_mco



jumps$state <- str_to_title(jumps$state)

new_merged_data_temp <- new_merged_data %>%
  left_join(jumps, by = "state")

saveRDS(jumps, file = paste0(path, "/Temp/jumps.rds"))

# Calculate increase in mandate for treatment year
mandates2 <- left_join(mandates, jumps, by = c("stname" = "state"))
mandates3 <- mandates2 %>% select(-pct_in_comp_mco, -treatment_year)

# Join state-year mandate data to panel
new_merged_data_temp <- new_merged_data_temp %>%
  left_join(mandates3, by = c("state" = "stname", "year"))

# Keep states whose treatment years aren't NA or past 2001 (Mandates are only
# present from 1991 - 2001)
# Calculate the change in mandate percentage where the treatment year is defined
res <- mandates2 %>%
  filter(!is.na(treatment_year) & treatment_year <= 2001) %>%
  group_by(stname) %>%
  summarise(treatment_year = unique(treatment_year),
            pct_with_mandate_change = pct_with_mandate[year == treatment_year] - 
              pct_with_mandate[year == (treatment_year - 1)],
            pct_with_crb_change = pct_with_crb_mandate[year == treatment_year] - 
              pct_with_crb_mandate[year == (treatment_year - 1)]) %>%
  ungroup()

# Remove treatment year to avoid repeat column
res <- res %>% select(-treatment_year)

# Join % change in MMC for treatment year onto panel
new_merged_data_temp <- new_merged_data_temp %>%
  left_join(res, by = c("state" = "stname"))

new_merged_data_temp <- new_merged_data_temp %>%
  mutate(pct_with_crb_mandate = ifelse(pct_with_crb_mandate > 1, 
                                       1, 
                                       pct_with_crb_mandate))

# Get plots 
for (st in unique_states) {
  state_data <- new_merged_data_temp %>%
    filter(state == st) %>%
    mutate(pct_in_comp_mco.x = as.numeric(pct_in_comp_mco.x),
           pct_with_mandate = as.numeric(pct_with_mandate),
           pct_with_crb_mandate = as.numeric(pct_with_crb_mandate))
  
  y_max <- state_data %>%
    filter(year >= (treatment_year - 1) & year <= (treatment_year + 1)) %>%
    summarize(max_y = max(pct_in_comp_mco.x)) %>%
    pull(max_y)
  
  p <- ggplot(state_data) +
    geom_line(aes(x = year, 
                  y = pct_in_comp_mco.x, 
                  color = "Share of Comp. Risk-Based Enrollment"),
              linewidth = 1) +
    geom_line(data = state_data %>% filter(year <= 2001),
              aes(x = year, 
                  y = pct_with_crb_mandate, 
                  color = "Share of Counties with MMC Mandate"),
              linewidth = 1) +
    geom_line(data = state_data %>% filter(year >= 2001),
              aes(x = year, 
                  y = pct_with_crb_mandate, 
                  color = "Share of Counties with MMC Mandate"),
              linewidth = 1,
              linetype = "dashed") +
    labs(title = paste0("Share of Comprehensive Risk-Based Managed Care Enrollment: ", st),
         subtitle = "Mandate: 1991-2001, CRB MC: 1991-2022",
         x = "Year",
         y = "Share of Enrollees") +
    scale_x_continuous(breaks = seq(1991, 2022, by = 2),
                       limits = c(1991, 2022)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.20),
                       limits = c(0, 1),
                       labels = scales::percent) +
    geom_vline(data = state_data %>% filter(year == treatment_year),
               aes(xintercept = treatment_year, color = pubblue),
               linetype = "dashed", 
               linewidth = 1) +
    annotate("text",
             x = state_data$treatment_year + 1,
             y = y_max - 0.10,
             label = paste0("% in Comp. MCO: ", 
                            scales::percent(max(state_data$pct_in_comp_mco.y))),
             size = 4,
             hjust = 0,
             color = pubblue) +
    annotate("text",
             x = state_data$treatment_year + 1,
             y = y_max - 0.15,
             label = paste0("Prop. Jump in Mandate (Treat Yr): ", 
                            scales::percent(state_data$pct_with_crb_change)),
             size = 4,
             hjust = 0,
             color = pubred) +
    scale_color_manual(values = c("Share of Comp. Risk-Based Enrollment" = pubblue,
                                  "Share of Counties with MMC Mandate" = pubred)) +
    theme(plot.title = element_text(size = 26),
          plot.subtitle = element_text(size = 24),
          axis.title.x = element_text(size = 22), 
          axis.title.y = element_text(size = 22),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.title = element_text(size = 24),
          legend.text = element_text(size = 22)) +
    theme_pub()
  
  output_path <- file.path(output_dir, paste0(st, ".png"))
  ggsave(output_path, plot = p, width = 16, height = 12, dpi = 300)
}


# Plot distribution of jump % of MMC enrollment
ggplot(jumps, (aes(x = max_jump))) +
  geom_histogram(binwidth = 0.05) +
  labs(
    title = "Distribution of Jump in Prop. of Comprehensive MMC Enrollment",
    subtitle = "Treatment year ranges from 1991 - 2022",
    x = "Jump in Prop. of MMC Enrollment (%)",
    y = "Frequency"
  ) +
  theme_pub()

# Plot distribution of change in % of mandate in treatment year (derived from calculating
# the jumps)
ggplot(res, (aes(x = pct_with_mandate_change))) +
  geom_histogram(binwidth = 0.05) +
  labs(
    title = "Distribution of Change in % of Mandate in Treatment Year",
    subtitle = "Mandate data ranges from 1991 - 2003; state treatment year derived from jump calculation",
    x = "Change in % of Mandate",
    y = "Frequency"
    ) +
  theme_pub()

ggplot(new_merged_data_temp, aes(x = max_jump, y = pct_with_mandate_change)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Differences of Max Jump and Mandate Change by State",
       subtitle = "At Each State's Treatment Year",
       x = "Max Jump",
       y = "Mandate Change") +
  theme_pub()



### ------------ Linear Model and R^2 to Define Treatment Year ------------- ###

# If I want to find treatment year for comprehensive risk based managed care
new_merged_data <- new_merged_data %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(
    pct_in_comp_mco = crb_mc_enrollees / total_med_enr,
    change_comp_mco = pct_in_comp_mco - lag(pct_in_comp_mco),
  ) %>%
  ungroup()

states_vec <- unique(new_merged_data$state)
states_vec <- states_vec[states_vec != "Puerto Rico"]

# Initialize empty df to store results
final_res <- data.frame(
  state = character(),
  t_prime = integer(),
  r_squared = numeric()
)

for (state_i in states_vec) {
  print(state_i)
  
  # Filter dataframe for individual state per iteration
  state_data <- new_merged_data %>%
    filter(state == state_i)
  
  # Check if pct_in_comp_mco is constant or has only NA values
  if (all(is.na(state_data$pct_in_managed_care)) || var(state_data$pct_in_managed_care, na.rm = TRUE) == 0) {
    print(paste("Skipping state:", state_i, "- pct_in_comp_mco is constant or all NA"))
    next
  }
  # Initialize new empty dataframe for each state every iteration
  state_res <- data.frame(t_prime = integer(), 
                          r_squared = numeric())
  
  # Get vector of years for every pass
  unique_years <- unique(state_data$year)
  
  # Loop through each possible t'
  for (t_prime in unique_years) {
    
    print(t_prime)
    
    # Indicator for current t'
    state_data <- state_data %>%
      mutate(treat = as.numeric(year >= t_prime))
    
    # Fit regression
    model <- lm(pct_in_managed_care ~ treat, data = state_data)
    
    # Extract R-squared and save result
    r_squared <- summary(model)$r.squared
    r_squared <- ifelse(is.nan(r_squared), 0, r_squared)
    state_res <- rbind(state_res, data.frame(t_prime = t_prime, 
                                             r_squared = r_squared))
    
  }
  
  # For state store the t' with the highest R-squared in a dataframe
  best_row <- state_res %>%
    filter(r_squared == max(r_squared)) %>%
    slice(1)
  
  best_t_prime <- best_row$t_prime
  
  best_r_squared <- best_row$r_squared
  
  final_res <- rbind(final_res, data.frame(state = state_i, 
                                           t_prime = best_t_prime, 
                                           r_squared = best_r_squared))
}

# final_res <- final_res %>%
#   mutate(t_prime = ifelse(r_squared == 0, NA, t_prime))

new_merged_data <- new_merged_data %>%
  left_join(final_res, by = "state")

unique_states <- unique(new_merged_data$state)

output_dir <- file.path(path, "Output", "state_plots")

for (st in unique_states) {
  state_data <- new_merged_data %>%
    filter(state == st)
  
  p <- ggplot(state_data, aes(x = year, 
                              y = crb_mc_enrollees / total_med_enr, 
                              color = state)) +
    geom_line(size = 1) +
    labs(title = "Share of Comprehensive Risk-Based Managed Care by State",
         subtitle = "We only have Comprehensive MCO enrollment from 1999-2022",
         x = "Year",
         y = "Share of Enrollees") +
    scale_x_continuous(breaks = seq(1999, 2023, by = 2),
                       limits = c(1999, 2023)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.20),
                       limits = c(0, 1),
                       labels = scales::percent) +
    geom_vline(data = state_data %>% filter(year == t_prime), 
               aes(xintercept = t_prime, color = state), 
               linetype = "dashed", size = 1) +      
    theme_pub()
  
  output_path <- file.path(output_dir, paste0(st, ".png"))
  ggsave(output_path, plot = p, width = 8, height = 6, dpi = 300)
}


saveRDS(final_res, file = paste0(path, "/Temp/t_primes.rds"))





