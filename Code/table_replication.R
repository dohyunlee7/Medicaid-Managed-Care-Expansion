
#'
#' table_replication.R -- Replicate tables in D&H
#'

library(dplyr)
library(kableExtra)
library(haven)
library(datasets)
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

t_data <- read_dta(paste0(path, "/Input_Data/CMS_64_report/fmr90-05_2.dta"))
t_data <- t_data %>%
  filter(!statecode %in% c(22, 53:57, 99)) %>%
  filter(payshare == 3) %>%
  filter(year %in% 1991:2009)

data <- readRDS(paste0(path, "/Temp/new_merged_panel3.rds"))
data <- data %>%
  filter(state != "Puerto Rico") %>%
  filter(year %in% 1991:2009)

data <- data %>%
  mutate(across(everything(), ~replace(., is.na(.), 0)))

### ------------------------------- Table 1 -------------------------------- ###

data <- data %>%
  mutate(mco = ifelse(is.na(mco), 0, mco),
         hmo = ifelse(is.na(hmo), 0, hmo))

tbl1 <- data %>%
  group_by(year) %>%
  summarise(
    total_med_enr = sum(total_med_enr, na.rm = TRUE),
    total_mmc_enr = sum(managed_care_enrollment, na.rm = TRUE),
    total_hmo_enr = sum(hmo + mco, na.rm = TRUE),
    total_crb_enr = sum(crb_mc_enrollees, na.rm = TRUE),
    
    pct_in_mmc = scales::percent(total_mmc_enr / total_med_enr, accuracy = 0.1),
    pct_in_hmo = scales::percent(total_hmo_enr / total_med_enr, accuracy = 0.1),
    pct_in_crb = scales::percent(total_crb_enr / total_med_enr, accuracy = 0.1),
  ) %>%
  mutate(total_med_enr_in_millions = round(total_med_enr / 1e6, 1),
         magnitude_diff_hmo = abs(total_hmo_enr - total_crb_enr)) %>%
  select(year, 
         total_med_enr_in_millions,
         total_hmo_enr,
         total_crb_enr,
         magnitude_diff_hmo,
         pct_in_hmo,
         pct_in_crb)

ggplot(tbl1, aes(x = total_hmo_enr, y = total_crb_enr)) +
  geom_point() +
  geom_abline(y = x) +
  labs()

x <- data %>%
  group_by(state, year) %>%
  summarise(
    total_med_enr = sum(total_med_enr, na.rm = TRUE),
    total_med_spend = sum(`total medicaid (mt + at)`, na.rm = TRUE),
  ) %>%
  select(year, 
         total_med_enr,
         total_med_spend)

y <- t_data %>%
  group_by(statename, year) %>%
  summarise(
    mdcdenroll = sum(mdcdenroll, na.rm = TRUE),
    totmdcd = sum(totmdcd), na.rm = TRUE) %>%
  select(year, 
         mdcdenroll,
         totmdcd)

z <- left_join(x, y, by = c("state" = "statename", "year"))

z <- z %>%
  mutate(diff_magnitude_enr = total_med_enr - mdcdenroll,
         diff_magnitude_spend = total_med_spend - totmdcd) %>%
  filter(year == 1996)


# See % in county with MMC/HMO calculations in mandate_data_prepper.R


### ------------------------------- Table 2 -------------------------------- ###

mandate_data <- readRDS(paste0(path, "/Temp/mandate_pcts_by_st_yr_expanded.rds"))
mandate_data <- mp_agg_tbl2 %>%
  filter(year %in% c(1991, 2003)) %>%
  select(stname, year, pct_with_mandate) %>%
  pivot_wider(names_from = year, values_from = pct_with_mandate)

mandate_data <- mandate_data %>%
  mutate(across(c(`1991`, `2003`), 
         ~ scales::percent(., accuracy = 0.1)))

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
  mutate(across(c(`1991`, `2003`, `2009`), 
                ~ scales::percent(pmin(.x, 1), accuracy = 0.1)))

tbl2 <- tbl2 %>%
  mutate(across(everything(), ~ replace_na(.x, "0.0%")))

tbl2$state <- tolower(tbl2$state)

tbl2 <- left_join(tbl2, mandate_data, by = c("state" = "stname"))

tbl2 <- tbl2 %>%
  rename("1991_enr" = "1991.x",
         "2003_enr" = "2003.x",
         "2009_enr" = "2009",
         "1991_mandate" = "1991.y",
         "2003_mandate" = "2003.y")

tbl2[tbl2$state == "alaska", ]$`1991_mandate` <- "0.0%"
tbl2[tbl2$state == "alaska", ]$`2003_mandate` <- "0.0%"

### ----------------------------- Table 3 ---------------------------------- ###

data <- data %>%
  mutate(allspend = `total medicaid (mt + at)` + `total schip (ct + st)`,
         allspendnod = allspend - `m-dental services` - `s-dental services` - `c-dental services`)

tbl3_data <- data %>%
  filter(year %in% c(1991, 1997, 2003, 2009)) #%>%
 # select(state, year, total_med_enr, `total medicaid (mt + at)`)

tbl3_data <- tbl3_data %>%
  group_by(year) %>%
  summarise(total_med_spending = sum(`total medicaid (mt + at)`, na.rm = T),
            
            # Administration
            m_adm = sum(`at-total net expenditures`, na.rm = T),
            
            # Hospital
            m_inpatient_reg = sum(`m-inpatient hospital - reg. payments`, na.rm = T),
            m_inpatient_dsh = sum(`m-inpatient hospital - dsh`, na.rm = T),
            m_outpatient = sum(`m-outpatient hospital services`, na.rm = T),
            
            # Long term care
            m_nursfacserv = sum(`m-nursing facility services`, na.rm = T),
            m_interprivate = sum(`m-intermediate care - private`, na.rm = T),
            m_interpublic = sum(`m-intermediate care facility - public`, na.rm = T),
            m_personalserv = sum(`m-personal care services`, na.rm = T),
            
            # Physician
            m_physserv = sum(`m-physicians' services`, na.rm = T),
            m_otherprac = sum(`m-other practitioners`, na.rm = T),
            m_clinicserv = sum(`m-clinic services`, na.rm = T),

            # Mental Health
            m_mental_reg = sum(`m-mental health facility services - reg. payments`, na.rm = T),
            m_mental_dsh = sum(`m-mental health facility - dsh`, na.rm = T),
            
            # Home health
            m_home_comm = sum(`m-home and community`, na.rm = T),
            m_home_comm_dis = sum(`m-home and community - disabled elderly`, na.rm = T),
            m_home_health = sum(`m-home health services`, na.rm = T),
            m_hospice = sum(`m-hospice benefits`, na.rm = T),
            
            # Prescription drugs
            m_rx = sum(`m-prescribed drugs`, na.rm = T),
            m_rebate_natl = sum(`m-drug rebate offset - national`, na.rm = T),
            m_rebate_state = sum(`m-drug rebate offset - state sidebar agreement`, na.rm = T),

            # Medicare
            m_medicare_a = sum(`m-medicare - part a`, na.rm = T),
            m_medicare_b = sum(`m-medicare - part b`, na.rm = T),
            
            # Coinsurance
            m_medcoin = sum(`m-medicaid - coinsurance`, na.rm = T),
            m_coin = sum(`m-coinsurance`, na.rm = T),
            
            # Managed Care
            m_mdcdmco = sum(`m-medicaid - mco`, na.rm = T),
            m_prepaidamb = sum(`m-prepaid ambulatory health plan`, na.rm = T),
            m_prepaidinp = sum(`m-prepaid inpatient health plan`, na.rm = T),
            m_mdcdcoin = sum(`m-medicaid - coinsurance`, na.rm = T),
            m_allcareeld = sum(`m-all-inclusive care elderly`, na.rm = T),
            m_targetcasemgmt = sum(`m-targeted case management`, na.rm = T),
            m_primcasemgmt = sum(`m-primary care case management`, na.rm = T),
            m_mdcdphp = sum(`m-medicaid - php`, na.rm = T)
            
            ) %>%
  mutate(adm_total = m_adm, 
         hosp_total = m_inpatient_reg + m_inpatient_dsh + m_outpatient,
         ltc_total = m_nursfacserv + m_interprivate + m_interpublic + m_personalserv,
         phys_total = m_physserv + m_otherprac + m_clinicserv,
         ment_total = m_mental_reg + m_mental_dsh,
         home_total = m_home_comm + m_home_comm_dis + m_home_health,
         rx_total = m_rx + m_rebate_natl + m_rebate_state,
         medicare_total = m_medicare_a + m_medicare_b,
         coin_total = m_medcoin + m_coin,
         #mc_total = m_mdcdmco,
         
         administration = scales::percent(m_adm / total_med_spending, 0.1),
         hospital = scales::percent(hosp_total / total_med_spending, 0.1),
         ltc = scales::percent(ltc_total / total_med_spending, 0.1),
         #prop_phys = scales::percent(phys_total / total_med_spending, 0.1),
         mental_health = scales::percent(ment_total / total_med_spending, 0.1),
         home_health = scales::percent(home_total / total_med_spending, 0.1),
         prescription_drugs = scales::percent(rx_total / total_med_spending, 0.1),
         medicare = scales::percent(medicare_total / total_med_spending, 0.1),
         #prop_mc = scales::percent(mc_total / total_med_spending, 0.1),
         coin = scales::percent(coin_total / total_med_spending, 0.1)
         ) %>%
  select(year,
         hospital, 
         ltc,
         prescription_drugs,
         home_health, 
         mental_health, 
         medicare,
         coin,
         administration)

tbl3 <- tbl3_data %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = year, values_from = value)

# Join annual CPI to panel

# Get API Key from Fed. Reserve
fredr_set_key("8664fb88934dc0a2a037b8c6b153e4e5")

# Fetch annual CPI data for 'CPIAUCSL' 
# (Consumer Price Index for All Urban Consumers, All Items)
cpi_data <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2010-12-31"),
  frequency = "m"
)


# Calculate year-over-year percentage change (inflation rate) in CPI
# cpi_data <- cpi_data %>%
#   arrange(date) %>%
#   mutate(year = as.numeric(format(date, "%Y"))) %>%
#   rename(cpi = value) %>%
#   select(year, cpi)

cpi_data <- cpi_data %>%
  arrange(date) %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  filter(month == 6) %>%
  rename(cpi = value) %>%
  select(year, cpi)

data_adj <- left_join(tbl3_data, cpi_data, by = "year")

cpi_2010 <- cpi_data[cpi_data$year == 2010, ]$cpi

data_adj <- data_adj %>%
  mutate(adj_factor = cpi_2010 / cpi)

data_adj <- data_adj %>%
  mutate(total_med_spending = total_med_spending * adj_factor) %>%
  mutate(total_med_spending = scales::dollar(total_med_spending / 1e6))

### ----------------------------- Table 4 ---------------------------------- ###
df <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
            "Colorado", "Connecticut", "Delaware", "District of Columbia", 
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana"),
  `dh_1991` = c(4590, 4903, 4423, 3805, 3192, 6034, 8667, 6464, 7892, 5424, 4969, 1995, 6650, 3609, 8668),
  `dh_2003` = c(5679, 11296, 5994, 5229, 5980, 9646, 10665, 7453, 10717, 6281, 5518, 5472, 6604, 7410, 7444),
  `dh_2009` = c(5753, 11386, 7428, 5658, 6758, 8206, 12905, 7389, 11215, 6623, 6019, 5868, 7019, 6045, 6523)
)

# Read FY Medicaid enrollment (potentially enrolled on June 30 of year)
fymcdben <- read_dta(paste0(path, file.path("/Input_Data",
                                            "Medicaid_managedcare_enrollment_report",
                                            "external",
                                            "fymcdben.dta")))

# Read Medicaid enrollment (potentially "ever enrolled in the year")
mcdben <- read_dta(paste0(path, file.path("/Input_Data",
                                          "Medicaid_managedcare_enrollment_report",
                                          "external",
                                          "mcdben.dta")))

enrollment_data <- left_join(fymcdben, mcdben, by = c("state", "year"))

# Extend state.abb and state.name with DC name
state.abb <- c(state.abb, "DC", "XX")
state.name <- c(state.name, "District of Columbia", "National Total")

# Match state abbreviations to state names 
enrollment_data$state <- state.name[match(enrollment_data$state, state.abb)]

# Omit 1990 data and calculate the 1996 ratio
enrollment_data <- enrollment_data %>%
  filter(year != 1990)

# Filter data for 1996 only, calculate ratio for the two enrollments for 1996
enrollment_full <- enrollment_data %>%
  filter(year %in% 1996:2002) %>%
  mutate(mcd_ratio = mcdben / fymcdben) %>%
  select(state, year, mcd_ratio) %>%
  pivot_wider(names_from = year, 
              values_from = mcd_ratio,
              names_prefix = "mcd") %>%
  rename_with(~ paste0("mcd", substr(., 4, 7), "rat"), 
              starts_with("mcd"))

enrollment_full <- enrollment_full %>%
  rowwise() %>%
  mutate(ratio_all = mean(c_across(starts_with("mcd")), na.rm = TRUE),
         ratio_96_97 = mean(c_across(starts_with(c("mcd1996rat", 
                                                   "mcd1997rat"))), na.rm = TRUE),
         ratio_96_98 = mean(c_across(starts_with(c("mcd1996rat", 
                                                   "mcd1997rat", 
                                                   "mcd1998rat"))), na.rm = TRUE),
         ratio_96_99 = mean(c_across(starts_with(c("mcd1996rat", 
                                                   "mcd1997rat", 
                                                   "mcd1998rat",
                                                   "mcd1999rat"))), na.rm = TRUE),
         ratio_96_00 = mean(c_across(starts_with(c("mcd1996rat", 
                                                   "mcd1997rat", 
                                                   "mcd1998rat",
                                                   "mcd1999rat",
                                                   "mcd2000rat"))), na.rm = TRUE),
         ratio_96_01 = mean(c_across(starts_with(c("mcd1996rat", 
                                                   "mcd1997rat", 
                                                   "mcd1998rat",
                                                   "mcd1999rat",
                                                   "mcd2000rat",
                                                   "mcd2001rat"))), na.rm = TRUE),
         ratio_96_02 = mean(c_across(starts_with(c("mcd1996rat", 
                                                   "mcd1997rat", 
                                                   "mcd1998rat",
                                                   "mcd1999rat",
                                                   "mcd2000rat",
                                                   "mcd2001rat",
                                                   "mcd2002rat"))), na.rm = TRUE)) %>%
  ungroup()


# Filter for 1991-1995
est_91_95_enrollment <- enrollment_data %>%
  filter(year %in% 1991:1995)

# Join 1991-1995 enrollment with the 1996 ratio
est_91_95_enrollment <- left_join(est_91_95_enrollment,
                                  enrollment_full,
                                  by = "state")

# Multiply FY enrollment by the ratio
est_91_95_enrollment <- est_91_95_enrollment %>%
  mutate(mcdben = fymcdben * ratio_96_01)

# Select variables and rename enrollment variable
est_91_95_enrollment <- est_91_95_enrollment %>%
  select(state, year, mcdben) %>%
  rename(total_med_enr = mcdben)

updated_data <- data %>%
  left_join(est_91_95_enrollment, by = c("state", "year"), suffix = c("_old", "_new"))

updated_data <- updated_data %>%
  mutate(total_med_enr_old = ifelse(!is.na(total_med_enr_new), 
                               total_med_enr_new,
                               total_med_enr_old))


tbl4_data <- updated_data %>%
  filter(year %in% c(1991, 2003, 2009)) #%>%
  #select(state, year, total_med_enr, `total medicaid (mt + at)`)

library(fredr)

# Get API Key from Fed. Reserve
fredr_set_key("8664fb88934dc0a2a037b8c6b153e4e5")

# Fetch annual CPI data for 'CPIAUCSL' 
# (Consumer Price Index for All Urban Consumers, All Items)
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
data_adj <- left_join(tbl4_data, cpi_data, by = "year")

cpi_2010 <- cpi_data[cpi_data$year == 2010, ]$cpi

data_adj <- data_adj %>%
  mutate(adj_factor = cpi_2010 / cpi, 3)

data_adj <- data_adj %>%
  mutate(across(30:470, ~. * adj_factor))

data_adj <- data_adj %>%
  mutate(spending_per_recip = `total medicaid (mt + at)` / total_med_enr_old)

tbl4 <- data_adj %>%
  select(state, year, spending_per_recip)

tbl4 <- tbl4 %>%
  pivot_wider(
    names_from = year,
    values_from = spending_per_recip
  )

tbl4 <- tbl4 %>%
  mutate(across(c(`1991`, `2003`, `2009`), ~ as.numeric(gsub("[\\$,]", "", .))))

merged_data <- df %>%
  left_join(tbl4, by = "state")

merged_data <- merged_data %>%
  mutate(
    diff_1991 = `dh_1991` - `1991`,
    diff_2003 = `dh_2003` - `2003`,
    diff_2009 = `dh_2009` - `2009`
  ) %>%
  select(state, diff_1991, diff_2003, diff_2009)


### ------------------------------------------------------------------------ ###

data_91_05_copy <- data_91_05 %>%
  filter(year <= 2003) %>%
  filter(state != "Puerto Rico") %>%
  select(state, year, `total medicaid (mt + at)`) %>%
  rename(tam_spending = `total medicaid (mt + at)`)

new_merged_data_copy <- new_merged_data %>%
  filter(year >= 1991 & year <= 2003) %>%
  filter(state != "Puerto Rico") %>%
  select(state, year, `total medicaid (mt + at)`) %>%
  rename(my_spending = `total medicaid (mt + at)`)

joined_set <- full_join(new_merged_data_copy, 
                        data_91_05_copy, 
                        by = c("state", "year")) %>%
  mutate(consistent = my_spending == tam_spending,
         diff = my_spending - tam_spending)


ggplot(joined_set, aes(x = tam_spending, y = my_spending)) +
  geom_point(aes(color = consistent), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("blue", "red"), labels = c("Consistent", "Inconsistent")) +
  labs(title = "Scatterplot Comparing Spending (My Data vs Tamara's)",
       x = "Tamara's Data",
       y = "My Data",
       color = "Consistency") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_pub()





