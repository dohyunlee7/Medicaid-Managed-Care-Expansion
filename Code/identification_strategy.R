library(dplyr)
library(haven)
library(datasets)
library(readxl)
library(pubtheme)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel.rds"))

# Get the 663 observations
new_merged_data <- new_merged_data %>%
  filter(state != "Puerto Rico",
         year <= 2003)

mandates <- readRDS(paste0(path, "/Temp/mandate_pcts_by_st_yr.rds"))

# Reformat state names
mandates$stname <- str_to_title(mandates$stname)
new_merged_data$state <- str_to_title(new_merged_data$state)

# Merge 1991-2003 panel with mandate data
main_data <- left_join(new_merged_data, mandates, by = c("state" = "stname",
                                                         "year"))

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
  scale_y_continuous(labels = scales::percent) +
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


### RELATIONSHIP BETWEEN MMC ENROLLMENT AND MEDICAID SPENDING ###
model1 <- lm(log(`total medicaid (mt + at)`) ~ factor(state) + factor(year) +
               log(total_med_enr) + pct_with_mandate + factor(state) * year,
             data = main_data)

summary(model1)

### EFFECT OF STATE AND LOCAL MANDATES ON MMC ENROLLMENT ##

# Column 0 (mu, sigma)
column0 <- main_data %>%
  summarise(
    # Mandatory MMC county
    mmc_mean = mean(pct_with_mandate, na.rm = TRUE),
    mmc_sd = sd(pct_with_mandate, na.rm = TRUE),
    
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


# Run model for specification 1:
# Percent in MC ~ % with mandate + state and year fixed effects
spec1 <- lm(pct_in_managed_care ~ pct_with_mandate + factor(state) + 
              factor(year), data = main_data)
summary(spec1)

# Mean = 0.487
# SD = 0.0257

# Run model for specification 2 (Missing data used for controls)
spec2 <- lm(pct_in_managed_care ~ pct_with_mandate + factor(state) +
              factor(year) + , data = main_data)


### ----------------------- New Treatment Definition ----------------------- ###

# Calculating jump of proportion of Comp MCO 10%, 20%
new_merged_data <- new_merged_data %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(
    pct_in_comp_mco = comprehensive_mco_enr / total_med_enr,
    change_comp_mco = pct_in_comp_mco - lag(pct_in_comp_mco),
    treatment_10 = ifelse(change_comp_mco > 0.10, 1, 0),
    treatment_20 = ifelse(change_comp_mco > 0.20, 1, 0)
  ) %>%
  ungroup()

unique_years <- unique(new_merged_data$year)

# Initialize empty df to store results
res <- data.frame(
  t_prime = integer(),
  r_squared = numeric()
)

# Loop through each possible t'
for (t_prime in unique_years) {
  
  # Indicator for current t'
  new_merged_data <- new_merged_data %>%
    mutate(treat = as.numeric(year >= t_prime))
  
  # Fit regression
  model <- lm(pct_in_comp_mco ~ treat, data = new_merged_data)
  
  # Extract R-squared and save result
  r_squared <- summary(model)$r.squared
  res <- rbind(res, data.frame(t_prime = t_prime, r_squared = r_squared))
}

# Get year with highest R-squared
best_t_prime <- res %>%
  filter(r_squared == max(r_squared)) %>%
  pull(t_prime)








