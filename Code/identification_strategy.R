library(dplyr)
library(haven)
library(datasets)
library(readxl)
library(pubtheme)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

new_merged_data <- readRDS(paste0(path, "/Temp/new_merged_panel3.rds"))

mandates <- readRDS(paste0(path, "/Temp/mandate_pcts_by_st_yr_expanded.rds"))

# Reformat state names to title case
mandates$stname <- str_to_title(mandates$stname)
new_merged_data$state <- str_to_title(new_merged_data$state)

# Get the 663 observations (for just specification replication)
new_merged_data <- new_merged_data %>%
  filter(state != "Puerto Rico",
         year <= 2003)

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

### EFFECT OF STATE AND LOCAL MANDATES ON MMC ENROLLMENT ###

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


### ----------------------- Jump in Prop. of MMC --------------------------- ###
new_merged_data <- new_merged_data %>%
  mutate(pct_in_comp_mco = ifelse(pct_in_comp_mco > 1, 
                                      1, 
                                      pct_in_comp_mco))

unique_states <- unique(new_merged_data$state)
output_dir <- file.path(path, "Output", "state_plots")

# Calculate the magnitude of the jump in any MMC enrollment
jumps <- new_merged_data %>%
  group_by(state) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    mmc_jump = pct_in_comp_mco - lag(pct_in_comp_mco),
    mmc_lag = lag(pct_in_comp_mco)
  ) %>%
  filter(!is.na(mmc_jump)) %>%
  summarise(
    max_jump = max(mmc_jump, na.rm = TRUE),
    treatment_year = ifelse(max_jump > 0,
                            year[which.max(mmc_jump)],
                            NA)
  ) %>%
  ungroup()

new_merged_data_temp <- new_merged_data %>%
  left_join(jumps, by = "state")

saveRDS(jumps, file = paste0(path, "/Temp/jumps.rds"))

# Calculate increase in mandate for treatment year
mandates2 <- left_join(mandates, jumps, by = c("stname" = "state"))
mandates3 <- mandates2 %>% select(-max_jump, -treatment_year)

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
              pct_with_mandate[year == (treatment_year - 1)]) %>%
  ungroup()

# Remove treatment year to avoid repeat column
res <- res %>% select(-treatment_year)

# Join % change in MMC for treatment year onto panel
new_merged_data_temp <- new_merged_data_temp %>%
  left_join(res, by = c("state" = "stname"))

# Any MCO Enrollment
for (st in unique_states) {
  state_data <- new_merged_data_temp %>%
    filter(state == st) %>%
    mutate(pct_in_managed_care = as.numeric(pct_in_managed_care),
           pct_with_mandate = as.numeric(pct_with_mandate))
  
  y_max <- state_data %>%
    filter(year >= (treatment_year - 1) & year <= (treatment_year + 1)) %>%
    summarize(max_y = max(pct_in_managed_care)) %>%
    pull(max_y)
  
  p <- ggplot(state_data) +
    geom_line(aes(x = year, 
                  y = pct_in_managed_care, 
                  color = "Share of MMC Enrollment"),
              linewidth = 1) +
    geom_line(data = state_data %>% filter(year <= 2001),
              aes(x = year, 
                  y = pct_with_mandate, 
                  color = "Share of Counties with MMC Mandate"),
              linewidth = 1) +
    geom_line(data = state_data %>% filter(year >= 2001),
              aes(x = year, 
                  y = pct_with_mandate, 
                  color = "Share of Counties with MMC Mandate"),
              linewidth = 1,
              linetype = "dashed") +
    labs(title = paste0("Share of Managed Care Enrollment: ", st),
         subtitle = "1991-2022",
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
             label = paste0("Jump Magnitude: ", 
                            scales::percent(max(state_data$max_jump))),
             size = 4,
             hjust = 0,
             color = pubblue) +
    annotate("text",
             x = state_data$treatment_year + 1,
             y = y_max - 0.15,
             label = paste0("Prop. Jump in Mandate (Treat Yr): ", 
                            scales::percent(state_data$pct_with_mandate_change)),
             size = 4,
             hjust = 0,
             color = pubred) +
    scale_color_manual(values = c("Share of MMC Enrollment" = pubblue,
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

# Comprehensive risk-based managed care
for (st in unique_states) {
  state_data <- new_merged_data_temp %>%
    filter(state == st) %>%
    mutate(pct_in_comp_mco = as.numeric(pct_in_comp_mco),
           pct_with_mandate = as.numeric(pct_with_mandate))
  
  y_max <- state_data %>%
    filter(year >= (treatment_year - 1) & year <= (treatment_year + 1)) %>%
    summarize(max_y = max(pct_in_comp_mco)) %>%
    pull(max_y)
  
  p <- ggplot(state_data) +
    geom_line(aes(x = year, 
                  y = pct_in_comp_mco, 
                  color = "Share of Comp. Risk-Based MMC Enrollment"),
              linewidth = 1) +
    geom_line(data = state_data %>% filter(year <= 2001),
              aes(x = year, 
                  y = pct_with_mandate, 
                  color = "Share of Counties with MMC Mandate"),
              linewidth = 1) +
    geom_line(data = state_data %>% filter(year >= 2001),
              aes(x = year, 
                  y = pct_with_mandate, 
                  color = "Share of Counties with MMC Mandate"),
              linewidth = 1,
              linetype = "dashed") +
    labs(title = paste0("Share of Comprehensive Risk-Based Managed Care Enrollment: ", st),
         subtitle = "Mandate: 1991-2001, CRB MC: 1995-2022",
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
             label = paste0("Jump Magnitude: ", 
                            scales::percent(max(state_data$max_jump))),
             size = 4,
             hjust = 0,
             color = pubblue) +
    annotate("text",
             x = state_data$treatment_year + 1,
             y = y_max - 0.15,
             label = paste0("Prop. Jump in Mandate (Treat Yr): ", 
                            scales::percent(state_data$pct_with_mandate_change)),
             size = 4,
             hjust = 0,
             color = pubred) +
    scale_color_manual(values = c("Share of Comp. Risk-Based MMC Enrollment" = pubblue,
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
    subtitle = "Treatment year ranges from 1995 - 2022",
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

### --------- Re-replicating Table 6 with Okay-State Subset -------- ###
okay_states <- c("Alabama", "Alaska", "Colorado", "Connecticut", "Delaware",
                 "Hawaii", "Indiana", "Iowa", "Louisiana", "Maryland", 
                 "Massachusetts", "Michigan", "Missouri", "Nebraska", "Nevada",
                 "New Hampshire", "New Jersey", "North Dakota", "Oregon",
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
                 "Tennessee", "Washington")

okay_states_data <- new_merged_data_temp %>%
  filter(state %in% okay_states) %>%
  filter(year %in% 1991:2003)

new_spec1 <- lm(pct_in_managed_care ~ pct_with_mandate + factor(state) + 
              factor(year), data = okay_states_data)
summary(new_spec1)

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










