
#'
#' check_enrollment_diffs.R -- Check enrollment differences between Tamara's 
#'                             data and claims
#'

# # Identify which column names are the same
# common_cols <- as.data.frame(intersect(colnames(data_91_05), colnames(data)))
#
# # Identify which column names are distinct from each other
# not_in_df2 <- as.data.frame(setdiff(colnames(data_91_05), colnames(data)))

### Get difference by state and weighted average (For Ruochen) ###
### Goal: Making sure the data for 2006-2007 Medicaid enrollment pulled from
###       the claims is reliable given we don't have 2006-7 reports
df2 <- read_csv(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/claims/enrollment_2011.csv"))

names(df2) <- tolower(names(df2))

# Convert state abbreviations to full name
df2$state <- state.name[match(df2$state, state.abb)]

# Select state name and total MC enrollment
df2 <- df2 %>%
  select(state, num_bene_id_any_el_days)

df1 <- new_merged_data %>%
  filter(year == 2011) %>%
  select(state,
         year,
         total_med_enr)

df3 <- left_join(df1, df2, by = c("state"))

df3 <- df3 %>%
  filter(!state %in% c("District of Columbia", "Puerto Rico"))

# Get magnitude of the difference of managed care enrollment
df3$diff <- (df3$num_bene_id_any_el_days - df3$total_med_enr) / df3$total_med_enr

weighted_mean <- weighted.mean(df3$diff, df3$total_med_enr)

p <- ggplot(df3, aes(x = diff)) +
  geom_histogram(binwidth = 0.05, fill = "darkgray") +
  geom_vline(xintercept = weighted_mean,
             linetype = "dashed",
             color = "brown") +
  annotate("text",
           x = weighted_mean, 
           y = 9,
           label = paste("Weighted Mean:", round(weighted_mean, 3)),
           color = "brown", 
           hjust = -0.1) +
  annotate("text",
           x = df3$diff[df3$state == "California"], 
           y = 1.5,
           label = "CA") +
  labs(x = "Difference in Total Enrollment (%)",
       y = "Number of States",
       title = "Difference in Total Enrollment",
       subtitle = "2011",
       caption = "Note: x is calculated by (Total Enrollment from Claims - Total Reported Enrollment) / Total Reported Enrollment") +
  theme_minimal()

ggsave(paste0(path, "/Output/mmc_diff_hist_tot.png"),
       plot = p,
       width = 12,
       height = 8,
       dpi = 300)


# weighted_avg <- sum(df3$diff * df3$total_enrollment) / sum(df3$total_enrollment)
# write_csv(df3, file = paste0(path, "/Temp/enrollment_with_diffs.csv"))

### ------- Checking for enrollment difference for any MCO enrollment ------ ###

tbl1 <- read_csv(paste0(path, "/Input_Data", 
                        "/Medicaid_managedcare_enrollment_report", 
                        "/program_summary_from_report", 
                        "/data_2018.csv"))

tbl1 <- tbl1 %>%
  mutate(state = gsub("[0-9]+$", "", state)) %>%
  select(-notes)

tbl1 <- tbl1 %>%
  mutate(across(-state, ~ gsub(",", "", .))) %>% # Remove commas from all except 'state'
  mutate(across(-state, ~ na_if(., "--"))) %>% # Replace '--' with NA (excluding 'state')
  mutate(across(-state, ~ na_if(., "n/a"))) %>% # Replace 'n/a' with NA (excluding 'state')
  mutate(across(-state, as.numeric))

ter <- c("TOTALS", 
         "American Samoa",
         "Guam", 
         "Northern Mariana Islands",
         "Virgin Islands",
         "Puerto Rico")

tbl1 <- tbl1 %>% 
  filter(!state %in% ter)

tbl1 <- tbl1 %>% 
  select(state, total_medicaid_enrollment_in_any_type_of_managed_care)

tbl2 <- read_csv(paste0(path, "/Input_Data", 
                        "/Medicaid_managedcare_enrollment_report", 
                        "/by_program_pop_from_report", 
                        "/data_2018.csv"))

tbl2 <- tbl2 %>%
  mutate(state = gsub("[0-9]+$", "", state)) %>%
  select(-notes, -year, -total_medicaid_enrollees)

tbl2 <- tbl2 %>%
  mutate(across(-state, ~ gsub(",", "", .))) %>% # Remove commas from all except 'state'
  mutate(across(-state, ~ na_if(., "--"))) %>% # Replace '--' with NA (excluding 'state')
  mutate(across(-state, ~ na_if(., "n/a"))) %>% # Replace 'n/a' with NA (excluding 'state')
  mutate(across(-state, as.numeric))

numeric_cols <- sapply(tbl2, is.numeric)

tbl2$any_mco <- rowSums(tbl2[, numeric_cols], na.rm = TRUE)
tbl2$new_any_mco <- rowSums(tbl2[, c("comprehensive_mco_with_or_without_mltss",
                                     "pccm",
                                     "pccm_entity",
                                     "mltss_only",
                                     "other")], na.rm = TRUE)

tbl2 <- tbl2 %>% 
  filter(!state %in% ter)

tbl2 <- tbl2 %>%
  select(state, new_any_mco)

tbl3 <- left_join(tbl1, tbl2, by = "state")

# Get magnitude of the difference of any managed care enrollment
tbl3$diff <- (tbl3$new_any_mco - tbl3$total_medicaid_enrollment_in_any_type_of_managed_care) / 
  tbl3$total_medicaid_enrollment_in_any_type_of_managed_care

# df3$total_enrollment <- df3$`managed care enrollment, as of june 30` +
#   df3$total_mmc_enrollment

weighted_mean <- weighted.mean(tbl3$diff, 
                               tbl3$total_medicaid_enrollment_in_any_type_of_managed_care)

p <- ggplot(tbl3, aes(x = diff)) +
  geom_histogram(binwidth = 0.05, fill = "darkgray") +
  geom_vline(xintercept = weighted_mean,
             linetype = "dashed",
             color = "brown") +
  annotate("text",
           x = weighted_mean, 
           y = 9,
           label = paste("weighted mean:", round(weighted_mean, 3)),
           color = "brown", 
           hjust = -0.1) +
  annotate("text",
           x = tbl3$diff[tbl3$state == "Idaho"], 
           y = 1.5,
           label = "ID") +
  labs(x = "Difference in Any MC enrollment (%)",
       y = "Number of states",
       title = "Difference in Any MC Enrollment",
       caption = "Note: x is calculated by (Sum of Program Types (Table 2) - Any MCO (Table 1)) / Any MCO (Table 1)") +
  theme_minimal()

x <- read_dta(paste0(path, "/Input_Data/Medicaid_managedcare_enrollment_report/external/fymcdben.dta"))










