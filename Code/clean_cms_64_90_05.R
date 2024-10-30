#'
#' clean_cms_64_90_05.R -- clean the 1990-2005 Stata files
#'

library(dplyr)
library(haven)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

# Read in 1990-2005 CMS 64 reports
fmr90_05_2 <- read_dta(paste0(path, "/Input_Data/CMS_64_report/fmr90-05_2.dta"))

# Omit territories/misc.
temp <- fmr90_05_2 %>%
  filter(year %in% 1991:2005,
         !statename %in% c("American Samoa",
                           "Guam",
                           "Northern Mariana Islands",
                           "Puerto Rico",
                           "Virgin Islands",
                           "x National Totals",
                           "Massachusetts Blind"))

# Hard code enrollment numbers and percentages -- error with decimal placement
temp[temp$statename == "Delaware" & temp$year == 1996, ]$mdcdenroll <- 73798
temp[temp$statename == "Wyoming" & temp$year == 1996, ]$mdcdenroll <- 38956

temp[temp$statename == "Delaware" & temp$year == 1996, ]$percenthmo <- 
  temp[temp$statename == "Delaware" & temp$year == 1996, ]$hmoenroll /
  temp[temp$statename == "Delaware" & temp$year == 1996, ]$mdcdenroll

temp[temp$statename == "Wyoming" & temp$year == 1996, ]$percenthmo <-
  temp[temp$statename == "Wyoming" & temp$year == 1996, ]$hmoenroll /
  temp[temp$statename == "Wyoming" & temp$year == 1996, ]$mdcdenroll

# De-duplicate state-year rows
temp <- temp %>%
  filter(!is.na(gdp),
         !is.na(respop),
         !is.na(gdpcapita),
         !is.na(fmap),
         !is.na(numpoor),
         !is.na(povrate))

var_labels <- sapply(temp, function(x) attr(x, "label"))

new_colnames <- ifelse(!is.na(var_labels), var_labels, names(df))

colnames(temp) <- new_colnames

# Save
saveRDS(temp, file = paste0(path, "/Temp/data_1991_2005.rds"))














