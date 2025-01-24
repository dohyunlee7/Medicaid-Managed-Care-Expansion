#'
#' mandate_data_prepper.R -- Consolidate all the CMS-64 dataframes and Medicaid 
#'                           enrollment into one panel dataset
#'                        

library(dplyr)
library(haven)
library(readxl)
library(tidyverse)

path <- file.path("D:", "Groups", "YSPH-HPM-Ndumele", "Networks", "Dohyun",
                  "medicaid_privatization_exp")

# Read in county level mandate level
mandate <- read_dta(paste0(path, file.path("/Input_Data",
                                           "Medicaid_managedcare_enrollment_report",
                                           "external",
                                           "uimmc.dta")))


# Read in county-level Census population data for 1990-2023
census_1990 <- read_excel(paste0(path, file.path("/Input_Data",
                                                 "Medicaid_managedcare_enrollment_report",
                                                 "census",
                                                 "census1990_2000.xlsx")))

census_2000 <- read_csv(paste0(path, file.path("/Input_Data",
                                               "Medicaid_managedcare_enrollment_report",
                                               "census",
                                               "co-est00int-tot.csv")))

census_2010 <- read_csv(paste0(path, file.path("/Input_Data",
                                               "Medicaid_managedcare_enrollment_report",
                                               "census",
                                               "co-est2019-alldata.csv")))

census_2020 <- read_excel(paste0(path, file.path("/Input_Data",
                                                 "Medicaid_managedcare_enrollment_report",
                                                 "census",
                                                 "co-est2023-pop.xlsx")))

# Read in file with info on when to extend the mandate for each state
mandate_ext <- read_excel(paste0(path, file.path("/Input_Data",
                                                 "Medicaid_managedcare_enrollment_report",
                                                 "external",
                                                 "mandate_tracker.xlsx")))

census_1990 <- census_1990[, -(14:18)]

names(census_1990) <- tolower(names(census_1990))

# Add 0 in front of the FIPS codes that are supposed to have 0's in front
census_1990$fipscode <- gsub("^([0-9]{4})$", "0\\1", census_1990$fipscode)

census_1990 <- census_1990 %>%
  mutate(stname = ifelse(ctyname %in% state.name, ctyname, NA)) %>%
  fill(stname, .direction = "down")

census_1990 <- census_1990 %>%
  select(fipscode, 
         ctyname, 
         stname, 
         estimatesbase1990,
         popestimate1990,
         popestimate1991,
         popestimate1992,
         popestimate1993,
         popestimate1994,
         popestimate1995,
         popestimate1996,
         popestimate1997,
         popestimate1998,
         popestimate1999)

census_1990$ctyname <- tolower(census_1990$ctyname)
census_1990$stname <- tolower(census_1990$stname)

census_1990$ctyname <- gsub("-", " ", census_1990$ctyname)

census_1990 <- census_1990 %>%
  filter(!is.na(ctyname))


# Lower case variable names
names(census_2000) <- tolower(names(census_2000))

# Select variables
# census_2000 <- census_2000 %>%
#   select(stname, ctyname, popestimate2000)

census_2000$ctyname <- tolower(census_2000$ctyname)
census_2000$stname <- tolower(census_2000$stname)

census_2000$ctyname <- gsub("-", " ", census_2000$ctyname)

census_2000 <- census_2000 %>%
  select(ctyname, 
         stname, 
         popestimate2000, 
         popestimate2001,
         popestimate2002,
         popestimate2003,
         popestimate2004,
         popestimate2005,
         popestimate2006,
         popestimate2007,
         popestimate2008,
         popestimate2009)

# Manipulating 2010-2019 population
names(census_2010) <- tolower(names(census_2010))

# Error reading because of special character (tilde n)
census_2010[census_2010$state == "35" & census_2010$county == "013", ]$ctyname <-
  "Dona Ana County"

census_2010$ctyname <- tolower(census_2010$ctyname)
census_2010$stname <- tolower(census_2010$stname)

census_2010$ctyname <- gsub("-", " ", census_2010$ctyname)

census_2010 <- census_2010 %>%
  select(stname, 
         ctyname, 
         popestimate2010,
         popestimate2011,
         popestimate2012,
         popestimate2013,
         popestimate2014,
         popestimate2015,
         popestimate2016,
         popestimate2017,
         popestimate2018,
         popestimate2019)

# Joining 
census_data <- left_join(census_1990, census_2000, by = c("stname", "ctyname"))
census_data <- left_join(census_data, census_2010, by = c("stname", "ctyname"))

# Adding on 2020-2023 county populations
census_2020$ctyname <- tolower(census_2020$ctyname)
census_2020$stname <- tolower(census_2020$stname)
census_2020$ctyname <- gsub("-", " ", census_2020$ctyname)

census_2020 <- census_2020 %>%
  select(ctyname, stname, popestimate2020, popestimate2021, popestimate2022)

census_data <- left_join(census_data, census_2020, by = c("stname", "ctyname"))

# Delete DC rows for state code (duplicate)
census_data <- census_data %>%
  filter(fipscode != "11")

# Add " county" at the end of string if it doesn't contain "State of" or DC
mandate$county97 <- ifelse(
  grepl("State of|DISTRICT OF COLUMBIA", mandate$county97),
  mandate$county97,
  paste0(mandate$county97, " county")
)

# Lower case county and state names
mandate$county97 <- tolower(mandate$county97)
mandate$stname97 <- tolower(mandate$stname97)

# Louisiana's counties are called parishes
mandate$county97 <- ifelse(
  mandate$stname97 == "louisiana",
  gsub(" county", " parish", mandate$county97),
  mandate$county97
)

# Omit the "State of" tag in front of state names
mandate$county97 <- gsub("state of ", "", mandate$county97)

mandate$stname97 <- gsub("dc", "district of columbia", mandate$stname97)

# Add on extra mandate data for counties/states that had a mandate past 2001
# Additional data cleaning 
names(mandate_ext) <- tolower(names(mandate_ext))
names(mandate_ext) <- gsub("%", "", names(mandate_ext))
names(mandate_ext) <- gsub(" ", "_", names(mandate_ext))

mandate_ext$year_expanded_post_dh_mandate_data <- gsub("NA", NA, mandate_ext$year_expanded_post_dh_mandate_data)
mandate_ext$year_expanded_post_dh_mandate_data <- as.numeric(mandate_ext$year_expanded_post_dh_mandate_data)

mandate_ext$state <- tolower(mandate_ext$state)

mandate_ext <- mandate_ext %>%
  select(state, year_expanded_post_dh_mandate_data)

mandate <- left_join(mandate, mandate_ext, by = c("stname97" = "state"))

# Extend mandates for 2002 - 2022
### *Note: I'm still waiting on a response from CA, CO, MN, NY, PA
new_rows <- mandate %>%
  group_by(stname97, county97) %>%
  filter(year == 2001) %>%
  mutate(year = list(2002:2022)) %>%
  unnest(year) %>%
  bind_rows(mandate) %>%
  arrange(stname97, county97, year) %>%
  group_by(stname97, county97) %>%
  mutate(
    year_expanded_post_dh_mandate_data = ifelse(
      is.na(year_expanded_post_dh_mandate_data),
      9999,
      year_expanded_post_dh_mandate_data),
    
    hmom = ifelse(
      hmom == 0 & year >= year_expanded_post_dh_mandate_data,
      1,
      hmom),
    
    nommc = ifelse(
      hmom == 1 & year >= year_expanded_post_dh_mandate_data,
      0,
      nommc
    )
    ) %>%
  mutate(
    hmom = ifelse(stname97 == "connecticut" & year >= 2012, 0, hmom),
    hmom = ifelse(stname97 == "oklahoma" & year >= 2004, 0, hmom),
    hmom = ifelse(stname97 == "vermont" & year >= 2001, 0, hmom))

write_csv(new_rows, paste0(path, "/Temp/new_mandate_data.csv"))

mandate_pop <- left_join(census_data, 
                         new_rows, 
                         by = c("fipscode" = "fips97", 
                                #"stname" = "stname97",
                                "ctyname" = "county97"))

# Remove Yellowstone National Park, which likely doesn't have a human population
mandate_pop <- mandate_pop %>%
  filter(fipscode != 30111) %>%
  filter(year != 1990)%>%
  select(-estimatesbase1990, -popestimate1990)


# Create dummy for inverse of "no MMC" (better readability)
mandate_pop$mmc <- mandate_pop$nommc - 1
mandate_pop$mmc <- mandate_pop$mmc * -1


mandate_pop <- mandate_pop %>%
  mutate(
    popestimate = case_when(
      year == 1991 ~ popestimate1991,
      year == 1992 ~ popestimate1992,
      year == 1993 ~ popestimate1993,
      year == 1994 ~ popestimate1994,
      year == 1995 ~ popestimate1995,
      year == 1996 ~ popestimate1996,
      year == 1997 ~ popestimate1997,
      year == 1998 ~ popestimate1998,
      year == 1999 ~ popestimate1999,
      year == 2000 ~ popestimate2000,
      year == 2001 ~ popestimate2001,
      year == 2002 ~ popestimate2002,
      year == 2003 ~ popestimate2003,
      year == 2004 ~ popestimate2004,
      year == 2005 ~ popestimate2005,
      year == 2006 ~ popestimate2006,
      year == 2007 ~ popestimate2007,
      year == 2008 ~ popestimate2008,
      year == 2009 ~ popestimate2009,
      year == 2010 ~ popestimate2010,
      year == 2011 ~ popestimate2011,
      year == 2012 ~ popestimate2012,
      year == 2013 ~ popestimate2013,
      year == 2014 ~ popestimate2014,
      year == 2015 ~ popestimate2015,
      year == 2016 ~ popestimate2016,
      year == 2017 ~ popestimate2017,
      year == 2018 ~ popestimate2018,
      year == 2019 ~ popestimate2019,
      year == 2020 ~ popestimate2020,
      year == 2021 ~ popestimate2021,
      year == 2022 ~ popestimate2022
    ),
    # Apply the multipliers
    hmom_pop = hmom * popestimate,
    hmov_pop = hmov * popestimate,
    pccmm_pop = pccmm * popestimate,
    pccmv_pop = pccmv * popestimate,
    mmc_pop = mmc * popestimate,
    hmo = ifelse(hmom == 1 | hmov == 1, 1, 0),
    hmopop = hmo * popestimate,
    pccmm_only_pop = pccmm_only * popestimate,
    onlyvol_pop = onlyvol * popestimate,
    mandhmo_pop = mandhmo * popestimate,
    mixedmand_pop = mixedmand * popestimate
  )

mandate_pop <- mandate_pop %>%
  distinct()


# Constructing percentages with MMC/HMO mandate Table 1
mp_agg <- mandate_pop %>%
  group_by(year) %>%
  summarise(
    pop = sum(popestimate2000, na.rm = TRUE),
    pop_with_pccm_only = sum(pccmm_only_pop, na.rm = TRUE),
    pop_with_mandhmo = sum(mandhmo_pop, na.rm = TRUE),
    pop_with_mixedmand = sum(mixedmand_pop, na.rm = TRUE),
    pct_with_pccm_only = pop_with_pccm_only / pop,
    pct_with_mandhmo = pop_with_mandhmo / pop,
    pct_with_mixedmand = pop_with_mixedmand / pop
  )

# How Tamara constructed the percent residing in county with MMC mandate
mp_agg <- mp_agg %>%
  mutate(pct_with_mandate = scales::percent(pct_with_pccm_only + 
                                              pct_with_mandhmo + 
                                              pct_with_mixedmand, 
                                            accuracy = 0.1),
         pct_with_mandhmo = scales::percent(pct_with_mandhmo, 
                                            accuracy = 0.1)) %>%
  select(year, pct_with_mandate, pct_with_mandhmo)

# Constructing percentages with MMC mandate Table 2
mp_agg_tbl2 <- mandate_pop %>%
  group_by(stname, year) %>%
  summarise(
    # Total population
    pop = sum(popestimate, na.rm = TRUE),
    
    # Population by different mandate types
    pop_with_pccm_only = sum(pccmm_only_pop, na.rm = TRUE),
    pop_with_mandhmo = sum(mandhmo_pop, na.rm = TRUE),
    pop_with_mixedmand = sum(mixedmand_pop, na.rm = TRUE),
    
    # Percent of different mandate types
    pct_with_pccm_only = pop_with_pccm_only / pop,
    pct_with_mandhmo = pop_with_mandhmo / pop,
    pct_with_mixedmand = pop_with_mixedmand / pop,
    
    # Percent of population in mandatory MMC county (D&H definition)
    pct_with_mandate = pct_with_mandhmo + pct_with_mixedmand + pct_with_pccm_only,
    
    # Percent of population in mandatory MMC county (No PCCM)
    pct_with_crb_mandate = pct_with_mandhmo + pct_with_mixedmand
  ) %>%
  select(stname, 
         year, 
         pct_with_mandate, 
         pct_with_mandhmo,
         pct_with_pccm_only,
         pct_with_mixedmand,
         pct_with_crb_mandate)

# Manually changing mandate pcts for Connecticut 2012 onward
# mp_agg_tbl2 <- mp_agg_tbl2 %>%
#   mutate(pct_with_crb_mandate = ifelse(stname == "connecticut" & year >= 2012, 
#                                        0,
#                                        pct_with_crb_mandate),
#          pct_with_crb_mandate = ifelse(stname == "oklahoma" & year >= 2004, 
#                                        0,
#                                        pct_with_crb_mandate))


saveRDS(mp_agg_tbl2, paste0(path, "/Temp/mandate_pcts_by_st_yr_expanded.rds"))
write_csv(mp_agg_tbl2, paste0(path, "/Temp/mandate_pcts_by_st_yr_expanded.csv"))
