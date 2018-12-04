# Vaccine Explorer
# Format Data
# Richard Qiu



# Load packages
# Tidyverse MUST be loaded BEFORE Hmisc to avoid a function conflict with importing the data
# Tigris needed for FIPS codes
# Sjlabelled for dealing with labelled data

library(tigris)
library(sjlabelled)
library(readxl)
library(janitor)
library(stringr)
library(tidyverse)
library(Hmisc)



# Load in data for FIPS codes from tigris
# distinct() essentially removes the county codes from the dataset
# Make state_code numeric and state_name lowercase to facilitate joining with other data

data(fips_codes)

fips <- 
  fips_codes %>% 
  distinct(state, state_code, state_name) %>%
  mutate(state_code = as.numeric(state_code),
         state_name = str_to_lower(state_name))


# Load in CDC National Immunization Survey data
# From https://www.cdc.gov/vaccines/imz-managers/nis/about.html
# Retrieved November 2018
# Created from .DAT files in .R scripts in /format_data/ folder
# Convert each RData object into a tibble

load("cdc_data/NISPUF12.RData")
load("cdc_data/NISPUF13.RData")
load("cdc_data/NISPUF14.RData")
load("cdc_data/NISPUF15.RData")
load("cdc_data/NISPUF16.RData")

raw_12 <- as_tibble(NISPUF12)
raw_13 <- as_tibble(NISPUF13)
raw_14 <- as_tibble(NISPUF14)
raw_15 <- as_tibble(NISPUF15)
raw_16 <- as_tibble(NISPUF16)


# Bind all years into a masive tibble, with each one ID'd by year
# Left join with fips codes to ID states

raw <- 
  bind_rows("2012" = raw_12,
            "2013" = raw_13,
            "2014" = raw_14,
            "2015" = raw_15,
            "2016" = raw_16,
            .id = "year") %>% 
  mutate(year = as.integer(year)) %>% 
  
  mutate(state_code = as.numeric(STATE)) %>% 
  left_join(fips, by= "state_code")



# Read in insurance data with custom col names 
# Data from US Census Bureau
# Retrieved from Table 6 from https://www.census.gov/library/publications/2017/demo/p60-260.html

# Decision to use Census Bureau elaborated on in explore.Rmd
# In summary, through the CDC survey asks for insurance coverage, only households with adequate healthcare provider information count are valid responses in the survey.
# As may be expected, the CDC numbers therefore skew high, and Census data is used in lieu

raw_insurance_cols <- c("state", "medicaid", "per_2013", "margin_2013", "per_2014", "margin_2014", "per_2015", "margin_2015", "per_2016", "margin_2016", "remove_1", "remove_2", "remove_3", "remove_4")
raw_insurance_per <- 
  read_excel("census_data/table6.xls", skip = 12, col_names = raw_insurance_cols) 


# Tidies up the raw Census Bureau insurance data
# Recode Medicaid expansion by year
# "y" represents medicaid expansion on January 1st for year in question
# Medicaid expansion dates confirmed with timeline from Henry J Kaiser Family Foundation 
# https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions-interactive-map/

insurance_per <- 
  raw_insurance_per %>% 
  
  # Remove non-data rows
  
  remove_empty() %>% 
  slice(1:51) %>%
  
  # General formatting tweaks
  
  mutate(state = str_to_lower(str_remove_all(state, "[[:punct:]]")),
         medicaid = str_to_lower(medicaid)) %>% 
  select(-starts_with("remove"), -starts_with("margin")) %>% 
  
  # Make data tidy
  
  gather(year, per_ins, -state, -medicaid) %>% 
  mutate(year = str_remove(year, "per_"),
         year = as.integer(year),
         per_ins = 100 - per_ins) %>% 
  
  # Recode Medicaid info
  
  mutate(expansion_year = case_when(medicaid == "y" ~ 2014,
                                    medicaid == "^y" ~ 2015,
                                    medicaid == "+y" ~ 2016),
         medicaid = case_when(year >= expansion_year ~ "y",
                              year < expansion_year ~ "n",
                              is.na(expansion_year) ~ "n"),
         medicaid = as.factor(medicaid),
         medicaid_num = recode(medicaid, "y" = 1,
                               "n" = 0))


# Read in and tidy up historical poverty data from Census Bureau
# Source: https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-people.html
# Only percentages extracted from Table 21 from relevant years

raw_pov <-
  read_excel("census_data/histpov.xlsx")

pov <- 
  raw_pov %>% 
  rename(state = STATE) %>% 
  mutate(state = str_to_lower(state)) %>% 
  gather(year, per_pov, -state) %>% 
  mutate(year = as.integer(year))


