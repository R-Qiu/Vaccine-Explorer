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
library(mefa)
library(magrittr)
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

load("cdc_data/NISPUF10.RData")
load("cdc_data/NISPUF11.RData")
load("cdc_data/NISPUF12.RData")
load("cdc_data/NISPUF13.RData")
load("cdc_data/NISPUF14.RData")
load("cdc_data/NISPUF15.RData")
load("cdc_data/NISPUF16.RData")

raw_10 <- as_tibble(NISPUF10)
raw_11 <- as_tibble(NISPUF11)
raw_12 <- as_tibble(NISPUF12)
raw_13 <- as_tibble(NISPUF13)
raw_14 <- as_tibble(NISPUF14)
raw_15 <- as_tibble(NISPUF15)
raw_16 <- as_tibble(NISPUF16)


# Bind all years into a masive tibble, with each one ID'd by year
# Left join with fips codes to ID states

raw <- 
  bind_rows("2010" = raw_10,
            "2011" = raw_11,
            "2012" = raw_12,
            "2013" = raw_13,
            "2014" = raw_14,
            "2015" = raw_15,
            "2016" = raw_16,
            .id = "year") %>% 
  mutate(year = as.integer(year)) %>% 
  
  mutate(state_code = as.numeric(STATE)) %>% 
  left_join(fips, by= "state_code")



# Read in insurance data with custom col names 
# Read in historical insurance figures
# First dataset ("Table 6") necessary for encoding Medicaid expansion
# Second dataset ("HIC-6") necessary for extending insurance data back to 2010
# Data from US Census Bureau
# Table 6 retrieved from https://www.census.gov/library/publications/2017/demo/p60-260.html
# Historical data retrieved from https://www.census.gov/data/tables/time-series/demo/health-insurance/historical-series/hic.html

raw_insurance_med_cols <- c("state", "medicaid", "per_2013", "margin_2013", "per_2014", "margin_2014", "per_2015", "margin_2015", "per_2016", "margin_2016", "remove_1", "remove_2", "remove_3", "remove_4")
raw_insurance_med <- 
  read_excel("census_data/table6.xls", skip = 12, col_names = raw_insurance_med_cols) 

raw_insurance_hist_cols <- c("state", "coverage", paste("per", 2017:2008, sep = "_"))
raw_insurance_hist <- 
  read_excel("census_data/hic04_acs.xls", skip = 4) %>% 
  fill.na()

insurance_hist <-
  raw_insurance_hist %>% 
  select(X__1, X__2, starts_with("Percent")) %>% 
  set_colnames(raw_insurance_hist_cols) %>% 
  mutate(state = str_to_lower(state),
         coverage = str_to_lower(coverage)) %>% 
  filter(coverage == "any coverage",
         state != "united states") %>% 
  
  # Tidy up data
  
  select(-coverage) %>% 
  gather(year, per_ins, -state) %>% 
  
  # Encode medicare coverage
  # Only need data for states before 2013 and after 2009
  # All of which have not expanded medicaid coverae
  
  mutate(year = as.integer(str_remove(year, "per_")),
         medicaid = "No",
         medicaid_num = 0) %>% 
  filter(year < 2013 & year > 2009)
  
  

# Tidies up the raw Census Bureau insurance data
# Recode Medicaid expansion by year
# "y" represents medicaid expansion on January 1st for year in question
# Medicaid expansion dates confirmed with timeline from Henry J Kaiser Family Foundation 
# https://www.kff.org/medicaid/issue-brief/status-of-state-medicaid-expansion-decisions-interactive-map/

insurance_med <- 
  raw_insurance_med %>% 
  
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
  # Medicaid_num column needed for animation
  # For some reason, animated plotly will not change color in an animation unless the variable is numeric
  
  mutate(expansion_year = case_when(medicaid == "y" ~ 2014,
                                    medicaid == "^y" ~ 2015,
                                    medicaid == "+y" ~ 2016),
         medicaid = case_when(year >= expansion_year ~ "Yes",
                              year < expansion_year ~ "No",
                              is.na(expansion_year) ~ "No"),
         medicaid = as.factor(medicaid),
         medicaid_num = recode(medicaid, "Yes" = 1,
                               "No" = 0))

expansion_year_lookup <- 
  insurance_med %>% 
  distinct(state, expansion_year)

# Combine Medicaid-expansion and historical data

ins <- 
  insurance_hist %>% 
  bind_rows(insurance_med) %>% 
  select(-expansion_year) %>% 
  left_join(expansion_year_lookup)


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




# Relabel vaccinations into either adequate, incomplete, or none
# Based on CDC recommended immunization schedule
# Found here: https://www.cdc.gov/vaccines/schedules/easy-to-read/child-easyread.html

vax_recode <-
  raw %>% 
  
  # Removes responses w/o adequate vaccine provider info (essentially, no response since survey is provider based)
  
  filter(PDAT == 1) %>% 
  
  mutate(
    # There exist 2- and 3-dose vaccines for Rotavirus
    # Rotarix (GlaxoSmithKline) is 2-dose, RotaTeq (Merck) is 3-dose
    # Assumed that 1st vaccination dose is indicative of remainder of given vaccine schedule
    # i.e. if the first dose type is "RM" for "ROTATEQ (MERCK)" then the remainder of 
    # Rotavirus vaccines are also from Merck
    # Unknown vaccine brands assumed to be 2-dose
    # All Rotavirus vaccines should be completed before 19 months, no need to account for child age
    
    rota = case_when(P_NUMROT == 0 ~ "none",
                     P_NUMROT == 1 ~ "incomplete",
                     P_NUMROT == 2 & XROTTY1 == "RM" ~ "incomplete",
                     P_NUMROT == 2 & XROTTY1 == "RG" ~ "adequate",
                     P_NUMROT == 2 & XROTTY1 == "RO" ~ "adequate",
                     P_NUMROT >= 3 ~ "adequate"),
    
    # There exist both 2- and 3-dose vaccines for Hep A, but based on Rotavirus dose-vaccine matching,
    # No need to distinguish by dose--everyone who takes 2+ doses completes the vaccine schedule
    # Therefore, in the survey data, 2 doses and up counted as "adequate"
    # The first dose should be given before 23 months, with a second dose within 6 months of the 1st dose
    # Benefit of the doubt is given
    # i.e. the most generous possible binning combinations of age and vaccination
    # Status are used
    
    hepa = case_when(AGEGRP == 1 ~ "adequate",
                     AGEGRP == 2 & P_NUMHEA == 0 ~ "none",
                     AGEGRP == 2 & P_NUMHEA >= 1 ~ "adequate",
                     AGEGRP == 3 & P_NUMHEA == 0 ~ "none",
                     AGEGRP == 3 & P_NUMHEA == 1 ~ "incomplete",
                     AGEGRP == 3 & P_NUMHEA >- 2 ~ "adequate"),
    
    # There exist both 2- and 3-dose vaccines for Hep B; 2 doses and up counted as "adequate"
    # All childhood HepB vaccines shold be completed by 18 months, no need to account for child age
    # HepB detailed schedule found here: https://www.cdc.gov/mmwr/volumes/67/rr/pdfs/rr6701-H.PDF
    
    hepb = case_when(P_NUMHEP == 0 ~ "none",
                     P_NUMHEP == 1 ~ "incomplete",
                     P_NUMHEP >= 2 ~ "adequate"),
    
    # 4 doeses of DTap recommended by 18 months, with next dose after 36 months
    # Therfore, no need to account for child age at time of survey
    
    dtap = case_when(P_NUMDTP == 0 ~ "none",
                     P_NUMDTP > 0 & P_NUMDTP < 4 ~ "incomplete",
                     P_NUMDTP >= 4 ~ "adequate"),
    
    # 3 doses of IPV (polio vaccine) are recommended by age 18 months, no need to account for child age
    
    polio = case_when(P_NUMPOL == 0 ~ "none",
                      P_NUMPOL == 1 ~ "incomplete",
                      P_NUMPOL == 2 ~ "incomplete",
                      P_NUMPOL >= 3 ~ "adequate"),
    
    # Only one dose of the MMR vaccine is recommended before 19 months, no need to account for child age
    
    mmr = case_when(P_NUMMMR == 0 ~ "none",
                    P_NUMMMR == 1 ~ "adequate"),
    
    # Only one dose of the varicella vaccine is recommended before 19 months, no need to account for child age
    
    vrc = case_when(P_NUMVRC == 0 ~ "none",
                    P_NUMVRC == 1 ~ "adequate"),
    
    # 4 doses of the PCV vaccine is recommended before age 19, with none until after age 65
    # Therefore, no need to account for child age
    
    pcv = case_when(P_NUMPCV == 0 ~ "none",
                    P_NUMPCV > 0 & P_NUMPCV < 4 ~ "incomplete",
                    P_NUMPCV >= 4 ~ "adequate"))



# Tidies up recoded vax data

vax_tidy <-
  vax_recode %>% 
  select(year, state, state_name, rota:pcv) %>% 
  gather(vaccine, status, -year, -state, -state_name) 


# Summarizes by proportion of vaccinations at each level by year
# Status kept at 3 levels (rather than summing "incomplete"" and "inadequate") to allow for greater flexibility
# i.e. this can always be done after the fact

vax_year <-
  vax_tidy %>% 
  group_by(year, state, state_name) %>% 
  count(vaccine, status) %>% 
  group_by(year, state, state_name, vaccine) %>% 
  mutate(per_vax = 100*n/sum(n)) %>% 
  ungroup() %>% 
  select(-n)


vax_factor <-
  vax_year %>%
  left_join(ins, by = c("state_name" = "state", "year")) %>% 
  left_join(pov, by = c("state_name" = "state", "year"))


write_rds(vax_factor, "App/vax_factor.rds")



