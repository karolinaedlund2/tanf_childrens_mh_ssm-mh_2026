################################################################################
################################################################################
## Program: TANF_datamgmt2_02.01.26_10.14.26
## Last updated: Friday, October 16, 2024
## Programmer: Karolina Edlund

## Purpose: This program pulls in data for our state level control variables
   # State minimum wage data come from the "UKCPR_complete" dataset
          # Source: The University of Kentucky Center for Poverty Research's National Welfare Data
   # State Medicaid income eligibility limit data for children ages 1-5 come from Nolan's "Medicaid eligibility, ages 1-5, state-year, raw.csv" dataset
          # Source: The Kaiser Family Foundation
   # State Medicaid income eligibility limit data for children ages 6-18 come from Nolan's "Medicaid eligibility, ages 1-5, state-year, raw.csv" dataset
          # Source: The Kaiser Family Foundation
   # State EITC data come from Nolan's "EITC policies, state-year, clean.tab" dataset
          # Source: The Urban Institute and Brookings Institution's Tax Policy Center

  # This program merges the four datasets listed above ("ukcpr", "medicaid_1_5_df", "medicaid_6_18_df", and "eitc")
  # It then creates a 3-level variable that combines information on whether each state has an EITC and, if so, whether that EITC is refundable or not
  # The resulting clean merged dataset "state_level_controls" is subsequently saved to Karolina Edlund's /data folder (working directory path shown below)
################################################################################
################################################################################

# Load packages
library(readxl) # For reading in .xlsx and .xls files
library(dplyr)
library(tidyr)
library(readr) # For reading in .csv files

#Set working directory
getwd()
setwd("/Users/karolinaedlund/Desktop/HSPH/Research/HSPH/FINAL TANF MANUSCRIPT/TANF FOR SUBMISSION/SUBMISSION/Revise and Resubmit/data/")

#############################################################################
###################### Read in UKPCR minumum wage data ######################
#############################################################################

# Read in UKCPR_complete data
# Source: University of Kentucky Center for Poverty Research's National Welfare Data - https://ukcpr.uky.edu/resources
ukcpr_complete <- read_excel("UKCPR_complete.xlsx")

# Restrict to variables of interest
ukcpr <- ukcpr_complete %>%
  select(state_name, state_fips, year, minwage_state)

# Rename state_name to st_abbr
ukcpr <- ukcpr %>%
  rename(st_abbr = state_name)

# Add a column of state names to the left of the state abbreviations column
ukcpr <- ukcpr %>%
  mutate(
    state = case_when(
      st_abbr == "DC" ~ "District of Columbia",
      TRUE ~ state.name[match(st_abbr, state.abb)]
    ),
    .before = st_abbr
  )

# Rename columns 
colnames(ukcpr)  <- c("state", "st_abbr", "fipsst", "year", "state_minwage")

# Restrict ukcpr datasets to 2016-2022
ukcpr <- ukcpr %>%
  filter(year >= 2016 & year <= 2022)

##############################################################
################## Read in Nolan's Medicaid data #############
##############################################################

# Read Nolan's Medicaid datasets into R
# Source: The Kaiser Family Foundation - https://www.kff.org/data-collections/trends-in-medicaid-income-eligibility-limits/
medicaid_1_5_df  <- read.csv("Medicaid eligibility, ages 1-5, state-year, raw.csv")
medicaid_6_18_df <- read.csv("Medicaid eligibility, ages 6-18, state-year, raw.csv")

# Restructure data to long format
library(reshape2)
medicaid_1_5_df  <- melt(medicaid_1_5_df, id.vars = c("X"),
                         variable.name = "year", value.name = "elig_1_5")
medicaid_6_18_df <- melt(medicaid_6_18_df, id.vars = c("X"),
                         variable.name = "year", value.name = "elig_6_18")

# Clean year variable
# Drop leading "X" in strings
medicaid_1_5_df$year  <- substring(medicaid_1_5_df$year,  2)
medicaid_6_18_df$year <- substring(medicaid_6_18_df$year, 2)

# Rename columns
colnames(medicaid_1_5_df)  <- c("state", "year", "percent_1to5")
colnames(medicaid_6_18_df) <- c("state", "year", "percent_6to18")

# Add a column of abbreviations to the right of the state names column
medicaid_1_5_df <- medicaid_1_5_df %>%
  mutate(
    st_abbr = case_when(
      state == "District of Columbia" ~ "DC",
      TRUE ~ state.abb[match(state, state.name)]
    ),
    .after = state)

medicaid_6_18_df <- medicaid_6_18_df %>%
  mutate(
    st_abbr = case_when(
      state == "District of Columbia" ~ "DC",
      TRUE ~ state.abb[match(state, state.name)]
    ),
    .after = state)

# Convert names to FIPS
# Necessary for smooth merge
medicaid_1_5_df$fipsst  <- cdlTools::fips(medicaid_1_5_df$state,  to="FIPS")
medicaid_6_18_df$fipsst <- cdlTools::fips(medicaid_6_18_df$state, to="FIPS")

# Treat year as numeric
medicaid_1_5_df$year  <- as.numeric(medicaid_1_5_df$year)
medicaid_6_18_df$year <- as.numeric(medicaid_6_18_df$year)

# Restrict Medicaid datasets to 2016-2022
medicaid_1_5_df <- medicaid_1_5_df %>%
  filter(year >= 2016 & year <= 2022)

medicaid_6_18_df <- medicaid_6_18_df %>%
  filter(year >= 2016 & year <= 2022)

##############################################################
################## Read in Nolan's EITC data #################
##############################################################

# Read Nolan's EITC dataset into R
# Source: The Urban Institute and Brookings Institution's Tax Policy Center - https://taxpolicycenter.org/statistics/state-eitc-percentage-federal-eitc
# When reading in .tab dataset, it does include 2022
eitc <- read_tsv("EITC policies, state-year, clean.tab")

#Restrict eitc dataset to 2016-2022
eitc <- eitc %>%
  filter(year >= 2016 & year <= 2022)

##########################################################################
############# Merge minimum wage, Medicaid, and EITC datasets ############
##########################################################################

# Merge ukcpr, medicaid_1_5_df, medicaid_6_18_df, and eitc datasets together
# The left_join function only allows us to merge 2 datasets at a time
state_level_controls <- ukcpr %>%
  left_join(
    medicaid_1_5_df %>% select(-any_of(c("state", "st_abbr"))), # Ensures we only keep one copy of the state and st_abbr variables when merging
    by = c("fipsst", "year")
  ) %>%
  left_join(
    medicaid_6_18_df %>% select(-any_of(c("state", "st_abbr"))),
    by = c("fipsst", "year")
  ) %>%
  left_join(
    eitc %>% select(-any_of(c("state", "st_abbr"))),
    by = c("fipsst", "year")
  )

#################################################################
############# Create a 3-level EITC control variable ############
#################################################################

# Generate binary indicator for whether each state offers an EITC (0 if no, 1 if yes)
state_level_controls <- state_level_controls %>%
  mutate(state_eitc = ifelse(federal_pct != 0, 1, 0))

# Generate a 3-level categorical variable that combines info on whether each state has an EITC and, if so, whether that EITC is refundable or not
state_level_controls <- state_level_controls %>% 
  mutate(
    eitc_3level = case_when(
      state_eitc == 0 ~ "No EITC",
      state_eitc == 1 & refundable == 0 ~ "Nonrefundable EITC",
      state_eitc == 1 & refundable == 1 ~ "Refundable EITC"
    )
  )
state_level_controls$eitc_3level <- factor(state_level_controls$eitc_3level, levels = c("No EITC", "Nonrefundable EITC", "Refundable EITC"))

############################################################################
############# Save the new merged state_levels_controls dataset ############
############################################################################

# Save merged dataset to the appropriate /data folder
library(openxlsx)
write.xlsx(state_level_controls, file = "/Users/karolinaedlund/Desktop/HSPH/Research/HSPH/FINAL TANF MANUSCRIPT/TANF FOR SUBMISSION/SUBMISSION/Revise and Resubmit/data/state_level_controls.xlsx", rowNames = FALSE)


