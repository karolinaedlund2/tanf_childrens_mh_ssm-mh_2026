################################################################################
################################################################################
# Program: TANF_datamngmt3_05.29.24_10.17.26
# Programmer: Karolina Edlund
# Last updated: Wednesday, October 21, 2026

# Purpose: This program pulls in and merges Nolan's NSCH data from 2016-2022
  # It also defines our demographic covariates and outcomes
  # Finally, it generates and saves our primary analytic dataset (TANF_master)
    # TANF_master contains a 1-year lagged exposure (maxben_3fam)
################################################################################
################################################################################

# Load packages
library(readxl)       
library(dplyr) 
library(tidyr)
library(readstata13) # For reading in .dta files

#Set working directory
getwd()
setwd("/Users/karolinaedlund/Desktop/HSPH/Research/HSPH/FINAL TANF MANUSCRIPT/TANF FOR SUBMISSION/SUBMISSION/Revise and Resubmit/data/")

#######################################################################
##################### Read in Nolan's NSCH data #######################
#######################################################################

# Read NSCH datasets into R
# Source: National Survey of Children's Health - https://www.census.gov/programs-surveys/nsch.html
nsch_2016_top <- read.dta13("nsch_2016_topical.dta")
nsch_2017_top <- read.dta13("nsch_2017_topical.dta")
nsch_2018_top <- read.dta13("nsch_2018_topical.dta")
nsch_2019_top <- read.dta13("nsch_2019_topical.dta")
nsch_2020_top <- read.dta13("nsch_2020_topical.dta")
nsch_2021_top <- read.dta13("nsch_2021_topical.dta")
nsch_2022_top <- read.dta13("nsch_2022e_topical.dta")

# Treat stratum as character
# Necessary for a smooth merge
nsch_2016_top$stratum <- as.character(nsch_2016_top$stratum)

# Bind all survey years together
nsch <- bind_rows(nsch_2016_top, nsch_2017_top, nsch_2018_top, nsch_2019_top,
                  nsch_2020_top, nsch_2021_top, nsch_2022_top)

########################################################################
############### Prepare demographic control variables ##################
########################################################################

# Child's age
nsch <- nsch %>%
  mutate(age = sc_age_years)

# Create three-level age variable
nsch <- nsch %>%
  mutate(age_group = case_when(
    age >= 3 & age <= 5 ~ "3-5 years",
    age >= 6 & age <= 11 ~ "6-11 years",
    age >= 12 & age <= 17 ~ "12-17 years",
    TRUE ~ NA_character_  # Otherwise, list as NA 
  ))
nsch$age_group <- factor(nsch$age_group, levels = c("3-5 years", "6-11 years", "12-17 years"))

# Create dichotomous age variable
nsch <- nsch %>%
  mutate(age_group_bin = case_when(
    age >= 3 & age <= 11 ~ "3-11 years",
    age >= 12 & age <= 17 ~ "12-17 years",
    TRUE ~ NA_character_  # Otherwise, list as NA 
  ))
nsch$age_group_bin <- factor(nsch$age_group_bin, levels = c("3-11 years", "12-17 years"))

# Child's sex
nsch <- nsch %>% 
  mutate(
    sex = case_when(
      sc_sex == 1 ~ "Male",
      sc_sex == 2 ~ "Female",
      TRUE ~ NA_character_ # Otherwise, list as NA   
    ))
nsch$sex <- factor(nsch$sex, levels = c("Male", "Female"))

# Child's race/ethnicity
nsch <- nsch %>% mutate(
  race_ethnicity = case_when(
    sc_hispanic_r == 1    ~ "Hispanic/Latino",
    sc_race_r == 1        ~ "White",
    sc_race_r == 2        ~ "Black or African American",
    sc_race_r == 3        ~ "American Indian or Alaska Native",
    sc_race_r %in% c(4:5) ~ "Asian, Native Hawaiian, or Pacific Islander",
    sc_race_r %in% c(6:7) ~ "Other or mixed race",
    TRUE ~ NA_character_    # Otherwise, list as NA  
  ))
nsch$race_ethnicity <- factor(nsch$race_ethnicity, levels = c("American Indian or Alaska Native", "Asian, Native Hawaiian, or Pacific Islander", "Black or African American", "Hispanic/Latino", "White", "Other or mixed race"))

# Child's race/ethnicity: binary (non-Hispanic White vs. Non-white)
nsch <- nsch %>% mutate(
  race_ethnicity_bin = case_when(
    is.na(race_ethnicity)      ~ NA_character_,
    race_ethnicity == "White"  ~ "White",
    TRUE                       ~ "Non-white"
  ))
nsch$race_ethnicity_bin <- factor(nsch$race_ethnicity_bin, levels = c("White", "Non-white"))

# Household nativity
nsch <- nsch %>% mutate(
  nativity = case_when(
    house_gen %in% c(1:2) ~ "Child and or parent born out of US",
    house_gen == 4 ~ "Child born in US, parent data missing",
    house_gen == 3 ~ "Child and parent born in US",
    TRUE ~ NA_character_  # Otherwise, list as NA  
  ))
nsch$nativity <- factor(nsch$nativity, levels = c("Child and or parent born out of US", "Child born in US, parent data missing", "Child and parent born in US"))

# Family structure 
nsch <- nsch %>% mutate(
  fam_structure = case_when(
    family %in% c(1,3)  | family_r %in% c(1,3)  ~ "Two parents, married",
    family %in% c(2,4)  | family_r %in% c(2,4)  ~ "Two parents, not married",
    family %in% c(5:8)  | family_r %in% c(5:6)  ~ "Single parent",
    family %in% c(1:99) | family_r %in% c(1:99) ~ "Another family structure",
    TRUE ~ NA_character_  # Otherwise, list as NA  
  ))
nsch$fam_structure <- factor(nsch$fam_structure, levels = c("Two parents, married", "Two parents, not married", "Single parent", "Another family structure"))

# Adults' highest educational attainment
nsch <- nsch %>% 
  mutate(
    adult_edu = case_when(
      higrade_tvis == 1 ~ "Less than high school",
      higrade_tvis == 2 ~ "High school (including vocational)",
      higrade_tvis == 3 ~ "Some college or associate degree",
      higrade_tvis == 4 ~ "College degree or higher",
      TRUE ~ NA_character_  # Otherwise, list as NA 
    ))
nsch$adult_edu <- factor(nsch$adult_edu, levels = c("Less than high school", "High school (including vocational)", "Some college or associate degree", "College degree or higher"))

# Generate mean estimated federal poverty level (FPL)
# The later NSCH years generated predicted FPLs if a household was missing income data
# To represent prediction uncertainty, it made 6 separate predictions; we will take the average of those imputed FPL variables
nsch$fpl_mean <- rowMeans(cbind(nsch$fpl_i1, nsch$fpl_i2, nsch$fpl_i3,
                                    nsch$fpl_i4, nsch$fpl_i5, nsch$fpl_i6), na.rm=T)

# Household's federal poverty level
# Categorizes FPL using either "fpl" (early years of NSCH) or "fpl_mean" (later years of NSCH)
nsch <- nsch %>% mutate(
  fpl_category = case_when(
    fpl %in% c(50:99)   | fpl_mean < 100 ~ "Less than 100%",
    fpl %in% c(100:199) | fpl_mean < 200 ~ "100% to 199%",
    fpl %in% c(200:299) | fpl_mean < 300 ~ "200% to 299%",
    fpl %in% c(300:399) | fpl_mean < 400 ~ "300% to 399%",
    fpl %in% c(400:999) | fpl_mean < 999 ~ "400% or greater",
    TRUE ~ NA_character_ # Otherwise, list as NA
  ))
nsch$fpl_category <- factor(nsch$fpl_category, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))

# Dichotomize FPL
# Low-income (<200% FPL) vs. higher-income
nsch <- nsch %>% mutate(
  low_income = case_when(
    fpl %in% c(50:199)  | fpl_mean < 200 ~ "Lower-income", 
    fpl %in% c(200:999) | fpl_mean < 999 ~ "Higher-income", 
    TRUE ~ NA_character_ # Otherwise, list as NA
  ))
nsch$low_income <- factor(nsch$low_income, levels = c("Lower-income", "Higher-income"))

############################################################
############### Prepare outcome variables ##################
############################################################

# Current diagnosed anxiety
nsch <- nsch %>% mutate(
  anxiety = case_when(
    k2q33b %in% c(1)                      ~ 1,
    k2q33b %in% c(2) | k2q33a %in% c(1:2) ~ 0
  ))

# Current moderate or severe diagnosed anxiety
nsch <- nsch %>% mutate(
  anxiety_severity = case_when(
    k2q33c %in% c(2:3)                                         ~ 1,
    k2q33c %in% c(1) | k2q33b %in% c(1:2) | k2q33a %in% c(1:2) ~ 0
  ))

# Current diagnosed depression
nsch <- nsch %>% mutate(
  depression = case_when(
    k2q32b %in% c(1)                      ~ 1,
    k2q32b %in% c(2) | k2q32a %in% c(1:2) ~ 0
  ))

# Current moderate or severe diagnosed depression
nsch <- nsch %>% mutate(
  depression_severity = case_when(
    k2q32c %in% c(2:3)                                         ~ 1,
    k2q32c %in% c(1) | k2q32b %in% c(1:2) | k2q32a %in% c(1:2) ~ 0
  ))

# Current diagnosed ADD/ADHD
nsch <- nsch %>% mutate(
  adhd = case_when(
    k2q31b %in% c(1)                      ~ 1,
    k2q31b %in% c(2) | k2q31a %in% c(1:2) ~ 0
  ))

# Current moderate or severe diagnosed ADD/ADHD
nsch <- nsch %>% mutate(
  adhd_severity = case_when(
    k2q31c %in% c(2:3)                                         ~ 1,
    k2q31c %in% c(1) | k2q31b %in% c(1:2) | k2q31a %in% c(1:2) ~ 0
  ))

# Current diagnosed behavioral problems
nsch <- nsch %>% mutate(
  bx_problems = case_when(
    k2q34b %in% c(1)                      ~ 1,
    k2q34b %in% c(2) | k2q34a %in% c(1:2) ~ 0
  ))

# Current moderate or severe diagnosed behavioral problems
nsch <- nsch %>% mutate(
  bxprobs_severity = case_when(
    k2q34c %in% c(2:3)                                         ~ 1,
    k2q34c %in% c(1) | k2q34b %in% c(1:2) | k2q34a %in% c(1:2) ~ 0
  ))

############################################################################
############### Merge to create a master analytic dataset ##################
############################################################################

# Read in maxben_merged dataset
# Contains: maxben_3fam
maxben_merged <- read_excel("maxben_merged.xlsx")

# Add 1 year to maxben_merged "year" variable, to enable merging with a 1-year-lagged exposure
maxben_merged_lag <- maxben_merged
maxben_merged_lag$year <- maxben_merged_lag$year + 1

# Divide maxben_3fam variable by 100 for interpretability (i.e., the associated coefficient now corresponds to a $100 increase in maxben_3fam)
maxben_merged_lag$maxben_3fam <- maxben_merged_lag$maxben_3fam/100

# Read in state_level controls dataset
# Contains: state_minwage, percent_1to5, percent_6to18, eitc_3level
state_level_controls <- read_excel("state_level_controls.xlsx")

# Restrict to variables of interest in the nsch dataset
nsch_var <- nsch %>%
  select(fipsst, year, age, age_group, age_group_bin, sex, race_ethnicity, race_ethnicity_bin, nativity, fam_structure, adult_edu, low_income, anxiety, depression, adhd,
         bx_problems, anxiety_severity, depression_severity, adhd_severity, bxprobs_severity, hhid, stratum, fwc)

# Treat fixed effects as factors
maxben_merged_lag$year      <- as.factor(maxben_merged_lag$year)
maxben_merged_lag$fipsst    <- as.factor(maxben_merged_lag$fipsst)
state_level_controls$year   <- as.factor(state_level_controls$year)
state_level_controls$fipsst <- as.factor(state_level_controls$fipsst)
nsch_var$year               <- as.factor(nsch_var$year)
nsch_var$fipsst             <- as.factor(nsch_var$fipsst)

# Merge datasets together
TANF_master <- maxben_merged_lag %>%
  left_join(
    state_level_controls %>% select(-any_of(c("state", "st_abbr"))),
    by = c("fipsst", "year")
  ) %>%
  left_join(
    nsch_var,
    by = c("fipsst", "year")
  )

# Count missing values for each variable
colSums(is.na(TANF_master))

# Calculate percentage missing for each variable
round(colMeans(is.na(TANF_master)) * 100, 2)

###################################################################
##################### Save TANF_master dataset ####################
###################################################################

# Save merged dataset to the appropriate /data folder
library(openxlsx)
write.xlsx(TANF_master, file = "/Users/karolinaedlund/Desktop/HSPH/Research/HSPH/FINAL TANF MANUSCRIPT/TANF FOR SUBMISSION/SUBMISSION/Revise and Resubmit/data/TANF_master.xlsx", rowNames = FALSE)
