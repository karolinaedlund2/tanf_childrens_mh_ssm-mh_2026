##########################################################################################################################################################
##########################################################################################################################################################
# Program name: TANF_script_file_10.29.2026.R
# Programmer: Karolina Edlund
# Last updated: Friday, November 6th, 2026

# Purpose: This program runs our primary and sensitivity analyses, generating the following figures and tables:
  # Figure 1: Flowchart illustrating inclusion and exclusion criteria for our primary analytic sample
  # Figure 2: Maximum Monthly TANF Cash Benefits as a Percent of the Federal Poverty Line in 2015
  # Figure 3: Maximum Monthly TANF Cash Benefits as a Percent of the Federal Poverty Line in 2021
  # Figure 4: Changes in Maximum Monthly TANF Cash Benefits Between 2015 and 2021
  # Table 1: Characteristics of children in our primary analytic sample
  # Table 2: Primary results from cross-sectional and TWFE OLS models, clustering SEs at the state level but not accounting for the NSCH's complex survey design
  # Supplemental Table 1: Average maximum monthly TANF cash benefits, change over time
  # Supplemental Table 2: Primary Model 3 results from Table 2 (cross-sectional and TWFE), with Bonferroni-corrected p-values and 98.75% CIs for four outcomes
  # Supplemental Table 3: Primary Model 3 results from Table 2 (cross-sectional and TWFE), with inference based on the wild cluster bootstrap instead of cluster-robust SEs
  # Supplemental Table 4: Primary Model 3 results from Table 2 (cross-sectional and TWFE), re-estimated with svyglm() to account for the NSCH's complex survey design (strata, clustering, and weights) instead of state-clustered SEs
  # Supplemental Figures 1-4: Primary cross-sectional Model 3 results from Table 2, re-estimated omitting one state at a time (one figure per outcome)
  # Supplemental Table 5: Primary Model 3 (cross-sectional and TWFE) stratified by sex, age group, and race/ethnicity, with Wald tests of effect modification from fully interacted models
  # Supplemental Table 6: Primary Model 3 (cross-sectional and TWFE) with moderate/severe forms of each outcome
  # Supplemental Table 7: Primary Model 3 (cross-sectional and TWFE) with TANF benefits measured concurrently (0-year lag) or lagged by 2 years (primary analyses use a 1-year lag)
  # Supplemental Table 8 (and Supplemental Figures TBD): Continuous-treatment difference-in-differences (Callaway, Goodman-Bacon, and Sant'Anna) as a heterogeneity-robust alternative to the TWFE models in Table 2, plus HonestDiD sensitivity analyses for violations of parallel trends
    # Dose defined as the size of each state's first TANF benefit increase relative to 2015 (to account for staggered treatment adoption), and total 2015–2021 benefit change in a long-difference design (to capture the cumulative effect of multiple benefit changes); benefits are lagged 1 year, so these correspond to the 2016 and 2022 NSCH survey years

# Please contact karolinaedlund@hsph.harvard.edu with any questions

#############################################################################################################################################################
#############################################################################################################################################################

# Load packages
library(readxl)       
library(dplyr) 
library(tidyr)

# Set working directory
getwd()
setwd("/Users/karolinaedlund/Desktop/HSPH/Research/HSPH/FINAL TANF MANUSCRIPT/TANF FOR SUBMISSION/SUBMISSION/Revise and Resubmit/data/")

###################################################
################### Figure 1 ######################
###################################################

# Read in TANF_master dataset
TANF_master <- read_excel("TANF_master.xlsx")

# Redefine strata
# Recommended by the NSCH for multi-year analyses
TANF_master <- TANF_master %>% mutate(
  strata = case_when(
    stratum %in% c("1")       ~ "1",
    stratum %in% c("2", "2A") ~ "2"
  ))

# Divide "fwc" by the number of combined survey years, here 7 (2016-2022)
# Recommended by the NSCH for multi-year analyses
TANF_master$weights <- TANF_master$fwc/7 # fwc corresponds to the selected child weight, Topical surveys
summary(TANF_master$weights)
261.682*7 # confirmed to match the original mean = 1831.774

# Rescale weights so mean = 1
TANF_master$weights <- TANF_master$fwc/1831.774
summary(TANF_master$weights)

# List variables of interest
variables <- c(
  "fipsst", "year", "age", "sex", "race_ethnicity", "nativity", "fam_structure","adult_edu", "low_income",
  "anxiety", "depression", "adhd", "bx_problems", 
  "anxiety_severity", "depression_severity","adhd_severity", "bxprobs_severity")

# Identify complete cases across specified variables
TANF_master$complete_cases <- complete.cases(TANF_master[, variables])

# Total children included in the 2016–2022 waves of the NSCH
n_total <- nrow(TANF_master)
cat("Total sample:", n_total, "\n")

# Exclude children outside our designated age range (3–17)
TANF_master_age <- subset(TANF_master, age >= 3 & age <= 17)
n_age <- nrow(TANF_master_age)
cat("Number excluded due to age:", n_total - n_age, "\n")

# Exclude children residing in higher-income households
TANF_master_age_lowincome <- subset(
  TANF_master_age,
  low_income == "Lower-income")

n_lowincome <- nrow(TANF_master_age_lowincome)
cat("Number excluded due to income:", n_age - n_lowincome, "\n")

# Exclude observations with missing analytic variables
TANF_master_age_lowincome_complete <- subset(
  TANF_master_age_lowincome,
  complete_cases == TRUE)

n_complete <- nrow(TANF_master_age_lowincome_complete)
cat("Number excluded due to missingness:", n_lowincome - n_complete, "\n")

# Final analytic sample
cat("Final analytic sample:", n_complete, "\n")

# Define primary analytic dataset
TANF_master_exclusions <- TANF_master_age_lowincome_complete

###################################################
#############@###### Table 1 ######################
###################################################

# Ensure variables are in proper numeric or factor forms before printing tables
TANF_master_exclusions$age <- as.numeric(TANF_master_exclusions$age)
TANF_master_exclusions$sex <- as.factor(TANF_master_exclusions$sex)
TANF_master_exclusions$race_ethnicity <- as.factor(TANF_master_exclusions$race_ethnicity)
TANF_master_exclusions$nativity <- as.factor(TANF_master_exclusions$nativity)
TANF_master_exclusions$fam_structure <- as.factor(TANF_master_exclusions$fam_structure)
TANF_master_exclusions$adult_edu <- as.factor(TANF_master_exclusions$adult_edu)
TANF_master_exclusions$anxiety <- as.factor(TANF_master_exclusions$anxiety)
TANF_master_exclusions$depression <- as.factor(TANF_master_exclusions$depression)
TANF_master_exclusions$adhd <- as.factor(TANF_master_exclusions$adhd)
TANF_master_exclusions$bx_problems <- as.factor(TANF_master_exclusions$bx_problems)
TANF_master_exclusions$anxiety_severity <- as.factor(TANF_master_exclusions$anxiety_severity)
TANF_master_exclusions$depression_severity <- as.factor(TANF_master_exclusions$depression_severity)
TANF_master_exclusions$adhd_severity <- as.factor(TANF_master_exclusions$adhd_severity)
TANF_master_exclusions$bxprobs_severity <- as.factor(TANF_master_exclusions$bxprobs_severity)

# Child characteristics: unweighted
library(arsenal)
summary(tableby(~ age + sex + race_ethnicity + nativity + fam_structure + adult_edu + anxiety + depression + adhd + bx_problems +
                  anxiety_severity + depression_severity + adhd_severity + bxprobs_severity,
                data=TANF_master_exclusions,  digits.pct=0), text=T)

# Child characteristics: weighted
summary(tableby(~ age + sex + race_ethnicity + nativity + fam_structure + adult_edu + anxiety + depression + adhd + bx_problems +
                  anxiety_severity + depression_severity + adhd_severity + bxprobs_severity,
                data=TANF_master_exclusions,  digits.pct=0, weights=weights), text=T)

# Convert binary outcomes back to numeric form for feols() regression
outcome_vars <- c(
  "anxiety", "depression", "adhd", "bx_problems",
  "anxiety_severity", "depression_severity",
  "adhd_severity", "bxprobs_severity")

TANF_master_exclusions[outcome_vars] <- lapply(
  TANF_master_exclusions[outcome_vars],
  function(x) as.numeric(as.character(x)))

###################################################
#############@###### Table 2 ######################
###################################################

# The following feols() models cluster SEs at the state level but do not adequately account for the NSCH's complex survey design
# Supplemental Table 4 compares results from identical mean and weights svyglm() models that account for this complex survey design but which are unable to simultaneously cluster SEs at the state level
# For more information, see this document shared by Dr. Jarvis Chen on 10/27/26: https://drive.google.com/file/d/1LAfFBKkzK4PCt2xGiOkVjBLkWpfRVvJ-/view?usp=sharing
# Dr. Josh Gilbert, instructor of EDU S043: Multilevel & Longitudinal Modeling at HGSE also confirmed primary models should cluster SEs at the state level

###################################################
# Cross-sectional OLS Models
###################################################

library(fixest)

# Model 1: unadjusted
mod1_cross_anxiety_state <- feols(
  anxiety ~ maxben_3fam,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_cross_anxiety_state)
confint(mod1_cross_anxiety_state)

mod1_cross_depression_state <- feols(
  depression ~ maxben_3fam,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_cross_depression_state)
confint(mod1_cross_depression_state)

mod1_cross_adhd_state <- feols(
  adhd ~ maxben_3fam,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_cross_adhd_state)
confint(mod1_cross_adhd_state)

mod1_cross_bxproblems_state <- feols(
  bx_problems ~ maxben_3fam,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_cross_bxproblems_state)
confint(mod1_cross_bxproblems_state)

# Model 2: adjusted for individual-level covariates including child age, sex, race/ethnicity, family nativity status, family structure, and highest adult education
mod2_cross_anxiety_state <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_cross_anxiety_state)
confint(mod2_cross_anxiety_state)

mod2_cross_depression_state <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_cross_depression_state)
confint(mod2_cross_depression_state)

mod2_cross_adhd_state <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_cross_adhd_state)
confint(mod2_cross_adhd_state)

mod2_cross_bxproblems_state <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_cross_bxproblems_state)
confint(mod2_cross_bxproblems_state)

# Model 3: additionally adjusted for state-level covariates including state minimum wage, Medicaid income eligibility limits for children aged 1-5 and 6-18, and EITC status
mod3_cross_anxiety_state <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_anxiety_state)
confint(mod3_cross_anxiety_state)

mod3_cross_depression_state <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_depression_state)
confint(mod3_cross_depression_state)

mod3_cross_adhd_state <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_adhd_state)
confint(mod3_cross_adhd_state)

mod3_cross_bxproblems_state <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_bxproblems_state)
confint(mod3_cross_bxproblems_state)

# Function to extract TANF coefficients, 95% CIs, and p-values
extract_tanf <- function(model, outcome, model_name) {
  
  # Extract coefficient
  estimate <- as.numeric(coef(model)["maxben_3fam"])
  
  # Extract 95% confidence interval
  ci <- as.numeric(confint(model)["maxben_3fam", ])
  
  # Extract p-value
  p_value <- as.numeric(
    coeftable(model)["maxben_3fam", "Pr(>|t|)"]
  )
  
  # Convert estimates to percentage points and round
  data.frame(
    Outcome = outcome,
    Model = model_name,
    N = nobs(model),
    Estimate = round(estimate * 100, 2),
    CI_lower = round(ci[1] * 100, 2),
    CI_upper = round(ci[2] * 100, 2),
    P_value = round(p_value, 4))}

# Combine results from all 12 feols() models with SEs clustered at the state level
cross_table2_results <- bind_rows(
  
  # Model 1: Unadjusted
  extract_tanf(mod1_cross_anxiety_state, "Anxiety", "Model 1"),
  extract_tanf(mod1_cross_depression_state, "Depression", "Model 1"),
  extract_tanf(mod1_cross_adhd_state, "ADHD", "Model 1"),
  extract_tanf(mod1_cross_bxproblems_state, "Behavioral problems", "Model 1"),
  
  # Model 2: Individual-level covariates
  extract_tanf(mod2_cross_anxiety_state, "Anxiety", "Model 2"),
  extract_tanf(mod2_cross_depression_state, "Depression", "Model 2"),
  extract_tanf(mod2_cross_adhd_state, "ADHD", "Model 2"),
  extract_tanf(mod2_cross_bxproblems_state,"Behavioral problems", "Model 2"),
  
  # Model 3: Individual- and state-level covariates
  extract_tanf(mod3_cross_anxiety_state, "Anxiety", "Model 3"),
  extract_tanf(mod3_cross_depression_state, "Depression", "Model 3"),
  extract_tanf(mod3_cross_adhd_state, "ADHD", "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state, "Behavioral problems", "Model 3"))

# Print results
print(cross_table2_results, row.names = FALSE)

###################################################
# Two-Way Fixed Effects OLS Models
###################################################

# Model 1: unadjusted
mod1_twfe_anxiety_state <- feols(
  anxiety ~ maxben_3fam | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_twfe_anxiety_state)
confint(mod1_twfe_anxiety_state)

mod1_twfe_depression_state <- feols(
  depression ~ maxben_3fam | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_twfe_depression_state)
confint(mod1_twfe_depression_state)

mod1_twfe_adhd_state <- feols(
  adhd ~ maxben_3fam | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_twfe_adhd_state)
confint(mod1_twfe_adhd_state)

mod1_twfe_bxproblems_state <- feols(
  bx_problems ~ maxben_3fam | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod1_twfe_bxproblems_state)
confint(mod1_twfe_bxproblems_state)

# Model 2: adjusted for individual-level covariates including child age, sex, race/ethnicity, family nativity status, family structure, and highest adult education
mod2_twfe_anxiety_state <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_twfe_anxiety_state)
confint(mod2_twfe_anxiety_state)

mod2_twfe_depression_state <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_twfe_depression_state)
confint(mod2_twfe_depression_state)

mod2_twfe_adhd_state <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_twfe_adhd_state)
confint(mod2_twfe_adhd_state)

mod2_twfe_bxproblems_state <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod2_twfe_bxproblems_state)
confint(mod2_twfe_bxproblems_state)

# Model 3: additionally adjusted for state-level covariates including state minimum wage, Medicaid income eligibility limits for children aged 1-5 and 6-18, and EITC status
mod3_twfe_anxiety_state <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_anxiety_state)
confint(mod3_twfe_anxiety_state)

mod3_twfe_depression_state <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_depression_state)
confint(mod3_twfe_depression_state)

mod3_twfe_adhd_state <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_adhd_state)
confint(mod3_twfe_adhd_state)

mod3_twfe_bxproblems_state <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_bxproblems_state)
confint(mod3_twfe_bxproblems_state)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 12 feols() models with state and year fixed effects and state-clustered SEs
twfe_table2_results <- dplyr::bind_rows(
  
  # Model 1: Unadjusted with state and year fixed effects
  extract_tanf(mod1_twfe_anxiety_state, "Anxiety", "Model 1"),
  extract_tanf(mod1_twfe_depression_state, "Depression", "Model 1"),
  extract_tanf(mod1_twfe_adhd_state, "ADHD", "Model 1"),
  extract_tanf(mod1_twfe_bxproblems_state,"Behavioral problems", "Model 1"),
  
  # Model 2: Additionally adjusted for individual-level covariates
  extract_tanf(mod2_twfe_anxiety_state, "Anxiety", "Model 2"),
  extract_tanf(mod2_twfe_depression_state, "Depression", "Model 2"),
  extract_tanf(mod2_twfe_adhd_state,"ADHD", "Model 2"),
  extract_tanf(mod2_twfe_bxproblems_state,"Behavioral problems", "Model 2"),
  
  # Model 3: Additionally adjusted for state-level covariates
  extract_tanf(mod3_twfe_anxiety_state, "Anxiety", "Model 3"),
  extract_tanf(mod3_twfe_depression_state,"Depression", "Model 3"),
  extract_tanf(mod3_twfe_adhd_state,"ADHD", "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state,"Behavioral problems", "Model 3"))

# Print results
print(twfe_table2_results, row.names = FALSE)

###################################################
#################### Figure 2 #####################
###################################################

library(sf)
library(usmap)

# Read in maxben_merged dataset
maxben_merged <-read_excel("maxben_merged.xlsx")

# Federal poverty guidelines by year (HHS ASPE main page)
# https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references
# Federal poverty guidelines for a family of three in 2015
# https://aspe.hhs.gov/sites/default/files/documents/80acdac5d3d45b1760ddedff5b33ca96/HHS-Poverty-Guidelines-Fed-Register-2015.pdf
# Federal poverty guidelines for a family of three in 2021
# https://aspe.hhs.gov/sites/default/files/documents/d2eececdc1ca66dfd41ca1d2a524e076/HHS-Poverty-Guidelines-Fed-Register-2021.pdf

# Creating percent/federal poverty level variables for 2015
# Monthly income calculations
# 48 contiguous states and DC
20090/12 # 1674.167 ~ 1674
# Alaska
25120/12 # 2093.333 ~ 2093
# Hawaii
23110/12 # 1925.833 ~ 1925

# Create filtered df for 2015
df_15 <- maxben_merged %>%
  filter(as.numeric(year) == 2015)

# Create new percent_fpl variable
df_15 <- df_15 %>%
  mutate(percent_fpl = case_when(
    st_abbr == "AK" ~ maxben_3fam / 2093,
    st_abbr == "HI" ~ maxben_3fam / 1925,
    TRUE ~ maxben_3fam / 1674))

summary(df_15$percent_fpl)
df_15$percent_fpl <- df_15$percent_fpl*100
summary(df_15$percent_fpl)

# Create categorical percent_fpl_cat variable
df_15 <- df_15 %>%
  mutate(
    percent_fpl_cat = case_when(
      percent_fpl >= 10 & percent_fpl < 20 ~ "10-19%",
      percent_fpl >= 20 & percent_fpl < 30 ~ "20-29%",
      percent_fpl >= 30 & percent_fpl < 40 ~ "30-39%",
      percent_fpl >= 40 & percent_fpl <= 60 ~ "40-60%",
      TRUE ~ NA_character_),
    percent_fpl_cat = factor(
      percent_fpl_cat,
      levels = c("10-19%", "20-29%", "30-39%", "40-60%")))

summary(df_15$percent_fpl_cat)

# Plot the heatmap for 2015
# Create a 'fips' column for usmap; usmap matches on 'fips', 'abbr', or 'state'
df_15 <- df_15 %>%
  mutate(fips = sprintf("%02d", as.integer(fipsst)))

# Define custom colors for categories
custom_colors <- c(
  "10-19%" = "#e0f0ff",  # very light blue
  "20-29%" = "#99c2ff",  # medium light
  "30-39%" = "#3385ff",  # strong blue
  "40-60%" = "#003366"   # deep navy
  )

state_map <- usmap::us_map(regions = "states")

state_centers <- state_map %>%
  st_as_sf() %>%
  st_centroid() %>%
  mutate(
    coords = st_coordinates(st_geometry(.)),
    x = coords[, 1],
    y = coords[, 2]) %>%
  as_tibble() %>%
  select(abbr, x, y) %>%
  mutate(
    x = dplyr::case_when(
      abbr == "CA" ~ x - 42500,
      abbr == "ID" ~ x - 30000,
      abbr == "WV" ~ x - 12000,
      abbr == "LA" ~ x - 52000,
      abbr == "MI" ~ x + 40000,
      abbr == "NY" ~ x + 35000,
      abbr == "ME" ~ x + 14000,
      abbr == "FL" ~ x + 85000,
      TRUE         ~ x
    ),
    y = dplyr::case_when(
      abbr == "AK" ~ y + 60000,
      TRUE         ~ y))

# Move whiskers for geographically small states
east_stack_ids <- c("MA", "RI", "CT", "NJ", "DE", "MD", "DC")
tiny_states    <- c(east_stack_ids, "VT", "NH", "HI")

east_label_x <- max(state_centers$x) + 260000

east_stack <- state_centers %>%
  filter(abbr %in% east_stack_ids) %>%
  arrange(desc(y)) %>%
  mutate(
    # For NJ 
    x = if_else(abbr == "NJ", x + 21000, x),
    
    label_y = seq(
      from = max(y) + 300000,
      to   = min(y) - 300000,
      length.out = n()),
    label_x = east_label_x,
    elbow_x = label_x - 60000 - (row_number() - 1) * 12000,
    elbow_y = label_y,
    hjust   = 0)

# For VT and NH 
top_y <- max(state_centers$y) + 130000

vt_nh_labels <- state_centers %>%
  filter(abbr %in% c("VT", "NH")) %>%
  mutate(
    label_x = dplyr::case_when(
      abbr == "VT" ~ x - 280000,
      abbr == "NH" ~ x - 150000,
      TRUE         ~ x
    ),
    label_y = top_y,
    hjust   = 1)

# For HI
hi_label <- state_centers %>%
  filter(abbr == "HI") %>%
  mutate(
    label_x = x - 200000,
    label_y = y - 70000,
    elbow_x = x - 100000,
    elbow_y = y - 35000,
    hjust   = 1)

tiny_labels <- bind_rows(
  east_stack %>% select(abbr, x, y, label_x, label_y, hjust),
  vt_nh_labels %>% select(abbr, x, y, label_x, label_y, hjust),
  hi_label %>% select(abbr, x, y, label_x, label_y, hjust))

# Add small colorful boxes for geographically small states
tiny_key_boxes <- east_stack %>%
  left_join(
    df_15 %>%
      select(st_abbr, percent_fpl_cat),
    by = c("abbr" = "st_abbr")) %>%
  mutate(
    box_w   = 70000,
    box_h   = 40000,
    key_xmin = label_x + 180000,
    key_xmax = label_x + 180000 + box_w,
    key_ymin = label_y - box_h / 2,
    key_ymax = label_y + box_h / 2)

white_states <- c("AK", "CA", "OR", "WA", "MT", "WY", "SD", "MN", "WI", "NY")

map_2015 <- plot_usmap(
  data    = df_15,
  values  = "percent_fpl_cat",
  regions = "states",
  labels  = FALSE) +
  
  geom_text(
    data = state_centers %>% filter(!abbr %in% tiny_states),
    aes(
      x = x,
      y = y,
      label = abbr,
      color = if_else(abbr %in% white_states, "white", "black")),
    inherit.aes = FALSE,
    size   = 2.3,
    family = "Times New Roman",
    fontface = "bold",
    show.legend = FALSE) +
  scale_color_identity() +
  
  geom_segment(
    data = east_stack,
    aes(x = x, y = y, xend = elbow_x, yend = elbow_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  geom_segment(
    data = east_stack,
    aes(x = elbow_x, y = elbow_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = vt_nh_labels,
    aes(x = label_x, y = label_y, xend = x, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = hi_label,
    aes(x = x, y = y, xend = elbow_x, yend = elbow_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = hi_label,
    aes(x = elbow_x, y = elbow_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_rect(
    data = tiny_key_boxes,
    aes(
      xmin = key_xmin,
      xmax = key_xmax,
      ymin = key_ymin,
      ymax = key_ymax,
      fill = percent_fpl_cat),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.3) +
  
  geom_text(
    data = tiny_labels,
    aes(x = label_x, y = label_y, label = abbr, hjust = hjust),
    inherit.aes = FALSE,
    size   = 2.3,
    color  = "black",
    family = "Times New Roman",
    fontface = "bold") +
  scale_fill_manual(values = custom_colors, drop = FALSE) +
  labs(title = "Maximum Monthly TANF Benefits as a Percent of the FPL in 2015") +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just  = "center",
    legend.margin    = margin(t = -40),   # move legend closer to map
    legend.title     = element_blank(),
    legend.key.width = unit(0.7, "cm"),
    legend.key.height= unit(0.4, "cm"),
    legend.text      = element_text(family = "Times New Roman", size = 10),
    plot.title       = element_text(hjust = 0.5, family = "Times New Roman"),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    text             = element_text(family = "Times New Roman"))

map_2015

###################################################
#################### Figure 3 #####################
###################################################

# Creating percent/federal poverty level variables for 2021
# Monthly income calculations
# 48 contiguous states and DC
21960/12 # 1830
# Alaska
27450/12 # 2287.5 ~ 2288
# Hawaii
25260/12 # 2105

# Create filtered df for 2021
df_21 <- maxben_merged %>%
  filter(as.numeric(year) == 2021)

# Create new percent_fpl variable
df_21 <- df_21 %>%
  mutate(percent_fpl = case_when(
    st_abbr == "AK" ~ maxben_3fam / 2288,
    st_abbr == "HI" ~ maxben_3fam / 2105,
    TRUE ~ maxben_3fam / 1830))

summary(df_21$percent_fpl)
df_21$percent_fpl <- df_21$percent_fpl*100
summary(df_21$percent_fpl)

# Create categorical percent_fpl_cat variable
df_21 <- df_21 %>%
  mutate(
    percent_fpl_cat = case_when(
      percent_fpl >= 10 & percent_fpl < 20 ~ "10-19%",
      percent_fpl >= 20 & percent_fpl < 30 ~ "20-29%",
      percent_fpl >= 30 & percent_fpl < 40 ~ "30-39%",
      percent_fpl >= 40 & percent_fpl <= 60 ~ "40-60%",
      TRUE ~ NA_character_),
    percent_fpl_cat = factor(
      percent_fpl_cat,
      levels = c("10-19%", "20-29%", "30-39%", "40-60%")))

summary(df_21$percent_fpl_cat)

# Plot the heatmap for 2021
# Create a 'fips' column for usmap; usmap matches on 'fips', 'abbr', or 'state'
df_21 <- df_21 %>%
  mutate(fips = sprintf("%02d", as.integer(fipsst)))

# Define custom colors for categories
custom_colors <- c(
  "10-19%" = "#e0f0ff",  # very light blue
  "20-29%" = "#99c2ff",  # medium light
  "30-39%" = "#3385ff",  # strong blue
  "40-60%" = "#003366"   # deep navy
)

state_map <- usmap::us_map(regions = "states")

state_centers <- state_map %>%
  st_as_sf() %>%
  st_centroid() %>%
  mutate(
    coords = st_coordinates(st_geometry(.)),
    x = coords[, 1],
    y = coords[, 2]) %>%
  as_tibble() %>%
  select(abbr, x, y) %>%
  mutate(
    x = dplyr::case_when(
      abbr == "CA" ~ x - 42500,
      abbr == "ID" ~ x - 30000,
      abbr == "WV" ~ x - 12000,
      abbr == "LA" ~ x - 52000,
      abbr == "MI" ~ x + 40000,
      abbr == "NY" ~ x + 35000,
      abbr == "ME" ~ x + 14000,
      abbr == "FL" ~ x + 85000,
      TRUE         ~ x),
    y = dplyr::case_when(
      abbr == "AK" ~ y + 60000,
      TRUE         ~ y))

# Move whiskers for geographically small states
east_stack_ids <- c("MA", "RI", "CT", "NJ", "DE", "MD", "DC")
tiny_states    <- c(east_stack_ids, "VT", "NH", "HI")

east_label_x <- max(state_centers$x) + 260000

east_stack <- state_centers %>%
  filter(abbr %in% east_stack_ids) %>%
  arrange(desc(y)) %>%
  mutate(
    # For NJ 
    x = if_else(abbr == "NJ", x + 21000, x),
    
    label_y = seq(
      from = max(y) + 300000,
      to   = min(y) - 300000,
      length.out = n()
    ),
    label_x = east_label_x,
    elbow_x = label_x - 60000 - (row_number() - 1) * 12000,
    elbow_y = label_y,
    hjust   = 0)

# For VT and NH
top_y <- max(state_centers$y) + 130000

vt_nh_labels <- state_centers %>%
  filter(abbr %in% c("VT", "NH")) %>%
  mutate(
    label_x = dplyr::case_when(
      abbr == "VT" ~ x - 280000,
      abbr == "NH" ~ x - 150000,
      TRUE         ~ x),
    label_y = top_y,
    hjust   = 1)

# For HI
hi_label <- state_centers %>%
  filter(abbr == "HI") %>%
  mutate(
    label_x = x - 200000,
    label_y = y - 70000,
    elbow_x = x - 100000,
    elbow_y = y - 35000,
    hjust   = 1)

tiny_labels <- bind_rows(
  east_stack %>% select(abbr, x, y, label_x, label_y, hjust),
  vt_nh_labels %>% select(abbr, x, y, label_x, label_y, hjust),
  hi_label %>% select(abbr, x, y, label_x, label_y, hjust))

# Add small colorful boxes for geographically small states
tiny_key_boxes <- east_stack %>%
  left_join(
    df_21 %>%
      select(st_abbr, percent_fpl_cat),
    by = c("abbr" = "st_abbr")) %>%
  mutate(
    box_w   = 70000,
    box_h   = 40000,
    key_xmin = label_x + 180000,
    key_xmax = label_x + 180000 + box_w,
    key_ymin = label_y - box_h / 2,
    key_ymax = label_y + box_h / 2)

white_states <- c("AK", "CA", "OR", "WA", "MT", "WY", "SD", "MN", "WI", "NY")

map_2021 <- plot_usmap(
  data    = df_21,
  values  = "percent_fpl_cat",
  regions = "states",
  labels  = FALSE) +
  
  geom_text(
    data = state_centers %>% filter(!abbr %in% tiny_states),
    aes(
      x = x,
      y = y,
      label = abbr,
      color = if_else(abbr %in% white_states, "white", "black")),
    inherit.aes = FALSE,
    size   = 2.3,
    family = "Times New Roman",
    fontface = "bold",
    show.legend = FALSE) +
  scale_color_identity() +
  
  geom_segment(
    data = east_stack,
    aes(x = x, y = y, xend = elbow_x, yend = elbow_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = east_stack,
    aes(x = elbow_x, y = elbow_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = vt_nh_labels,
    aes(x = label_x, y = label_y, xend = x, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = hi_label,
    aes(x = x, y = y, xend = elbow_x, yend = elbow_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = hi_label,
    aes(x = elbow_x, y = elbow_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_rect(
    data = tiny_key_boxes,
    aes(
      xmin = key_xmin,
      xmax = key_xmax,
      ymin = key_ymin,
      ymax = key_ymax,
      fill = percent_fpl_cat),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.3) +
  
  geom_text(
    data = tiny_labels,
    aes(x = label_x, y = label_y, label = abbr, hjust = hjust),
    inherit.aes = FALSE,
    size   = 2.3,
    color  = "black",
    family = "Times New Roman",
    fontface = "bold") +
  scale_fill_manual(values = custom_colors, drop = FALSE) +
  labs(title = "Maximum Monthly TANF Benefits as a Percent of the FPL in 2021") +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just  = "center",
    legend.margin    = margin(t = -40),   # move legend closer to map
    legend.title     = element_blank(),
    legend.key.width = unit(0.7, "cm"),
    legend.key.height= unit(0.4, "cm"),
    legend.text      = element_text(family = "Times New Roman", size = 10),
    plot.title       = element_text(hjust = 0.5, family = "Times New Roman"),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    text             = element_text(family = "Times New Roman"))

map_2021

###################################################
#################### Figure 4 #####################
###################################################

# Mapping absolute difference between 2015 and 2021
# Check if any state's maxben_3fam decreased over time
maxben_merged %>%
  group_by(st_abbr) %>%
  arrange(year, .by_group = TRUE) %>%
  summarize(
    ever_decrease = any(diff(maxben_3fam) < 0, na.rm = TRUE),
    .groups = "drop") %>%
  print(n = Inf) # Confirmed min change over time was 0 USD

# Create a variable for the difference between 2016 and 2022
df_diff <- merge(df_15, df_21, by = "st_abbr", suffixes = c("_2015", "_2021"))
df_diff$maxben_3fam_diff1521 <- df_diff$maxben_3fam_2021 - df_diff$maxben_3fam_2015

summary(df_diff$maxben_3fam_diff1521)
print(df_diff$maxben_3fam_diff1521)

# Create categorical percent_fpl_cat variable
df_diff <- df_diff %>%
  mutate(
    maxben_3fam_diff1521_cat = case_when(
      maxben_3fam_diff1521 == 0 ~ "$0",
      maxben_3fam_diff1521 > 0  & maxben_3fam_diff1521 <= 30  ~ "$1–30",
      maxben_3fam_diff1521 > 30 & maxben_3fam_diff1521 <= 100 ~ "$31–100",
      maxben_3fam_diff1521 > 100 & maxben_3fam_diff1521 <= 200 ~ "$101–200",
      maxben_3fam_diff1521 > 200 & maxben_3fam_diff1521 <= 423 ~ "$201–423",
      TRUE ~ NA_character_),
    maxben_3fam_diff1521_cat = factor(
      maxben_3fam_diff1521_cat,
      levels = c("$0", "$1–30", "$31–100", "$101–200", "$201–423")))

summary(df_diff$maxben_3fam_diff1521_cat)

# Plot the heatmap for the absolute benefit differences btw 2015-2021
# Create a 'fips' column for usmap; usmap matches on 'fips', 'abbr', or 'state'
df_diff <- df_diff %>%
  mutate(fips = sprintf("%02d", as.integer(fipsst_2015)))

# Define custom colors for categories
custom_colors <- c(
  "$0" = "#F0F0F0",   #E5E5E5
  "$1–30" = "#c2e699",  
  "$31–100" = "#78c679",  
  "$101–200" = "#31a354",   
  "$201–423" = "#006837")

state_map <- usmap::us_map(regions = "states")

state_centers <- state_map %>%
  st_as_sf() %>%
  st_centroid() %>%
  mutate(
    coords = st_coordinates(st_geometry(.)),
    x = coords[, 1],
    y = coords[, 2]) %>%
  as_tibble() %>%
  select(abbr, x, y) %>%
  mutate(
    x = dplyr::case_when(
      abbr == "CA" ~ x - 42500,
      abbr == "ID" ~ x - 30000,
      abbr == "WV" ~ x - 12000,
      abbr == "LA" ~ x - 52000,
      abbr == "MI" ~ x + 40000,
      abbr == "NY" ~ x + 35000,
      abbr == "ME" ~ x + 14000,
      abbr == "FL" ~ x + 85000,
      TRUE         ~ x),
    y = dplyr::case_when(
      abbr == "AK" ~ y + 60000,
      TRUE         ~ y))

# Move whiskers for geographically small states
east_stack_ids <- c("MA", "RI", "CT", "NJ", "DE", "MD", "DC")
tiny_states    <- c(east_stack_ids, "VT", "NH", "HI")

east_label_x <- max(state_centers$x) + 260000

east_stack <- state_centers %>%
  filter(abbr %in% east_stack_ids) %>%
  arrange(desc(y)) %>%
  mutate(
    # For NJ 
    x = if_else(abbr == "NJ", x + 21000, x),
    
    label_y = seq(
      from = max(y) + 300000,
      to   = min(y) - 300000,
      length.out = n()
    ),
    label_x = east_label_x,
    elbow_x = label_x - 60000 - (row_number() - 1) * 12000,
    elbow_y = label_y,
    hjust   = 0)

# For VT and NH
top_y <- max(state_centers$y) + 130000

vt_nh_labels <- state_centers %>%
  filter(abbr %in% c("VT", "NH")) %>%
  mutate(
    label_x = dplyr::case_when(
      abbr == "VT" ~ x - 280000,
      abbr == "NH" ~ x - 150000,
      TRUE         ~ x),
    label_y = top_y,
    hjust   = 1)

# For HI
hi_label <- state_centers %>%
  filter(abbr == "HI") %>%
  mutate(
    label_x = x - 200000,
    label_y = y - 70000,
    elbow_x = x - 100000,
    elbow_y = y - 35000,
    hjust   = 1)

tiny_labels <- bind_rows(
  east_stack %>% select(abbr, x, y, label_x, label_y, hjust),
  vt_nh_labels %>% select(abbr, x, y, label_x, label_y, hjust),
  hi_label %>% select(abbr, x, y, label_x, label_y, hjust))

# Add small colorful boxes for geographically small states
tiny_key_boxes <- east_stack %>%
  left_join(
    df_diff %>%
      select(st_abbr, maxben_3fam_diff1521_cat),
    by = c("abbr" = "st_abbr")) %>%
  mutate(
    box_w   = 70000,
    box_h   = 40000,
    key_xmin = label_x + 180000,
    key_xmax = label_x + 180000 + box_w,
    key_ymin = label_y - box_h / 2,
    key_ymax = label_y + box_h / 2)

white_states <- c("WA", "CA", "NE", "IL", "TN", "WV", "VA", "ME")

map_diff1521 <- plot_usmap(
  data    = df_diff,
  values  = "maxben_3fam_diff1521_cat",
  regions = "states",
  labels  = FALSE) +
  
  geom_text(
    data = state_centers %>% filter(!abbr %in% tiny_states),
    aes(
      x = x,
      y = y,
      label = abbr,
      color = if_else(abbr %in% white_states, "white", "black")),
    inherit.aes = FALSE,
    size   = 2.3,
    family = "Times New Roman",
    fontface = "bold",
    show.legend = FALSE) +
  scale_color_identity() +
  
  geom_segment(
    data = east_stack,
    aes(x = x, y = y, xend = elbow_x, yend = elbow_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = east_stack,
    aes(x = elbow_x, y = elbow_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = vt_nh_labels,
    aes(x = label_x, y = label_y, xend = x, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = hi_label,
    aes(x = x, y = y, xend = elbow_x, yend = elbow_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_segment(
    data = hi_label,
    aes(x = elbow_x, y = elbow_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color     = "black") +
  
  geom_rect(
    data = tiny_key_boxes,
    aes(
      xmin = key_xmin,
      xmax = key_xmax,
      ymin = key_ymin,
      ymax = key_ymax,
      fill = maxben_3fam_diff1521_cat
    ),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.3) +
  
  geom_text(
    data = tiny_labels,
    aes(x = label_x, y = label_y, label = abbr, hjust = hjust),
    inherit.aes = FALSE,
    size   = 2.3,
    color  = "black",
    family = "Times New Roman",
    fontface = "bold") +
  scale_fill_manual(values = custom_colors, drop = FALSE) +
  labs(title = "Absolute Difference in Maximum Monthly TANF Benefits Between 2015-2021 (USD)") +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just  = "center",
    legend.margin    = margin(t = -40),
    
    legend.key.spacing.x = unit(0.5, "cm"),  # increase for more horizontal space
    
    legend.title     = element_blank(),
    legend.key.width = unit(0.7, "cm"),
    legend.key.height= unit(0.4, "cm"),
    legend.text      = element_text(family = "Times New Roman", size = 10),
    
    plot.title       = element_text(hjust = 0.5, family = "Times New Roman"),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    text             = element_text(family = "Times New Roman"))

map_diff1521

################################################################
#################### Supplemental Table 1 ######################
################################################################

# maxben_merged dataset was previously read in for Figures 2-4
# State-year benefits with year-over-year changes
benefits <- maxben_merged %>%
  distinct(fipsst, year, maxben_3fam) %>%
  filter(year >= 2015, year <= 2021) %>%
  arrange(fipsst, year) %>%
  group_by(fipsst) %>%
  mutate(
    change     = maxben_3fam - lag(maxben_3fam),
    pct_change = 100 * change / lag(maxben_3fam)) %>%
  ungroup()

# Yearly mean, SD, and average change across states
yearly <- benefits %>%
  group_by(year) %>%
  summarise(
    mean       = mean(maxben_3fam),
    sd         = sd(maxben_3fam),
    change     = mean(change),
    pct_change = mean(pct_change),
    .groups = "drop")

# Average total change from 2015 to 2021
total <- benefits %>%
  group_by(fipsst) %>%
  summarise(
    change = maxben_3fam[year == 2021] - maxben_3fam[year == 2015],
    pct_change = 100 * change / maxben_3fam[year == 2015]) %>%
  summarise(change = mean(change), pct_change = mean(pct_change))

# Format table
tableS1 <- bind_rows(
  yearly %>%
    transmute(
      Year = as.character(year),
      Mean = sprintf("$%.0f", mean),
      SD   = sprintf("$%.0f", sd),
      `Average change`   = ifelse(is.na(change), "/", sprintf("$%.2f", change)),
      `Average % change` = ifelse(is.na(pct_change), "/", sprintf("%.2f", pct_change))),
  tibble(
    Year = "2015-2021", Mean = "/", SD = "/",
    `Average change`   = sprintf("$%.2f", total$change),
    `Average % change` = sprintf("%.1f", total$pct_change)))

# Print table
print(tableS1)

# Calculate average change between consecutive years for footnote
benefits %>%
  summarise(
    avg_change     = mean(change, na.rm = TRUE),
    avg_pct_change = mean(pct_change, na.rm = TRUE))

################################################################
#################### Supplemental Table 2 ######################
################################################################

# Bonferroni correction (alpha = 0.05/4 = 0.0125, 98.75% CIs) for the four outcomes within each fully adjusted Model 3 (cross-sectional and TWFE separately)
n_tests    <- 4
bonf_level <- 1 - 0.05 / n_tests   # 0.9875 -> 98.75% CIs

# Extract TANF estimate with 95% and Bonferroni-adjusted CIs (unrounded p-values)
extract_tanf_bonf <- function(model, outcome, design) {
  ci95  <- confint(model, parm = "maxben_3fam", level = 0.95)
  ci_bf <- confint(model, parm = "maxben_3fam", level = bonf_level)
  
  data.frame(
    Design        = design,
    Outcome       = outcome,
    N             = nobs(model),
    Estimate      = coef(model)[["maxben_3fam"]] * 100,
    CI95_lower    = ci95[1, 1] * 100,
    CI95_upper    = ci95[1, 2] * 100,
    CI_bonf_lower = ci_bf[1, 1] * 100,
    CI_bonf_upper = ci_bf[1, 2] * 100,
    P_value       = coeftable(model)["maxben_3fam", "Pr(>|t|)"]
  )
}

bonf_results <- bind_rows(
  # Cross-sectional Model 3
  extract_tanf_bonf(mod3_cross_anxiety_state,    "Anxiety",             "Cross-sectional"),
  extract_tanf_bonf(mod3_cross_depression_state, "Depression",          "Cross-sectional"),
  extract_tanf_bonf(mod3_cross_adhd_state,       "ADHD",                "Cross-sectional"),
  extract_tanf_bonf(mod3_cross_bxproblems_state, "Behavioral problems", "Cross-sectional"),
  
  # TWFE Model 3
  extract_tanf_bonf(mod3_twfe_anxiety_state,     "Anxiety",             "TWFE"),
  extract_tanf_bonf(mod3_twfe_depression_state,  "Depression",          "TWFE"),
  extract_tanf_bonf(mod3_twfe_adhd_state,        "ADHD",                "TWFE"),
  extract_tanf_bonf(mod3_twfe_bxproblems_state,  "Behavioral problems", "TWFE")
) %>%
  group_by(Design) %>%                                 
  mutate(
    P_bonf    = p.adjust(P_value, method = "bonferroni"),
    Sig_unadj = P_value < 0.05,
    Sig_bonf  = P_bonf  < 0.05,
    Changed   = Sig_unadj != Sig_bonf                   # Flags results whose statistical significance changes after Bonferroni correction
  ) %>%
  ungroup() %>%
  mutate(
    across(c(Estimate, starts_with("CI")), ~ round(.x, 2)),
    across(starts_with("P_"), ~ round(.x, 4))
  )

print(bonf_results, row.names = FALSE)

# Check whether adjustment changed any conclusions
bonf_results %>% filter(Changed)

#############################################################################
###### Supplemental Table 3 #################################################
#############################################################################

# Conduct a wild cluster bootstrap sensitivity analysis
# For more information see Cameron, Gelbach & Douglas (REStat, 2008): https://www.nber.org/system/files/working_papers/t0344/t0344.pdf
# While 51 states is likely enough to stick with cluster robust SEs, the effective number of clusters contributing identifying variation is substantially lower, given benefits increased over time in only 27 states and DC
# The wild cluster bootstrap provides an alternative approach to inference that can improve finite-sample performance, particularly when the number of independent clusters is limited
# This sensitivity analysis aligns with our primary analytic approach using feols() models, but does not fully account for the NSCH's complex survey design; see Supplemental Table 4 for additional sensitivity checks

# fwildclusterboot package was removed from CRAN; for more information on downloading the package see Stack Overflow from author: https://stackoverflow.com/questions/74029657/installing-the-package-fwildclusterboot
# To download in this way, must account for the fact R 4.3 is configured to use GNU Fortran 12.2.0 (Fortran is a "compiler" that translates code written in a programming language like R into instructions our computers can execute)
# utils::install.packages("fwildclusterboot", repos = "https://s3alfisc.r-universe.dev", type = "source")

library(fwildclusterboot)
library(purrr)
library(dqrng)

# Classify year and fipsst as numeric variables
# Necessary because when retrieving the cluster variable, boottest() rebuilds the model frame with base R's stats::expand.model.frame(), which evaluates the fixed effects after | (year + fipsst) as arithmetic and errors when they are stored as character
# This does not change feols() estimates (fixed effects after | are always treated as categorical), and boottest() converts numeric fixed effects to factors internally
TANF_master_exclusions$year   <- as.numeric(TANF_master_exclusions$year)
TANF_master_exclusions$fipsst <- as.numeric(TANF_master_exclusions$fipsst)

# Function to extract bootstrap p-values and CIs from existing Table 2 feols() models
extract_wildboot <- function(model, outcome, model_name, analysis) {
  
  # Wild cluster bootstrap
  boot <- boottest(
    model,
    param = "maxben_3fam",
    clustid = "fipsst",
    fe = NULL,   # Default, so including just for clarity; required with regression weights; ensures year and state FEs are added as dummies
    B = 9999,
    type = "rademacher",
    impose_null = TRUE,
    bootstrap_type = "fnw11",
    conf_int = TRUE,
    sign_level = 0.05
  )
  
  # Extract results
  data.frame(
    Analysis = analysis,
    Outcome = outcome,
    Model = model_name,
    N = nobs(model),
    N_clusters = as.numeric(boot$N_G[1]),
    Estimate = round(
      as.numeric(coef(model)["maxben_3fam"]) * 100, 2),
    Boot_CI_lower = round(
      as.numeric(boot$conf_int[1]) * 100, 2),
    Boot_CI_upper = round(
      as.numeric(boot$conf_int[2]) * 100, 2),
    Boot_P_value = round(
      as.numeric(boot$p_val), 4))
}

# Organize existing Model 3 cross-sectional models
cross_models <- list(
  Anxiety               = mod3_cross_anxiety_state,
  Depression            = mod3_cross_depression_state,
  ADHD                  = mod3_cross_adhd_state,
  `Behavioral problems`  = mod3_cross_bxproblems_state)

# Organize existing Model 3 TWFE models
twfe_models <- list(
  Anxiety               = mod3_twfe_anxiety_state,
  Depression            = mod3_twfe_depression_state,
  ADHD                  = mod3_twfe_adhd_state,
  `Behavioral problems`  = mod3_twfe_bxproblems_state)

# Function to run bootstrap for each outcome's Model 3
run_wildboot_models <- function(models, analysis) {
  imap_dfr(models, function(model, outcome) {
    extract_wildboot(model, outcome, "Model 3", analysis)})}

# Set reproducible seeds
set.seed(12345)
dqrng::dqset.seed(12345)

# Run all 4 fully-adjusted cross-sectional models
cross_wildboot_results <- run_wildboot_models(
  cross_models,
  "Cross-sectional")

# Run all 4 fully-adjusted TWFE models
twfe_wildboot_results <- run_wildboot_models(
  twfe_models,
  "TWFE")

# Combine results
wildboot_table2_results <- bind_rows(
  cross_wildboot_results,
  twfe_wildboot_results)

# Print results
print(wildboot_table2_results, row.names = FALSE)

################################################################
#################### Supplemental Table 4 ######################
################################################################

###################################################
# Account for NSCH's complex survey design
###################################################

# For more information see the following links:
# 2022 NSCH Source and Accuracy Statement: https://www2.census.gov/programs-surveys/nsch/technical-documentation/source-and-accuracy/2022-NSCH-Source-and-Accuracy-Statement.pdf
# 2022 NSCH Methodology Report: https://www2.census.gov/programs-surveys/nsch/technical-documentation/methodology/2022-NSCH-Methodology-Report.pdf
# NSCH Guide to Multi-Year Estimates: https://www2.census.gov/programs-surveys/nsch/technical-documentation/methodology/NSCH-Guide-to-Multi-Year-Estimates.pdf

library(survey)

# TANF_master was read in and prepared in the Figure 1 section; we do not re-read it here so that we retain:
  # (a) the redefined strata (recommended by the NSCH for multi-year analyses), and
  # (b) the combined-year weights (fwc divided by 7 survey years, as recommended by the NSCH), rescaled to mean = 1 (rescaling by a constant does not change svyglm() estimates or SEs)

# Cross state and sampling stratum
# NSCH recommends generating a new stratum variable with 102 levels by crossing strata (2 levels) with fipsst (51 levels)
TANF_master$stratacross <- interaction(
  TANF_master$fipsst,
  TANF_master$strata,
  drop = TRUE)

# Define complex survey design
TANF_master_design <- svydesign(
  id = ~hhid,
  strata = ~stratacross,
  weights = ~weights,
  data = TANF_master,
  nest = TRUE)

# List variables of interest
variables <- c(
  "fipsst", "year", "age", "sex", "race_ethnicity", "nativity", "fam_structure","adult_edu", "low_income",
  "anxiety", "depression", "adhd", "bx_problems", 
  "anxiety_severity", "depression_severity","adhd_severity", "bxprobs_severity",
  "hhid", "stratacross", "weights")

# Identify rows with complete cases
complete_cases <- complete.cases(
  TANF_master_design$variables[, variables])

# Add complete-case indicator to survey design
TANF_master_design <- update(
  TANF_master_design,
  complete_cases = complete_cases)

# Implement exclusions to define primary analytic sample
TANF_master_design_exclusions <- subset(
  TANF_master_design,
  age >= 3 & age <= 17 &
    low_income == "Lower-income" &
    complete_cases == TRUE)

# Print final analytic sample size
n_complete <- nrow(TANF_master_design_exclusions$variables)
cat("Final analytic sample:", n_complete, "\n")

###################################################
# Cross-sectional OLS Models
###################################################

# Account for NSCH's complex survey design by using TANF_design_exclusions
mod3_cross_anxiety_svy <- svyglm(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_cross_anxiety_svy)
confint(mod3_cross_anxiety_svy)

mod3_cross_depression_svy <- svyglm(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_cross_depression_svy)
confint(mod3_cross_depression_svy)

mod3_cross_adhd_svy <- svyglm(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_cross_adhd_svy)
confint(mod3_cross_adhd_svy)

mod3_cross_bxproblems_svy <- svyglm(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_cross_bxproblems_svy)
confint(mod3_cross_bxproblems_svy)

##################################################
# Two-Way Fixed Effects OLS Models
##################################################

mod3_twfe_anxiety_svy <- svyglm(
  anxiety ~ maxben_3fam + as.factor(year) + as.factor(fipsst) + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_twfe_anxiety_svy)
confint(mod3_twfe_anxiety_svy)

mod3_twfe_depression_svy <- svyglm(
  depression ~ maxben_3fam + as.factor(year) + as.factor(fipsst) + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_twfe_depression_svy)
confint(mod3_twfe_depression_svy)

mod3_twfe_adhd_svy <- svyglm(
  adhd ~ maxben_3fam + as.factor(year) + as.factor(fipsst) + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_twfe_adhd_svy)
confint(mod3_twfe_adhd_svy)

mod3_twfe_bxproblems_svy <- svyglm(
  bx_problems ~ maxben_3fam + as.factor(year) + as.factor(fipsst) + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  design = TANF_master_design_exclusions,
  family = gaussian())

summary(mod3_twfe_bxproblems_svy)
confint(mod3_twfe_bxproblems_svy)

# Function to extract TANF coefficients, 95% CIs, and p-values
extract_tanf <- function(model, outcome, model_name) {
  
  # Extract TANF coefficient
  estimate <- as.numeric(coef(model)["maxben_3fam"])
  
  # Extract 95% confidence interval
  ci <- as.numeric(confint(model)["maxben_3fam", ])
  
  # Extract p-value
  p_value <- if (inherits(model, "fixest")) {
    as.numeric(coeftable(model)["maxben_3fam", 4])
  } else {
    as.numeric(summary(model)$coefficients["maxben_3fam", 4])}
  
  # Convert estimates to percentage points and round
  data.frame(
    Outcome = outcome,
    Model = model_name,
    N = nobs(model),
    Estimate = round(estimate * 100, 2),
    CI_lower = round(ci[1] * 100, 2),
    CI_upper = round(ci[2] * 100, 2),
    P_value = round(p_value, 4))}

supp_table4_results <- dplyr::bind_rows(
  
  # Cross-sectional models
  extract_tanf(mod3_cross_anxiety_svy, "Anxiety", "Cross-sectional"),
  extract_tanf(mod3_cross_depression_svy, "Depression", "Cross-sectional"),
  extract_tanf(mod3_cross_adhd_svy, "ADHD", "Cross-sectional"),
  extract_tanf(mod3_cross_bxproblems_svy, "Behavioral problems", "Cross-sectional"),
  
  # Two-way fixed-effects models
  extract_tanf(mod3_twfe_anxiety_svy, "Anxiety", "TWFE"),
  extract_tanf(mod3_twfe_depression_svy, "Depression", "TWFE"),
  extract_tanf(mod3_twfe_adhd_svy, "ADHD", "TWFE"),
  extract_tanf(mod3_twfe_bxproblems_svy, "Behavioral problems", "TWFE"))

print(supp_table4_results, row.names = FALSE)

###################################################################
#################### Supplemental Figures 1-4 #####################
###################################################################

library(purrr)
library(ggplot2)
library(ggtext)

dat <- TANF_master_exclusions

# Outcomes (order must match the c() on the left of fml_mod3) and plot titles
outcomes <- c("anxiety", "depression", "adhd", "bx_problems")
outcome_labels <- c(
  anxiety     = "Anxiety",
  depression  = "Depression",
  adhd        = "ADHD/ADD",
  bx_problems = "Behavioral Problems")

# Model 3 cross-sectional formula: all 4 outcomes fit in one feols() call
fml_mod3 <- c(anxiety, depression, adhd, bx_problems) ~ maxben_3fam + age +
  as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) +
  as.factor(fam_structure) + as.factor(adult_edu) +
  state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level)

################################################################################
# Function: Fit Model 3 for all outcomes & extract maxben_3fam coefficient
################################################################################

run_mod3 <- function(data) {
  fits <- feols(fml_mod3, data = data, weights = ~weights, vcov = ~fipsst)
  
  map_dfr(seq_along(outcomes), function(i) {
    m <- fits[[i]]
    stopifnot(as.character(m$fml[[2]]) == outcomes[i])  # guard outcome order
    ci <- confint(m)["maxben_3fam", ]
    
    tibble(
      outcome  = outcomes[i],
      beta_pp  = coef(m)[["maxben_3fam"]] * 100,
      lower_pp = ci[[1]] * 100,
      upper_pp = ci[[2]] * 100,
      p_value  = pvalue(m)[["maxben_3fam"]],
      n        = nobs(m)   # actual N used after dropping missing
    )
  })
}

###################################################################
# Fipsst to state abbreviations (alphabetical by state name)
###################################################################

state_lookup <- tibble(
  fips = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19,
           20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
           35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50,
           51, 53, 54, 55, 56),
  
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                 "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                 "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                 "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                 "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                 "WY"))

################################################################################
# Run overall & 51 leave-one-out models (204 outcome-specific estimates)
################################################################################

overall <- run_mod3(dat) %>%
  mutate(state_abbr = "None", result_type = "Overall")

states <- sort(unique(dat$fipsst))
stopifnot(length(states) == 51)

loo_results <- map_dfr(states, function(s) {
  cat("Omitting FIPS", as.character(s), "\n")
  run_mod3(filter(dat, fipsst != s)) %>%
    mutate(fips = as.numeric(as.character(s)))
}) %>%
  left_join(state_lookup, by = "fips") %>%
  mutate(result_type = "Leave-one-out")

# Checks
stopifnot(nrow(loo_results) == 204, !anyNA(loo_results$state_abbr))
table(loo_results$outcome)

# Combine and format for plotting
plot_order <- c("None", state_lookup$state_abbr)

loo_all <- bind_rows(overall, select(loo_results, -fips)) %>%
  mutate(
    outcome    = factor(outcome, levels = outcomes),
    state_abbr = factor(state_abbr, levels = rev(plot_order)),  # None on top
    beta_CI    = sprintf("%.2f (%.2f, %.2f)", beta_pp, lower_pp, upper_pp))

# Range of leave-one-out estimates estimates by outcome
loo_all %>%
  filter(result_type == "Leave-one-out") %>%
  group_by(outcome) %>%
  summarise(
    min_beta  = min(beta_pp),  max_beta  = max(beta_pp),
    min_lower = min(lower_pp), max_upper = max(upper_pp))

write.csv(loo_all, "TANF_leave_one_state_out_results.csv", row.names = FALSE)

################################################################
# Print forest plots (one per outcome)
################################################################

plot_font <- "Times New Roman"

# Common x-axis computed from the data (rounded out to the nearest 0.25)
x_min  <- floor(min(loo_all$lower_pp) * 4) / 4
x_max  <- ceiling(max(loo_all$upper_pp) * 4) / 4
text_x <- x_max + 0.08 * (x_max - x_min)

make_loo_plot <- function(y) {
  ggplot(
    filter(loo_all, outcome == y),
    aes(x = beta_pp, y = state_abbr, color = result_type)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(aes(xmin = lower_pp, xmax = upper_pp), width = 0) +
    geom_point(size = 2) +
    geom_text(aes(label = beta_CI), x = text_x,
              hjust = 0, size = 3, family = plot_font) +
    scale_color_manual(
      values = c("Leave-one-out" = "black", "Overall" = "blue"),
      guide = "none") +
    scale_y_discrete(labels = function(x) {
      ifelse(x == "None", "<span style='color:blue;'>None</span>", x)}) +
    scale_x_continuous(breaks = pretty(c(x_min, x_max))) +
    coord_cartesian(xlim = c(x_min, x_max), clip = "off") +
    labs(
      title = outcome_labels[[y]],
      x = "TANF coefficient, percentage points (95% CI)",
      y = "State omitted") +
    theme_classic(base_family = plot_font) +
    theme(
      plot.title   = element_text(face = "bold", hjust = 0.5),
      axis.text.y  = element_markdown(size = 9),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.margin  = margin(10, 130, 10, 10)  # room for the CI text column
    )}

loo_plots <- map(set_names(outcomes), make_loo_plot)

# View
loo_plots$anxiety
loo_plots$depression
loo_plots$adhd
loo_plots$bx_problems

################################################################
#################### Supplemental Table 5 ######################
################################################################

###################################################
# STRATIFIED BY SEX
###################################################

###################################################
# Cross-sectional OLS Models
###################################################

# Model 3 (not adjusting for sex due to stratification)
# Male children
mod3_cross_anxiety_state_male <- feols(
  anxiety ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_cross_anxiety_state_male)
confint(mod3_cross_anxiety_state_male)

mod3_cross_depression_state_male <- feols(
  depression ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_cross_depression_state_male)
confint(mod3_cross_depression_state_male)

mod3_cross_adhd_state_male <- feols(
  adhd ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_cross_adhd_state_male)
confint(mod3_cross_adhd_state_male)

mod3_cross_bxproblems_state_male <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_cross_bxproblems_state_male)
confint(mod3_cross_bxproblems_state_male)

# Female children
mod3_cross_anxiety_state_female <- feols(
  anxiety ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_cross_anxiety_state_female)
confint(mod3_cross_anxiety_state_female)

mod3_cross_depression_state_female <- feols(
  depression ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_cross_depression_state_female)
confint(mod3_cross_depression_state_female)

mod3_cross_adhd_state_female <- feols(
  adhd ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_cross_adhd_state_female)
confint(mod3_cross_adhd_state_female)

mod3_cross_bxproblems_state_female <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_cross_bxproblems_state_female)
confint(mod3_cross_bxproblems_state_female)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 8 stratified feols() models with state-clustered SEs
cross_tableS5_mod3_sex <- dplyr::bind_rows(
  extract_tanf(mod3_cross_anxiety_state_male,    "Anxiety (male)",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_male, "Depression (male)",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_male,       "ADHD (male)",                "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_male, "Behavioral Problems (male)", "Model 3"),
  extract_tanf(mod3_cross_anxiety_state_female,    "Anxiety (female)",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_female, "Depression (female)",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_female,       "ADHD (female)",                "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_female, "Behavioral Problems (female)", "Model 3"))

print(cross_tableS5_mod3_sex, row.names = FALSE)

###################################################
# Two-Way Fixed Effects OLS Models
###################################################

# Model 3 (not adjusting for sex due to stratification)
# Male children
mod3_twfe_anxiety_state_male <- feols(
  anxiety ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_twfe_anxiety_state_male)
confint(mod3_twfe_anxiety_state_male)

mod3_twfe_depression_state_male <- feols(
  depression ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_twfe_depression_state_male)
confint(mod3_twfe_depression_state_male)

mod3_twfe_adhd_state_male <- feols(
  adhd ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_twfe_adhd_state_male)
confint(mod3_twfe_adhd_state_male)

mod3_twfe_bxproblems_state_male <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Male")

summary(mod3_twfe_bxproblems_state_male)
confint(mod3_twfe_bxproblems_state_male)

# Female children
mod3_twfe_anxiety_state_female <- feols(
  anxiety ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_twfe_anxiety_state_female)
confint(mod3_twfe_anxiety_state_female)

mod3_twfe_depression_state_female <- feols(
  depression ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_twfe_depression_state_female)
confint(mod3_twfe_depression_state_female)

mod3_twfe_adhd_state_female <- feols(
  adhd ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_twfe_adhd_state_female)
confint(mod3_twfe_adhd_state_female)

mod3_twfe_bxproblems_state_female <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ sex == "Female")

summary(mod3_twfe_bxproblems_state_female)
confint(mod3_twfe_bxproblems_state_female)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 8 stratified feols() models with state and year fixed effects and state-clustered SEs
twfe_tableS5_mod3_sex <- dplyr::bind_rows(
  extract_tanf(mod3_twfe_anxiety_state_male,    "Anxiety (male)",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_male, "Depression (male)",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_male,       "ADHD (male)",                "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_male, "Behavioral Problems (male)", "Model 3"),
  extract_tanf(mod3_twfe_anxiety_state_female,    "Anxiety (female)",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_female, "Depression (female)",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_female,       "ADHD (female)",                "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_female, "Behavioral Problems (female)", "Model 3"))

print(twfe_tableS5_mod3_sex, row.names = FALSE)

################################################################################
# STRATIFIED BY AGE GROUP
################################################################################

###################################################
# Cross-sectional OLS Models
###################################################

# Model 3 (still adjusting for ordinal age)
# Children aged 3-11 years old
mod3_cross_anxiety_state_3to11 <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_cross_anxiety_state_3to11)
confint(mod3_cross_anxiety_state_3to11)

mod3_cross_depression_state_3to11 <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_cross_depression_state_3to11)
confint(mod3_cross_depression_state_3to11)

mod3_cross_adhd_state_3to11 <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_cross_adhd_state_3to11)
confint(mod3_cross_adhd_state_3to11)

mod3_cross_bxproblems_state_3to11 <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_cross_bxproblems_state_3to11)
confint(mod3_cross_bxproblems_state_3to11)

# Children aged 12-17 years old
mod3_cross_anxiety_state_12to17 <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_cross_anxiety_state_12to17)
confint(mod3_cross_anxiety_state_12to17)

mod3_cross_depression_state_12to17 <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_cross_depression_state_12to17)
confint(mod3_cross_depression_state_12to17)

mod3_cross_adhd_state_12to17 <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_cross_adhd_state_12to17)
confint(mod3_cross_adhd_state_12to17)

mod3_cross_bxproblems_state_12to17 <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_cross_bxproblems_state_12to17)
confint(mod3_cross_bxproblems_state_12to17)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 8 stratified feols() models with state-clustered SEs
cross_tableS5_mod3_age <- dplyr::bind_rows(
  extract_tanf(mod3_cross_anxiety_state_3to11,    "Anxiety (3-11 year olds)",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_3to11, "Depression (3-11 year olds)",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_3to11,       "ADHD (3-11 year olds)",                "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_3to11, "Behavioral Problems (3-11 year olds)", "Model 3"),
  extract_tanf(mod3_cross_anxiety_state_12to17,    "Anxiety (12-17 year olds)",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_12to17, "Depression (12-17 year olds)",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_12to17,       "ADHD (12-17 year olds)",                "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_12to17, "Behavioral Problems (12-17 year olds)", "Model 3"))

print(cross_tableS5_mod3_age, row.names = FALSE)

###################################################
# Two-Way Fixed Effects OLS Models
###################################################

# Model 3 (still adjusting for ordinal age)
# Children aged 3-11 years old
mod3_twfe_anxiety_state_3to11 <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_twfe_anxiety_state_3to11)
confint(mod3_twfe_anxiety_state_3to11)

mod3_twfe_depression_state_3to11 <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_twfe_depression_state_3to11)
confint(mod3_twfe_depression_state_3to11)

mod3_twfe_adhd_state_3to11 <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_twfe_adhd_state_3to11)
confint(mod3_twfe_adhd_state_3to11)

mod3_twfe_bxproblems_state_3to11 <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "3-11 years")

summary(mod3_twfe_bxproblems_state_3to11)
confint(mod3_twfe_bxproblems_state_3to11)

# Children aged 12-17 years old
mod3_twfe_anxiety_state_12to17 <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_twfe_anxiety_state_12to17)
confint(mod3_twfe_anxiety_state_12to17)

mod3_twfe_depression_state_12to17 <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_twfe_depression_state_12to17)
confint(mod3_twfe_depression_state_12to17)

mod3_twfe_adhd_state_12to17 <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_twfe_adhd_state_12to17)
confint(mod3_twfe_adhd_state_12to17)

mod3_twfe_bxproblems_state_12to17 <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) +  as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ age_group_bin == "12-17 years")

summary(mod3_twfe_bxproblems_state_12to17)
confint(mod3_twfe_bxproblems_state_12to17)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 8 stratified feols() models with state and year fixed effects and state-clustered SEs
twfe_tableS5_mod3_age <- dplyr::bind_rows(
  extract_tanf(mod3_twfe_anxiety_state_3to11,    "Anxiety (3-11 year olds)",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_3to11, "Depression (3-11 year olds)",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_3to11,       "ADHD (3-11 year olds)",                "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_3to11, "Behavioral Problems (3-11 year olds)", "Model 3"),
  extract_tanf(mod3_twfe_anxiety_state_12to17,    "Anxiety (12-17 year olds)",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_12to17, "Depression (12-17 year olds)",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_12to17,       "ADHD (12-17 year olds)",                "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_12to17, "Behavioral Problems (12-17 year olds)", "Model 3"))

print(twfe_tableS5_mod3_age, row.names = FALSE)

################################################################################
# STRATIFIED BY RACE/ETHNICITY GROUP
################################################################################

# To remain consistent with adjustment for continuous age within strata of age_group_bin, models fitted among non-white children also adjust for the more granular race_ethnicity variable
# Non-white children of different races/ethnicities (e.g., Black, Hispanic, and Asian children) differ in states of residence and outcome diagnosis rates
  # Note: fixest will report "The variable 'as.factor(race_ethnicity)White' has been removed because of collinearity"
  # This is expected and benign. The factor retains its "White" level from the full dataset, but no white children are in this subset, so that dummy is all zeros and is dropped; maxben_3fam estimates are unaffected

###################################################
# Cross-sectional OLS Models
###################################################

# Model 3 (not adjusting for race due to stratification)
# Non-Hispanic white children
mod3_cross_anxiety_state_white <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_cross_anxiety_state_white)
confint(mod3_cross_anxiety_state_white)

mod3_cross_depression_state_white <- feols(
  depression ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_cross_depression_state_white)
confint(mod3_cross_depression_state_white)

mod3_cross_adhd_state_white <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_cross_adhd_state_white)
confint(mod3_cross_adhd_state_white)

mod3_cross_bxproblems_state_white <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_cross_bxproblems_state_white)
confint(mod3_cross_bxproblems_state_white)

# Non-white children
mod3_cross_anxiety_state_nonwhite <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_cross_anxiety_state_nonwhite)
confint(mod3_cross_anxiety_state_nonwhite)

mod3_cross_depression_state_nonwhite <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_cross_depression_state_nonwhite)
confint(mod3_cross_depression_state_nonwhite)

mod3_cross_adhd_state_nonwhite <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_cross_adhd_state_nonwhite)
confint(mod3_cross_adhd_state_nonwhite)

mod3_cross_bxproblems_state_nonwhite <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_cross_bxproblems_state_nonwhite)
confint(mod3_cross_bxproblems_state_nonwhite)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 8 stratified feols() models with state-clustered SEs
cross_tableS5_mod3_race <- dplyr::bind_rows(
  extract_tanf(mod3_cross_anxiety_state_white,    "Anxiety (white)",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_white, "Depression (white)",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_white,       "ADHD (white)",                "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_white, "Behavioral Problems (white)", "Model 3"),
  extract_tanf(mod3_cross_anxiety_state_nonwhite,    "Anxiety (non-white)",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_nonwhite, "Depression (non-white)",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_nonwhite,       "ADHD (non-white)",                "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_nonwhite, "Behavioral Problems (non-white)", "Model 3"))

print(cross_tableS5_mod3_race, row.names = FALSE)

###################################################
# Two-Way Fixed Effects OLS Models
###################################################

# Model 3 (not adjusting for race due to stratification)
# Non-Hispanic white children
mod3_twfe_anxiety_state_white <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_twfe_anxiety_state_white)
confint(mod3_twfe_anxiety_state_white)

mod3_twfe_depression_state_white <- feols(
  depression ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_twfe_depression_state_white)
confint(mod3_twfe_depression_state_white)

mod3_twfe_adhd_state_white <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_twfe_adhd_state_white)
confint(mod3_twfe_adhd_state_white)

mod3_twfe_bxproblems_state_white <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex)  + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "White")

summary(mod3_twfe_bxproblems_state_white)
confint(mod3_twfe_bxproblems_state_white)

# Non-white children
mod3_twfe_anxiety_state_nonwhite <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_twfe_anxiety_state_nonwhite)
confint(mod3_twfe_anxiety_state_nonwhite)

mod3_twfe_depression_state_nonwhite <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_twfe_depression_state_nonwhite)
confint(mod3_twfe_depression_state_nonwhite)

mod3_twfe_adhd_state_nonwhite <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_twfe_adhd_state_nonwhite)
confint(mod3_twfe_adhd_state_nonwhite)

mod3_twfe_bxproblems_state_nonwhite <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst,
  subset = ~ race_ethnicity_bin == "Non-white")

summary(mod3_twfe_bxproblems_state_nonwhite)
confint(mod3_twfe_bxproblems_state_nonwhite)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 8 stratified feols() models with state and year fixed effects and state-clustered SEs
twfe_tableS5_mod3_race <- dplyr::bind_rows(
  extract_tanf(mod3_twfe_anxiety_state_white,    "Anxiety (white)",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_white, "Depression (white)",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_white,       "ADHD (white)",                "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_white, "Behavioral Problems (white)", "Model 3"),
  extract_tanf(mod3_twfe_anxiety_state_nonwhite,    "Anxiety (non-white)",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_nonwhite, "Depression (non-white)",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_nonwhite,       "ADHD (non-white)",                "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_nonwhite, "Behavioral Problems (non-white)", "Model 3"))

print(twfe_tableS5_mod3_race, row.names = FALSE)

#####################################
# FULLY-INTERACTED WALD TESTS 
#####################################

# Define outcomes and display labels
outcomes <- c(anxiety        = "Anxiety",
              depression     = "Depression",
              adhd           = "ADHD",
              bx_problems    = "Behavioral Problems")


# Model 3 covariates for each potential effect modifier
covars <- list(
  sex = "age + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) +
         as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level)",
  
  age_group_bin = "age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) +
         as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 +
         percent_6to18 + as.factor(eitc_3level)",
  
  race_ethnicity_bin = "age + as.factor(sex) + as.factor(nativity) + as.factor(fam_structure) +
         as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level)")


modifier_labels <- c(sex                = "Sex",
                     age_group_bin      = "Age group",
                     race_ethnicity_bin = "Race/ethnicity")

# Function: fit fully-interacted model, return Wald p-values
wald_full <- function(outcome, modifier, design = c("Cross-sectional", "TWFE")) {
  design <- match.arg(design)
  
  # Modifier interacted with TANF and every covariate
  fml <- paste0(outcome, " ~ as.factor(", modifier, ") * (maxben_3fam + ",
                covars[[modifier]], ")")
  
  # Allow year and state fixed effects to also vary by stratum
  if (design == "TWFE") {
    fml <- paste0(fml, " | year^", modifier, " + fipsst^", modifier)}
  
  
  m <- feols(
    as.formula(fml),
    data    = TANF_master_exclusions,
    weights = ~weights,
    vcov    = ~fipsst)
  
  # Wald test of the modifier x maxben_3fam term(s)
  w <- wald(m, keep = ":maxben_3fam$", print = FALSE)
  
  tibble(
    Design   = design,
    Modifier = modifier_labels[[modifier]],
    Outcome  = outcomes[[outcome]],
    Wald_F   = w$stat,
    df1      = w$df1,
    Wald_p   = w$p)}

# Run all combinations: 2 designs x 3 modifiers x 4 outcomes = 24 tests
grid <- expand.grid(
  outcome  = names(outcomes),
  modifier = names(covars),
  design   = c("Cross-sectional", "TWFE"),
  stringsAsFactors = FALSE)

wald_full_table <- bind_rows(
  lapply(seq_len(nrow(grid)), function(i) {
    wald_full(grid$outcome[i], grid$modifier[i], grid$design[i])})) %>%
  arrange(Design, Modifier, Outcome)

# Print table
print(wald_full_table, n = Inf)

################################################################
#################### Supplemental Table 6 ######################
################################################################

###################################################
# Test moderate/severe forms of each outcome
###################################################

###################################################
# Cross-sectional OLS Models
###################################################

# Model 3 (fully-adjusted)
mod3_cross_anxiety_severity_state <- feols(
  anxiety_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_anxiety_severity_state)
confint(mod3_cross_anxiety_severity_state)

mod3_cross_depression_severity_state <- feols(
  depression_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_depression_severity_state)
confint(mod3_cross_depression_severity_state)

mod3_cross_adhd_severity_state <- feols(
  adhd_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_adhd_severity_state)
confint(mod3_cross_adhd_severity_state)

mod3_cross_bxproblems_severity_state <- feols(
  bxprobs_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_bxproblems_severity_state)
confint(mod3_cross_bxproblems_severity_state)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 4 feols() models with state-clustered SEs
cross_tableS6_mod3 <- dplyr::bind_rows(
  extract_tanf(mod3_cross_anxiety_severity_state,    "Anxiety Mod/Sev",             "Model 3"),
  extract_tanf(mod3_cross_depression_severity_state, "Depression Mod/Sev",          "Model 3"),
  extract_tanf(mod3_cross_adhd_severity_state,       "ADHD Mod/Sev",                "Model 3"),
  extract_tanf(mod3_cross_bxproblems_severity_state, "Behavioral Problems Mod/Sev", "Model 3"))

print(cross_tableS6_mod3, row.names = FALSE)

###################################################
# Two-Way Fixed Effects OLS Models
###################################################

# Model 3 (fully-adjusted)
mod3_twfe_anxiety_severity_state <- feols(
  anxiety_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_anxiety_severity_state)
confint(mod3_twfe_anxiety_severity_state)

mod3_twfe_depression_severity_state <- feols(
  depression_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_depression_severity_state)
confint(mod3_twfe_depression_severity_state)

mod3_twfe_adhd_severity_state <- feols(
  adhd_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_adhd_severity_state)
confint(mod3_twfe_adhd_severity_state)

mod3_twfe_bxproblems_severity_state <- feols(
  bxprobs_severity ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_bxproblems_severity_state)
confint(mod3_twfe_bxproblems_severity_state)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 4 feols() models with state and year fixed effects and state-clustered SEs
twfe_tableS6_mod3 <- dplyr::bind_rows(
  extract_tanf(mod3_twfe_anxiety_severity_state,    "Anxiety Mod/Sev",             "Model 3"),
  extract_tanf(mod3_twfe_depression_severity_state, "Depression Mod/Sev",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_severity_state,       "ADHD Mod/Sev",                "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_severity_state, "Behavioral Problems Mod/Sev", "Model 3"))

print(twfe_tableS6_mod3, row.names = FALSE)

################################################################
#################### Supplemental Table 7 ######################
################################################################

###################################################
# Exposure lagged by ZERO years (concurrent)
###################################################

#################
# Prepare data
#################

# Read in TANF_master_concurrent
TANF_master_concurrent <- read_excel("TANF_master_concurrent.xlsx")

# Redefine strata
# Recommended by the NSCH for multi-year analyses
TANF_master_concurrent <- TANF_master_concurrent %>% mutate(
  strata = case_when(
    stratum %in% c("1")       ~ "1",
    stratum %in% c("2", "2A") ~ "2"))

# Divide "fwc" by the number of combined survey years, here 7 (2016-2022)
# Recommended by the NSCH for multi-year analyses
TANF_master_concurrent$weights <- TANF_master_concurrent$fwc/7 # fwc corresponds to the selected child weight, Topical surveys
summary(TANF_master_concurrent$weights)
261.682*7 # confirmed to match the original mean = 1831.774

# Rescale weights so mean = 1
TANF_master_concurrent$weights <- TANF_master_concurrent$fwc/1831.774
summary(TANF_master_concurrent$weights)

# List variables of interest
variables <- c(
  "fipsst", "year", "age", "sex", "race_ethnicity", "nativity", "fam_structure","adult_edu", "low_income",
  "anxiety", "depression", "adhd", "bx_problems", 
  "anxiety_severity", "depression_severity","adhd_severity", "bxprobs_severity")

# Identify complete cases across specified variables
TANF_master_concurrent$complete_cases <- complete.cases(TANF_master_concurrent[, variables])

# Implement exclusions 
TANF_master_concurrent_exclusions <- subset(
  TANF_master_concurrent,
  age >= 3 & age <= 17 &
    low_income == "Lower-income" &
    complete.cases(TANF_master_concurrent[, variables]))

# Confirm concurrent analytic sample includes 60218 children
cat("Final analytic sample:", nrow(TANF_master_concurrent_exclusions), "\n")

###################################################
# Cross-sectional OLS Models
###################################################

# Model 3 (fully-adjusted)
mod3_cross_anxiety_state_concurrent <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_anxiety_state_concurrent)
confint(mod3_cross_anxiety_state_concurrent)

mod3_cross_depression_state_concurrent <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_depression_state_concurrent)
confint(mod3_cross_depression_state_concurrent)

mod3_cross_adhd_state_concurrent <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_adhd_state_concurrent)
confint(mod3_cross_adhd_state_concurrent)

mod3_cross_bxproblems_state_concurrent <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_bxproblems_state_concurrent)
confint(mod3_cross_bxproblems_state_concurrent)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 4 feols() models with state-clustered SEs
cross_tableS7_mod3_concurrent <- dplyr::bind_rows(
  extract_tanf(mod3_cross_anxiety_state_concurrent,    "Anxiety",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_concurrent, "Depression",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_concurrent,       "ADHD/ADD",            "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_concurrent, "Behavioral Problems", "Model 3"))

print(cross_tableS7_mod3_concurrent, row.names = FALSE)

###################################################
# Two-Way Fixed Effects OLS Models
###################################################

# Model 3 (fully-adjusted)
mod3_twfe_anxiety_state_concurrent <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_anxiety_state_concurrent)
confint(mod3_twfe_anxiety_state_concurrent)

mod3_twfe_depression_state_concurrent <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_depression_state_concurrent)
confint(mod3_twfe_depression_state_concurrent)

mod3_twfe_adhd_state_concurrent <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_adhd_state_concurrent)
confint(mod3_twfe_adhd_state_concurrent)

mod3_twfe_bxproblems_state_concurrent <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_concurrent_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_bxproblems_state_concurrent)
confint(mod3_twfe_bxproblems_state_concurrent)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 4 feols() models with state and year fixed effects and state-clustered SEs
twfe_tableS7_mod3_concurrent <- dplyr::bind_rows(
  extract_tanf(mod3_twfe_anxiety_state_concurrent,    "Anxiety",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_concurrent, "Depression",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_concurrent,       "ADHD/ADD",            "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_concurrent, "Behavioral Problems", "Model 3"))

print(twfe_tableS7_mod3_concurrent, row.names = FALSE)

###################################################
# Exposure lagged by TWO years 
###################################################

#################
# Prepare data
#################

# Read in TANF_master_2yrlag
TANF_master_2yrlag <- read_excel("TANF_master_2yrlag.xlsx")

# Redefine strata
# Recommended by the NSCH for multi-year analyses
TANF_master_2yrlag <- TANF_master_2yrlag %>% mutate(
  strata = case_when(
    stratum %in% c("1")       ~ "1",
    stratum %in% c("2", "2A") ~ "2"))

# Divide "fwc" by the number of combined survey years, here 7 (2016-2022)
# Recommended by the NSCH for multi-year analyses
TANF_master_2yrlag$weights <- TANF_master_2yrlag$fwc/7 # fwc corresponds to the selected child weight, Topical surveys
summary(TANF_master_2yrlag$weights)
261.682*7 # confirmed to match the original mean = 1831.774

# Rescale weights so mean = 1
TANF_master_2yrlag$weights <- TANF_master_2yrlag$fwc/1831.774
summary(TANF_master_2yrlag$weights)

# List variables of interest
variables <- c(
  "fipsst", "year", "age", "sex", "race_ethnicity", "nativity", "fam_structure","adult_edu", "low_income",
  "anxiety", "depression", "adhd", "bx_problems", 
  "anxiety_severity", "depression_severity","adhd_severity", "bxprobs_severity")

# Identify complete cases across specified variables
TANF_master_2yrlag$complete_cases <- complete.cases(TANF_master_2yrlag[, variables])

# Implement exclusions 
TANF_master_2yrlag_exclusions <- subset(
  TANF_master_2yrlag,
  age >= 3 & age <= 17 &
    low_income == "Lower-income" &
    complete.cases(TANF_master_2yrlag[, variables]))

# Confirm 2yrlag analytic sample includes 60218 children
cat("Final analytic sample:", nrow(TANF_master_2yrlag_exclusions), "\n")

###################################################
# Cross-sectional OLS Models
###################################################

# Model 3 (fully-adjusted)
mod3_cross_anxiety_state_2yrlag <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_anxiety_state_2yrlag)
confint(mod3_cross_anxiety_state_2yrlag)

mod3_cross_depression_state_2yrlag <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_depression_state_2yrlag)
confint(mod3_cross_depression_state_2yrlag)

mod3_cross_adhd_state_2yrlag <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_adhd_state_2yrlag)
confint(mod3_cross_adhd_state_2yrlag)

mod3_cross_bxproblems_state_2yrlag <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level),
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_cross_bxproblems_state_2yrlag)
confint(mod3_cross_bxproblems_state_2yrlag)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 4 feols() models with state-clustered SEs
cross_tableS7_mod3_2yrlag <- dplyr::bind_rows(
  extract_tanf(mod3_cross_anxiety_state_2yrlag,    "Anxiety",             "Model 3"),
  extract_tanf(mod3_cross_depression_state_2yrlag, "Depression",          "Model 3"),
  extract_tanf(mod3_cross_adhd_state_2yrlag,       "ADHD/ADD",            "Model 3"),
  extract_tanf(mod3_cross_bxproblems_state_2yrlag, "Behavioral Problems", "Model 3"))

print(cross_tableS7_mod3_2yrlag, row.names = FALSE)

###################################################
# Two-Way Fixed Effects OLS Models
###################################################

# Model 3 (fully-adjusted)
mod3_twfe_anxiety_state_2yrlag <- feols(
  anxiety ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_anxiety_state_2yrlag)
confint(mod3_twfe_anxiety_state_2yrlag)

mod3_twfe_depression_state_2yrlag <- feols(
  depression ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_depression_state_2yrlag)
confint(mod3_twfe_depression_state_2yrlag)

mod3_twfe_adhd_state_2yrlag <- feols(
  adhd ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_adhd_state_2yrlag)
confint(mod3_twfe_adhd_state_2yrlag)

mod3_twfe_bxproblems_state_2yrlag <- feols(
  bx_problems ~ maxben_3fam + age + as.factor(sex) + as.factor(race_ethnicity) + as.factor(nativity) + as.factor(fam_structure) + as.factor(adult_edu) + state_minwage + percent_1to5 + percent_6to18 + as.factor(eitc_3level) | year + fipsst,
  data = TANF_master_2yrlag_exclusions,
  weights = ~weights,
  vcov = ~fipsst)

summary(mod3_twfe_bxproblems_state_2yrlag)
confint(mod3_twfe_bxproblems_state_2yrlag)

# Use previously defined function to extract TANF coefficients, 95% CIs, and p-values
# Combine results from all 4 feols() models with state and year fixed effects and state-clustered SEs
twfe_tableS7_mod3_2yrlag <- dplyr::bind_rows(
  extract_tanf(mod3_twfe_anxiety_state_2yrlag,    "Anxiety",             "Model 3"),
  extract_tanf(mod3_twfe_depression_state_2yrlag, "Depression",          "Model 3"),
  extract_tanf(mod3_twfe_adhd_state_2yrlag,       "ADHD/ADD",            "Model 3"),
  extract_tanf(mod3_twfe_bxproblems_state_2yrlag, "Behavioral Problems", "Model 3"))

print(twfe_tableS7_mod3_2yrlag, row.names = FALSE)

################################################################################
########## Supplemental Table 8 (and Supplemental Figure TBD) ##################
################################################################################

################################################################################
# DiD with a Continuous Treatment in a Staggered Treatment Adoption Setting
################################################################################

# For more information see: Callaway, Goodman-Bacon & Sant'Anna (NBER 2024/AER 2026): https://www.nber.org/system/files/working_papers/w32117/w32117.pdf
# And package documented updated on 10/30/26: https://cran.r-project.org/web/packages/contdid/contdid.pdf
# And the author's own github repository: https://github.com/bcallaway11/contdid

library(contdid)
library(dplyr)
library(fixest)
library(purrr)
library(ggplot2)
library(did) # For binarized event study analyses

#########################################################
# Preparation for continuous-treatment DiD (contdid)
#########################################################

dat <- TANF_master_exclusions |>
  mutate(year = as.numeric(as.character(year)))

outcomes <- c(
  anxiety     = "Anxiety",
  depression  = "Depression",
  adhd        = "ADHD",
  bx_problems = "Behavioral Problems")

covariate_rhs <- 
  ~ age +
  as.factor(sex) +
  as.factor(race_ethnicity) +
  as.factor(nativity) +
  as.factor(fam_structure) +
  as.factor(adult_edu) +
  state_minwage +
  percent_1to5 +
  percent_6to18 +
  as.factor(eitc_3level)

covars <- all.vars(covariate_rhs)

baseline_year <- 2016
final_year    <- 2022

set.seed(12345)
n_boot <- 1000  

# Define treatment timing and doses (one row per state)
ben <- dat |>
  group_by(fipsst, year) |>
  summarise(
  maxben   = first(maxben_3fam), # Assumes benefit level in each state-year is the same for all children, as it should be 
  n_values = n_distinct(maxben_3fam),
  .groups  = "drop")

# Calculate year-over-year change in benefits (2016 = baseline, so change = 0; note 1-year lag, so 2016 TANF values are actually from 2015)
ben <- ben |>
  arrange(fipsst, year) |>
  group_by(fipsst) |>
  mutate(chg = coalesce(maxben - lag(maxben), 0)) |>
  ungroup()

# Treatment timing and dose for each state
timing <- ben |>
  group_by(fipsst) |>
  summarise(
    n_increases = sum(chg > 1e-8),
    G           = if (n_increases > 0) min(year[chg > 1e-8]) else 0, # G = survey year of first increase (0 = never increased)
    D_first     = if (n_increases > 0) chg[year == G] else 0, # D_first = size of first increase (main dose)
    G2          = if (n_increases > 1) sort(year[chg > 1e-8])[2] else Inf, # G2 = survey year of second increase (Inf = never had a second increase)
    D_total     = maxben[year == final_year] - maxben[year == baseline_year], # D_total = 2022 level - 2016 level (long-difference dose)
   .groups     = "drop")

# Check distribution of first-increase years, number of increases, doses, and comparison states
cat("\nStates by first-increase cohort (0 = never):\n")
print(table(timing$G))

cat("\nStates by number of increases:\n")
print(table(timing$n_increases))

cat("\nFirst-increase dose ($100 units), treated:\n")
print(summary(timing$D_first[timing$G > 0]))

cat("\nTotal 2016-2022 change ($100 units), treated:\n") # Note that we are still using the primary 1-year-lagged exposure, so a state's listed 2016 TANF value actually corresponds to their 2015 value
print(summary(timing$D_total[timing$G > 0]))

cat("\nComparison states per survey year (never treated vs. not yet treated):\n")
print(data.frame(
  year            = baseline_year:final_year,
  never_treated   = sum(timing$G == 0),
  not_yet_treated = sapply(baseline_year:final_year,
                           function(t) sum(timing$G == 0 | timing$G > t))))

cat("\nStates with more than one increase (survey years of each increase):\n")
ben |>
  filter(chg > 1e-8) |>
  group_by(fipsst) |>
  summarise(
  n_increases    = n(),
  increase_years = paste(year, collapse = ", "),
  .groups        = "drop") |>
  filter(n_increases > 1) |>
  arrange(desc(n_increases), fipsst) |>
  as.data.frame() |>
  print(row.names = FALSE)

print(timing, n = Inf)

# Add treatment timing and doses to child-level data
# Untreated = never-treated states and years before first increase
dat <- dat |>
  left_join(
  select(timing, fipsst, G, G2, D_first, D_total),
  by = "fipsst") |>
  mutate(untreated = (G == 0) | (year < G))

################################################################
# Covariate adjustment (not currently supported by contdid)
################################################################

# For more information on this approach, see: https://www.bu.edu/econ/files/2024/07/two-stage-differences-in-differences.pdf

# Adjust each outcome for covariates using untreated children only
# Fit covariates + state and year FE (weighted), then subtract each child's covariate contribution
adjust_outcome <- function(y) {
  d <- dat
  # First-stage regression on untreated children only
  fml <- as.formula(
    paste(y, "~", deparse1(covariate_rhs[[2]]), "| fipsst + year"))
  
  fs <- feols(fml, data = filter(d, untreated), weights = d$weights[d$untreated])
  b <- coef(fs)
  
  # Define covariate design matrix for every child
  X <- model.matrix(
       covariate_rhs,
       model.frame(covariate_rhs, d, na.action = na.pass))
  
  # Warn if any covariate has no first-stage coefficient (e.g., dropped for collinearity with FE), since it would be left out of the adjustment
  missing_b <- setdiff(colnames(X)[colnames(X) != "(Intercept)"], names(b))
  if (length(missing_b)) {
    warning(
      y, ": no first-stage coefficient for ",
      paste(missing_b, collapse = ", "))}
  
  # Subtract the covariate contribution
  d$y_adj <- d[[y]] - drop(X[, names(b), drop = FALSE] %*% b)
  list(data = d, first_stage = fs)}

################################################################
# Aggregate adjusted outcomes to state-year means (panels)
################################################################

# Each cell = survey-weighted mean of y_adj.
# state_wt = the state's average share of children across years (constant within state); used by the did event study.

# Build MAIN panel: 51 states x 7 years
build_main_panel <- function(d) {
  d |>
    group_by(fipsst, year) |>
    summarise(
      Y       = weighted.mean(y_adj, weights),
      cell_wt = sum(weights),
      G       = first(G),
      G2      = first(G2),
      D       = first(D_first),
      .groups = "drop") |>
    group_by(fipsst) |>
    mutate(state_wt = mean(cell_wt)) |>
    ungroup() |>
    mutate(
      state_wt = state_wt / mean(state_wt),
      state_id = as.integer(factor(fipsst)),
      t_idx    = year - baseline_year + 1, # 2016 = 1
      G_idx    = if_else(G == 0, 0, G - baseline_year + 1)) |>
    as.data.frame()}

# Define LONG-DIFFERENCE panel: 2015 (period 1) and 2021 (period 2); note we are using our primary 1-year-lagged exposure here
build_longdiff_panel <- function(d) {
  d |>
    filter(year %in% c(baseline_year, final_year)) |>
    group_by(fipsst, year) |>
    summarise(
      Y       = weighted.mean(y_adj, weights),
      cell_wt = sum(weights),
      D       = first(D_total),
      .groups = "drop") |>
    group_by(fipsst) |>
    mutate(state_wt = mean(cell_wt)) |>
    ungroup() |>
    mutate(
      state_wt = state_wt / mean(state_wt),
      state_id = as.integer(factor(fipsst)),
      period   = if_else(year == baseline_year, 1, 2),
      G_ld     = if_else(D > 0, 2, 0)) |>
    as.data.frame()}

################################################################
# Define functions used to estimate and display results
# Called in Sections 7 and 8 (nothing is estimated here)
################################################################

# Define run_cd(): runs every contdid model with the same settings (straight-line dose, state-level bootstrap)
run_cd <- function(panel,
                   target,
                   agg,
                   label,
                   tname   = "t_idx",
                   gname   = "G_idx",
                   degree  = 1, 
                   control = "notyettreated") {
  cont_did(
    yname                  = "Y",
    dname                  = "D",
    tname                  = tname,
    gname                  = gname,
    idname                 = "state_id",
    data                   = panel,
    target_parameter       = target,
    aggregation            = agg,
    treatment_type         = "continuous",
    dose_est_method        = "parametric",
    degree                 = degree, # Degree = 1, num_knots = 0 (default); dose-response assumed to be a straight line
    num_knots              = 0,
    control_group          = control,
    anticipation           = 0,
    allow_unbalanced_panel = FALSE,
    weightsname            = NULL,   # Warning in working package: "sampling weights not tested yet, may not work"
    base_period            = "universal",
    bstrap                 = TRUE,   # State-level bootstrap
    cband                  = TRUE,   # Uniform bands
    biters                 = n_boot,
    alp                    = 0.05)}

# Define drop_small_cohorts(): removes cohorts with < 3 treated states (too few to fit a dose-response line)
# A 2-state cohort fits a line perfectly, giving zero error and breaking contdid's standard errors
min_cohort_states <- 3

drop_small_cohorts <- function(p,
                               gvar  = "G_idx",
                               min_n = min_cohort_states) {
  sizes <- p |>
    filter(.data[[gvar]] > 0) |>
    distinct(state_id, g = .data[[gvar]]) |>
    count(g)
  
  small <- sizes$g[sizes$n < min_n]
  
  if (length(small)) {message(" Dropping cohort(s) with < ", min_n, " states (period index): ",
      paste(small, collapse = ", "))}
  
  filter(p, !(.data[[gvar]] %in% small))}

# Define run_did_es(): yes/no ("any increase") event study with the did package
# Uses the same panel, controls, and base period as contdid, but includes all 28 treated states and is weighted
run_did_es <- function(panel, label) {
  gt <- att_gt(
    yname         = "Y",
    tname         = "t_idx",
    idname        = "state_id",
    gname         = "G_idx",
    data          = panel,
    control_group = "notyettreated",
    anticipation  = 0,
    weightsname   = "state_wt",
    base_period   = "universal",
    bstrap        = TRUE,
    cband         = TRUE,
    biters        = n_boot)
  
  list(
    event_study = aggte(gt, type = "dynamic", na.rm = TRUE),
    event_study_min4 = aggte(gt, type = "dynamic", min_e = -4, na.rm = TRUE),   # Pre-periods -4 to -1 only (for HonestDiD)
    overall     = aggte(gt, type = "simple",  na.rm = TRUE))}

# Define show_summary(): prints a result to the console under a labelled header
show_summary <- function(obj, label) {
  cat("\n==============================================================\n")
  cat(label, "\n")
  cat("==============================================================\n")
  
  print(summary(obj))}

# Define show_dose_plot(): draws a dose-response plot in the Plots pane
show_dose_plot <- function(obj, type, title) {
  print(ggcont_did(obj, type = type) + ggtitle(title))}

##############################################################################
# Run functions for all four outcomes; covariate-adjust, then build panels
##############################################################################

adjusted        <- map(set_names(names(outcomes)), adjust_outcome)
main_panels     <- map(adjusted, ~ build_main_panel(.x$data))
longdiff_panels <- map(adjusted, ~ build_longdiff_panel(.x$data))

iwalk(
  main_panels,
  ~ cat(sprintf(
    "[MAIN %s] states = %d, cells = %d, treated = %d\n",
    .y,
    n_distinct(.x$state_id),
    nrow(.x),
    n_distinct(.x$state_id[.x$G > 0]))))

##############################################################################
# Event study of each state's first TANF benefit increase
##############################################################################

# es_level : effect of any increase, by years since first increase (did)
# es_slope : ACRT (per $100 of first increase) by years since first increase

main_results <- imap(main_panels, function(p, y) {
  message("Main analysis: ", y)
  p_dose <- drop_small_cohorts(p)  # For the contdid slope run only
  res <- list(
    es_level = run_did_es(p, paste(y, "es_level")),   # all 28 treated states
    es_slope = run_cd(
      p_dose,
      target = "slope",
      agg    = "eventstudy",
      label  = paste(y, "es_slope")))
  
  show_summary(
    res$es_level$overall,
    paste("MAIN | any increase, overall ATT |", outcomes[[y]]))
  show_summary(
    res$es_level$event_study,
    paste("MAIN | event study, any increase |", outcomes[[y]]))
  print(
    ggdid(res$es_level$event_study) +
      ggtitle(paste(outcomes[[y]], "- any increase")))
  show_summary(
    res$es_slope,
    paste("MAIN | event study, ACRT (per $100) |", outcomes[[y]]))
  
  res})

##############################################################################
# Long difference of each state's total TANF benefit increase (2016 vs 2022)
##############################################################################

# Linear dose-response
longdiff_results <- imap(longdiff_panels, function(p, y) {
  message("Long difference: ", y)
  
  res <- run_cd(
    p,
    target  = "slope",
    agg     = "dose",
    label   = paste(y, "longdiff"),
    tname   = "period",
    gname   = "G_ld",
    control = "nevertreated")
  
  show_summary(res, paste("Long difference, 2015 vs 2021 |", outcomes[[y]]))
  show_dose_plot(res, "att", paste(outcomes[[y]], "- ATT(d), 2015 vs 2021"))
  
  res})

# Quadratic dose-response
longdiff_quad <- imap(longdiff_panels, function(p, y) {
  
  res <- run_cd(
    p,
    target  = "slope",
    agg     = "dose",
    label   = paste(y, "longdiff quadratic"),
    tname   = "period",
    gname   = "G_ld",
    control = "nevertreated",
    degree  = 2)
  
  show_summary(res, paste("Long difference, quadratic, 2015 vs 2021 |", outcomes[[y]]))
  show_dose_plot(res, "att", paste(outcomes[[y]], "- ATT(d), quadratic, 2015 vs 2021"))
  
  res})

# Linear and quadratic dose-response, excluding New Hampshire (fipsst 33)
# Checks whether NH's unusually large increase ($423) drives the dose-response
longdiff_noNH <- imap(longdiff_panels, function(p, y) {
  
  p2 <- filter(p, fipsst != "33")
  
  lin <- run_cd(
    p2,
    target  = "slope",
    agg     = "dose",
    label   = paste(y, "noNH linear"),
    tname   = "period",
    gname   = "G_ld",
    control = "nevertreated")
  
  quad <- run_cd(
    p2,
    target  = "slope",
    agg     = "dose",
    label   = paste(y, "noNH quadratic"),
    tname   = "period",
    gname   = "G_ld",
    control = "nevertreated",
    degree  = 2)
  
  show_summary(lin,  paste("Long difference, 2015 vs 2021, linear, 2, no NH |", outcomes[[y]]))
  show_summary(quad, paste("Long difference, 2015 vs 2021, quadratic, no NH  |", outcomes[[y]]))
  show_dose_plot(quad, "att", paste(outcomes[[y]], "- ATT(d), 2015 vs 2021, quadratic, no NH"))
  
  list(linear = lin, quadratic = quad)})

#####################################################################################################
# Check the sensitivity of the event-study results to violations of parallel trends (HonestDiD)
#####################################################################################################

# For more information, see: https://www.jonathandroth.com/assets/files/HonestParallelTrends_Main.pdf
# And the authors' github page: https://github.com/asheshrambachan/HonestDiD

# Asks if parallel trends doesn't hold exactly, how wide does the confidence interval for the average post-increase effect (event times 0-5) become?
# Mbar = size of the allowed post-period violation, relative to the largest pre-period violation (0 = exact parallel trends; 1 = as large as the worst pre-period difference)
# Runs twice:
  # All pre-periods (-6 to -1): main sensitivity result (conservative)
  # Pre-periods -4 to -1 only: complement; drops -6 and -5, which come from only the 2021-2022 cohorts (~6 states)

library(HonestDiD)

# Define honest_es(): confidence interval for the average post-increase effect, allowing parallel trends to be violated
honest_es <- function(es, Mbarvec = c(0, 0.25, 0.5, 0.75, 1)) {
  
  # Event-study estimates and their variance (from the did influence functions)
  inf  <- es$inf.function$dynamic.inf.func.e
  n    <- nrow(inf)
  V    <- t(inf) %*% inf / n / n
  beta <- es$att.egt
  et   <- es$egt
  
  # Drop the reference period (event time -1, fixed at 0)
  ref  <- which(et == -1)
  beta <- beta[-ref]
  V    <- V[-ref, -ref]
  et   <- et[-ref]
  
  # Count pre- and post-periods; average the post-period effects
  npre  <- sum(et < 0)
  npost <- sum(et >= 0)
  l_vec <- rep(1 / npost, npost)
  
  # Confidence intervals allowing violations of size Mbar
  robust <- createSensitivityResults_relativeMagnitudes(
    betahat        = beta,
    sigma          = V,
    numPrePeriods  = npre,
    numPostPeriods = npost,
    l_vec          = l_vec,
    Mbarvec        = Mbarvec)
  
  # Original confidence interval (exact parallel trends)
  original <- constructOriginalCS(
    betahat        = beta,
    sigma          = V,
    numPrePeriods  = npre,
    numPostPeriods = npost,
    l_vec          = l_vec)
  
  list(robust = robust, original = original)}

# Define show_honest(): prints both intervals and draws the sensitivity plot
show_honest <- function(h, label) {
  cat("\n==============================================================\n")
  cat(label, "\n")
  cat("==============================================================\n")
  print(h$original)
  print(h$robust)
  print(
    createSensitivityPlot_relativeMagnitudes(h$robust, h$original) +
      ggtitle(label))}

# All pre-periods (-6 to -1): main sensitivity result
honest_results <- imap(main_results, function(res, y) {
  h <- honest_es(res$es_level$event_study)
  show_honest(h, paste("HonestDiD, all pre-periods |", outcomes[[y]]))
  h})

# Pre-periods -4 to -1 only: complement to all pre-periods results
honest_results_min4 <- imap(main_results, function(res, y) {
  h <- honest_es(res$es_level$event_study_min4)
  show_honest(h, paste("HonestDiD, pre-periods -4 to -1 |", outcomes[[y]]))
  h})


