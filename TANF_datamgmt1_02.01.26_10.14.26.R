################################################################################
################################################################################
## Program: TANF_datamgmt1_02.01.26_10.14.26
## Last updated: Friday, October 16, 2026
## Programmer: Karolina Edlund

## Purpose: This program pulls in and merges data for our 1-year lagged maximum TANF benefits exposure variable
   # Source: The Urban Institute's Welfare Rules Database - https://wrd.urban.org/welfare-rules-databook
        # The dataset naming convention "_collapsed" simply denotes that Karolina Edlund collapsed rows for collapsed rows for Nonexempt vs. Exempt for CA, MA and WI based on email from the Urban Institute who clarified TANF majority caseloads and work requirement exemptions in these states
   # The resulting clean merged dataset "maxben_merged" is subsequently saved to Karolina Edlund's /data folder (working directory path shown below)
################################################################################
################################################################################

# Load packages
library(readxl)
library(dplyr)
library(tidyr)

#Set working directory
getwd()
setwd("/Users/karolinaedlund/Desktop/HSPH/Research/HSPH/FINAL TANF MANUSCRIPT/TANF FOR SUBMISSION/SUBMISSION/Revise and Resubmit/data/")

#############################################################################
#################### Read in Urban Institute TANF data ######################
#############################################################################

# Read in datasets
# Source: The Urban Institute's Welfare Rules Database - https://wrd.urban.org/welfare-rules-databook
MaxBen_3Fam_2015_collapsed <- read_excel("MaxBen_3Fam_2015_collapsed.xlsx")
MaxBen_3Fam_2016_collapsed <- read_excel("MaxBen_3Fam_2016_collapsed.xlsx")
MaxBen_3Fam_2017_collapsed <- read_excel("MaxBen_3Fam_2017_collapsed.xlsx")
MaxBen_3Fam_2018_collapsed <- read_excel("MaxBen_3Fam_2018_collapsed.xlsx")
MaxBen_3Fam_2019_collapsed <- read_excel("MaxBen_3Fam_2019_collapsed.xlsx")
MaxBen_3Fam_2020_collapsed <- read_excel("MaxBen_3Fam_2020_collapsed.xlsx")
MaxBen_3Fam_2021_collapsed <- read_excel("MaxBen_3Fam_2021_collapsed.xlsx")

# Merge datasets
# This stacks each year's data on top of one another, with 2015 at the top and 2021 at the bottom
maxben_merged <- rbind(MaxBen_3Fam_2015_collapsed, MaxBen_3Fam_2016_collapsed, MaxBen_3Fam_2017_collapsed, MaxBen_3Fam_2018_collapsed,
                       MaxBen_3Fam_2019_collapsed, MaxBen_3Fam_2020_collapsed, MaxBen_3Fam_2021_collapsed)

# Add a column of abbreviations to the right of the state names column
maxben_merged <- maxben_merged %>%
  mutate(
    state_name = if_else(
      state_name == "DC", "District of Columbia", state_name # Ensure state_name column includes "District of Columbia" not DC
    ),
    st_abbr = case_when(
      state_name == "District of Columbia" ~ "DC",
      TRUE ~ state.abb[match(state_name, state.name)]
    ),
    .after = state_name
  )

# Rename columns
colnames(maxben_merged)  <- c("state", "st_abbr", "year", "maxben_3fam")

# Convert names to FIPS
# Necessary for smooth merge
maxben_merged$fipsst  <- cdlTools::fips(maxben_merged$state,  to="FIPS")

# Treat year as numeric
maxben_merged$year  <- as.numeric(maxben_merged$year)

# Reorder columns
maxben_merged <- maxben_merged %>%
  select(state, st_abbr, fipsst, year, maxben_3fam)

# Guidance received via email from the Urban Institute's Welfare Rules Database staff on 7/9/2023
    # Subject: RE: Guidance re exempt vs. nonexempt maximum TANF benefits (2015-2022))
    # I did review the WRD and I can confirm that the components that make up a majority of the caseload are as follows:
         # California: Non-exempt
         # Massachusetts: Exempt (through 11/29/2018)
         # Wisconsin: W-2T
    # The reason you only see one row for MA beginning in 2019 is because benefits did not vary across Exempt and Non-exempt groups in 2019 and later. 
    # For Wisconsin, if you average the W-2T and CSJ benefits, you would still be missing the benefits policies for the other two component groups (CSJ and TEMP). 
    # I might recommend showing the policies for a single component (W-2T) instead, as is done in the WRD.

# Fix California's maxben_3fam values --> make all NON-EXEMPT
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "CA" & year == 2015, 704, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "CA" & year == 2016, 704, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "CA" & year == 2017, 714, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "CA" & year == 2018, 714, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "CA" & year == 2019, 785, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "CA" & year == 2020, 878, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "CA" & year == 2021, 878, maxben_3fam))


# Fix Massachusett's values --> make EXEMPT, through 11/29/2018
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "MA" & year == 2015, 633, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "MA" & year == 2016, 633, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "MA" & year == 2017, 633, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "MA" & year == 2018, 633, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "MA" & year == 2019, 633, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "MA" & year == 2020, 633, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "MA" & year == 2021, 752, maxben_3fam))

# FIX Wisconsins's values --> make W2-T
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "WI" & year == 2015, 608, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "WI" & year == 2016, 608, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "WI" & year == 2017, 608, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "WI" & year == 2018, 608, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "WI" & year == 2019, 608, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "WI" & year == 2020, 608, maxben_3fam))
maxben_merged <- maxben_merged %>%
  mutate(maxben_3fam = ifelse(st_abbr == "WI" & year == 2021, 608, maxben_3fam))

#############################################################
############# Save the new maxben_merged dataset ############
#############################################################

# Save dataset to desktop
library(openxlsx)
write.xlsx(maxben_merged, file = "~/Desktop/HSPH/Research/HSPH/FINAL TANF MANUSCRIPT/TANF FOR SUBMISSION/SUBMISSION/Revise and Resubmit/data/maxben_merged.xlsx", rowNames = FALSE)

# Clear datasets from R Global Environment
rm(MaxBen_3Fam_2015_collapsed, MaxBen_3Fam_2016_collapsed, MaxBen_3Fam_2017_collapsed, MaxBen_3Fam_2018_collapsed,
   MaxBen_3Fam_2019_collapsed, MaxBen_3Fam_2020_collapsed, MaxBen_3Fam_2021_collapsed)

