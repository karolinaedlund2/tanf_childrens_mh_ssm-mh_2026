# tanf_children-s_mh_ssm-mh_2026
================================================================================
README
================================================================================

Full title: Temporary Assistance for Needy Families and Children's Mental Health:
            Evidence from the National Survey of Children's Health (2016-2022)

Short title:      TANF and Children's Mental Health
Prepared for:     Revise and resubmit, SSM - Mental Health
Article type:     Full Length Article
Section:          Psychiatric Epidemiology and Population Mental Health Science
Keywords:         Children's mental health, social determinants, economic policies

Authors:          Karolina M. Edlund, MS
                  Nolan M. Kavanagh, MD, PhD, MPH
                  Vinicius Prado, MA
                  Rachael A. Lennon, PhD, MPA
                  Natalie Slopen, ScD, MA

Please direct any questions to: karolinaedlund@hsph.harvard.edu

================================================================================
OVERVIEW
================================================================================

This folder contains the R programs used to build the analytic datasets and
produce all main and supplemental tables and figures for the manuscript.

The analysis links state-level maximum monthly TANF cash benefits for a family
of three (maxben_3fam) to child mental health outcomes (anxiety, depression,
ADHD, and behavioral problems) among lower-income children aged 3-17 in the
2016-2022 waves of the National Survey of Children's Health (NSCH).

Exposure timing: Primary analyses use a 1-year lagged exposure, so TANF
benefits from 2015-2021 are linked to NSCH survey years 2016-2022. Sensitivity
analyses use concurrent (0-year lag) and 2-year lagged exposures.

================================================================================
RUN ORDER
================================================================================

Programs should be run in the following order. Programs 1-5 build the datasets;
Program 6 runs all analyses.

  Program: 				     Resulting dataset:
  1. TANF_datamgmt1_02.01.26_10.14.26     -> maxben_merged
  2. TANF_datamgmt2_02.01.26_10.14.26     -> state_level_controls
  3. TANF_datamngmt3_05.29.24_10.17.26    -> TANF_master (primary, 1-year lag)
  4. TANF_datamngmt4_02.01.26_10.21.26    -> TANF_master_concurrent (0-year lag)
  5. TANF_datamngmt5_02.01.26_10.21.26    -> TANF_master_2yrlag (2-year lag)
  6. TANF_script_file_10.29.2026.R        -> all tables and figures

All datasets are saved to and read from Karolina Edlund's /data folder; update
the working directory path set at the top of each program before running.

================================================================================
PROGRAM DESCRIPTIONS
================================================================================

--------------------------------------------------------------------------------
1. TANF_datamgmt1_02.01.26_10.14.26
   Last updated: Friday, October 16, 2026
--------------------------------------------------------------------------------
Purpose:
  Pulls in and merges data for the 1-year lagged maximum TANF benefit exposure.

Source:
  The Urban Institute's Welfare Rules Database
  https://wrd.urban.org/welfare-rules-databook

Notes:
  Datasets with the suffix "_collapsed" are versions in which Karolina Edlund
  collapsed the Nonexempt vs. Exempt rows for CA, MA, and WI, following
  clarification from the Urban Institute regarding majority TANF caseloads and
  work requirement exemptions in these states.

Output dataset: maxben_merged.xlsx (clean merged state-year benefit dataset)

--------------------------------------------------------------------------------
2. TANF_datamgmt2_02.01.26_10.14.26
   Last updated: Friday, October 16, 2026
--------------------------------------------------------------------------------
Purpose:
  Pulls in and merges state-level control variables.

Inputs and sources:
  - State minimum wage: "UKCPR_complete"
      Source: University of Kentucky Center for Poverty Research, National Welfare Data
  - Medicaid income eligibility limits, children ages 1-5: "Medicaid eligibility, ages 1-5, state-year, raw.csv" (Nolan Kavanagh)
      Source: Kaiser Family Foundation
  - Medicaid income eligibility limits, children ages 6-18: "Medicaid eligibility, ages 6-18, state-year, raw.csv" (Nolan Kavanagh)
      Source: Kaiser Family Foundation
  - State EITC policies: "EITC policies, state-year, clean.tab" (Nolan Kavanagh)
      Source: Urban Institute and Brookings Institution Tax Policy Center

Processing:
  Merges the four datasets ("ukcpr", "medicaid_1_5_df", "medicaid_6_18_df",
  and "eitc") and creates a 3-level EITC variable indicating whether each state
  has an EITC and, if so, whether it is refundable.

Output dataset: state_level_controls.xlsx

--------------------------------------------------------------------------------
3. TANF_datamngmt3_05.29.24_10.17.26
   Last updated: Wednesday, October 21, 2026
--------------------------------------------------------------------------------
Purpose:
  Pulls in and merges NSCH data from 2016-2022 (prepared by Nolan Kavanagh),
  defines demographic covariates and outcomes, and generates the primary
  analytic dataset.

Output dataset: TANF_master.xlsx (contains the 1-year lagged exposure, maxben_3fam)

--------------------------------------------------------------------------------
4. TANF_datamngmt4_02.01.26_10.21.26
   Last updated: Wednesday, October 21, 2026
--------------------------------------------------------------------------------
Purpose:
  Generates a merged dataset with CONCURRENT (0-year lag) maxben_3fam for
  sensitivity analyses (Supplemental Table 7).

Output dataset: TANF_master_concurrent.xlsx

--------------------------------------------------------------------------------
5. TANF_datamngmt5_02.01.26_10.21.26
   Last updated: Wednesday, October 21, 2026
--------------------------------------------------------------------------------
Purpose:
  Generates a merged dataset with 2-YEAR-LAGGED maxben_3fam for sensitivity
  analyses (Supplemental Table 7).

Output dataset: TANF_master_2yrlag.xlsx

--------------------------------------------------------------------------------
6. TANF_script_file_10.29.2026.R
   Last updated: Friday, November 6, 2026
--------------------------------------------------------------------------------
Purpose:
  Runs all primary and sensitivity analyses.

Inputs:
  TANF_master.xlsx, TANF_master_concurrent.xlsx, TANF_master_2yrlag.xlsx,
  maxben_merged.xlsx

Main figures and tables:
  Figure 1   Flowchart of inclusion and exclusion criteria for the primary
             analytic sample
  Figure 2   Maximum monthly TANF cash benefits as a percent of the federal
             poverty line, 2015
  Figure 3   Maximum monthly TANF cash benefits as a percent of the federal
             poverty line, 2021
  Figure 4   Changes in maximum monthly TANF cash benefits, 2015-2021
  Table 1    Characteristics of children in the primary analytic sample
  Table 2    Primary results from cross-sectional and two-way fixed effects
             (TWFE) OLS models, with SEs clustered by state (not accounting
             for the NSCH's complex survey design)

Supplemental figures and tables:
  Supp. Table 1     Average maximum monthly TANF cash benefits and change
                    over time
  Supp. Table 2     Model 3 results (cross-sectional and TWFE) with
                    Bonferroni-corrected p-values and 98.75% CIs for the four
                    outcomes
  Supp. Table 3     Model 3 results with wild cluster bootstrap inference
                    instead of cluster-robust SEs
  Supp. Table 4     Model 3 results re-estimated with svyglm() to account for
                    the NSCH's complex survey design (strata, clustering, and
                    weights) instead of state-clustered SEs
  Supp. Figs 1-4    Cross-sectional Model 3 results re-estimated omitting one
                    state at a time (one figure per outcome)
  Supp. Table 5     Model 3 stratified by sex, age group, and race/ethnicity,
                    with Wald tests of effect modification from fully
                    interacted models
  Supp. Table 6     Model 3 with moderate/severe forms of each outcome
  Supp. Table 7     Model 3 with TANF benefits measured concurrently (0-year
                    lag) or lagged by 2 years
  Supp. Table 8     Continuous-treatment difference-in-differences (Callaway,
  (and Supp. Figs   Goodman-Bacon, and Sant'Anna) as a heterogeneity-robust
  TBD)              alternative to the TWFE models in Table 2, plus HonestDiD
                    sensitivity analyses for violations of parallel trends.
                    Dose is defined as (a) the size of each state's first
                    benefit increase relative to 2015, to account for
                    staggered treatment adoption, and (b) the total 2015-2021
                    benefit change in a long-difference design, to capture the
                    cumulative effect of multiple benefit changes. Because
                    benefits are lagged 1 year, these correspond to the 2016
                    and 2022 NSCH survey years.

================================================================================
REPRODUCIBILITY NOTES
================================================================================

- Survey weights: Following NSCH guidance for multi-year analyses, the child
  weight (fwc) is divided by the number of combined survey years (7) and
  rescaled to mean = 1. Strata are redefined as recommended by the NSCH and
  crossed with state (102 levels) for the svyglm() analyses.

- Standard errors: Primary analyses (Table 2 and most supplemental tables)
  use survey weights but cluster SEs by state rather than applying the full
  NSCH complex survey design. The TANF exposure varies at the state level, so
  children within the same state share both their exposure and any
  state-level shocks, and inference must account for that correlation. In
  the NSCH design, state enters as a stratum rather than a cluster, so
  design-based SEs treat households within a state as independent and can
  understate uncertainty for a state-level exposure. Because svyglm() cannot
  additionally cluster by state, we instead re-estimate Model 3 under the
  full complex survey design as a sensitivity analysis (Supp. Table 4).

- Random seeds: Bootstrap analyses use set.seed(12345). The wild cluster
  bootstrap (Supp. Table 3) additionally sets dqrng::dqset.seed(12345), as
  required by fwildclusterboot.

- fwildclusterboot: This package has been removed from CRAN, 
  though it can still be installed from the author's r-universe repository: 
  utils::install.packages("fwildclusterboot", repos = "https://s3alfisc.r-universe.dev", type = "source")

- contdid: Covariate adjustment is not currently supported by the contdid
  package, so outcomes are first residualized on covariates and state and year
  fixed effects using untreated observations only (two-stage DiD approach)
  before aggregation to state-year means.

