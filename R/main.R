# Main Analysis Script
# This script runs the complete analysis for the COVID-19 Bereavement Impact Study

# Load required libraries
library(tidyverse)
library(mice)
library(ltmle)
library(survey)
library(gtsummary)
library(ggplot2)
library(ggsci)
library(knitr)
library(kableExtra)
library(haven)

# Load functions from other scripts
source("R/data_preparation.R")
source("R/analysis.R")
source("R/visualization.R")

# STEP 1: Load and prepare data
data_path <- "PATH_TO_DATA.dta"  # Replace with actual data path
df.original <- load_original_data(data_path)
df <- create_derived_variables(df.original)

# STEP 2: Create datasets for each analysis
# Analysis 1: Short-term effect during lockdown
df_lockdown <- create_lockdown_analysis_dataset(df)
df_wgt_lockdown <- create_survey_weights(df_lockdown, week_to_filter = 1)

# Analysis 2: Short-term effect post-lockdown
df_postlockdown <- create_postlockdown_analysis_dataset(df)
df_wgt_postlockdown <- create_survey_weights(df_postlockdown, week_to_filter = 16)

# Analysis 3: Medium-term effect 
df_midterm <- create_midterm_analysis_dataset(df)
df_wgt_midterm <- create_survey_weights(df_midterm, week_to_filter = 1)

# STEP 3: Create wide format datasets for analysis
df.wide1 <- create_wide_dataset(df_lockdown, week1 = 1, week2 = 7, df_wgt_lockdown)
df.wide2 <- create_wide_dataset(df_postlockdown, week1 = 16, week2 = 22, df_wgt_postlockdown)
df.wide3 <- create_wide_dataset(df_midterm, week1 = 1, week2 = 15, df_wgt_midterm)

# STEP 4: Combine datasets for imputation
df.wide.combined <- bind_rows(
  df.wide1 %>% mutate(sample = 1, lockdown = 1, short = 1),
  df.wide2 %>% mutate(sample = 2, lockdown = 0, short = 1) %>% rename(loss.pandemic = loss.nonpandemic),
  df.wide3 %>% mutate(sample = 3, lockdown = 1, short = 0)
)

# STEP 5: Run multiple imputation
df.imputed.long <- run_multiple_imputation(df.wide.combined, m = 5)

# STEP 6: Run all analyses
all_results <- run_all_analyses(df.imputed.long, m = 5)

# STEP 7: Create tables
# Demographic tables for each analysis
table_demographics_lockdown <- create_demographic_table(df_lockdown, df_wgt_lockdown, by_var = "loss.pandemic", week_num = 1)
table_demographics_postlockdown <- create_demographic_table(df_postlockdown, df_wgt_postlockdown, by_var = "loss.nonpandemic", week_num = 16)
table_demographics_midterm <- create_demographic_table(df_midterm, df_wgt_midterm, by_var = "loss.pandemic", week_num = 1)

# Outcome tables for each analysis
table_outcomes_lockdown <- create_outcome_table(df_lockdown, df_wgt_lockdown, outcome_week = 7)
table_outcomes_postlockdown <- create_outcome_table(df_postlockdown, df_wgt_postlockdown, outcome_week = 22)
table_outcomes_midterm <- create_outcome_table(df_midterm, df_wgt_midterm, outcome_week = 15)

# STEP 8: Create visualization
plots <- create_forest_plots(all_results)

# Save plots
ggsave("figures/continuous_outcomes.png", plots$continuous_plot, width = 10, height = 8)
ggsave("figures/binary_outcomes.png", plots$binary_plot, width = 10, height = 8)

# Print results table
all_results %>%
  select(outcome, analysis, estimate.combined, ci.low, ci.up, p.value, sig) %>%
  arrange(analysis, outcome) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)