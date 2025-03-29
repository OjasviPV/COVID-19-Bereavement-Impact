# Data Preparation
# This script prepares the data for analysis, including:
# 1. Loading and formatting data
# 2. Creating derived variables
# 3. Sample selection

library(tidyverse)
library(haven)
library(anesrake)

# Load original data (update path as needed)
load_original_data <- function(data_path) {
  haven::read_dta(data_path)
}

# Create derived variables
create_derived_variables <- function(df) {
  df %>% 
    mutate(
      # Aggregate measures
      stressorsminor = rowSums(select(., stressorsminor___1:stressorsminor___16)),
      stressorsmajor = rowSums(select(., stressorsmajor___1:stressorsmajor___16)),
      
      # Binary measures
      volunteer = ifelse(acta8 == 0, "No", "Yes"),
      caring = ifelse(acta7 == 0, "No", "Yes"),
      socdistance = ifelse(socdist %in% c(0,1,2), "Yes", "No"),
      socisolation = as.numeric(followingisolation),         
      smoking = ifelse(smokechange == 3, "More than usual", "As usual or less/don't smoke"),
      drinking = ifelse(alcoholchange == 3, "More than usual", "As usual or less/don't drink"),
      diet = ifelse(dietchange_3 == 1, "Less healthy than usual", "As usual or more healthy"),
      self.harm = ifelse(harm1 == 1, "Not at all", "At least one day"),
      pa.1 = ifelse(actb3 == 0, "No", "Yes"),
      pa.2 = ifelse(actb4 == 0, "No", "Yes"),
      pa.3 = ifelse(actb6 == 0, "No", "Yes"),         
      sleep.bin = ifelse(sleep %in% c(1,2), "Good Sleep", "Average or Poor Sleep"),
      
      # Convert to numeric and factor
      illness = rowSums(select(., contains("illness___"))),
      illness = ifelse(illness___11 == 1, 0, illness),
      edu = factor(edu, labels = c("GCSE or below", "A levels or equivalent", "Degree or above")),
      lowincome = factor(lowincome, labels = c("≥£30 000", "<£30 000")),
      socfreq.three = ifelse(socfreq == 1, "Every day", 
                            ifelse(socfreq %in% c(2,3), "Once a week or more often", 
                                  "Less than once a week")),
      Service.attendance.category = ifelse(religattend <= 2, "At least once a week", 
                                         ifelse(religattend <= 4, "Less than once a week", 
                                               "Not at all")),
      Service.attendance.category = factor(Service.attendance.category),
      current.smoking = ifelse(smoker == 1, "Non-smoker", 
                             ifelse(smoker == 2, "Ex-smoker", "Current smoker"))
    ) %>% 
    mutate_at(vars(onssat, onshappy, onsworth, socisolation), as.numeric) %>% 
    mutate_at(vars(smoking, drinking, diet, self.harm, sleep.bin, pa.1, pa.2, pa.3), as.factor)
}

# Helper function to get unique participant IDs for specified weeks
get_unique_ids <- function(df, weeks) {
  df %>%
    filter(week %in% weeks) %>%
    select(record_id) %>%
    distinct() %>%
    pull(record_id)
}

# Filter dataset for analysis 1: Short-term effect during lockdown
create_lockdown_analysis_dataset <- function(df) {
  # Get IDs for participants in required weeks
  ids_week1 <- get_unique_ids(df, 1)
  ids_week7 <- get_unique_ids(df, 7)
  ids_week15 <- get_unique_ids(df, 15)
  ids_lockdown <- get_unique_ids(df, c(3, 4, 5, 6))
  
  # Filter to include only participants in all required weeks
  df <- df %>% 
    filter(record_id %in% ids_week1,
           record_id %in% ids_week7,
           record_id %in% ids_week15,
           record_id %in% ids_lockdown)
  
  # Create exposure variable
  df_exposure <- df %>% 
    filter(week %in% c(3, 4, 5, 6)) %>% 
    group_by(record_id) %>% 
    mutate(loss.pandemic = ifelse(sum(adverse___8, na.rm = TRUE) != 0, 1, 0)) %>%
    ungroup() %>% 
    select(record_id, loss.pandemic) %>% 
    distinct()
  
  df <- df %>% left_join(df_exposure)
  
  # Exclude participants who had loss in weeks 1-2
  ids_preexposed <- df %>% 
    filter(week %in% c(1, 2)) %>% 
    mutate(preexposed = ifelse(adverse___8 == 1, 1, 0)) %>% 
    filter(preexposed == 1) %>% 
    select(record_id) %>% 
    distinct() %>% 
    pull(record_id)
  
  df <- df %>% filter(!(record_id %in% ids_preexposed))
  
  # Exclude participants with loss in the past year before the pandemic
  ids_lossprioryear <- df %>% 
    filter(lifeevent2 != 1) %>% 
    select(record_id) %>% 
    distinct() %>% 
    pull(record_id)
  
  df <- df %>% 
    mutate(loss.prioryear = ifelse(record_id %in% ids_lossprioryear, 1, 0),
           exclude.flag = ifelse(loss.pandemic == 1, 0, ifelse(loss.prioryear == 1, 1, 0))) %>% 
    filter(exclude.flag == 0)
  
  return(df)
}

# Filter dataset for analysis 2: Short-term effect post-lockdown
create_postlockdown_analysis_dataset <- function(df) {
  # Get IDs for participants in required weeks
  ids_week16 <- get_unique_ids(df, 16)
  ids_week22 <- get_unique_ids(df, 22)
  ids_postlockdown <- get_unique_ids(df, c(18, 19, 20, 21))
  
  # Filter to include only participants in all required weeks
  df <- df %>% 
    filter(record_id %in% ids_week16,
           record_id %in% ids_week22,
           record_id %in% ids_postlockdown)
  
  # Create exposure variable
  df_exposure <- df %>% 
    filter(week %in% c(18, 19, 20, 21)) %>% 
    group_by(record_id) %>% 
    mutate(loss.nonpandemic = ifelse(sum(adverse___8, na.rm = TRUE) != 0, 1, 0)) %>%
    ungroup() %>% 
    select(record_id, loss.nonpandemic) %>% 
    distinct()
  
  df <- df %>% left_join(df_exposure)
  
  # Exclude participants who had loss in weeks 1-17
  ids_preexposed <- df %>% 
    filter(week %in% c(1:17)) %>% 
    mutate(preexposed = ifelse(adverse___8 == 1, 1, 0)) %>% 
    filter(preexposed == 1) %>% 
    select(record_id) %>% 
    distinct() %>% 
    pull(record_id)
  
  df <- df %>% filter(!(record_id %in% ids_preexposed))
  
  # Exclude participants with loss in the past year
  ids_lossprioryear <- df %>% 
    filter(lifeevent2 != 1) %>% 
    select(record_id) %>% 
    distinct() %>% 
    pull(record_id)
  
  df <- df %>% 
    mutate(loss.prioryear = ifelse(record_id %in% ids_lossprioryear, 1, 0),
           exclude.flag = ifelse(loss.nonpandemic == 1, 0, ifelse(loss.prioryear == 1, 1, 0))) %>% 
    filter(exclude.flag == 0)
  
  return(df)
}

# Filter dataset for analysis 3: Medium-term effect
create_midterm_analysis_dataset <- function(df) {
  # Get IDs for participants in required weeks
  ids_week1 <- get_unique_ids(df, 1)
  ids_week7 <- get_unique_ids(df, 7)
  ids_week15 <- get_unique_ids(df, 15)
  ids_lockdown <- get_unique_ids(df, c(3, 4, 5, 6))
  
  # Filter to include only participants in all required weeks
  df <- df %>% 
    filter(record_id %in% ids_week1,
           record_id %in% ids_week7,
           record_id %in% ids_week15,
           record_id %in% ids_lockdown)
  
  # Create exposure variable
  df_exposure <- df %>% 
    filter(week %in% c(3, 4, 5, 6)) %>% 
    group_by(record_id) %>% 
    mutate(loss.pandemic = ifelse(sum(adverse___8, na.rm = TRUE) != 0, 1, 0)) %>%
    ungroup() %>% 
    select(record_id, loss.pandemic) %>% 
    distinct()
  
  df <- df %>% left_join(df_exposure)
  
  # Exclude participants who had loss in weeks 1-2
  ids_preexposed <- df %>% 
    filter(week %in% c(1, 2)) %>% 
    mutate(preexposed = ifelse(adverse___8 == 1, 1, 0)) %>% 
    filter(preexposed == 1) %>% 
    select(record_id) %>% 
    distinct() %>% 
    pull(record_id)
  
  df <- df %>% filter(!(record_id %in% ids_preexposed))
  
  # Exclude participants with loss in the past year before the pandemic
  ids_lossprioryear <- df %>% 
    filter(lifeevent2 != 1) %>% 
    select(record_id) %>% 
    distinct() %>% 
    pull(record_id)
  
  df <- df %>% 
    mutate(loss.prioryear = ifelse(record_id %in% ids_lossprioryear, 1, 0),
           exclude.flag = ifelse(loss.pandemic == 1, 0, ifelse(loss.prioryear == 1, 1, 0))) %>% 
    filter(exclude.flag == 0)
  
  return(df)
}

# Create weights for survey analysis
create_survey_weights <- function(df, week_to_filter) {
  df_wgt <- df %>% 
    filter(week == week_to_filter) %>% 
    select(record_id, female, country, agegrp4, nonwhite, edu) %>% 
    mutate_at(vars(female, country, agegrp4, nonwhite, edu), as.integer) %>%
    mutate_at(vars(female, nonwhite), function(x) x + 1) %>% 
    as.data.frame()
  
  df_wgt$caseid <- 1:nrow(df_wgt)
  
  # Target proportions for raking
  female_target <- c(0.494, 0.506)
  country_target <- c(0.843, 0.047, 0.082, 0.028)
  agegrp4_target <- c(0.195, 0.261, 0.241, 0.303)
  nonwhite_target <- c(0.872, 0.128)
  edu_target <- c(0.327, 0.339, 0.334)
  
  targets <- list(female_target, country_target, agegrp4_target, nonwhite_target, edu_target)
  names(targets) <- c("female", "country", "agegrp4", "nonwhite", "edu")
  
  # Generate weights
  raked_output <- anesrake(targets, df_wgt, caseid = df_wgt$caseid)
  
  df_wgt$wgt <- raked_output$weightvec
  df_wgt <- df_wgt %>% select(record_id, wgt)
  
  return(df_wgt)
}

# Function to create wide format datasets for analysis
create_wide_dataset <- function(df, week1, week2, df_wgt) {
  # For scale standardization
  scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  
  # Create wide-format outcomes dataset
  df_wide_outcomes <- df %>% 
    filter(week %in% c(week1, week2)) %>% 
    select(record_id, week, PHQ, GAD, stressorsminor, stressorsmajor, onssat, onshappy, onsworth, 
           support, lonely, smoking, drinking, diet, self.harm, sleep.bin, socisolation, 
           volunteer, caring, pa.1, pa.2, pa.3) %>% 
    mutate(
      smoking = ifelse(smoking == "As usual or less/don't smoke", 1, 0),
      drinking = ifelse(drinking == "As usual or less/don't drink", 1, 0),
      diet = ifelse(diet == "As usual or more healthy", 1, 0),
      self.harm = ifelse(self.harm == "Not at all", 0, 1),
      sleep.bin = ifelse(sleep.bin == "Good Sleep", 1, 0),
      volunteer = ifelse(volunteer == "Yes", 1, 0),
      caring = ifelse(caring == "Yes", 1, 0),
      pa.1 = ifelse(pa.1 == "Yes", 1, 0),
      pa.2 = ifelse(pa.2 == "Yes", 1, 0),
      pa.3 = ifelse(pa.3 == "Yes", 1, 0)
    ) %>% 
    distinct_at(vars(record_id, week), .keep_all = TRUE)
  
  # Standardize continuous outcomes and pivot to wide format
  df_wide_outcomes <- df_wide_outcomes %>%
    mutate_at(vars(PHQ, GAD, stressorsminor, stressorsmajor, onssat, onshappy, onsworth, 
                  support, lonely, socisolation), scale2) %>%
    # For mid-term and post-lockdown analyses, we need to recode week numbers
    mutate(week = ifelse(week == week2, 7, 1)) %>%
    pivot_wider(
      names_from = week,
      values_from = c(PHQ, GAD, stressorsminor, stressorsmajor, onssat, onshappy, onsworth, 
                     support, lonely, smoking, drinking, diet, self.harm, sleep.bin, 
                     socisolation, volunteer, caring, pa.1, pa.2, pa.3)
    )
  
  # Get covariates from baseline week
  df_wide_cov <- df %>% 
    filter(week == week1) %>% 
    select(record_id, loss.pandemic, age, female, nonwhite, alone, edu, employed, keyworker, 
           lowincome, illness, Service.attendance.category, current.smoking, alcohol,
           socfreq.three, closefriends, BFI_n, BFI_e, BFI_o, BFI_a, BFI_c) %>% 
    mutate_if(is.character, as.factor)
  
  # For post-lockdown dataset, we need the 'loss.nonpandemic' variable
  if ("loss.nonpandemic" %in% colnames(df)) {
    df_wide_cov <- df %>% 
      filter(week == week1) %>% 
      select(record_id, loss.nonpandemic, age, female, nonwhite, alone, edu, employed, keyworker, 
             lowincome, illness, Service.attendance.category, current.smoking, alcohol,
             socfreq.three, closefriends, BFI_n, BFI_e, BFI_o, BFI_a, BFI_c) %>% 
      mutate_if(is.character, as.factor)
  }
  
  # Join datasets
  df_wide <- df_wide_outcomes %>% 
    left_join(df_wide_cov) %>%
    left_join(df_wgt)
  
  return(df_wide)
}