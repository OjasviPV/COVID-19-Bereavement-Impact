# Visualization Functions
# This script contains functions for creating tables and figures

library(tidyverse)
library(gtsummary)
library(ggplot2)
library(ggsci)
library(knitr)
library(kableExtra)

# Create demographic tables
create_demographic_table <- function(df, df_wgt, by_var = "loss.pandemic", week_num = 1) {
  # Create a survey design object
  svy_df <- df %>% 
    filter(week == week_num) %>% 
    select(-Service.attendance.category) %>% 
    left_join(df_wgt) %>%
    mutate_at(vars(closefriends, alcohol), as.numeric) %>% 
    select(
      age, female, nonwhite, alone, edu, employed, keyworker, lowincome, illness,
      socfreq.three, closefriends, BFI_n, BFI_e, BFI_o, BFI_a, BFI_c, 
      all_of(by_var), onssat, onsworth, support, lonely, volunteer, caring, 
      socisolation, PHQ, GAD, stressorsminor, stressorsmajor, current.smoking,
      alcohol, smoking, drinking, pa.1, pa.2, pa.3, sleep.bin, record_id, wgt
    )
  
  svy_df <- survey::svydesign(
    id = ~record_id,
    weights = ~wgt,
    data = svy_df
  )
  
  # Create the gtsummary table
  tbl <- svy_df %>% 
    tbl_svysummary(
      by = all_of(by_var),
      statistic = list(all_continuous() ~ "{mean} ({sd})"),
      missing_text = "(Missing)",
      include = c(
        age, female, nonwhite, alone, edu, employed, keyworker, lowincome, illness,
        socfreq.three, closefriends, BFI_n, BFI_e, BFI_o, BFI_a, BFI_c, 
        onssat, onsworth, support, lonely, volunteer, caring, socisolation, PHQ, 
        GAD, stressorsminor, stressorsmajor, current.smoking, alcohol, smoking, 
        drinking, pa.1, pa.2, pa.3, sleep.bin
      ),
      label = list(
        age ~ "Age",
        female ~ "Female Gender",
        nonwhite ~ "Non-white Ethnicity",
        edu ~ "Education",
        lowincome ~ "Low Income (<£30 000)",
        illness ~ "Number of health conditions",
        socfreq.three ~ "Frequency of meeting up with people in usual life",
        closefriends ~ "Number of close friends",
        current.smoking ~ "Smoking status",
        alcohol ~ "Number of alcoholic drinks in the past week",
        stressorsmajor ~ "Stress major", 
        stressorsminor ~ "Stress minor",
        onssat ~ "Satisfaction with life",
        onsworth ~ "Meaning",
        volunteer ~ "Volunteering",
        caring ~ "Caring",
        socisolation ~ "Compliance with social isolation",
        smoking ~ "No unhealthy smoking change",
        drinking ~ "No unhealthy drinking change",
        pa.1 ~ "Gentle physical activity",
        pa.2 ~ "High intensity physical activity",
        pa.3 ~ "Exercising at home",      
        sleep.bin ~ "Good Sleep"
      ),
      type = list(
        lonely ~ "continuous", 
        illness ~ "continuous", 
        socisolation ~ "continuous",
        sleep.bin ~ "dichotomous", 
        drinking ~ "dichotomous", 
        smoking ~ "dichotomous", 
        lowincome ~ "dichotomous"
      ),
      value = list(
        sleep.bin ~ "Good Sleep", 
        smoking ~ "As usual or less/don't smoke", 
        drinking ~ "As usual or less/don't drink", 
        lowincome ~ "<£30 000"
      ),
      missing = "no"   
    ) %>% 
    bold_labels() %>% 
    add_overall() %>% 
    modify_header(
      update = list(label ~ "**Pre-baseline Characteristics**")
    ) %>% 
    modify_spanning_header(starts_with("stat_") ~ "**Bereavement**")
  
  return(tbl)
}

# Create outcome tables for each analysis
create_outcome_table <- function(df, df_wgt, outcome_week) {
  # Create a survey design object
  svy_df <- df %>% 
    filter(week == outcome_week) %>% 
    left_join(df_wgt)
  
  svy_df <- survey::svydesign(
    id = ~record_id,
    weights = ~wgt,
    data = svy_df
  )
  
  # Create the gtsummary table
  tbl <- svy_df %>% 
    tbl_svysummary(
      include = c(
        onssat, onshappy, onsworth, support, lonely, volunteer, caring, socisolation,
        PHQ, GAD, stressorsminor, stressorsmajor, self.harm, smoking, drinking, 
        diet, pa.1, pa.2, pa.3, sleep.bin, loss.pandemic
      ),
      by = loss.pandemic,
      statistic = list(all_continuous() ~ "{mean} ({sd})"),
      missing_text = "(Missing)",
      label = list(
        stressorsmajor ~ "Stress major", 
        stressorsminor ~ "Stress minor",
        onssat ~ "Satisfaction with life",
        onshappy ~ "Happiness",
        onsworth ~ "Meaning",
        volunteer ~ "Volunteering",
        caring ~ "Caring",
        socisolation ~ "Compliance with social isolation",
        smoking ~ "No unhealthy smoking change",
        drinking ~ "No unhealthy drinking change",
        diet ~ "No unhealthy diet change",
        self.harm ~ "Self-harm ideation",
        pa.1 ~ "Gentle physical activity",
        pa.2 ~ "High intensity physical activity",
        pa.3 ~ "Exercising at home",
        sleep.bin ~ "Good Sleep"
      ),
      type = list(
        lonely ~ "continuous", 
        socisolation ~ "continuous",
        sleep.bin ~ "dichotomous", 
        drinking ~ "dichotomous", 
        smoking ~ "dichotomous"
      ),
      value = list(
        sleep.bin ~ "Good Sleep", 
        smoking ~ "As usual or less/don't smoke", 
        drinking ~ "As usual or less/don't drink", 
        diet ~ "As usual or more healthy", 
        self.harm ~ "At least one day"
      ),
      missing = "no"
    ) %>% 
    bold_labels() %>% 
    modify_header(
      update = list(label ~ paste0("**Outcome at Week ", outcome_week, "**"))
    ) %>% 
    add_p() %>% 
    add_overall() %>% 
    modify_spanning_header(starts_with("stat_") ~ "**Bereavement during the lockdown**")
  
  return(tbl)
}

# Create forest plots for TMLE results
create_forest_plots <- function(results) {
  # Define continuous and categorical outcomes
  continuous_outcomes <- c("onssat", "onshappy", "onsworth", "support", "lonely", 
                         "socisolation", "PHQ", "GAD", "stressorsminor", "stressorsmajor")
  
  # Plot for continuous outcomes
  continuous_plot <- results %>%
    filter(outcome %in% continuous_outcomes) %>%
    mutate(
      analysis = factor(analysis, 
                      levels = c("Short-term during lockdown", "Short-term post-lockdown", "Medium-term")),
      # Create more readable outcome labels
      outcome_label = case_when(
        outcome == "onssat" ~ "Life Satisfaction",
        outcome == "onshappy" ~ "Happiness",
        outcome == "onsworth" ~ "Life Meaning",
        outcome == "support" ~ "Social Support",
        outcome == "lonely" ~ "Loneliness",
        outcome == "socisolation" ~ "Social Isolation",
        outcome == "PHQ" ~ "Depression (PHQ)",
        outcome == "GAD" ~ "Anxiety (GAD)",
        outcome == "stressorsminor" ~ "Minor Stressors",
        outcome == "stressorsmajor" ~ "Major Stressors",
        TRUE ~ outcome
      ),
      # Order outcomes for visualization
      outcome_label = factor(outcome_label, levels = c(
        "Life Satisfaction", "Happiness", "Life Meaning", "Social Support", 
        "Loneliness", "Social Isolation", "Depression (PHQ)", "Anxiety (GAD)", 
        "Minor Stressors", "Major Stressors"
      ))
    ) %>%
    ggplot(aes(x = outcome_label, y = estimate.combined, 
              ymin = ci.low, ymax = ci.up, 
              shape = analysis, color = analysis)) +
    geom_pointrange(position = position_dodge(width = 0.6)) +
    coord_flip() +
    scale_color_nejm() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(
      x = "",
      y = "Standardized Mean Difference",
      color = "Analysis Period",
      shape = "Analysis Period",
      title = "Effect of Bereavement on Continuous Outcomes"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Plot for binary outcomes
  binary_plot <- results %>%
    filter(!(outcome %in% continuous_outcomes)) %>%
    mutate(
      analysis = factor(analysis, 
                      levels = c("Short-term during lockdown", "Short-term post-lockdown", "Medium-term")),
      # Create more readable outcome labels
      outcome_label = case_when(
        outcome == "volunteer" ~ "Volunteering",
        outcome == "caring" ~ "Caring",
        outcome == "self.harm" ~ "Self-harm Ideation",
        outcome == "smoking" ~ "Healthy Smoking Behavior",
        outcome == "drinking" ~ "Healthy Drinking Behavior",
        outcome == "diet" ~ "Healthy Diet",
        outcome == "pa.1" ~ "Gentle Physical Activity",
        outcome == "pa.2" ~ "High Intensity Activity",
        outcome == "pa.3" ~ "Home Exercise",
        outcome == "sleep.bin" ~ "Good Sleep",
        TRUE ~ outcome
      ),
      # Order outcomes for visualization
      outcome_label = factor(outcome_label, levels = c(
        "Volunteering", "Caring", "Self-harm Ideation", "Healthy Smoking Behavior",
        "Healthy Drinking Behavior", "Healthy Diet", "Gentle Physical Activity", 
        "High Intensity Activity", "Home Exercise", "Good Sleep"
      ))
    ) %>%
    ggplot(aes(x = outcome_label, y = estimate.combined, 
              ymin = ci.low, ymax = ci.up, 
              shape = analysis, color = analysis)) +
    geom_pointrange(position = position_dodge(width = 0.6)) +
    coord_flip() +
    scale_color_nejm() +
    geom_hline(yintercept = 1, linetype = "dotted") +
    labs(
      x = "",
      y = "Risk Ratio",
      color = "Analysis Period",
      shape = "Analysis Period",
      title = "Effect of Bereavement on Binary Outcomes"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(list(continuous_plot = continuous_plot, binary_plot = binary_plot))
}