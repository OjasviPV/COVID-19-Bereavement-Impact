# Analysis Functions
# This script contains the analysis functions for the COVID-19 Bereavement Impact Study

library(tidyverse)
library(survey)
library(ltmle)
library(mice)
library(broom)

# Extract estimates from TMLE for continuous outcomes
extract.estimates.ltmle <- function(model.output.TMLE) {
  output.TMLE <- summary(model.output.TMLE)
  output.TMLE <- output.TMLE$effect.measures
  ATE.TMLE <- output.TMLE$ATE %>% as.data.frame()
  return(ATE.TMLE)
}

# Extract estimates from TMLE for binary outcomes
extract.estimates.ltmle.binary <- function(model.output.TMLE) {
  output.TMLE <- summary(model.output.TMLE)
  output.TMLE <- output.TMLE$effect.measures
  RR.TMLE <- output.TMLE$RR %>% as.data.frame()
  return(RR.TMLE)
}

# Function to perform TMLE analysis for continuous outcomes
perform.TMLE <- function(outcome.name, sample.num, df.imputed.long, m = 5) {
  outcome <- paste(outcome.name, "_7", sep = "")  # week of outcome assessment
  data <- df.imputed.long %>% 
    filter(sample == sample.num) %>% 
    mutate(Yvar = (!!as.name(outcome)))
  
  output <- data %>% 
    group_nest(.imp) %>% 
    mutate(
      # Select relevant variables
      data = map(data, function(x) x %>% select(
        PHQ_1, GAD_1, stressorsmajor_1, stressorsminor_1, onssat_1, onsworth_1, 
        support_1, lonely_1, smoking_1, drinking_1, sleep.bin_1, volunteer_1, 
        caring_1, pa.1_1, pa.2_1, pa.3_1, Service.attendance.category, age, 
        female, nonwhite, alone, edu, employed, keyworker, lowincome, illness, 
        current.smoking, alcohol, socfreq.three, closefriends, BFI_n, BFI_e, 
        BFI_o, BFI_a, BFI_c, loss.pandemic, Yvar
      )),
      # Run TMLE for each imputation
      fit = map(data, function(x) x %>% ltmle(
        Anodes = "loss.pandemic",
        Ynodes = "Yvar",
        abar = list(1, 0),
        observation.weights = x$wgt
      )),
      # Extract estimates
      estimate = map(fit, function(x) extract.estimates.ltmle(x))
    ) 
  
  # Pool results across imputations
  output <- output %>%
    select(.imp, estimate) %>% 
    unnest(estimate) %>% 
    mutate(variance = std.dev^2) %>% 
    summarise(
      estimate.combined = mean(estimate),
      Vw = sum(variance)/m,  # within-imputation variance
      Vb = sum((estimate - mean(estimate))^2/(m-1)),  # between-imputation variance
      Vt = Vw + (1 + 1/m)*Vb,  # total variance
      SE.combined = sqrt(Vt),  # combined standard error
      vm = (m-1)*(1 + (Vw/((1+1/m)*Vb)))^2,  # degrees of freedom
      ci.low = estimate.combined - qt(0.975, vm)*SE.combined,  # lower CI
      ci.up = estimate.combined + qt(0.975, vm)*SE.combined,  # upper CI
      p.value = pt(-abs(estimate.combined/SE.combined), vm)*2  # p-value
    ) %>% 
    mutate_if(is.numeric, round, 3) %>% 
    mutate(outcome = outcome.name) %>% 
    select(outcome, estimate.combined, SE.combined, ci.low, ci.up, p.value)   
  
  return(output)
}

# Function to perform TMLE analysis for binary outcomes
perform.TMLE.binary <- function(outcome.name, sample.num, df.imputed.long, m = 5) {
  outcome <- paste(outcome.name, "_7", sep = "")  # week of outcome assessment
  data <- df.imputed.long %>% 
    filter(sample == sample.num) %>% 
    mutate(Yvar = (!!as.name(outcome)))
  
  output <- data %>% 
    group_nest(.imp) %>% 
    mutate(
      # Select relevant variables
      data = map(data, function(x) x %>% select(
        PHQ_1, GAD_1, stressorsmajor_1, stressorsminor_1, onssat_1, onsworth_1, 
        support_1, lonely_1, smoking_1, drinking_1, sleep.bin_1, volunteer_1, 
        caring_1, pa.1_1, pa.2_1, pa.3_1, Service.attendance.category, age, 
        female, nonwhite, alone, edu, employed, keyworker, lowincome, illness, 
        current.smoking, alcohol, socfreq.three, closefriends, BFI_n, BFI_e, 
        BFI_o, BFI_a, BFI_c, loss.pandemic, Yvar
      )),
      # Run TMLE for each imputation
      fit = map(data, function(x) x %>% ltmle(
        Anodes = "loss.pandemic",
        Ynodes = "Yvar",
        abar = list(1, 0),
        observation.weights = x$wgt
      )),
      # Extract estimates
      estimate = map(fit, function(x) extract.estimates.ltmle.binary(x))
    ) 
  
  # Pool results across imputations
  output <- output %>%
    select(.imp, estimate) %>% 
    unnest(estimate) %>% 
    mutate(variance = std.dev^2) %>% 
    summarise(
      estimate.combined = mean(estimate),
      Vw = sum(variance)/m,  # within-imputation variance
      Vb = sum((log(estimate) - mean(log(estimate)))^2/(m-1)),  # between-imputation variance on log scale
      Vt = Vw + (1 + 1/m)*Vb,  # total variance
      SE.combined = sqrt(Vt),  # combined standard error
      vm = (m-1)*(1 + (Vw/((1+1/m)*Vb)))^2,  # degrees of freedom
      ci.low = exp(log(estimate.combined) - qt(0.975, vm)*SE.combined),  # lower CI
      ci.up = exp(log(estimate.combined) + qt(0.975, vm)*SE.combined),  # upper CI
      p.value = pt(-abs(log(estimate.combined)/SE.combined), vm)*2  # p-value
    ) %>% 
    mutate_if(is.numeric, round, 4) %>% 
    mutate(outcome = outcome.name) %>% 
    select(outcome, estimate.combined, SE.combined, ci.low, ci.up, p.value)   
  
  return(output)
}

# Run multiple imputation
run_multiple_imputation <- function(df.wide.combined, m = 5) {
  # Check missing data
  miss.check <- df.wide.combined %>% 
    select(-record_id) %>% 
    summarise_all(funs(sum(is.na(.)))) %>%
    pivot_longer(
      everything(),
      names_to = "Variable",
      values_to = "n.miss"
    ) %>% 
    arrange(desc(n.miss))
  
  # Exclude variables with complete missing data
  vars.ordered <- miss.check %>% 
    filter(n.miss < nrow(df.wide.combined)) %>% 
    select(Variable) %>% 
    pull(Variable) %>% 
    as.vector()
  
  # Set up data for imputation
  df.analytic <- df.wide.combined %>% 
    select(all_of(vars.ordered), record_id)
  
  # Perform multiple imputation
  set.seed(123)
  df.imputed <- df.analytic %>% 
    mice(., m = m, pred = quickpred(., exclude = c("record_id")), print = FALSE)
  
  # Convert to long format
  df.imputed.long <- df.imputed %>% complete("long")
  
  return(df.imputed.long)
}

# Run all analyses and combine results
run_all_analyses <- function(df.imputed.long, m = 5) {
  # Define outcomes
  continuous_outcomes <- c("onssat", "onshappy", "onsworth", "support", "lonely", 
                          "socisolation", "PHQ", "GAD", "stressorsminor", "stressorsmajor")
  binary_outcomes <- c("volunteer", "caring", "self.harm", "smoking", "drinking", 
                      "diet", "pa.1", "pa.2", "pa.3", "sleep.bin")
  
  # Analysis 1: Short-term effect during lockdown
  results_lockdown <- bind_rows(
    # Continuous outcomes
    map_df(continuous_outcomes, ~perform.TMLE(.x, 1, df.imputed.long, m)),
    # Binary outcomes
    map_df(binary_outcomes, ~perform.TMLE.binary(.x, 1, df.imputed.long, m))
  ) %>%
    mutate(analysis = "Short-term during lockdown")
  
  # Analysis 2: Short-term effect post-lockdown
  results_postlockdown <- bind_rows(
    # Continuous outcomes
    map_df(continuous_outcomes, ~perform.TMLE(.x, 2, df.imputed.long, m)),
    # Binary outcomes
    map_df(binary_outcomes, ~perform.TMLE.binary(.x, 2, df.imputed.long, m))
  ) %>%
    mutate(analysis = "Short-term post-lockdown")
  
  # Analysis 3: Medium-term effect
  results_midterm <- bind_rows(
    # Continuous outcomes
    map_df(continuous_outcomes, ~perform.TMLE(.x, 3, df.imputed.long, m)),
    # Binary outcomes
    map_df(binary_outcomes, ~perform.TMLE.binary(.x, 3, df.imputed.long, m))
  ) %>%
    mutate(analysis = "Medium-term")
  
  # Combine all results
  all_results <- bind_rows(
    results_lockdown,
    results_postlockdown,
    results_midterm
  ) %>%
    mutate(
      # Add significance indicators
      sig = ifelse(p.value*20 < 0.05, "***",  # Bonferroni correction
                  ifelse(p.value < 0.01, "**",
                        ifelse(p.value < 0.05, "*", "N.S.")))
    )
  
  return(all_results)
}