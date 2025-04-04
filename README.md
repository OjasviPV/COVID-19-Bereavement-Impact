# COVID-19 Bereavement Impact Study

This repository contains code and documentation for a longitudinal analysis examining the impact of bereavement on wellbeing indices during and after the COVID-19 pandemic, using data from the UK COVID Social Study.

## Overview

The COVID-19 pandemic led to unprecedented levels of mortality and grief worldwide. This study investigates how bereavement during different phases of the pandemic affected various aspects of mental health and wellbeing over time.

### Research Questions

1. How did bereavement during the COVID-19 lockdown impact mental wellbeing?
2. Does the timing of bereavement (during or after lockdown) affect the severity of its impact on wellbeing?
3. What are the short-term, mid-term, and post-lockdown effects of bereavement on different aspects of life?

### Key Findings

- **Short-Term Effects During Lockdown:** Bereavement during lockdown was associated with immediate declines in happiness and increases in minor stressors.

- **Medium-Term Effects:** Delayed effects of bereavement during lockdown included increased social isolation and major stressors emerging in the mid-term analysis.

- **Post-Lockdown Effects:** Bereavement after lockdown was associated with higher levels of depression, anxiety, and loneliness compared to bereavement during lockdown.

- **Behavior Changes:** Post-lockdown bereavement was associated with a significant increase in self-harm, while those bereaved during lockdown exhibited higher levels of volunteering in the mid-term.

These findings emphasize the importance of timing in bereavement support, suggesting the need for tailored interventions to address both immediate and delayed effects on wellbeing.

## Repository Structure

- `OW.bereavement.Rmd`: R Markdown file containing the complete analysis
- `Poster presentation.pdf`: PDF of the research poster presentation
- `R/`: Directory containing R code - same as the Rmd file
  - `data_preparation.R`: Functions for data loading, cleaning, and preparation
  - `analysis.R`: Implementation of statistical analyses including TMLE
  - `visualization.R`: Functions for creating tables and figures
  - `main.R`: Script to run the complete analysis pipeline

## Analysis Approach

The study employed a rigorous methodological approach:

### Study Design

Three separate analyses were conducted:

1. **Short-term effect during lockdown:**
   - Baseline covariates: Week 1
   - Bereavement exposure: Weeks 3-6
   - Outcomes: Week 7

2. **Medium-term effect:**
   - Baseline covariates: Week 1
   - Bereavement exposure: Weeks 3-6
   - Outcomes: Week 15

3. **Short-term effect post-lockdown:**
   - Baseline covariates: Week 16
   - Bereavement exposure: Weeks 18-21
   - Outcomes: Week 22

### Statistical Methods

- **Survey Weighting:** Raking methods were used to weight the sample to match population demographics.
- **Multiple Imputation:** Used to handle missing data (5 imputations).
- **Targeted Maximum Likelihood Estimation (TMLE):** Used for causal effect estimation to reduce bias.
- **Outcome-Wide Analysis:** Examined effects across multiple wellbeing dimensions:
  - Mental health (depression, anxiety)
  - Subjective wellbeing (life satisfaction, happiness, meaning)
  - Social factors (support, isolation)
  - Health behaviors (smoking, drinking, diet, physical activity)
  - Other outcomes (volunteering, caring, self-harm, sleep)

## Running the Analysis

To run the analysis:

1. Clone the repository
2. Update the data path in `R/main.R` to point to your data file
3. Install required R packages:
   ```R
   install.packages(c("tidyverse", "gtsummary", "mice", "survey", "anesrake", 
                    "ltmle", "broom", "knitr", "kableExtra", "ggplot2", "ggsci"))
   ```
4. Run the main script:
   ```R
   source("R/main.R")
   ```

Alternatively, you can knit the R Markdown file:
```R
rmarkdown::render("OW.bereavement.formatted.Rmd")
```

## Data Source

This analysis uses data from the UK COVID Social Study, a longitudinal survey tracking mental health and wellbeing during the pandemic.

## Authors

- Koichiro Shiba
- Ojasvi Pranav Vachharajani
