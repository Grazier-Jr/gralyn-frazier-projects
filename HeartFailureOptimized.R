
###################### Re-Admittance to the hospital for heart failure ######################

# Load necessary libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(gtsummary)
library(naniar)
library(car)

# Unused libraries commented out
# library(haven)
# library(AICcmodavg)
# library(broom)
# library(corrr)
# library(knitr)
# library(kableExtra)
# library(readr)
# library(tidyr)
# library(stargazer)
# library(vtable)
# library(COUNT)
# library(tidycomm)
# library(labelled)

# Set working directory
setwd("C:/Users/grazi/Documents")

# Load data
data <- read_excel("SHEET FOR ANALYSIS.xlsx")

# Remove excess null rows
data <- data[1:201, ]

# Select relevant columns
data <- data %>%
  select(ID_2 = `ID 2`, SEX, Age, SMOKING, DRINKING, COMPLIANCE, SURGERY, T2DM, HTN, Dyslipidemia, KidneyDisease, ASTHMACOPD, READMISSIONS)

# Create dummy variables and recode factors
data <- data %>%
  mutate(across(c(SMOKING, DRINKING, COMPLIANCE, SURGERY, T2DM, HTN, ASTHMACOPD, Dyslipidemia, KidneyDisease),
                ~ recode(.x, 'not stated' = NA_character_, 'N/A' = NA_character_, 
                         'never' = '0', 'Never' = '0', 'yes' = '1', 'Yes' = '1', 'N' = '0', 'Y' = '1', 'n' = '0', 'y' = '1'))) %>%
  mutate(SEX = recode(SEX, 'F' = '0', 'M' = '1')) %>%
  replace_with_na_all(condition = ~.x %in% c("", "NA"))

# Remove rows with missing values
data_clean <- na.omit(data)

# Create a binary variable for re-admission
data_clean <- data_clean %>%
  mutate(Readmitted = ifelse(READMISSIONS >= 1, 1, 0))

####################### Summary Statistics #########################

# Basic summary statistics
summary_stats <- data_clean %>%
  select(Readmitted, Age) %>%
  summary()

# Print summary statistics for review
print(summary_stats)

####################### Visualizations #########################

# Histogram of Age distribution by Re-Admission Status
ggplot(data_clean, aes(Age)) +
  geom_histogram(aes(fill = as.factor(SEX)), color = "black", binwidth = 2) +
  facet_wrap(~ Readmitted, labeller = labeller(Readmitted = c('0' = "No Re-Admission", '1' = "Re-Admission"))) +
  ggtitle("Age Distribution by Re-Admission Status")

######################## Logistic Regression Models ###########################

# Univariate regression models
model_smoking <- glm(Readmitted ~ SMOKING, family = binomial, data = data_clean)
model_age_sex <- glm(Readmitted ~ Age + SEX, family = binomial, data = data_clean)
model_full <- glm(Readmitted ~ SMOKING + DRINKING + COMPLIANCE + SURGERY + T2DM + HTN + KidneyDisease + ASTHMACOPD + Dyslipidemia, 
                  family = binomial, data = data_clean)

# Model summaries
summary(model_smoking)
summary(model_age_sex)
summary(model_full)

# Check for multicollinearity
vif(model_full)

# Correlation matrix
cor(data_clean %>% select(SMOKING, DRINKING, COMPLIANCE, SUGERY, T2DM, HTN, KidneyDisease, ASTHMACOPD, Dyslipidemia), use = complete.obs)

# Stepwise selection
best_model <- stepAIC(model_full, direction = "both")
summary(best_model)

# Regression table using gtsummary
tbl_regression(best_model, exponentiate = TRUE) %>%
  bold_p(t = 0.05) %>%
  italicize_levels() %>%
  modify_caption("Multivariate Regression for Re-Admission")


