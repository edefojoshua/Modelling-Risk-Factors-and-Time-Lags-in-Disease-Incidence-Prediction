Modelling Risk Factors and Time Lags in Disease Incidence Prediction
================
Joshua Edefo
2025-02-17

The code models the risk of developing coronary heart disease (CHD) by
generating a synthetic dataset of 1,000 individuals with varying risk
factors such as age, sex, deprivation index, alcohol consumption,
smoking, and obesity. It estimates time lags between exposure to risk
factors and disease onset using a shifted binomial distribution. The
model calculates combined relative risks (RR) for individuals,
considering factors like obesity and smoking, and adjusts the CHD
incidence based on these RRs. The final dataset provides personalized
risk predictions, accounting for time lags and risk factors. This
framework aids in predictive health modelling and can inform targeted
interventions for disease prevention.

Libraries `{r b, message=FALSE} library(dplyr) library(usethis)`

The modelling \`\`\`{r c} \# Step 1: Create a synthetic dataset with
age, sex, DIMD, and risk factors set.seed(123) \# For reproducibility

n \<- 1000 \# Number of individuals

# Generate synthetic data

data \<- data.frame( ID = 1:n, Age = sample(30:80, n, replace = TRUE),
\# Random ages between 30-80 Sex = sample(c(“Male”, “Female”), n,
replace = TRUE), DIMD = sample(1:5, n, replace = TRUE), \# Deprivation
index (1 = least deprived, 5 = most deprived) Alcohol_Intake = rbinom(n,
1, 0.3), \# 30% of individuals consume alcohol Smoking = rbinom(n, 1,
0.25), \# 25% are smokers Obesity = rbinom(n, 1, 0.35) \# 35% are obese
)

# Step 2: Define a function to estimate time lags based on a shifted binomial distribution

estimate_time_lag \<- function(mean_lag, min_lag = 1, max_lag = 10) {
time_lag \<- rbinom(1, max_lag - min_lag, mean_lag / max_lag) + min_lag
return(time_lag) }

# Step 3: Assign time lags for each individual based on risk-disease pairs

data \<- data %\>% mutate( Alcohol_BC_Lag = ifelse(Alcohol_Intake == 1,
estimate_time_lag(9), NA), \# 9-year mean lag for alcohol and breast
cancer Smoking_CHD_Lag = ifelse(Smoking == 1, estimate_time_lag(5), NA),
\# 5-year mean lag for smoking and CHD Obesity_CHD_Lag = ifelse(Obesity
== 1, estimate_time_lag(7), NA) \# 7-year mean lag for obesity and CHD )

# Step 4: Compute incidence probabilities based on the multiplicative risk model

# Relative risks for CHD:

RR_Obesity_CHD \<- 1.5 \# Relative risk for obesity and coronary heart
disease RR_Smoking_CHD \<- 1.3 \# Relative risk for smoking and coronary
heart disease

# Compute combined relative risk for each individual

data \<- data %\>% mutate( Combined_RR_CHD = case_when( Obesity == 1 &
Smoking == 1 ~ RR_Obesity_CHD \* RR_Smoking_CHD, \# Obese smoker Obesity
== 1 & Smoking == 0 ~ RR_Obesity_CHD, \# Obese non-smoker Smoking == 1 &
Obesity == 0 ~ RR_Smoking_CHD, \# Smoker non-obese TRUE ~ 1 \#
Non-smokers and non-obese individuals ) )

# Step 5: Calculate incidence based on the combined relative risk (adjusted for 2013 baseline incidence)

# Example total incidence of CHD for 2013 (let’s assume 0.05 or 5%)

total_incidence_2013 \<- 0.05

# Adjusted incidence based on risk factors

data \<- data %\>% mutate( Adjusted_Incidence_CHD = total_incidence_2013
\* Combined_RR_CHD )

# View the first few rows of the modified data

head(data)


    session information
    ```{r e, echo=FALSE}
    sessionInfo()
