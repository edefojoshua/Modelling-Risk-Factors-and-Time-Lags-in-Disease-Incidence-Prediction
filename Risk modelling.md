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

Libraries

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.3.3

``` r
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

Risk modelling

``` r
## Step 1: Create a synthetic dataset with age, sex, DIMD, and risk factors
set.seed(123)  # For reproducibility

n <- 1000  ## Number of individuals

## Generate synthetic data
data <- data.frame(
  ID = 1:n,
  Age = sample(30:80, n, replace = TRUE),  ## Random ages between 30-80
  Sex = sample(c("Male", "Female"), n, replace = TRUE),
  DIMD = sample(1:5, n, replace = TRUE),  ## Deprivation index (1 = least deprived, 5 = most deprived)
  Alcohol_Intake = rbinom(n, 1, 0.3),  ## 30% of individuals consume alcohol
  Smoking = rbinom(n, 1, 0.25),  ## 25% are smokers
  Obesity = rbinom(n, 1, 0.35)  ## 35% are obese
)

## Step 2: Define a function to estimate time lags based on a shifted binomial distribution
estimate_time_lag <- function(mean_lag, min_lag = 1, max_lag = 10) {
  time_lag <- rbinom(1, max_lag - min_lag, mean_lag / max_lag) + min_lag
  return(time_lag)
}

## Step 3: Assign time lags for each individual based on risk-disease pairs
data <- data %>%
  mutate(
    Alcohol_BC_Lag = ifelse(Alcohol_Intake == 1, estimate_time_lag(9), NA),  ## 9-year mean lag for alcohol and breast cancer
    Smoking_CHD_Lag = ifelse(Smoking == 1, estimate_time_lag(5), NA),  ## 5-year mean lag for smoking and CHD
    Obesity_CHD_Lag = ifelse(Obesity == 1, estimate_time_lag(7), NA)  ## 7-year mean lag for obesity and CHD
  )

## Step 4: Compute incidence probabilities based on the multiplicative risk model

## Relative risks for CHD:
RR_Obesity_CHD <- 1.5   # Relative risk for obesity and coronary heart disease
RR_Smoking_CHD <- 1.3   # Relative risk for smoking and coronary heart disease

## Compute combined relative risk for each individual
data <- data %>%
  mutate(
    Combined_RR_CHD = case_when(
      Obesity == 1 & Smoking == 1 ~ RR_Obesity_CHD * RR_Smoking_CHD,  # Obese smoker
      Obesity == 1 & Smoking == 0 ~ RR_Obesity_CHD,  # Obese non-smoker
      Smoking == 1 & Obesity == 0 ~ RR_Smoking_CHD,  # Smoker non-obese
      TRUE ~ 1  # Non-smokers and non-obese individuals
    )
  )

## Step 5: Calculate incidence based on the combined relative risk (adjusted for 2013 baseline incidence)
## Example total incidence of CHD for 2013 (let's assume 0.05 or 5%)
total_incidence_2013 <- 0.05

## Adjusted incidence based on risk factors
data <- data %>%
  mutate(
    Adjusted_Incidence_CHD = total_incidence_2013 * Combined_RR_CHD
  )

## View the first few rows of the modified data
head(data)
```

    ##   ID Age    Sex DIMD Alcohol_Intake Smoking Obesity Alcohol_BC_Lag
    ## 1  1  60   Male    3              0       0       1             NA
    ## 2  2  44   Male    3              0       0       0             NA
    ## 3  3  80 Female    4              1       0       0             10
    ## 4  4  43 Female    5              0       0       1             NA
    ## 5  5  32 Female    1              0       0       1             NA
    ## 6  6  71   Male    5              0       1       1             NA
    ##   Smoking_CHD_Lag Obesity_CHD_Lag Combined_RR_CHD Adjusted_Incidence_CHD
    ## 1              NA               7            1.50                 0.0750
    ## 2              NA              NA            1.00                 0.0500
    ## 3              NA              NA            1.00                 0.0500
    ## 4              NA               7            1.50                 0.0750
    ## 5              NA               7            1.50                 0.0750
    ## 6               6               7            1.95                 0.0975

session information

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] usethis_2.2.2 dplyr_1.1.4  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] digest_0.6.33     utf8_1.2.3        R6_2.5.1          fastmap_1.2.0    
    ##  [5] tidyselect_1.2.0  xfun_0.40         magrittr_2.0.3    glue_1.6.2       
    ##  [9] tibble_3.2.1      knitr_1.44        pkgconfig_2.0.3   htmltools_0.5.8.1
    ## [13] rmarkdown_2.25    generics_0.1.3    lifecycle_1.0.3   cli_3.6.1        
    ## [17] fansi_1.0.4       vctrs_0.6.5       compiler_4.3.1    purrr_1.0.2      
    ## [21] rstudioapi_0.15.0 tools_4.3.1       pillar_1.9.0      evaluate_0.21    
    ## [25] yaml_2.3.7        fs_1.6.3          rlang_1.1.1
