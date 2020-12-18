library(tidyverse)
library(MASS)
library(stargazer)

# OMITTED VARIABLE BIAS ---------------------------------------------------

# b) DGP ------------------------------------------------------------------

## This function creates a random sample as specified in the task
samplefun <- function(N){

  sigma_matrix <- matrix(c(1, 0.5, 0.5, 1),
                         nrow = 2)

  multivariates <- mvrnorm(N,
                           mu = c(0,0),
                           Sigma = sigma_matrix)
  tibble(const = rep(1, N),
         educ = multivariates[,1],
         abil = multivariates[,2],
         u = rnorm(N),
         ln_wage = 2 + 0.05*educ + 0.1 * abil + u)
}


# c) OLS ------------------------------------------------------------------
## Create dataset
data <- samplefun(1000)

## Estimate OLS model
model_1 <- lm(ln_wage ~ educ + abil, data = data)


# d) omit ability ---------------------------------------------------------

## Estimate OLS model
model_omitted <- lm(ln_wage ~ educ, data = data)

stargazer(model_1, model_omitted,
          type = "text")

## Coefficient for education is biased:
delta <- lm(data$abil ~ data$educ)$coefficients[2]
gamma <- model_1$coefficients[3]

## Biased estimator according to beta + delta*gamma
beta_educ_omitted <- model_1$coefficients[2] + delta * gamma

## Does it match the result of the omitted model?
beta_educ_omitted == model_omitted$coefficients[2]


# e) Proxy for abil -------------------------------------------------------
## Create proxy variable
data_2 <- data %>%
  mutate(test_1 = 50 + 20 * abil)

## Use it in OLS instead of abil
model_proxy <- lm(ln_wage ~ educ + test_1, data = data_2)

## Results now are unbiased
stargazer(model_1, model_omitted, model_proxy,
          type = "text")

## REST OMITTED AS MEASUREMENT ERROR WAS NOT COVERED



