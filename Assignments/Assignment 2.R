library(tidyverse)

# PRELIMINARIES -----------------------------------------------------------

## Number of replications in Monte Carlo
r <- 1000

## Seed for random generation
set.seed(1337)

# OLS WITH MATRICES AND BUILT-IN lm() -------------------------------------


# a, b) Create Sample --------------------------------------------------------

## This function creates a random sample as specified in the task
samplefun <- function(N){
  tibble(const = rep(1, N),
         x_1 = rnorm(N, 2, sqrt(9)),
         x_2 = rnorm(N, 7, 1),
         u = rnorm(N, 0, 2),
         y = 12 - 3*x_1 + 5*x_2 + u)
}

data <- samplefun(800)

X_1 <- as.matrix(data[,1:3])
Y_1 <- as.matrix(data$y)


# c) Calculate beta by matrix algebra -------------------------------------

betas <- solve(t(X_1)%*%X_1) %*% t(X_1)%*%Y_1


# d) Create OLS function --------------------------------------------------

## OLS function that returns coefficients and standard errros
OLS <- function(Y, X) {
  ## Coefficients
  betas <- solve(t(X)%*%X) %*% t(X)%*%Y
  ## Residuals
  u_hat <- Y - (X%*%betas)
  ## sigma squared estimated
  sigma_sq_hat <- sum(u_hat^2)  / (nrow(X)-ncol(X))
  ## Asymptotic variance matrix
  avar <- sigma_sq_hat * solve(t(X)%*%X)
  ## Standard errors lie on diagonal
  se <- sqrt(diag(avar))
  ## Return results
  return(t(cbind(betas,se)))
}

## Test
OLS(Y_1, X_1)


# e) OLS() vs lm() --------------------------------------------------------

## Use lm and return summary with betas and se
summary(lm(y ~ x_1 + x_2, data = data))
## Compare with own OLS function
OLS(Y_1, X_1)


# f) Monte Carlo ----------------------------------------------------------

## Function that draws a sample, does an OLS estimation and returns the coefficients
betafun <- function(N){
  ## Draw sample as specified
  sample <- samplefun(N = N)

  model <- lm(y ~ x_1 + x_2, data = sample)

  return(
    model$coefficients
  )
}

## Do r replications of the OLS estimation
betas <- map_dfr(rep(800, r),
                 .f = betafun)

## Calculate sd for all columns
betas %>%
  summarise_all(.funs = sd)



# OLS AND HETEROSKEDASTICITY ----------------------------------------------


# a) Data Generating Process ----------------------------------------------

## This function creates a random sample as specified in the task
samplefun_2 <- function(N){
  tibble(const = rep(1, N),
         x = rchisq(N, 10),
         u = rnorm(N, 0, abs(3*x)),
         y = 1 -2*x + u)
}

data_2 <- samplefun_2(1000)

X_2 <- as.matrix(data_2[,1:2])
Y_2 <- as.matrix(data_2$y)

OLS_2 <- function(Y, X, robust = F) {
  ## Coefficients
  betas <- solve(t(X)%*%X) %*% t(X)%*%Y
  ## Residuals
  u_hat <- Y - (X%*%betas)
  ## Conventional Standard errors
  if(robust == F){
    ## sigma squared estimated
    sigma_sq_hat <- sum(u_hat^2)  / (nrow(X)-ncol(X))
    ## Asymptotic variance matrix
    avar <- sigma_sq_hat * solve(t(X)%*%X)
  } else {
    A <- t(X)%*%X
    B <- t(X) %*% diag(as.vector(u_hat^2)) %*% X
    ## Asymptotic variance matrix
    avar <- solve(A) %*% B %*% solve(A) * (nrow(X) / (nrow(X) - ncol(X)))
  }
  ## Standard errors lie on diagonal
  se <- sqrt(diag(avar))
  ## Return results
  return(t(cbind(betas,se)))
}

## Test
OLS_2(Y_2, X_2)
OLS_2(Y_2, X_2, robust = T)

## Direct way to get robust AVAR matrix:
library(sandwich)
avar <- vcovHC(x = lm(y ~ x, data = data_2), type = "HC1")
## Standard errors:
sqrt(diag(avar))


# b) Monte Carlo ----------------------------------------------------------

## Function that draws a sample, does an OLS estimation and returns the coefficients
betafun_2 <- function(N){
  ## Draw sample as specified
  sample <- samplefun_2(N)

  model <- lm(y ~ x, data = sample)

  return(
    model$coefficients
  )
}

# Replications
r <- 800

## Do r replications of the OLS estimation
betas_2 <- map_dfr(rep(1000, r),
                 .f = betafun_2)

## Calculate sd for all columns
betas_2 %>%
  summarise_all(.funs = sd)

OLS_2(Y_2, X_2, robust = T)


# c) Sample size 20 -------------------------------------------------------

## Do r replications of the OLS estimation
betas_3 <- map_dfr(rep(20, r),
                 .f = betafun_2)

## Calculate sd for all columns
betas_3 %>%
  summarise_all(.funs = sd)

OLS_2(Y_2, X_2, robust = T)
