library(tidyverse)

# PRELIMINARIES -----------------------------------------------------------

## Number of replications in Monte Carlo
r <- 1000

## Seed for random generation
set.seed(1337)
# MONTE CARLO SIMULATION 1 ------------------------------------------------

## Function that draws a random sample and calculates the mean
statfun <- function(N, mean, sd) {
  mean(rnorm(n = N,
             mean = mean,
             sd = sd)
       )
}

# a) Mean 1, sd 2, N = 100 ------------------------------------------------

## Loop r times using sample size 100
mu_hat <- map_dbl(rep(100, r),
                  .f = statfun,
                  mean = 1,
                  sd = 2)
## Mean and sd of our estimator
mean(mu_hat)
sd(mu_hat)


# b) Histogram ------------------------------------------------------------

## Using qplot from ggplto2 which intelligently picks a histogram
qplot(mu_hat)


# c) Mean 1, sd 2, N = 5 or 1000 ------------------------------------------

## Loop r times using sample size 5
mu_hat_5 <- map_dbl(rep(5, r),
                    .f = statfun,
                    mean = 1,
                    sd = 2)
## Mean and sd of our estimator
mean(mu_hat_5)
sd(mu_hat_5)

## Loop r times using sample size 1000
mu_hat_1000 <- map_dbl(rep(1000, r),
                    .f = statfun,
                    mean = 1,
                    sd = 2)
## Mean and sd of our estimator
mean(mu_hat_1000)
sd(mu_hat_1000)


# d, e) Chi Squared distribution ------------------------------------------

## Function that draws a random sample from chi_sq(1) and calculates the mean
statfun_chi <- function(N) {
  mean(rnorm(n = N)^2)
}
## Loop r times using sample size 5
mu_hat_chi_5 <- map_dbl(rep(5, r),
                  .f = statfun_chi)
## Mean and sd of our estimator
mean(mu_hat_chi_5)
sd(mu_hat_chi_5)

## Loop r times using sample size 100
mu_hat_chi <- map_dbl(rep(100, r),
                  .f = statfun_chi)
## Mean and sd of our estimator
mean(mu_hat_chi)
sd(mu_hat_chi)

## Loop r times using sample size 1000
mu_hat_chi_1000 <- map_dbl(rep(1000, r),
                  .f = statfun_chi)
## Mean and sd of our estimator
mean(mu_hat_chi_1000)
sd(mu_hat_chi_1000)

## Plots
qplot(mu_hat_chi_5)
qplot(mu_hat_chi)
qplot(mu_hat_chi_1000)


# f) T-Testing ------------------------------------------------------------

## Function drawing a sample and performing a t-test.
## Returns true if H0 mu=1 is rejected
testfun <- function(N) {
  ## Draw chi squared sample
  sample <-rnorm(n = N)^2
  ## Do t-test with H0: mu = 1
  test <- t.test(sample,
         mu = 1)
  ## Return true if p-value is smaller than 0.05
  test$p.value < 0.05
}

## Do r tests
tests <- map_dbl(rep(1000, r),
        .f = testfun)
## Fraction of samples where H0 got rejected
mean(tests)
