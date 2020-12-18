library(tidyverse)
library(MASS)
set.seed(1010)
# PML vs ML: A MONTE CARLO EXCERCISE --------------------------------------


# a) Simulate Poisson Variable --------------------------------------------

## This function creates a random poisson sample as specified in the task
samplefun_pois <- function(N){
  tibble(x_1 = rnorm(N),
         x_2 = rnorm(N),
         lambda = exp(0.2 + 0.1 * x_1 + 0.0*x_2 ),
         y = rpois(N, lambda))
}

data <- samplefun_pois(1000)


# b) Estimate using glm ---------------------------------------------------

## Estimate coefficients using glm with poisson
model_1 <- glm(formula = y ~ x_1 + x_2,
               data = data,
               family = poisson)

summary(model_1)

se_conv <- sqrt(diag(vcov(model_1)))

## Get solutions for beta parameters
betas <- model_1$coefficients

## Observation matrix
X <- cbind(1, data$x_1, data$x_2)
Y <- data$y

# Preparation: Derive Hessian Matrix --------------------------------------

## This function calculates the Hessian for observation i in matrix X
hessfun <- function(i, x, betas){
  ## Vector x_i
  x_i <- as.matrix(x[i,])
  ## Calculate mu(x)
  mu_x <- as.numeric(exp(t(x_i) %*% betas))

  ## Calculate hessian for observation i
  (x_i %*% t(x_i)) *( mu_x * -1)
}

## Loop over all observations and add/stack the results
hessian <- map(1:nrow(X), # this returns a list of all hessians
               .f = hessfun,
               x = X,
               betas = betas) %>%
  reduce(.f = `+`) # here the individual hessians get added


# c) Standard Errors --------------------------------------------

## Variance matrix as negative inverse of hessian:
avar <- (-1)*  solve(hessian)
## Corresponding Standard errors
se_hessian <- sqrt(diag(avar))
# Preparation: Derive Score Variance -----------------------------
## This function calculates the variance based on the score for observation i in matrix X
scorevarfun <- function(i, x, y, betas){
  ## Vector x and y
  x_i <- as.matrix(x[i,])
  ## Calculate mu(x)
  mu_x <- as.numeric(exp(t(x_i) %*% betas))
  ## Score Var
  x_i %*% t(x_i) * (mu_x - y[i])^2
}

## Loop over all observations and add/stack the results
scorevar <- map(1:nrow(X), # this returns a list of all scores
                .f = scorevarfun,
                x = X,
                y = Y,
                betas = betas) %>%
  reduce(.f = `+`) # here the individual hessians get added

## Variance matrix from sandwich formula
avar_score <- solve(hessian) %*% scorevar %*% solve(hessian)
## Corresponding Standard errors
se_sandwich <- sqrt(diag(avar_score))

## Show results
rbind(se_conv,
      se_hessian,
      se_sandwich)




# d) Misspecified density -------------------------------------------------

## Theta parameter
theta <- 3

## This function creates a random negbin sample as specified in the task
samplefun_negbin <- function(N){
  tibble(x_1 = rnorm(N),
         x_2 = rnorm(N),
         mu = exp(0.2 + 0.1 * x_1 ),
         var = mu + (mu^2/theta),
         y = rnbinom(N,
                     mu = mu,
                     size = theta))
}

data <- samplefun_negbin(1000)



# e/f) I: Poisson ML -------------- ---------------------------------

## Function that draws a sample, does an estimation and returns the coefficients and variances
betafun_1 <- function(n = NULL){
  ## Create Sample
  sample <- samplefun_negbin(1000)
  ## Estimate model
  model <- glm(formula = y ~ x_1 + x_2,
               data = sample,
               family = "poisson")
  ## Get coefficients
  betas <- model$coefficients
  ## Calculate se by inverse hessian
  X <- cbind(1, sample$x_1, sample$x_2)
  Y <- sample$y
  hessian <- map(1:nrow(X), # this returns a list of all hessians
                 .f = hessfun,
                 x = X,
                 betas = betas) %>%
    reduce(.f = `+`) # here the individual hessians get added
  ## Variance matrix as negative inverse of hessian:
  avar <- solve(hessian)
  scorevar <- map(1:nrow(X), # this returns a list of all scores
                  .f = scorevarfun,
                  x = X,
                  y = Y,
                  betas = betas) %>%
    reduce(.f = `+`) # here the individual hessians get added

  ## Variance matrix from sandwich formula
  avar_score <- avar %*% scorevar %*% avar
  ## Corresponding Standard errors
  se_sandwich <- sqrt(diag(avar_score))
  se_hessian <- sqrt(diag(-1*avar))

  ## Return beta_2, its standard error and whether it sis significant at 5
  ret <- c(betas[3],
           se_sandwich[3], se_hessian[3],
           abs(betas[3]/se_sandwich[3]) > 1.96, abs(betas[3]/se_hessian[3]) > 1.96)
  names(ret) <- c("beta_2", "se_sand", "se_hess", "signif_sand", "signif_hess")
  return(
    ret
  )
}

## Do r monte carlo repetitions
poisson <- map_dfr(1:500,
                   .f = betafun_1)

## Split results
poisson_sandwich <- poisson %>%
  dplyr::select(beta_2 = beta_2,
                se = se_sand,
                signif = signif_sand)

poisson_hessian <- poisson %>%
  dplyr::select(beta_2 = beta_2,
                se = se_hess,
                signif = signif_hess)

# e/f) II: Poisson ML with hessian ---------------------------------

## Function that draws a sample, does an estimation and returns the coefficients and variances
betafun_2 <- function(n = NULL){
  ## Create Sample
  sample <- samplefun_negbin(1000)
  ## Estimate model
  model <- glm.nb(formula = y ~ x_1 + x_2,
                  data = sample)
  ## Get coefficients
  betas <- model$coefficients
  ## Corresponding Standard errors
  se_hessian <- sqrt(diag(vcov(model)))

  ## Return beta_2, its standard error and whether it sis significant at 5
  ret <- c(betas[3], se_hessian[3], abs(betas[3]/se_hessian[3]) > 1.96)
  names(ret) <- c("beta_2", "se", "signif")
  return(
    ret
  )
}

## Do r monte carlo repetitions
negbin_hessian <- map_dfr(1:500,
                          .f = betafun_2)


# e/f) RESULT ------------------------------------------------------------

res_poisson_hessian <- poisson_hessian %>%
  summarise_all(.f = mean)

res_poisson_sandwich <- poisson_sandwich %>%
  summarise_all(.f = mean)

res_negbin_hessian <- negbin_hessian %>%
  summarise_all(.f = mean)

## Show table
rbind(res_poisson_hessian,
      res_poisson_sandwich,
      res_negbin_hessian) %>%
  mutate(model = c("Poisson Hess",
                   "Poisson Sand",
                   "Negbin  Hess"))

