library(tidyverse)
library(sandwich)
library(lfe) # for cluster robust standard errors

# CLUSTERED AND GROUPED SAMPLES -------------------------------------------

load("data/KruegerStar.Rda")


# b) OLS Estimation -------------------------------------------------------

## Estimate Model by OLS
model_a <- lm(pscore ~ cs,
              data = KruegerStar)

## Extract standard errors
se_conventional <- sqrt(diag(vcov(model_a)))
se_robust <- sqrt(diag(vcovHC(model_a, "HC1")))

## Show results
rbind(se_conventional, se_robust)


# d) Cluster robust standard errors ---------------------------------------

model_cluster <- felm(pscore ~ cs|0|0|classid,
                      data = KruegerStar)

## Extract cluster robust standard errors
se_cluster <- sqrt(diag(model_cluster$clustervcv))
## Show results
rbind(se_conventional, se_robust, se_cluster)


# e) Block Bootstrap ------------------------------------------------------

## Function that resamples the data by classes
resamplefun <- function(data){
  ## Unique class ids
  class_ids <- unique(data$classid)
  ## Resampled ids of classes we want in the resample
  resample_ids <- sample(class_ids,
                         length(class_ids),
                         replace = T)

  tibble(id = resample_ids) %>%
    mutate(class = map(id,
                       .f = ~data[data$classid==., c(2, 4)])) %>% ## here the classes are drawn
    unnest(cols = class)

}

## Function that draws a sample, does an OLS estimation and returns the coefficients
betafun <- function(n = NULL){
  ## Create sample
  sample <- resamplefun(KruegerStar)
  ## Estimate OLS
  model <- lm(pscore ~ cs,
              data = sample)
  return(
    model$coefficients
  )
}

## Do the bootstrap
betas <- map_dfr(1:100,
        .f = betafun)

## Calculate standard errors
se_boot <- betas %>%
  summarise_all(.funs = sd) %>%
  as.numeric()

##Show results
rbind(se_conventional, se_robust, se_cluster, se_boot)
