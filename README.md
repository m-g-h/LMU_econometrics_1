
# 2 Clustered and Grouped Samples

In this problem, you will analyze data from Project STAR, originally
analyzed in Krueger (1999). Project STAR was a three-year research
project consisting of an intervention with preschoolers in Lane County,
Oregon, United States. Consider the following bivariate regression
model, \[ Y_{ig} = \beta_0 + \beta_1 x_i + e_{ig}\] where \(Y_{ig}\), is
the test score of student \(i\) in class \(g\) and class size \(x_g\).
The goal of the research is to understand how class-size affects student
outcomes. This question partially replicates the example in Section 8.2
in Angrist and Piscke (2008).

### a) Random Sampling I

An important aspect of the STAR project is that the students are
randomly assigned to classes. What does this imply about the consistency
and unbiasedness of the OLS estimator?

  - Random assignment implies **random sampling** of variables, meaning
    they should be **i.i.d**
  - Given the assumptions *OLS.1* and *OLS.2* this implies that the
    estimators are consistent

### b) OLS Estimation

``` r
## Libraries
library(stargazer)
library(sandwich)
library(lfe)
library(tidyverse)
library(DT)
library(furrr)

plan(strategy = "multisession", workers = 2)

## LOAD `KruegerStar` DATA
load("data/KruegerStar.Rda")

## SHOW DATA (that poor kid in the first observation :/)
head(KruegerStar)
```

    ## # A tibble: 6 x 7
    ##     schidkn pscore classid    cs female nwhite     n
    ##   <dbl+lbl>  <dbl>   <dbl> <dbl>  <dbl>  <dbl> <dbl>
    ## 1        22 0         6317    15      0      1     1
    ## 2        56 0.0306   16647    21      0      0     1
    ## 3        33 0.0800    9700    26      1      1     1
    ## 4        45 0.357    13400    24      0      1     1
    ## 5        33 1.07      9700    26      0      1     1
    ## 6        60 0.110    17800    25      1      0     1

Estimate the model by OLS and interpret what you estimate for
\(\beta_1\). Calculate both conventional and robust standard errors.

``` r
## LM ESTIMATION
model_1 <- lm(pscore ~ cs,
              data = KruegerStar)

## NORMAL STANDARD ERRORS
se = c(1.655, 0.090)

## ROBUST STANDARD ERRORS
cov_1 <- vcovHC(x = model_1, type = "HC1")
robust_se = sqrt(diag(cov_1))


## SHOW RESULTS
stargazer(model_1, model_1,
          type = "text",
          se = list(se, robust_se))
```

    ## 
    ## ============================================================
    ##                                     Dependent variable:     
    ##                                 ----------------------------
    ##                                            pscore           
    ##                                      (1)            (2)     
    ## ------------------------------------------------------------
    ## cs                                -0.618***      -0.618***  
    ##                                    (0.090)        (0.090)   
    ##                                                             
    ## Constant                          64.067***      64.067***  
    ##                                    (1.655)        (1.861)   
    ##                                                             
    ## ------------------------------------------------------------
    ## Observations                        5,743          5,743    
    ## R2                                  0.008          0.008    
    ## Adjusted R2                         0.008          0.008    
    ## Residual Std. Error (df = 5741)     26.951        26.951    
    ## F Statistic (df = 1; 5741)        47.389***      47.389***  
    ## ============================================================
    ## Note:                            *p<0.1; **p<0.05; ***p<0.01

Is there a big difference?

  - Difference is virtually not noticeable in for *class size* and
    negligible for the *constant*

### c) Random Sampling (?) II

While the students are randomly assigned to classrooms, students in the
same classroom where exposed to the same teacher and classroom
environment. What does this imply about the consistency and the
asymptotic distribution of the OLS estimator?

  - There might be correlations of the error terms *within groups*
  - Typically, conventional standard errors *underestimate* the true
    standard errors because they do not take this within-group
    correlation into account.

### d) Cluster Robust Standard Errors

Estimate the standard errors for \(\beta_1\) using cluster robust
standard errors (The R package “plm” will help here. There are other
packages that do similar calculations).

``` r
model_2 <- felm(pscore ~ cs|0|0|classid,
                data = KruegerStar)

## SAVE CLUSTER STANDARD ERRORS FOR DISPLAY
se_clust <- c(4.573, 0.232)
## DISPLAY ALL MODELS
stargazer(model_1, model_1, model_2,
          type = "text",
          se = list(se,
                    robust_se,
                    se_clust))
```

    ## 
    ## =============================================================
    ##                                      Dependent variable:     
    ##                                 -----------------------------
    ##                                            pscore            
    ##                                         OLS           felm   
    ##                                    (1)       (2)       (3)   
    ## -------------------------------------------------------------
    ## cs                              -0.618*** -0.618*** -0.618***
    ##                                  (0.090)   (0.090)   (0.232) 
    ##                                                              
    ## Constant                        64.067*** 64.067*** 64.067***
    ##                                  (1.655)   (1.861)   (4.573) 
    ##                                                              
    ## -------------------------------------------------------------
    ## Observations                      5,743     5,743     5,743  
    ## R2                                0.008     0.008     0.008  
    ## Adjusted R2                       0.008     0.008     0.008  
    ## Residual Std. Error (df = 5741)  26.951    26.951    26.951  
    ## F Statistic (df = 1; 5741)      47.389*** 47.389***          
    ## =============================================================
    ## Note:                             *p<0.1; **p<0.05; ***p<0.01

# e) Block Bootstrap

Estimate the standard errors for \(\beta_1\) using block bootstrap. The
block bootstrap preserves the group structure in the original dataset by
drawing blocks of data defined by the groups. In this case, you will
create and estimate the model on bootstrap samples where you draw whole
classes. How do the block-bootstrap standard errors compare to the
cluster robust standard errors?

We follow the approaches of *Cameron, A. C., J. B. Gelbach, and D. L.
Miller (2008): Bootstrap-based improvements for inference with clustered
errors*

## Pairs Cluster Bootstrap

Resample the clusters itself:

  - We have \(G\) clusters \((\mathbf{y}_g, \mathbf{X}_g)\), each with
    \(N_g\) observations
  - For the bootstrap, we draw \(G_b\) clusters *with replacement*,
    where \(G_b\) can be half the number of original clusters

![Schema](pictures/PC_bootstrap.png)

### 1\. This function draws \(G_b\) random classes from the original sample

``` r
# SAMPLE FROM data.frame
pc_bs_sample <- function(G_b, pop_data){
  ## Create Random Index of N_i elemnts of pop_data  
  index <- sample(pop_data$classid,
                  size = G_b,
                  replace = T)
  
  ## Select N_i elements from pop_data
  pop_data[pop_data$classid %in% index, c("pscore", "cs")]
}

pc_bs_sample(100, KruegerStar) %>% 
  head()
```

    ## # A tibble: 6 x 2
    ##   pscore    cs
    ##    <dbl> <dbl>
    ## 1  1.81     26
    ## 2  2.25     17
    ## 3  0.189    26
    ## 4  1.87     15
    ## 5  1.00     20
    ## 6  1.25     15

### 2\. This function uses the sample generated by `pc_bs_sample()`, estimates an OLS model and returns the coefficients

``` r
## THIS FUNCTION ESTIMATES A MODEL AND GET COEFFICIENTS USING THE RANDOM SAMPLE
get_pc_coefficients <- function(index,
                                size = 0.5,
                                data){
  
  n_classes <- round(length(unique(data$classid)) * size,digits = 0)
  
  message(paste("Sample", index))
  ## Estimate model
  model <- lm(pscore ~ cs,
              data = pc_bs_sample(G_b = n_classes,
                                  pop_data = data))
  ## Get and return coefficients
  summary(model)$coefficients[1:2,1]
}

get_pc_coefficients(index = 1,
                    size = 1,
                    data = KruegerStar)
```

    ## (Intercept)          cs 
    ##  65.0978024  -0.6924857

## 3\. Here we do the PC bootstrap 10000 times:

``` r
pc_coefficients <- future_map_dfr(1:10000,
                                  .f = get_pc_coefficients,
                                  data = KruegerStar,
                                  size = 1,
                                  .options = furrr_options(seed = T))

head(pc_coefficients)
```

    ## # A tibble: 6 x 2
    ##   `(Intercept)`     cs
    ##           <dbl>  <dbl>
    ## 1          63.9 -0.632
    ## 2          60.5 -0.492
    ## 3          55.6 -0.227
    ## 4          63.7 -0.548
    ## 5          66.3 -0.694
    ## 6          58.6 -0.362

### 4\. Calculate standard errors as variance over the bootstraps:

\[s_{\beta_{i}} = \left(  \frac{1}{G_b-1} \sum_{g=1}^{G_b} (\hat{\beta}_{ig} - \bar{\hat{\beta_{ig}}}) \right)^{\frac{1}2}\]

``` r
## Save standard errors in a vector
pc_boot_se <- c(sd(pc_coefficients$`(Intercept)`),
                sd(pc_coefficients$cs)
)

## Present results
rbind("normal" = se,
      "robust" = robust_se,
      "cluster_robust" = se_clust,
      "pc_bootstrap" = pc_boot_se
)
```

    ##                (Intercept)         cs
    ## normal            1.655000 0.09000000
    ## robust            1.861063 0.08997004
    ## cluster_robust    4.573000 0.23200000
    ## pc_bootstrap      3.854848 0.18405637

## Wild (Residual) Bootstrap

Instead of resampling the clusters, we resample the residuals from the
estimated model on the complete sample

  - In the estimated model have \(i\) clusters
    \((\mathbf{\hat{y}}_i, \mathbf{X}_i, \mathbf{\hat{u}}_i)\), each
    with \(N_i\) observations
  - We also have the coefficients \(\beta_0\) and \(\beta_1\) from the
    conventional OLS estimation
  - We now draw \(G_b\) classes and use the regressors, resiudals and
    coefficients to calculate
    \(\hat{\mathbf{y}} = \mathbf{X}' \mathbf{\beta} + \mathbf{\hat{u}}\)

![Wild Bootstrap Schema](pictures/wild_bootstrap.png)

### 1\. This function creates a dataset containing \(G_b\) classes with pscore calculated via the wild bootstrap method using Rademacher weights

``` r
wild_bs_sample <- function(G_b,
                           data){
  
  ## Derive the OLS estimates and residuals
  model <- lm(pscore ~ cs, data = data)
  
  ## Get X and u_hat
  full_data <- tibble(classid = data$classid,
                      cs = data$cs,
                      u_hat = model$residuals) %>% 
    group_by(classid) %>% 
    mutate(weight = sample(c(1, -1),
                           size = 1,
                           replace = F))
  
  ## Get coefficients
  beta_0 <- model$coefficients[1]
  beta_1 <- model$coefficients[2]
  
  ## Index of classes to sample
  index <- sample(full_data$classid,
                  size = G_b,
                  replace = T)
  
  ## Select G_b classes from full_data
  full_data[full_data$classid %in% index, c("cs", "u_hat", "weight")] %>% 
    transmute(pscore = beta_0 + beta_1 * cs + u_hat*weight,
              cs = cs)
}

wild_bs_sample(data = KruegerStar) %>% 
  head()
```

    ## # A tibble: 6 x 2
    ##      pscore    cs
    ##       <dbl> <dbl>
    ## 1 -3.34e-11    15
    ## 2  3.06e- 2    21
    ## 3  9.59e+ 1    26
    ## 4  3.57e- 1    24
    ## 5  9.49e+ 1    26
    ## 6  9.71e+ 1    25

### 2\. This function uses the sample generated by `wild_bs_sample()`, estimates an OLS model and returns the coefficients

``` r
## THIS FUNCTION ESTIMATES A MODEL AND GET COEFFICIENTS USING THE RANDOM SAMPLE
get_wild_coefficients <- function(index,
                                  size = 0.5,
                                  data){
  message(paste("Sample", index))
  
  ## Sampel size (number of clusters to draw)
  n_classes <- round(length(unique(data$classid)) * size ,digits = 0)
  
  ## Estimate model
  model <- lm(pscore ~ cs,
              data = wild_bs_sample(G_b = n_classes,
                                    data = data))
  ## Get and return coefficients
  summary(model)$coefficients[1:2,1]
}

get_wild_coefficients(index = 1,
                      size = 1,
                      data = KruegerStar)
```

    ## (Intercept)          cs 
    ##  65.2066779  -0.6618401

## 3\. Here we do the wild bootstrap 10000 times:

``` r
wild_coefficients <- future_map_dfr(1:10000,
                                    .f = get_wild_coefficients,
                                    data = KruegerStar,
                                    size = 1,
                                    .options = furrr_options(seed = T)
)

head(wild_coefficients)
```

    ## # A tibble: 6 x 2
    ##   `(Intercept)`     cs
    ##           <dbl>  <dbl>
    ## 1          61.2 -0.468
    ## 2          65.1 -0.750
    ## 3          68.1 -0.811
    ## 4          62.4 -0.469
    ## 5          65.5 -0.761
    ## 6          54.6 -0.296

### 4\. Calculate standard errors as variance over the bootstraps:

\[s_{\beta_{ib}} = \left(  \frac{1}{B-1} \sum_{b=1}^B (\hat{\beta}_{ib} - \bar{\hat{\beta_{ib}}}) \right)^{\frac{1}2}\]

``` r
wild_boot_se <- c(sd(wild_coefficients$`(Intercept)`),
                  sd(wild_coefficients$cs)
)

rbind("normal" = se,
      "robust" = robust_se,
      "cluster_robust" = se_clust,
      "pc_bootstrap" = pc_boot_se,
      "wild_bootstrap" = wild_boot_se
)
```

    ##                (Intercept)         cs
    ## normal            1.655000 0.09000000
    ## robust            1.861063 0.08997004
    ## cluster_robust    4.573000 0.23200000
    ## pc_bootstrap      3.854848 0.18405637
    ## wild_bootstrap    6.108388 0.30173241
