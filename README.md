
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kfold

<!-- badges: start -->
<!-- badges: end -->

The goal of kfold is to …

## Installation

You can install the development version of kfold like so:

``` r
remotes::install_github("rpkgs/kfold")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(kfold)

# set.seed(1)
n <- 100 ; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)

# r_rf  <- kfold_rf(X, y)
r_lm  <- kfold_lm(X, y)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==============                                                        |  20%  |                                                                              |============================                                          |  40%  |                                                                              |==========================================                            |  60%  |                                                                              |========================================================              |  80%  |                                                                              |======================================================================| 100%
r_lm
#> # A tibble: 6 x 11
#>      R2    NSE    KGE  RMSE   MAE   Bias Bias_perc n_sim      R pvalue kfold
#>   <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>     <dbl> <dbl>  <dbl>  <dbl> <chr>
#> 1 0.079 -0.275 -5.06  0.869 0.588  0.455    -5.96     20  0.282  0.229 Fold1
#> 2 0.049 -0.77  -0.572 1.03  0.866 -0.533    -0.76     20 -0.222  0.348 Fold2
#> 3 0.006 -0.016 -0.657 0.957 0.747  0.121     1.08     20  0.077  0.748 Fold3
#> 4 0.005 -0.011 -0.339 1.04  0.811 -0.129    -0.342    20  0.071  0.767 Fold4
#> 5 0.054  0.035 -0.238 0.897 0.664  0.096     0.46     20  0.233  0.322 Fold5
#> 6 0.003 -0.063 -0.319 0.962 0.735  0.002     0.008   100 -0.052  0.606 all  
#> 
#> Folds:
#> List of 5
#>  $ Fold1: int [1:20] 1 15 16 20 25 28 29 40 47 49 ...
#>  $ Fold2: int [1:20] 6 7 10 14 18 32 35 37 38 44 ...
#>  $ Fold3: int [1:20] 11 17 19 21 23 27 36 39 41 45 ...
#>  $ Fold4: int [1:20] 9 12 13 22 24 30 31 33 34 48 ...
#>  $ Fold5: int [1:20] 2 3 4 5 8 26 42 43 46 50 ...
#> NULL
```
