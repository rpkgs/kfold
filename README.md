<!-- badges: start -->

[![R-CMD-check](https://github.com/rpkgs/rtrend/workflows/R-CMD-check/badge.svg)](https://github.com/rpkgs/rtrend/actions)
[![codecov](https://codecov.io/gh/rpkgs/rtrend/branch/master/graph/badge.svg)](https://codecov.io/gh/rpkgs/rtrend)

<!-- badges: end -->

# kfold

The goal of kfold is to …

## Installation

You can install the development version of kfold like so:

```r
remotes::install_github("rpkgs/kfold")
```

## Example

This is a basic example which shows you how to solve a common problem:

```r
library(kfold)
#> Registered S3 method overwritten by 'Ipaper':
#>   method           from
#>   print.data.table data.table
#> Registered S3 method overwritten by 'kfold':
#>   method         from
#>   predict.ranger ranger
devtools::load_all()
#> [1m[22m[36mℹ[39m Loading [34mkfold[39m
library(future)
plan(multisession, workers = 5)

# set.seed(1)
n <- 100 ; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)

# r_rf  <- kfold_rf(X, y)
r_lm <- kfold_lm(X, y, .progress = FALSE)
r_lm

#> # A tibble: 12 × 12
#>    kfold type     R2    NSE    KGE  RMSE   MAE   Bias Bias_perc n_sim      R pvalue
#>    <chr> <fct> <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>     <dbl> <dbl>  <dbl>  <dbl>
#>  1 1     train 0.048  0.048 -0.105 0.917 0.751  0         0        80  0.219  0.051
#>  2 1     test  0.017 -0.074 -0.469 1.12  0.872  0.069    -0.423    20 -0.131  0.583
#>  3 2     train 0.018  0.018 -0.227 0.971 0.789  0         0        80  0.132  0.242
#>  4 2     test  0     -0.047 -0.71  0.947 0.768 -0.122     1.14     20  0.018  0.94
#>  5 3     train 0.022  0.022 -0.202 0.965 0.779  0         0        80  0.15   0.185
#>  6 3     test  0.023  0.018 -0.314 0.935 0.77   0.061    -0.502    20  0.151  0.524
#>  7 4     train 0.036  0.036 -0.147 0.95  0.772  0         0        80  0.189  0.093
#>  8 4     test  0.057 -0.078 -0.518 1.01  0.839  0.011    -0.085    20 -0.24   0.309
#>  9 5     train 0.018  0.018 -0.226 0.975 0.787  0         0        80  0.133  0.239
#> 10 5     test  0.063  0.039 -0.202 0.893 0.747 -0.037     0.273    20  0.25   0.287
#> 11 all   train 0.028  0.028 -0.181 0.955 0.776  0         0        80  0.165  0.162
#> 12 all   test  0     -0.032 -0.322 0.985 0.799 -0.004     0.028   100 -0.02   0.84
#>
#> Folds:
#> List of 5
#>  $ : int [1:20] 40 17 24 88 90 9 81 51 98 21 ...
#>  $ : int [1:20] 25 4 31 29 15 96 23 85 38 35 ...
#>  $ : int [1:20] 13 6 56 92 64 50 49 73 86 70 ...
#>  $ : int [1:20] 19 12 89 30 52 61 45 53 75 74 ...
#>  $ : int [1:20] 91 97 78 77 67 22 66 95 20 58 ...
#> NULL
```
