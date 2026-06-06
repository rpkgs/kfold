
<!-- badges: start -->

[![R-CMD-check](https://github.com/rpkgs/kfold/workflows/R-CMD-check/badge.svg)](https://github.com/rpkgs/kfold/actions)
[![codecov](https://codecov.io/gh/rpkgs/kfold/branch/master/graph/badge.svg)](https://codecov.io/gh/rpkgs/kfold)
<!-- badges: end -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# kfold

**三阶段定义：**

| 阶段      | 用途                 | 模型参数是否更新 | 谁来”看”这部分数据 |
| --------- | -------------------- | ---------------- | ------------------ |
| **train** | 拟合模型参数         | ✅ 更新           | 优化器（梯度下降） |
| **valid** | 调超参、选模型、早停 | ❌ 不更新         | 人/调参逻辑        |
| **test**  | 评估最终泛化性能     | ❌ 不更新         | 只看一次（最后）   |

## Installation

You can install the development version of kfold like so:

``` r
remotes::install_github("rpkgs/kfold")
```

## Example

``` r
library(kfold)
#> Registered S3 method overwritten by 'Ipaper':
#>   method           from      
#>   print.data.table data.table
#> Registered S3 method overwritten by 'kfold':
#>   method         from  
#>   predict.ranger ranger

devtools::load_all()
#> ℹ Loading kfold
```

``` r
set.seed(1)
# train + valid
X <- matrix(rnorm(300), 100, 3); colnames(X) <- c("a", "b", "c")
Y <- X %*% c(1, -2, 0.5) + rnorm(100)

# test
Xt <- matrix(rnorm(60), 20, 3); colnames(Xt) <- c("a", "b", "c")
Yt <- Xt %*% c(1, -2, 0.5) + rnorm(20)

m <- kfold_xgboost(X, Y, kfold = 5, nrounds = 30)

# %% 
GOF(m) 
#> [data.table]: 
#> # A data frame: 12 × 12
#>    kfold    mode     R2   NSE    KGE   RMSE    MAE    Bias Bias_perc n_sim     R
#>    <chr>    <chr> <dbl> <dbl>  <dbl>  <dbl>  <dbl>   <dbl>     <dbl> <dbl> <dbl>
#>  1 1        train 1.000 1.000  0.996 0.0249 0.0193 3.65e-4  0.00150     80 1.000
#>  2 2        train 1.000 1.000  0.994 0.0400 0.0297 7.37e-4  0.00307     80 1.000
#>  3 3        train 1.000 1.000  0.996 0.0329 0.0243 2.16e-4  0.000857    80 1.000
#>  4 4        train 1.000 1.000  0.995 0.0337 0.0229 1.01e-4  0.000409    80 1.000
#>  5 5        train 1.000 1.000  0.995 0.0314 0.0226 6.71e-4  0.00247     80 1.000
#>  6 ensemble train 1.000 1.000  0.995 0.0285 0.0209 4.18e-4  0.00167    100 1.000
#>  7 1        valid 0.589 0.537 -0.842 1.72   1.57   5.18e-1  1.82        20 0.767
#>  8 2        valid 0.578 0.548  0.181 1.60   1.33   2.30e-1  0.777       20 0.761
#>  9 3        valid 0.641 0.629  0.154 1.50   1.20   1.99e-1  0.814       20 0.801
#> 10 4        valid 0.793 0.790  0.594 1.16   0.985  9.49e-2  0.361       20 0.890
#> # ℹ 2 more rows
#> # ℹ 1 more variable: pvalue <dbl>
GOF(m, test = list(X = Xt, Y = Yt))
#> [data.table]: 
#> # A data frame: 6 × 12
#>   kfold    mode     R2   NSE   KGE  RMSE   MAE    Bias Bias_perc n_sim     R
#>   <chr>    <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>     <dbl> <dbl> <dbl>
#> 1 1        test  0.767 0.750 0.872  1.32  1.06 -0.0193   -0.0298    20 0.876
#> 2 2        test  0.714 0.708 0.690  1.43  1.17  0.161     0.248     20 0.845
#> 3 3        test  0.728 0.674 0.823  1.51  1.25 -0.0319   -0.0492    20 0.853
#> 4 4        test  0.767 0.742 0.828  1.34  1.07 -0.0746   -0.115     20 0.876
#> 5 5        test  0.800 0.771 0.784  1.27  1.05  0.116     0.179     20 0.894
#> 6 ensemble test  0.787 0.775 0.878  1.25  1.03  0.0303    0.0467    20 0.887
#> # ℹ 1 more variable: pvalue <dbl>

predict(m, mode = "train") %>% str()
#> List of 6
#>  $ 1       : num [1:100] 1.686 -0.11 NA 0.708 NA ...
#>  $ 2       : num [1:100] 1.6937 -0.0751 3.6643 0.7084 2.0931 ...
#>  $ 3       : num [1:100] 1.68 NA 3.7 0.67 2.1 ...
#>  $ 4       : num [1:100] NA -0.0845 3.6729 0.6744 2.1429 ...
#>  $ 5       : num [1:100] 1.6608 -0.0694 3.6919 NA 2.113 ...
#>  $ ensemble: num [1:100] 1.6795 -0.0846 3.6823 0.6902 2.1114 ...
predict(m, Xt, mode = "test") %>% str()
#> List of 6
#>  $ 1       : num [1:20] -4.085 3.533 -0.494 4.409 -0.566 ...
#>  $ 2       : num [1:20] -2.465 3.558 -0.174 3.618 0.292 ...
#>  $ 3       : num [1:20] -1.469 3.627 0.293 4.674 -0.813 ...
#>  $ 4       : num [1:20] -4.156 3.765 0.241 4.036 -0.14 ...
#>  $ 5       : num [1:20] -3.74 4.579 0.236 4.263 -1.121 ...
#>  $ ensemble: num [1:20] -3.1829 3.8123 0.0205 4.2002 -0.4694 ...
```
