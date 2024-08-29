set.seed(1)
n <- 100 ; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- as.matrix(rnorm(n))

## kfold
r_lm  <- kfold_lm(X, y)
r_xgb <- kfold_xgboost(X, y)
# r_rf  <- kfold_rf(X, y)

## 70%-30% split
r = kfold_calib(X, y, ratio_valid = 0.7, nrounds=500, verbose=FALSE)
r$gof
