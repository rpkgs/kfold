set.seed(1)
n <- 100 ; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)

r_lm  <- kford_lm(X, y)
r_xgb <- kford_xgboost(X, y)
# r_rf  <- kford_rf(X, y)
