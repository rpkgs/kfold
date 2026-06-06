# kfold_test() 最小验证: 外部测试集上各折 (kfold = 1..5) 与集合 (mean) 的 GOF 与预测
devtools::load_all(".")

# %% 
set.seed(1)
# train + valid
X <- matrix(rnorm(300), 100, 3); colnames(X) <- c("a", "b", "c")
Y <- X %*% c(1, -2, 0.5) + rnorm(100)

# test
Xt <- matrix(rnorm(60), 20, 3); colnames(Xt) <- c("a", "b", "c")
Yt <- Xt %*% c(1, -2, 0.5) + rnorm(20)

m <- kfold_xgboost(X, Y, kfold = 5, nrounds = 30)

# %% 
# predict(m, mode = "train")
# predict(m, Xt, mode = "test")

GOF(m)
GOF(m, Xt, Yt)
