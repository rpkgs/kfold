library(data.table)
library(caret)
library(plyr)
library(randomForest)
library(xgboost)

df = fread("data-raw/runoff_raw.csv")

X = df[, -1] %>% as.matrix()
Y = df[, 1] %>% as.matrix()

r_rf  = rf_kford(X, Y, 6)
r_xgb = xgboost_kford(X, Y, 6, verbose = T, nrounds = 500)
