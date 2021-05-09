library(data.table)
library(caret)
library(magrittr)
library(randomForest)
library(xgboost)
# library(plyr)

df = fread("data-raw/runoff_raw.csv")[1:100, ]

X = df[, -1] %>% as.matrix()
Y = df[, 1] %>% as.matrix()
r_rf  = rf_kford(X, Y, 6)

## xgboost存在明显过拟合现象
eval_kge <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    err <- -KGE(preds, labels)
    return(list(metric = "KGE", value = err))
}

{
    r_xgb = xgboost_kford(X, Y, 6, verbose = T, nrounds = 100,
        params = list(eval_metric = eval_kge, max_depth = 3,
                      min_child_weight = 0.1,
                      subsample = 1, colsample_bytree = 0.85))
    r_xgb$info
}

save(r_rf, r_xgb, file = "OUTPUT/results_ML_RF&xgboost.rda")
