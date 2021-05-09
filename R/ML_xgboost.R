#' @import xgboost
#' @export
xgboost_kford <- function(X, Y, kfold = 5, verbose = FALSE, nrounds = 500, ...){ #, threshold = 5000
    model <- function(index, ...){
        x_train <- X[-index,, drop = F]
        y_train <- Y[-index,, drop = F]

        x_test  <- X[index,, drop = F]
        y_test  <- Y[index,, drop = F]

        m <- xgboost(x_train, y_train, nrounds = nrounds, verbose = verbose, ...)
        ypred = predict(m, x_test)
        # m <- randomForest(x_train, y_train, x_test, y_test, ntree = 500, ...)
        # m <- randomForest(CYA_sum~Velocity+Temperature+Salinity+Discharge, trainSet)
        # res <- predict(m, testSet)
        list(model = m, info = GOF(y_test[, 1], ypred), ypred = ypred)
    }

    ## 1. kfold index
    n <- nrow(X)
    set.seed(100)
    ind_lst <- createFolds(1:n, k = kfold, list = TRUE)

    ## 2. kfold RF
    res <- llply(ind_lst, model) 
    tidy_kford(res)
}
