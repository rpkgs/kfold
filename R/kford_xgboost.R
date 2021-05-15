#' kford xgboost
#' @inheritParams xgboost::xgboost
#' @import xgboost
#' @export
kford_xgboost <- function(X, Y, kfold = 5, verbose = FALSE, nrounds = 500, ...){ #, threshold = 5000    
    set.seed(100)
    ind_lst <- createFolds(1:nrow(X), k = kfold, list = TRUE)

    res <- llply(ind_lst, ml_kfold, 
        X = X, Y = Y, 
        FUN = xgboost, nrounds = nrounds, verbose = verbose, ..., .progress = "text")
    tidy_kford(res, ind_lst, Y)
}
