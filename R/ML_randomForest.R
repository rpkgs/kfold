#' kfold randomForest
#'
#' @inheritParams randomForest::randomForest
#' @import randomForest
#' @importFrom plyr llply
#' @export
rf_kford <- function(X, Y, kfold = 5, ntree = 500, ...){ #, threshold = 5000
    set.seed(100)    
    ind_lst <- createFolds(1:nrow(X), k = kfold, list = TRUE)
    res <- llply(ind_lst, ml_kfold,
        X = X, Y = Y,
        FUN = randomForest, ntree = ntree, ..., 
        .progress = "text"
    )
    tidy_kford(res, ind_lst, Y)
}
