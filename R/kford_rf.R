#' kfold randomForest
#'
#' @inheritParams randomForest::randomForest
#' @importFrom plyr llply
#' @export
kford_rf <- function(X, Y, kfold = 5, ntree = 500, ...){ #, threshold = 5000
    set.seed(100)    
    X = as.matrix(X)
    Y = as.matrix(Y)
    
    ind_lst <- createFolds(1:nrow(X), k = kfold, list = TRUE)
    res <- llply(ind_lst, ml_kfold,
        X = X, Y = Y,
        # FUN = randomForest, ntree = ntree, ..., 
        FUN = ranger, ntree = ntree , ..., 
        .progress = "text"
    )
    tidy_kford(res, ind_lst, Y)
}

# rewrite ranger function
# ' @import randomForest
predict.ranger <- function(object, data = NULL, ...) {
    ranger:::predict.ranger(object, data, ...)$predictions
}

# ' @import ranger
ranger <- function(x, y, ntree = 500, ...) {
    ranger::ranger(x = x, y = y, num.trees = ntree, ...)
}
