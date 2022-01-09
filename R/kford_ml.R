#' kfold machine learning
#' @name kford_ml
#' 
#' @example R/example/ex-kfold_ml.R
#' @importFrom plyr llply
#' @export
kford_ml <- function(X, Y, kfold = 5, FUN, ...){ #, threshold = 5000
    set.seed(100)
    X = as.matrix(X)
    Y = as.matrix(Y)

    ind_lst <- createFolds(1:nrow(X), k = kfold, list = TRUE)
    res <- llply(ind_lst, kford_calib,
        X = X, Y = Y,
        # FUN = randomForest, ntree = ntree, ...,
        FUN = FUN, ...,
        .progress = "text"
    )
    kford_tidy(res, ind_lst, Y)
}

#' @inheritParams randomForest::randomForest
#' @rdname kford_ml
#' @export
kford_rf <- function(X, Y, kfold = 5,
    FUN = ranger, ntree = 500, ...)
{
    kford_ml(X, Y, kfold,
        # FUN = randomForest, ntree = ntree, ...)
        FUN = FUN, ntree = ntree, ...)
}

#' @inheritParams xgboost::xgboost
#' @import xgboost
#' @rdname kford_ml
#' @export
kford_xgboost <- function(X, Y, kfold = 5, verbose = FALSE, nrounds = 500, ...) {
    kford_ml(X, Y, kfold,
        FUN = xgboost, nrounds = nrounds, verbose = verbose, ...)
}

#' @rdname kford_ml
#' @export
kford_lm <- function(X, Y, kfold = 5, ...) {
    kford_ml(X, Y, kfold, FUN = .lm2, ...)
}

# rewrite ranger function
# ' @import randomForest
predict.ranger <- function(object, data = NULL, ...) {
    ranger:::predict.ranger(object, data, ...)$predictions
}

#' @import ranger
ranger <- function(x, y, ntree = 500, ...) {
    ranger::ranger(x = x, y = y, num.trees = ntree, ...)
}

# ' @export
.lm2 <- function(X, Y, ...) {
    # ans = lm(Y ~ X, ...) # na.action
    ans = lm.fit(cbind(1, X), Y)
    class(ans) = "lm2"
    ans
}

predict.lm2 <- function(object, data = NULL, ...) {
    ysim = cbind(1, data) %*% as.matrix(object$coefficients)
    ysim[, 1]
    # browser()
}

#' @export
print.kfold <- function(x, ...) {
    print(x$gof %>% dplyr::tibble() %>% dt_round(3)) # 
    cat("\nFolds:\n")
    print(str(x$index))
}
