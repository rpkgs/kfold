#' kfold machine learning
#' @name kfold_ml
#' 
#' @example R/example/ex-kfold_ml.R
#' @seealso [ranger::ranger()], [xgboost::xgboost()]
#' 
#' @importFrom plyr llply
#' @importFrom furrr future_map
#' @export
kfold_ml <- function(X, Y, kfold = 5, FUN, ...){ #, threshold = 5000
    set.seed(100)
    X = as.matrix(X)
    Y = as.matrix(Y)

    # ind_lst <- createFolds(1:nrow(X), k = kfold, list = TRUE)
    ind_lst <- Ipaper::chunk(1:nrow(X), kfold)

    res <- future_map(ind_lst, kfold_calib,
        X = X, Y = Y,
        FUN = FUN, ...,
        .progress = TRUE
    )
    kfold_tidy(res, ind_lst, Y)
}

#' @inheritParams ranger::ranger
#' @rdname kfold_ml
#' @export
kfold_rf <- function(X, Y, kfold = 5,
    FUN = ranger, ntree = 500, importance = "none", ...)
{
    kfold_ml(X, Y, kfold,
        # FUN = randomForest, ntree = ntree, ...)
        FUN = FUN, ntree = ntree, ...)
}

#' @inheritParams xgboost::xgboost
#' @import xgboost
#' @rdname kfold_ml
#' @export
kfold_xgboost <- function(X, Y, kfold = 5, nrounds = 500, ...) {
    kfold_ml(X, Y, kfold, FUN = xgboost, nrounds = nrounds, ...)
}

#' @rdname kfold_ml
#' @export
kfold_lm <- function(X, Y, kfold = 5, ...) {
    kfold_ml(X, Y, kfold, FUN = .lm2, ...)
}

# rewrite ranger function
# ' @import randomForest
#' @export
predict.ranger <- function(object, data = NULL, ...) {
    ranger:::predict.ranger(object, data, ...)$predictions
}

#' @import ranger
ranger <- function(x, y, ntree = 500, ...) {
    ranger::ranger(x = x, y = drop(y), num.trees = ntree, ...)
}

# ' @export
.lm2 <- function(X, Y, ...) {
    # ans = lm(Y ~ X, ...) # na.action
    ans = lm.fit(cbind(1, X), Y)
    class(ans) = "lm2"
    ans
}


#' @export
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
