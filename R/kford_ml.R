#' kfold machine learning
#' @name kfold_ml
#' 
#' @example R/example/ex-kfold_ml.R
#' @seealso [ranger::ranger()], [xgboost::xgboost()]
#' 
#' @importFrom plyr llply
#' @importFrom furrr future_map furrr_options
#' @export
kfold_ml <- function(X, Y, kfold = 5, FUN, ..., 
    fn_chunk = chunk_stratified, .progress=TRUE){ #, threshold = 5000
    set.seed(100)
    X = as.matrix(X)
    Y = as.matrix(Y)

    # ind_lst <- createFolds(1:nrow(X), k = kfold, list = TRUE)
    ind_lst <- fn_chunk(Y, kfold)

    res <- future_map(ind_lst, kfold_calib,
        X = X, Y = Y,
        FUN = FUN, ...,
        .progress = .progress,
        .options = furrr_options(seed = TRUE)
    )

    kfold_names <- names(ind_lst)
    data <- listk(X, Y)
    index <- set_names(ind_lst, kfold_names)
    model <- map(res, "model") %>% set_names(kfold_names)
    listk(data, index, model) %>% set_class("kfold")
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
kfold_xgboost <- function(X, Y, kfold = 5, FUN = xgboost, nrounds = 500, ...) {
    kfold_ml(X, Y, kfold, FUN = FUN, nrounds = nrounds, ...)
}

#' @rdname kfold_ml
#' @export
kfold_lm <- function(X, Y, kfold = 5, ...) {
    kfold_ml(X, Y, kfold, FUN = .lm2, ...)
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
print.kfold <- function(x, ...) {
    print(GOF(x) %>% dplyr::tibble() %>% dt_round(3)) # train + valid gof
    cat("\nFolds:\n")
    print(str(x$index))
}
