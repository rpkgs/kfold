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
    # ind_lst <- Ipaper::chunk(1:nrow(X), kfold)
    ind_lst <- chunk_stratified(Y, kfold)

    res <- future_map(ind_lst, kfold_calib,
        X = X, Y = Y,
        FUN = FUN, ...,
        .progress = TRUE
    )
    kfold_tidy(res, ind_lst, Y)
}

# chunk <- function(x, nchunk = 6) {
#   split(x, cut(seq_along(x), nchunk, labels = FALSE)) %>% set_names(NULL)
# }

#' @export
chunk_stratified <- function(y, kfold = 5) {
    # 1. 获取按目标变量 Y 值大小排序的对应索引
    idx_sorted <- order(y)

    # 2. 计算能被切分成多少个大小为 kfold 的区块
    n_blocks <- ceiling(length(y) / kfold)

    # 3. 在每个区块内部进行 1:kfold 的随机乱序排列
    #    保证局部随机性，同时维持宏观的分布均匀
    set.seed(42) # 固定种子，保证交叉验证结果可精确复现
    groups <- unlist(lapply(1:n_blocks, function(x) sample(1:kfold)))

    # 4. 截去尾端多余的组号（对应 length(y) 不能整除 kfold 的情况）
    groups <- groups[1:length(y)]

    # 5. 将排序后的索引按照打乱后的组号分发，并去除 list 的 names
    ind_lst <- unname(split(idx_sorted, groups))
    return(ind_lst)
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
