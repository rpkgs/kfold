#' kfold_calib
#' @param index index of validation set
#' @export
kfold_calib <- function(X, Y, FUN = xgboost, index=NULL, ..., ratio_valid=0.3) {
    if (is.null(index)) {
      n <- nrow(X)
      index <- 1:floor(n * ratio_valid)
    }
    
    x_train <- X[-index, , drop = F]
    y_train <- Y[-index, , drop = F]

    x_valid <- X[index, , drop = F]
    y_valid <- Y[index, , drop = F]

    m <- FUN(x_train, y_train, ...)
    ypred_train <- predict(m, x_train)
    ypred_valid <- predict(m, x_valid)

    gof = list(
        train = GOF(y_train, ypred_train), 
        valid = GOF(y_valid, ypred_valid)
    ) %>% melt_list("mode")
    list(gof = gof, ypred = ypred_valid, model = m)
}

#' @export
kfold_tidy <- function(res, ind_lst, Y) {
    kfold_names <- names(ind_lst)
    if (is.null(kfold_names)) kfold_names <- paste0(seq_along(ind_lst))

    ## GOF information get
    val <- map(res, ~ .x$ypred) %>% unlist() # pred value
    ypred <- Y * NA
    ypred[unlist(ind_lst)] <- val

    model <- map(res, "model")
    
    gof_fold = map(res, "gof") %>% set_names(kfold_names) %>% 
        melt_list("kfold") %>% data.table()
    
    gof_all <- rbind(
        cbind(kfold = "all", type = "train", gof_fold[type == "train", -(1:2)][, lapply(.SD, mean)]),
        cbind(kfold = "all", type = "valid", GOF(Y, ypred))
    )
    gof <- rbind(gof_fold, gof_all)
    listk(gof, ypred, index = ind_lst, model) %>% set_class("kfold") # how to return back to original value?
}

#' kfold 测试期检验
#'
#' 用 [kfold_ml()] 训练得到的 k 个折模型, 在外部测试集 `X` 上分别预测, 并给出集合
#' 平均 (`mean`). 各折预测对应 `kfold = 1..k`, 集合平均对应 `kfold = "mean"`.
#'
#' @param object `kfold` 对象 (含 `model` 列表)
#' @param X 测试集自变量
#' @param Y 测试集观测, 用于计算 GOF
#' @param ... 透传给 [predict()]
#'
#' @return list:
#'   * `gof`  : 各折与集合 (`mean`) 的 GOF 长表 (`kfold`, `type = "test"`, ...)
#'   * `ypred`: 各折与集合预测的宽表 (每列一折, 末列 `mean`)
#' @export
kfold_test <- function(object, X, Y, ...) {
    X <- as.matrix(X)
    Y <- drop(as.matrix(Y))

    p <- sapply(object$model, \(m) predict(m, X, ...)) # n × k
    ypred <- cbind(p, mean = rowMeans(p))
    colnames(ypred) <- c(seq_len(ncol(p)), "mean")

    gof <- GOF(Y, ypred) # 多序列: 各折与 mean 各一行 (idcol = index)
    names(gof)[1] <- "kfold"
    gof[, type := "test"]
    gof <- gof[, c("kfold", "type", setdiff(names(gof), c("kfold", "type"))), with = FALSE]

    list(gof = gof, ypred = as.data.table(ypred))
}
