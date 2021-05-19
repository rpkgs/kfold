#' @export
ml_kfold <- function(index, X, Y, FUN = xgboost, ...) {

    x_train <- X[-index, , drop = F]
    y_train <- Y[-index, , drop = F]

    x_test <- X[index, , drop = F]
    y_test <- Y[index, ]

    m <- FUN(x_train, y_train, ...)
    ypred <- predict(m, x_test)
    list(model = m, info = GOF(y_test, ypred), ypred = ypred)
}

#' @export
tidy_kford <- function(res, ind_lst, Y) {
    kfold_names <- names(ind_lst)

    ## 3. GOF information get
    val <- map(res, ~ .x$ypred) %>% unlist() # pred value
    ypred <- Y * NA
    ypred[unlist(ind_lst)] <- val
    info_all <- GOF(Y, ypred)

    model <- map(res, "model")
    info <- map(res, "info") %>%
        c(., all = list(info_all)) %>%
        do.call(rbind, .) %>%
        as.data.table()
    info$kford <- c(kfold_names, "all")

    listk(model, ypred, info) %>% set_class("kfold") # how to return back to original value?
}

#' @export
predict.kfold <- function(object, newdata, ...) {
    lapply(object$model, function(m) predict(m, newdata, ...))
}
