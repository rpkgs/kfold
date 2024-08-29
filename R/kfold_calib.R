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

    x_test <- X[index, , drop = F]
    y_test <- Y[index, , drop = F]

    m <- FUN(x_train, y_train, ...)
    ypred <- predict(m, x_test)
    list(gof = GOF(y_test, ypred), ypred = ypred, model = m)
}

#' @export
kfold_tidy <- function(res, ind_lst, Y) {
    kfold_names <- names(ind_lst)

    ## 3. GOF information get
    val <- map(res, ~ .x$ypred) %>% unlist() # pred value
    ypred <- Y * NA
    ypred[unlist(ind_lst)] <- val
    info_all <- GOF(Y, ypred)

    model <- map(res, "model")
    gof <- map(res, "gof") %>%
        c(., all = list(info_all)) %>%
        do.call(rbind, .) %>%
        as.data.table()
    gof$kfold <- c(kfold_names, "all")

    listk(gof, ypred, index = ind_lst, model) %>% set_class("kfold") # how to return back to original value?
}

#' @export
predict.kfold <- function(object, newdata, ...) {
    lapply(object$model, function(m) predict(m, newdata, ...))
}
