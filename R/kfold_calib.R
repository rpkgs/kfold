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
kfold_tidy <- function(res, ind_lst, X, Y) {
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
        cbind(kfold = "all", mode = "train", gof_fold[mode == "train", -(1:2)][, lapply(.SD, mean)]),
        cbind(kfold = "all", mode = "valid", GOF(Y, ypred))
    )
    gof <- rbind(gof_fold, gof_all)
    data = listk(X, Y)
    listk(data, gof, ypred, index = ind_lst, model) %>% set_class("kfold")
}
