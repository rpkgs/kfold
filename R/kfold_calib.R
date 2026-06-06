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
