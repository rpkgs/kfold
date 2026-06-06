#' kfold_calib
#'
#' Calibrate a model on a single train/validation split.
#'
#' @param X Feature matrix (rows = observations).
#' @param Y Response matrix (rows = observations).
#' @param FUN Model fitting function with signature `FUN(x_train, y_train, ...)`.
#' @param index Integer vector of validation row indices. If `NULL`, the first
#'   `floor(n * ratio_valid)` rows are used.
#' @param ... Additional arguments forwarded to `FUN`.
#' @param ratio_valid Fraction of rows used as validation when `index = NULL`.
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
