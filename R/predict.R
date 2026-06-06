#' predict for kfold object
#'
#' @param object A `kfold` object returned by [kfold_ml()].
#' @param newdata New feature matrix for prediction. Required when `mode = "test"`.
#' @param ... Additional arguments forwarded to the underlying model's `predict` method.
#' @param mode Prediction mode: `"train"` (in-sample, hold-out fold masked),
#'   `"valid"` (out-of-fold only), or `"test"` (full new data).
#' @export
predict.kfold <- function(
  object, newdata = NULL, ...,
  mode = "test" # only for train mode
) {
    if (mode != "test") {
        !is.null(newdata) && warning("`train` and `valid` mode, `newdata` is ignored!")
        newdata <- object$data$X
    }
    if (mode == "test" && is.null(newdata)) {
        stop("`test` mode, `newdata` is required!")
    }

    res <- list()
    for (i in seq_along(object$model)) {
        inds_unseen <- object$index[[i]]
        pred <- predict(object$model[[i]], newdata, ...)
        if (mode == "train") {
            pred[inds_unseen] <- NA
        } else if (mode == "valid") {
            pred[-inds_unseen] <- NA
        }
        res[[i]] <- pred
    }
    names(res) <- names(object$model)

    YPRED <- do.call("cbind", res)
    res$ensemble <- rowMeans(YPRED, na.rm = TRUE)
    res
}

# rewrite ranger function
# ' @import randomForest
#' @export
predict.ranger <- function(object, data = NULL, ...) {
    ranger:::predict.ranger(object, data, ...)$predictions
}

#' @export
predict.lm2 <- function(object, data = NULL, ...) {
    ysim <- cbind(1, data) %*% as.matrix(object$coefficients)
    ysim[, 1]
}
