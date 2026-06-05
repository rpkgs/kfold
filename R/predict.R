#' @export
predict.kfold <- function(object, newdata, ..., include.ensemble = FALSE) {
    ans <- lapply(object$model, function(m) predict(m, newdata, ...))
    if (include.ensemble) {
        ans$ensemble <- Reduce(`+`, ans) / length(ans)
    }
    ans
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
