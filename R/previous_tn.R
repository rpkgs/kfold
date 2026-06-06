#' previous_tn
#'
#' Build a lagged matrix: column `t` is the original series, columns `t-1`,
#' `t-2`, … are progressively shifted (lagged) copies.
#'
#' @param x Numeric vector (default method) or `data.frame` to lag.
#' @param n Number of lags to create.
#' @param prefix Character prefix prepended to each column name.
#' @param ... Ignored.
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(10)
#' previous_tn(x, 7, "R1_")
#' # data.frame
#' d = data.frame(x)
#' previous_tn(d)
#' @export
previous_tn <- function(x, n = 7, prefix = "", ...) UseMethod("previous_tn")

#' @rdname previous_tn
#' @export
previous_tn.default <- function(x, n = 7, prefix = "", ...) {
    len <- length(x)
    names <- paste0("t-", 1:n) %>%
        c("t", .) %>%
        paste0(prefix, .)
    lapply(1:n, function(i) {
        # n_head <- i
        n_tail <- len - i #+ 1
        c(rep(NA_real_, i), x[1:n_tail])
    }) %>%
        do.call(cbind, .) %>%
        cbind(x, .) %>%
        set_colnames(names)
}

#' @rdname previous_tn
#' @export
previous_tn.data.frame <- function(x, n = 7, ...) {
    x %>% {
        mapply(previous_tn, .,
            n = n, prefix = names(.) %>% paste0("_"),
            SIMPLIFY = FALSE
        )
    }
}
