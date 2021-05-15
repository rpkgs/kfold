listk <- function(...) {
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars)) {
          seq_along(cols)
      } else {
        which(vars == "")
    }
    if (length(Id_noname) > 0) {
          vars[Id_noname] <- sapply(cols[Id_noname], deparse)
      }
    x <- setNames(list(...), vars)
    return(x)
}

#' previous_tn
#'
#' @examples
#' set.seed(1)
#' x = rnorm(10)
#' previous_tn(x, 7, "R1_")
#' @export
previous_tn <- function(x, n, prefix = "") {
    len <- length(x)
    names <- paste0("t-", 1:n) %>%
        c("t", .) %>%
        paste0(prefix, .)
    lapply(1:n, function(i) {
        n_head <- i
        n_tail <- len - i #+ 1
        c(rep(NA_real_, i), x[1:n_tail])
    }) %>%
        do.call(cbind, .) %>%
        cbind(x, .) %>%
        set_colnames(names)
}

dt_round <- function(d, digits = 4) {
    mutate(d, across(where(is.double), ~ round(.x, digits)))
}
