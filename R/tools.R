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

dt_round <- function(d, digits = 4) {
    mutate(d, across(where(is.double), ~ round(.x, digits)))
}

#' @importFrom dplyr select
#' @export
select.matrix <- function(.data, ...) {
    # browser()
    .data %>% as.data.frame() %>%
        dplyr::select(...) %>%
        as.matrix()
}

trans <- function(x) as.matrix(x) %>% t()
