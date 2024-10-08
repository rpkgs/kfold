#' @keywords internal
#' @importFrom stats predict cor.test setNames quantile
#' @importFrom data.table data.table as.data.table
#' @importFrom purrr map is_empty
#' @importFrom dplyr mutate across 
#' @import magrittr
#' 
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
    if (getRversion() >= "2.15.1") {
        utils::globalVariables(
            c(
                "."
            )
        )
    }
}
