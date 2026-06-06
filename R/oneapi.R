#' Compute GOF across multiple lead-time kfold objects
#'
#' @param objects Named list of `kfold` objects (one per lead time).
#' @param ds_test Named list of test datasets (one per lead time), each a list
#'   with `X` and `Y`.
#' @param ... Ignored.
#' @param idcol Column name for the lead-time id column.
#'
#' @return A `data.table` of GOF metrics with columns `lead` and `mode`.
#' @export
GOT_list <- function(list_kfold, list_test, ..., idcol = "lead") {
    list(
        train = map(list_kfold, GOF),
        test = mapply(\(object, test) GOF(object, test), list_kfold, list_test, SIMPLIFY = FALSE)
    ) %>%
        map(\(l) rbindlist(l, idcol = "lead")) %>%
        rbindlist() %>%
        subset(kfold == "ensemble") %>%
        mutate(
            lead = as.integer(lead),
            mode = factor(mode, c("train", "valid", "test"))
        ) %>%
        dt_round(4) %>%
        select(-kfold)
}

#' Build lagged feature matrices for multiple lead times
#'
#' @param data_full A `data.table` / `data.frame` with columns `Q_obs`, `P`,
#'   `PET_Romanenko`, and `Q_sim`.
#' @param leads Integer vector of lead times (in time steps) to build features for.
#'
#' @return A named list (one element per lead) of lists with `X` (feature matrix)
#'   and `Y` (response matrix).
#' @export
feature_leads <- function(data_full, leads = 1:12) {
    input <- data_full %>% add_previous(nlead = length(leads))
    data <- input[!is.na(Q_obs), ]

    vars_Q <- names(data) %>% .[grep("Q_t-", .)]
    Y <- select(data, Q_obs) %>% as.matrix()

    names(leads) <- leads
    map(leads, \(l){
        X <- select(data, P, PET = PET_Romanenko, Q_sim, all_of(vars_Q[l])) %>% as.matrix()
        listk(X, Y)
    })
}

#' add_previous
#' @param d with the variable of `Q_obs`
#' @param nlead the number of leads to add
#' @export
add_previous <- function(d, nlead = 12) {
    Qs <- previous_tn(d$Q_obs, nlead)[, -1] %>%
        as.data.table() %>%
        rename_with(\(x) paste0("Q_", x))
    cbind(d, Qs)
}
