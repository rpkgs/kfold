#' @import xgboost
#' @importFrom kfold kfold_xgboost
#' @export
oneapi <- function(model, ds_train, ...) {
    objects <- map(ds_train, \(d) model(d$X, d$Y, ...), .progress = TRUE)
}

#' @export
GOF_oneapi <- function(objects, ds_test, ..., idcol = "lead") {
    list(
        train = map(objects, GOF),
        test = mapply(\(object, test) GOF(object, test), objects, ds_test, SIMPLIFY = FALSE)
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
