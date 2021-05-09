#' kfold randomForest
#' @import randomForest
#' @export
rf_kford <- function(X, Y, kfold = 5, ...){ #, threshold = 5000
    model <- function(index, ...){
        x_train <- X[-index,, drop = F]
        y_train <- Y[-index,, drop = F]

        x_test  <- X[index,, drop = F]
        y_test  <- Y[index,, drop = F]

        m <- randomForest(x_train, y_train, x_test, y_test, ntree = 500, ...)
        # m <- randomForest(CYA_sum~Velocity+Temperature+Salinity+Discharge, trainSet)        
        ypred = m$test$predicted
        list(model=m, info=GOF(y_test[, 1], ypred), ypred = ypred)
    }

    ## 1. kfold index
    n       <- nrow(X)
    set.seed(100)
    ind_lst <- createFolds(1:n, k = kfold, list = TRUE)

    ## 2. kfold RF
    res <- llply(ind_lst, model) #%>% map(purrr::transpose)
    tidy_kford(res)
}

tidy_kford <- function(res, ind_lst) {
    kfold_names <- names(ind_lst)

    ## 3. GOF information get
    val <- map(res, ~ .x$ypred) %>% unlist() # pred value
    ypred <- Y * NA
    ypred[unlist(ind_lst)] <- val
    info_all <- GOF(Y, ypred)

    model <- map(res, "model")
    info <- map(res, "info") %>%
        c(., all = list(info_all)) %>%
        do.call(rbind, .) %>%
        as.data.table()
    info$kford <- c(kfold_names, "all")

    listk(model, ypred, info) # how to return back to original value?
}
