#' GOF
#'
#' Good of fitting
#'
#' @param Y_obs Numeric vector, observations
#' @param Y_sim Numeric vector, corresponding simulated values
#' @param w Numeric vector, weights of every points. If w included, when
#' calculating mean, Bias, MAE, RMSE and NSE, w will be taken into considered.
#' @param include.cv If true, cv will be included.
#' @param include.r If true, r and R2 will be included.
#' 
#' @return
#' * `RMSE` root mean square error
#' * `NSE` NASH coefficient
#' * `MAE` mean absolute error
#' * `AI` Agreement index (only good points (w == 1)) participate to
#' calculate. See details in Zhang et al., (2015).
#' * `Bias` bias
#' * `Bias_perc` bias percentage
#' * `n_sim` number of valid obs
#' * `cv` Coefficient of variation
#' * `R2` correlation of determination
#' * `R` pearson correlation
#' * `pvalue` pvalue of `R`
#'
#' @references
#' Zhang Xiaoyang (2015), http://dx.doi.org/10.1016/j.rse.2014.10.012
#'
#' @examples
#' Y_obs = rnorm(100)
#' Y_sim = Y_obs + rnorm(100)/4
#' GOF(Y_obs, Y_sim)
#' 
#' @importFrom hydroGOF KGE
#' @export
GOF <- function(Y_obs, Y_sim, w, include.cv = FALSE, include.r = TRUE){
    if (missing(w)) w <- rep(1, length(Y_obs))

    # remove NA_real_ and Inf values in Y_sim, Y_obs and w
    valid <- function(x) !is.na(x) & is.finite(x)

    I <- which(valid(Y_sim) & valid(Y_obs) & valid(w))
    # n_obs <- length(Y_obs)
    n_sim <- length(I)

    Y_sim <- Y_sim[I]
    Y_obs <- Y_obs[I]
    w     <- w[I]

    if (include.cv) {
        CV_obs <- cv_coef(Y_obs, w)
        CV_sim <- cv_coef(Y_sim, w)
    }
    if (is_empty(Y_obs)){
        out <- c(RMSE = NA_real_, 
            KGE = NA_real_,
            NSE = NA_real_, MAE = NA_real_, AI = NA_real_,
            Bias = NA_real_, Bias_perc = NA_real_, n_sim = NA_real_)

        if (include.r) out <- c(out, R2 = NA_real_, R = NA_real_, pvalue = NA_real_)
        if (include.cv) out <- c(out, obs = CV_obs, sim = CV_sim)
        return(out)
    }

    # R2: the portion of regression explained variance, also known as
    # coefficient of determination
    KGE = KGE(Y_sim, Y_obs)
    # https://en.wikipedia.org/wiki/Coefficient_of_determination
    # https://en.wikipedia.org/wiki/Explained_sum_of_squares
    y_mean <- sum(Y_obs * w) / sum(w)

    SSR    <- sum( (Y_sim - y_mean)^2 * w)
    SST    <- sum( (Y_obs - y_mean)^2 * w)
    # R2     <- SSR / SST

    RE     <- Y_sim - Y_obs
    Bias   <- sum ( w*RE)     /sum(w)                     # bias
    Bias_perc <- Bias/y_mean                              # bias percentage
    MAE    <- sum ( w*abs(RE))/sum(w)                     # mean absolute error
    RMSE   <- sqrt( sum(w*(RE)^2)/sum(w) )                # root mean sqrt error

    # https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient
    NSE  <- 1  - sum( (RE)^2 * w) / SST # NSE coefficient

    # Observations number are not same, so comparing correlation coefficient
    # was meaningless.
    # In the current, I have no idea how to add weights `R`.
    
    
    if (include.r){
        R      <- NA_real_
        pvalue <- NA_real_
        
        tryCatch({
            cor.obj <- cor.test(Y_obs, Y_sim, use = "complete.obs")
            R       <- cor.obj$estimate[[1]]
            pvalue  <- cor.obj$p.value
        }, error = function(e){
            message(e$message)
        })

        R2 = R^2
    }    
    # In Linear regression, R2 = R^2 (R is pearson cor)
    # R2     <- summary(lm(Y_sim ~ Y_obs))$r.squared # low efficient

    # AI: Agreement Index (only good values(w==1) calculate AI)
    AI <- NA_real_
    I2 <- which(w == 1)
    if (length(I2) >= 2) {
        Y_obs = Y_obs[I2]
        Y_sim = Y_sim[I2]
        y_mean = mean(Y_obs)
        AI = 1 - sum( (Y_sim - Y_obs)^2 ) / sum( (abs(Y_sim - y_mean) + abs(Y_obs - y_mean))^2 )
    }

    out <- c(RMSE = RMSE, KGE = KGE, NSE = NSE, MAE = MAE, AI = AI,
             Bias = Bias, Bias_perc = Bias_perc, n_sim = n_sim)

    if (include.r) out <- c(out, R2 = R2, R = R, pvalue = pvalue)
    if (include.cv) out <- c(out, obs = CV_obs, sim = CV_sim)
    return(out)
}


cv_coef <- function (x, w) {
    if (missing(w)) 
        w <- rep(1, length(x))
    if (length(x) == 0) {
        return(c(mean = NA_real_, sd = NA_real_, cv = NA_real_))
    }
    I <- is.finite(x)
    x <- x[I]
    w <- w[I]
    mean <- sum(x * w)/sum(w)
    sd <- sqrt(sum((x - mean)^2 * w)/sum(w))
    cv <- sd/mean
    c(mean = mean, sd = sd, cv = cv)
}
