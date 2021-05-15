library(data.table)
library(caret)
library(magrittr)
library(randomForest)
library(xgboost)
library(lubridate)
library(foreach)
library(iterators) 
# library(plyr)

df = fread("data-raw/INPUTS_daily.csv") %>% reorder_name(c("time", "Y"))
INPUTS = df[, -1] %>% {
    mapply(previous_tn, ., n = 7, prefix = names(.) %>% paste0("_"),
           SIMPLIFY = FALSE)
}

{
    n = 7
    ind_head = 1:n
    X = do.call(cbind, INPUTS) %>%
        .[-ind_head, ]
    Y = df[-ind_head, 2] %>% as.matrix()
    im = "impurity_corrected"
    im = "none"    
    day_pred = 7
    r_all <- kford_rf(X[, -(1:day_pred)], Y, 6, importance = im)#$info[7, ]
}

write_fig({
    par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0))
    t = df$time[-ind_head] %>% as_date()
    yobs = Y[, 1]
    ysim = r_all$ypred[, 1]
    GOF(yobs, ysim)
    plot(t, yobs, type = "l", col = "black",
         ylab = "Runoff", xlab = "Time")
    grid()
    legend("topright", c("OBS", "SIM"), col = c("black", "red"), lty = 1)
    lines(t, ysim, col = "red")
}, "a.pdf", 7, 4)

days_pred = 1:7 %>% set_names(., .)
res = lapply(days_pred, function(days){
    ind_head = 1:(days)
    X2 = X[, -ind_head]
    kford_rf(X2, Y, 6, importance = im)$info[7, ]
})

# df2 = fread("data-raw/XY_V2.csv")[, ]
# df = fread("data-raw/runoff_raw.csv")[, ]
# d = data.table(site = rep(1:20, each = 7), scale = 1:7) %>%
#     cbind(I = 1:nrow(.), .)
{
    # icols = d[scale != 7, I]
    # icols = d[site != 20, I]
    # i_bads = c(-140, -120)
    i_bads = info_bad$I[1:55]
    # X = df[, -1] %>% as.matrix() #%>% .[, -i_bads]
    # Y = df[, 1] %>% as.matrix()

    # kford_rf(X[, -i_bads], Y, 6, importance = "none")$info[7, ]
    res = foreach(i = 1:140) %do% {
        runningId(i)
        r <- kford_rf(X[, -i], Y, 6,
                      # max.depth = 20,
                      importance = im)
        r$info[7, ]
        # get_vip(r)
    }
    # system.time() # 160s
    # r$info
}

# RMSE       KGE       NSE      MAE        AI        Bias   Bias_perc n_sim        R2         R pvalue kford
# 1: 1.853811 0.6891135 0.6858585 1.315753 0.8892792 -0.04666586 -0.01245332  2186 0.6953415 0.8338714      0   all
