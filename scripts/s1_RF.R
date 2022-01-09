library(data.table)
library(caret)
library(magrittr)
# library(randomForest)
# library(xgboost)
library(lubridate)
library(Ipaper)
# library(plyr)

df = fread("data-raw/INPUTS_daily.csv") %>% reorder_name(c("time", "Y"))
INPUTS = df[, -1] %>% previous_tn(n = 7)
mat <- do.call(cbind, INPUTS)
# fwrite(mat %>% data.table(), "Runoff_daily_Previous_t6.csv")
tests <- read_xlsx2list("data-raw/检验数据集.xlsx") %>%
    map(~reorder_name(.[, -1], "Y"))

select_vars <- function(df, day_pred) {
    cols_del = (1:day_pred)
    X_R = df %>% select(matches("R")) %>% previous_tn() %>%
        do.call(cbind, .)
    # upper runoff
    X_U = df %>% select(matches("Y|U")) %>% previous_tn() %>%
        map(~.[, -cols_del]) %>% do.call(cbind, .)
    cbind(X_U, X_R)
}

{
    n = 7
    ind_head = 1:n
    # X = do.call(cbind, INPUTS) %>% .[-ind_head, ]
    Y = df[-ind_head, 2] %>% as.matrix()
    im = "impurity_corrected"
    im = "none"

    res = foreach(day_pred = 1:7 %>% set_names(., .)) %do% {
        X_train = select_vars(df, day_pred)[-(ind_head), ] # previous 7 days, NA value exist
        r_all <- kford_rf(X_train, Y, 6, importance = im)#$info[7, ]

        pred = foreach(d = tests) %do% {
            X_test <- select_vars(d, day_pred)[-(1:20), ][day_pred, , drop = FALSE]
            ypred = predict(r_all, X_test) # kfold集合预报
            as.data.table(ypred)
        } %>% melt_list("period")
        listk(pred, info = r_all$gof[kford == "all", ])
    }
    d_pred = purrr::transpose(res) %>% map(~melt_list(., "day"))
    d_pred$pred %<>% .[order(period, day)] %>% reorder_name(c("period"))
    # write_list2xlsx(d_pred, "RF_5检验时段_预报结果.xlsx")
}

# write_fig({
#     par(mar = c(3, 3, 1, 1), mgp = c(1.8, 0.6, 0))
#     t = df$time[-ind_head] %>% as_date()
#     yobs = Y[, 1]
#     ysim = r_all$ypred[, 1]
#     GOF(yobs, ysim)
#     plot(t, yobs, type = "l", col = "black",
#          ylab = "Runoff", xlab = "Time")
#     grid()
#     legend("topright", c("OBS", "SIM"), col = c("black", "red"), lty = 1)
#     lines(t, ysim, col = "red")
# }, "a.pdf", 7, 4)
