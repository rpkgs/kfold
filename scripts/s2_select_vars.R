
days_pred <- 1:7 %>% set_names(., .)
res <- lapply(days_pred, function(days) {
    ind_head <- 1:(days)
    X2 <- X[, -ind_head]
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
    i_bads <- info_bad$I[1:55]
    # X = df[, -1] %>% as.matrix() #%>% .[, -i_bads]
    # Y = df[, 1] %>% as.matrix()

    # kford_rf(X[, -i_bads], Y, 6, importance = "none")$info[7, ]
    res <- foreach(i = 1:140) %do% {
        runningId(i)
        r <- kford_rf(X[, -i], Y, 6,
            # max.depth = 20,
            importance = im
        )
        r$info[7, ]
        # get_vip(r)
    }
    # system.time() # 160s
    # r$info
}

# RMSE       KGE       NSE      MAE        AI        Bias   Bias_perc n_sim        R2         R pvalue kford
# 1: 1.853811 0.6891135 0.6858585 1.315753 0.8892792 -0.04666586 -0.01245332  2186 0.6953415 0.8338714      0   all

temp <- r_all$info[7, 1:11] %>% trans()
info <-
    {
        trans(do.call(rbind, res)[, 1:11]) - temp[, 1]
    } %>%
    t() %>%
    as.data.table() %>%
    cbind(d, .) %>%
    dt_round()
info_bad <- info[KGE > 0 & NSE > 0 & R2 > 0, ][order(-R2)]

save(info, info_bad, file = "RF_LOO_degradation.rda")
# permutation
get_vip <- function(r) {
    vip <- map(r$model, ~ .$variable.importance) %>% # %>% hist()
        do.call(cbind, .) %>%
        rowMeans()
    vip
}

ggplot(d, aes(scale, vip, color = scale)) +
    geom_histogram(stat = "identity", position = "stack") +
    facet_wrap(~site) # , scales = "free_y"

ggplot(d[, .(vip = mean(vip)), .(site)], aes(site, vip)) +
    geom_histogram(stat = "identity")
ggplot(d[, .(vip = mean(vip)), .(scale)], aes(scale, vip)) +
    geom_histogram(stat = "identity")
