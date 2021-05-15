trans <- function(x) as.matrix(x) %>% t()
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
