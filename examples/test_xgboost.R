# %%
pacman::p_load(
    Ipaper, data.table, dplyr, lubridate, ggplot2
)

# %%
data_full <- fread("examples/data_full_孤山_m05_ihacres_7p_1s.csv")
gof_hydro <- data_full[, GOF(Q_obs, Q_sim)]

years_test <- c(2021, 2023)
year_end <- max(data_full[!is.na(Q_obs), year(time)]) #
year_brk <- year_end - 1

data_calib <- data_full[year(time) < year_brk]
data_valid <- data_full[year(time) >= year_brk]

list_train <- feature_leads(data_calib, leads = 1:12)
list_test <- feature_leads(data_valid, leads = 1:12)

# %%
stratified <- TRUE
# stratified <- FALSE

fn_chunk <- ifelse(stratified, chunk_stratified, chunk)
subfix <- ifelse(stratified, "stratified", "chunk")
ylims <- if (stratified) c(0.7, 1.0) else c(0.4, 1.0)

oneapi <- function(model, list_train, list_test, ..., 
    nrounds = 50, learning_rate = 0.1) {
    model <- function(X, Y, ...) {
        kfold_xgboost(X, Y,
            fn_chunk = fn_chunk, ...,
            nrounds = nrounds, learning_rate = learning_rate, 
            early_stopping_rounds = 30,
            max_depth = 3, min_child_weight = 6,
            subsample = 1, min_split_loss = 1, reg_lambda = 2
        )
    }

    objects <- map(list_train, \(d) model(d$X, d$Y), .progress = TRUE)
    gof <- GOT_list(objects, list_test)

    # %%
    p <- ggplot(gof, aes(lead, NSE, color = mode)) +
        geom_line() +
        geom_point() +
        geom_hline(
            yintercept = gof_hydro$NSE, linetype = "dashed",
            color = "blue", alpha = 0.7
        ) +
        scale_x_continuous(breaks = seq(0, 12, 2)) +
        # scale_y_continuous(limits = ylims, expand = c(0, 0)) +
        coord_cartesian(ylim = ylims, expand = c(0, 0)) +
        theme_bw() +
        theme(
            legend.position = "top",
            legend.margin = margin(t = 0, b = -5, 0, 0)
        ) +
        labs(x = "Lead (Hours)", y = "NSE", color = NULL)
    write_fig(p, glue("Figures/Figure1_n{nrounds}_eta{learning_rate}.pdf"), 10, 8, show = FALSE)
    return(gof)
}


params <- expand.grid(
    nrounds = c(50, 100, 200, 300), 
    learning_rate = c(0.3, 0.2, 0.1, 0.05)
) %>% as_tibble()

res = foreach(i = seq_len(nrow(params))) %do% {
    nrounds = params$nrounds[i]
    learning_rate = params$learning_rate[i]

    oneapi(model, list_train, list_test,
        nrounds = nrounds, learning_rate = learning_rate
    )
}
save(res, file = "oneapi_results.rda")

# %% 
load("oneapi_results.rda")

learning_rates <- c(0.3, 0.2, 0.1, 0.05)
nrounds = c(50, 100, 200, 300)
params <- expand.grid(
    nrounds = nrounds, learning_rate = learning_rates) %>% as_tibble()
params$gof <- res
dat = tidyr::unnest(params, cols = c(gof)) %>% data.table() %>% 
    mutate(
        nrounds = factor(nrounds, levels = c(50, 100, 200, 300)),
        learning_rate = factor(learning_rate, learning_rates)
    )

dat = dat[learning_rate == "0.1"]
p <- ggplot(dat, aes(lead, NSE, color = learning_rate, shape = nrounds)) +
    geom_line() + geom_point() +
    facet_wrap(~mode) +
    scale_x_continuous(breaks = seq(0, 12, 2)) +
    theme_bw() +
    theme(
        legend.position = "top",
        legend.margin = margin(t = 0, b = -5, 0, 0)
    ) +
    labs(x = "Lead (Hours)", y = "NSE")

write_fig(p, "Figure1_all_V2.pdf", 12, 6, show = FALSE)
