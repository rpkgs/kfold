# %% 
pacman::p_load(
    Ipaper, data.table, dplyr, lubridate, ggplot2
)

# %% 
data_full <- fread("examples/data_full_孤山_m05_ihacres_7p_1s.csv")
gof_hydro <- data_full[, GOF(Q_obs, Q_sim)]

years_test = c(2021, 2023)
year_end = max(data_full[!is.na(Q_obs), year(time)]) #
year_brk = year_end - 1

data_calib <- data_full[year(time) < year_brk]
data_valid <- data_full[year(time) >= year_brk]

ds_train <- feature_leads(data_calib, leads = 1:12)
ds_test <- feature_leads(data_valid, leads = 1:12)

# %% 
model <- function(X, Y, ...) {
    kfold_xgboost(X, Y,
        fn_chunk = chunk_stratified, 
        # fn_chunk = chunk, 
        ..., 
        nrounds = 200, early_stopping_rounds = 30, learning_rate = 0.05,
        max_depth = 3, min_child_weight = 6,
        subsample = 1, min_split_loss = 1, reg_lambda = 2
    )
}

objects <- map(ds_train, \(d) model(d$X, d$Y), .progress = TRUE)
gof <- GOF_oneapi(objects, ds_test)

# %% 
p <- ggplot(gof, aes(lead, NSE, color = mode)) +
    geom_line() + geom_point() + 
    geom_hline(yintercept = gof_hydro$NSE, linetype = "dashed", 
        color = "blue", alpha = 0.7) +
    scale_x_continuous(breaks = seq(0, 12, 2)) +
    # scale_y_continuous(limits = c(0.5, 1.0)) +
    theme_bw() + 
    theme(
        legend.position = "top", 
        legend.margin = margin(t = 0, b = -5, 0, 0)
    ) + 
    labs(x = "Lead (Hours)", y = "NSE", color = NULL)
write_fig(p, "Figure1.pdf", 10, 5, show = FALSE)
