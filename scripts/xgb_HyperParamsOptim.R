# %%
devtools::load_all()
pacman::p_load(Ipaper, data.table, dplyr, magrittr, purrr, xgboost)

load("data-raw/xgb-孤山.rda")

# %%
# 用 xgb.train 复刻“吃满外层训练折”的基线路线，避免 .xgb 的折内 holdout
# 损失训练样本；所有调参候选都在同一个 OOF 口径下裁决。
.xgb_fixed <- function(X, Y, nrounds = 200, verbose = 0, ...) {
    params <- list(...)
    if (is.null(params$objective)) params$objective <- "reg:squarederror"
    if (is.null(params$eval_metric)) params$eval_metric <- "rmse"
    if (is.null(params$seed)) params$seed <- 100
    if (is.null(params$nthread)) params$nthread <- 1

    dtrain <- xgb.DMatrix(X, label = Y)
    xgb.train(params = params, data = dtrain, nrounds = nrounds, verbose = verbose)
}

# xgboost 包装器: 在训练折内部再切一小块 holdout 专供早停,
# 让 early_stopping 真正按验证误差触发, 同时不污染 OOF 折.
#' @export
#' @import xgboost
.xgb <- function(
  X, Y, nrounds = 500, early_stopping_rounds = 30,
  ratio_es = 0.2, verbose = 0, ...
) {
    set.seed(100)
    N <- nrow(X)
    i_es <- sample(N, floor(N * ratio_es))
    dtrain <- xgb.DMatrix(X[-i_es, , drop = FALSE], label = Y[-i_es])
    deval <- xgb.DMatrix(X[i_es, , drop = FALSE], label = Y[i_es])
    xgb.train(
        params = list(...), # eta / max_depth / subsample 等超参数
        data = dtrain, nrounds = nrounds,
        evals = list(eval = deval),
        early_stopping_rounds = early_stopping_rounds,
        verbose = verbose
    )
}

# xgb.cv 在折内定轮数, 再用整折数据重训 —— 不丢 20% 数据。
# 这个函数保留作对照，不作为主搜索路径。
.xgb_cv <- function(
  X, Y, nrounds = 500, early_stopping_rounds = 30,
  nfold = 5, verbose = 0, ...
) {
    params <- list(...)
    if (is.null(params$objective)) params$objective <- "reg:squarederror"
    if (is.null(params$eval_metric)) params$eval_metric <- "rmse"
    if (is.null(params$seed)) params$seed <- 100
    if (is.null(params$nthread)) params$nthread <- 1

    dtrain <- xgb.DMatrix(X, label = Y)
    cv <- xgb.cv(
        params = params, data = dtrain, nrounds = nrounds, nfold = nfold,
        early_stopping_rounds = early_stopping_rounds, verbose = verbose
    )
    xgb.train(
        params = params, data = dtrain,
        nrounds = cv$early_stop$best_iteration, verbose = verbose
    )
}

model_xgb_fixed <- function(X, Y, ...) {
    kfold_xgboost(X, Y,
        FUN = .xgb_fixed,
        ..., .progress = FALSE
    )
}

# %%
# 候选表是调参实验的输入，不写死在脚本里；每行一个完整参数组合。
xgb_candidates_file <- "data-raw/xgb_candidates.csv"
xgb_candidates <- fread(xgb_candidates_file)

write_xgb_candidates <- function(candidates = xgb_candidates,
    out_dir = "scripts/xgb_tuning")
{
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    fwrite(candidates, file.path(out_dir, "xgb_candidates.csv"))
}

candidate_args <- function(spec) {
    args <- as.list(spec[, !"candidate"])
    args <- args[!vapply(args, function(x) length(x) == 0 || is.na(x), logical(1))]
    args
}

run_one_candidate <- function(spec, leads = names(X$HydroMetQlagXGB)) {
    args <- candidate_args(spec)

    map(leads, function(lead) {
        Xi <- X$HydroMetQlagXGB[[lead]]
        fit <- do.call(model_xgb_fixed, c(list(X = Xi, Y = Y), args))
        gof <- copy(fit$gof[kfold == "all", -1])
        gof[, `:=`(lead = lead, candidate = spec$candidate)]
        setcolorder(gof, c("candidate", "lead", "type"))
        gof
    }) %>% rbindlist(fill = TRUE)
}

run_xgb_search <- function(
  candidates = xgb_candidates,
  leads = names(X$HydroMetQlagXGB),
  out_dir = "scripts/xgb_tuning",
  overwrite = FALSE
) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    fwrite(candidates, file.path(out_dir, "xgb_candidates.csv"))

    timing_file <- file.path(out_dir, "candidate_timing.csv")
    timing <- if (file.exists(timing_file)) fread(timing_file) else data.table()
    has_timing <- function(candidate_name) {
        all(c("candidate", "elapsed_sec") %in% names(timing)) &&
            any(timing[["candidate"]] == candidate_name & is.finite(timing[["elapsed_sec"]]))
    }

    ans <- vector("list", nrow(candidates))
    for (i in seq_len(nrow(candidates))) {
        spec <- candidates[i]
        fout <- file.path(out_dir, paste0(spec$candidate, ".csv"))

        if (file.exists(fout) && !overwrite && has_timing(spec$candidate)) {
            message("Reuse: ", fout)
            ans[[i]] <- fread(fout)
            next
        }

        if (file.exists(fout) && !overwrite) {
            message("Timing missing, rerun: ", spec$candidate)
        }
        message(sprintf("[%02d/%02d] %s", i, nrow(candidates), spec$candidate))
        started_at <- Sys.time()
        elapsed <- system.time({
            ans[[i]] <- run_one_candidate(spec, leads)
        })
        finished_at <- Sys.time()

        fwrite(ans[[i]], fout)

        timing_i <- data.table(
            candidate = spec$candidate,
            n_leads = length(leads),
            user_sec = unname(elapsed[["user.self"]]),
            system_sec = unname(elapsed[["sys.self"]]),
            elapsed_sec = unname(elapsed[["elapsed"]]),
            sec_per_lead = unname(elapsed[["elapsed"]]) / length(leads),
            started_at = format(started_at, "%Y-%m-%d %H:%M:%S %Z"),
            finished_at = format(finished_at, "%Y-%m-%d %H:%M:%S %Z"),
            out_csv = fout
        )
        timing <- rbind(timing[!timing[["candidate"]] %in% spec$candidate], timing_i, fill = TRUE)
        timing <- merge(
            candidates[, .(candidate_order = .I, candidate)],
            timing,
            by = "candidate",
            all.y = TRUE
        )[order(candidate_order)][, candidate_order := NULL]
        fwrite(timing, timing_file)
    }

    gof <- rbindlist(ans, fill = TRUE)
    fwrite(gof, file.path(out_dir, "all_candidates_gof.csv"))
    gof
}

summarise_xgb_search <- function(
  gof, baseline = "base_eta030_d3_mcw5",
  out_dir = "scripts/xgb_tuning"
) {
    valid <- copy(gof[type == "valid"])
    base <- valid[candidate == baseline, .(lead, NSE_base = NSE)]
    valid <- merge(valid, base, by = "lead", all.x = TRUE)
    valid[, delta_NSE := NSE - NSE_base]

    summary <- valid[, .(
        mean_NSE = mean(NSE),
        min_NSE = min(NSE),
        mean_delta_NSE = mean(delta_NSE),
        wins_vs_base = sum(delta_NSE > 0),
        mean_KGE = mean(KGE),
        mean_RMSE = mean(RMSE),
        mean_MAE = mean(MAE)
    ), by = candidate][order(-mean_NSE)]

    timing_file <- file.path(out_dir, "candidate_timing.csv")
    if (file.exists(timing_file)) {
        timing <- fread(timing_file)[, .(candidate, elapsed_sec, sec_per_lead)]
        summary <- merge(summary, timing, by = "candidate", all.x = TRUE)
        summary[, delta_NSE_per_min := mean_delta_NSE / (elapsed_sec / 60)]
        summary <- summary[order(-mean_NSE)]
    }

    fwrite(summary, file.path(out_dir, "summary_valid.csv"))
    fwrite(
        valid[, .SD[which.max(NSE)], by = lead][order(lead)],
        file.path(out_dir, "best_by_lead.csv")
    )
    summary
}

# %%
gof_search <- run_xgb_search()
summary_valid <- summarise_xgb_search(gof_search)
print(dt_round(summary_valid, 4))

# %%
# 逐 lead 看候选是否只是在个别 lead 上偶然赢。
best_candidate <- summary_valid$candidate[1]
lead_compare <- gof_search[
    type == "valid" & candidate %in% c("base_eta030_d3_mcw5", best_candidate),
    .(candidate, lead, NSE, KGE, RMSE, MAE)
][order(lead, candidate)]
fwrite(lead_compare, "scripts/xgb_tuning/best_vs_base_by_lead.csv")
print(dt_round(lead_compare, 4))
