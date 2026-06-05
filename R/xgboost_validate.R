# 验证期检验: 每个 lead 用 5 个 kfold 子模型预测, 并给出集合平均(mean)
# 集合写法参考 predict_xgb (R/xgboost_forecast.R)

# kfold 集合预测; only_ensemble = TRUE 仅返回集合平均 mean,
# FALSE 则同时返回各折 k01..k05 与 mean
predict_kfold <- function(models, X, only_ensemble = TRUE) {
  X <- as.matrix(X)
  p <- sapply(models, \(m) predict(m, X, validate_features = FALSE))
  if (only_ensemble) {
    return(data.table(mean = rowMeans(p)))
  }
  cbind(
    as.data.table(p) %>% set_names(sprintf("k%02d", seq_len(ncol(p)))),
    mean = rowMeans(p)
  )
}

#' 与 train_xgboost 配对的预测
#'
#' 与 [train_xgboost()] 配对: 在 `newdata` 上对每个 lead 用 5 个 kfold 子模型预测,
#' 并给出集合平均(`mean`). 特征构造与训练共用 `xgb_features()`, 保证一致.
#' @param object [train_xgboost()] 的返回
#' @param newdata 新数据(含 `Q_obs`, `P`, `Q_sim` 等), 通常为验证期 `data_valid`
#' @param leads 预见期, 默认由 `object` 推断 (与训练时一致)
#' @param only_ensemble `TRUE` 仅返回集合平均(`kfold = "mean"`); `FALSE` 同时返回各折 k01..k05
#' @return 长表 `[site, time, model, lead, Q_obs, kfold, Q_sim]`. model 取值
#'   Hydro(原始 Q_sim 基线) / MetXGB / HydroMetXGB / HydroMetQlagXGB / QlagXGB;
#'   验证期无观测时返回空表.
#' @export
predict_xgboost <- function(object, newdata, leads = seq_along(object$HydroMetQlagXGB),
                            only_ensemble = TRUE, ..., mode = "test") {
  X <- xgb_features(newdata, leads)
  meta <- X$data[, .(site, time, Q_obs)]
  nms <- names(object$HydroMetQlagXGB)[leads] # 与训练一致的 lead 子集
  # 单个模型在其特征 Xi 上集合预测 -> 长表
  one <- function(fit, Xi, model, lead) {
    if (nrow(Xi) == 0) {
      return(NULL)
    }
    cbind(meta, model, lead, predict_kfold(fit$model, Xi, only_ensemble)) %>%
      melt(c("site", "time", "Q_obs", "model", "lead"),
        variable.name = "kfold", value.name = "Q_sim", variable.factor = FALSE
      )
  }
  ans <- rbindlist(c(
    list(X$data[, .(site, time, Q_obs, model = "Hydro", lead = "-", kfold = "-", Q_sim)]),
    list(one(object$MetXGB, X$MetXGB, "MetXGB", "-")),
    list(one(object$HydroMetXGB, X$HydroMetXGB, "HydroMetXGB", "-")),
    map(nms, \(nm) one(object$HydroMetQlagXGB[[nm]], X$HydroMetQlagXGB[[nm]], "HydroMetQlagXGB", nm)),
    map(nms, \(nm) one(object$QlagXGB[[nm]], X$QlagXGB[[nm]], "QlagXGB", nm))
  ), use.names = TRUE)
  mutate(ans, mode = mode, .before = 1)
}

# 率定期 OOF (valid) 预测: 各模型族 $ypred 拼接为单序列 + Hydro 基线
# ypred = test 阶段对留出(未见)数据的预测拼接, 不泄漏; 标 mode = "valid"
predict_oof <- function(object, leads = seq_along(object$HydroMetQlagXGB)) {
  data <- object$data
  nms <- names(object$HydroMetQlagXGB)[leads] # 与训练一致的 lead 子集
  one <- function(fit, model, lead) {
    data[, .(site, time, Q_obs, model, lead, kfold = "all", Q_sim = as.numeric(fit$ypred), mode = "valid")]
  }
  rbindlist(c(
    list(data[, .(site, time, Q_obs, model = "Hydro", lead = "-", kfold = "all", Q_sim, mode = "valid")]),
    list(one(object$MetXGB, "MetXGB", "-")),
    list(one(object$HydroMetXGB, "HydroMetXGB", "-")),
    map(nms, \(nm) one(object$HydroMetQlagXGB[[nm]], "HydroMetQlagXGB", nm)),
    map(nms, \(nm) one(object$QlagXGB[[nm]], "QlagXGB", nm))
  ))
}

arrange_xgb <- \(d) {
  modes <- c("train", "valid", "test")
  d %<>% mutate(mode = factor(mode, modes))
  vars_common <- c("model", "lead", "mode") %>% intersect(names(d))
  setkeyv(d, vars_common)
  d
}

relocate_xgb <- \(d) {
  vars_common <- c("model", "lead", "kfold", "mode") %>% intersect(names(d))
  relocate(d, all_of(vars_common))
}

# 汇总率定期拟合优度 (直接取自 kfold 内部 $gof 的 kfold == "all" 行)
#   MODE = "train": 5 折各自训练 GOF 的均值;  "valid": 拼接 OOF ypred 的 GOF
gather_gof <- function(object, MODE = "valid", leads = seq_along(object$HydroMetQlagXGB)) {
  nms <- names(object$HydroMetQlagXGB)[leads] # 与训练一致的 lead 子集
  one <- function(fit, model, lead) {
    fit$gof %>% rename(mode = mode) %>%
     .[kfold == "all" & mode == MODE] %>%
      mutate(model = model, lead = lead) %>% relocate_xgb()
  }

  hydro <- cbind(
    object$data[, GOF(Q_obs, Q_sim)],
    model = "Hydro", lead = "-", kfold = "all", mode = MODE
  ) %>% relocate_xgb()

  rbindlist(c(
    list(hydro),
    list(one(object$MetXGB, "MetXGB", "-")),
    list(one(object$HydroMetXGB, "HydroMetXGB", "-")),
    map(nms, \(nm) one(object$HydroMetQlagXGB[[nm]], "HydroMetQlagXGB", nm)),
    map(nms, \(nm) one(object$QlagXGB[[nm]], "QlagXGB", nm))
  ))
}

#' 洪水场次合格率 (summary_xgboost 内部, train/valid/test 共用)
#' @param pred 含 `time, model, lead, kfold, Q_obs, Q_sim` 列的长表;
#' @param d_full 划分洪水场次的全序列
#' @param mode 情景标签, 写入 `info_pass$mode` ("train" / "valid" / "test")
eval_floods <- function(pred, d_full, mode) {
  SITE <- d_full$site[1]
  c(data_flood, info_flood) %<-% flood_divide(d_full, SITE)
  d_flood <- data_flood[, .(group, group_name, time)]
  n_flood <- d_flood$group_name %>% unique_length()

  info_pass <- merge(d_flood, pred, by = "time") %>%
    .[, eval_Qmax(Q_obs, Q_sim), .(model, lead, kfold, group, group_name)] %>%
    .[passed == TRUE, .(perc_pass = .N / n_flood, n_flood = n_flood), .(model, lead, kfold)] %>%
    arrange(model, lead, kfold) %>%
    mutate(mode = mode)
  listk(info_pass, info_flood)
}

#' 模型表现检验: train / valid / test 一次给全
#'
#' 三个概念 (`mode` 列):
#' - **train**: 5 个 kfold 各自训练 GOF 的均值 (率定期, 样本内拟合)
#' - **valid**: 拼接的 OOF `ypred` 的 GOF (率定期, 交叉验证, 不泄漏)
#' - **test** : 最后两年未见数据 `newdata` 上 5 模型集合预测(`mean`)的 GOF (样本外)
#'
#' train / valid 恒返回 (从 `object` 的 kfold 内部计算, 无需新数据);
#' 传入 `newdata` 时追加 test (集合预测见 [predict_xgboost()]).
#' @param object [train_xgboost()] 的返回
#' @param newdata 验证期数据 (最后两年, 训练时未用); `NULL` 则只出 train/valid
#' @param leads 预见期, 默认由 `object` 推断
#' @return `pred` (valid + test 时间序列) / `gof` (train/valid/test) /
#'   `info_pass` (洪水合格率) / `info_flood` (按 valid/test 分)
#' @export
summary_xgboost <- function(
  object, newdata = NULL,
  leads = seq_along(object$HydroMetQlagXGB)
) {
  # 率定期: train (5 折 train GOF 均值) + valid (OOF 拼接, 不泄漏); GOF 同取自 $gof
  pred_valid <- predict_oof(object, leads)
  gof_valid <- gather_gof(object, "valid", leads)
  c(pass_valid, flood_valid) %<-% eval_floods(pred_valid, object$data_full, "valid")

  pred_train <- predict_xgboost(object, object$data, leads, mode = "train")
  gof_train <- gather_gof(object, "train", leads)
  c(pass_train, flood_train) %<-% eval_floods(pred_train, object$data_full, "train")

  pred <- rbind(pred_train, pred_valid)
  gof <- rbind(gof_train, gof_valid)
  info_pass <- rbind(pass_train, pass_valid)
  info_flood <- list(train = flood_train, valid = flood_valid)

  # 验证期: test (集合预测 on 完全未见的 newdata)
  if (!is.null(newdata)) {
    pred_test <- predict_xgboost(object, newdata, leads, mode = "test")
    if (pred_test[, all(is.na(Q_obs))]) {
      warning(newdata$site[1], ": 验证期无观测(Q_obs), 跳过 test")
    } else {
      gof_test <- pred_test[!is.na(Q_sim), GOF(Q_obs, Q_sim), .(model, lead, kfold)] %>%
        mutate(mode = "test") %>% relocate_xgb()
      c(pass_test, flood_test) %<-% eval_floods(pred_test, newdata, "test")

      pred <- rbind(pred, pred_test)
      gof <- rbind(gof, gof_test) %>% arrange_xgb()
      info_pass <- rbind(info_pass, pass_test) %>% arrange_xgb()
      info_flood$test <- flood_test
    }
  }
  gof <- arrange(gof, model, lead, mode)
  listk(pred, gof, info_pass, info_flood)
}
