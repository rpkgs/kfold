# 构造 XGB 输入特征 (训练 train_xgboost / 预测 predict_xgboost 共用)
# 返回含有效观测行的 data, 及各模型的特征:
#   - MetXGB           : 仅气象 P, PET
#   - HydroMetXGB      : P, PET, Q_sim
#   - HydroMetQlagXGB  : 上述 + 各 lead 的滞后流量 Q_t-lead
#   - QlagXGB          : 仅用各 lead 的滞后流量 Q_t-lead
#' @importFrom kfold previous_tn
xgb_features <- function(data_full, leads = 1:12) {
  input <- data_full %>% add_previous(nlead = length(leads))
  data <- input[!is.na(Q_obs), ]

  vars_Q <- names(input) %>% .[grep("Q_t-", .)]
  names(leads) <- sprintf("lead_%02d", seq_along(leads))

  listk(
    data = data,
    # MetXGB = select(data, P, PET = PET_Romanenko),
    # QlagXGB = map(leads, \(l) select(data, all_of(vars_Q[l]))),
    # HydroMetXGB = select(data, P, PET = PET_Romanenko, Q_sim),
    HydroMetQlagXGB = map(leads, \(l) select(data, P, PET = PET_Romanenko, Q_sim, all_of(vars_Q[l])))
  )
}

#' 训练 XGB 洪水预报后处理模型
#'
#' 对 5 个模型族 (MetXGB / QlagXGB / HydroMetXGB / HydroMetQlagXGB) 分别做
#' kfold 训练. 特征由 `xgb_features()` 构造. 配套预测见 [predict_xgboost()],
#' 表现检验见 [summary_xgboost()].
#' @param data_full 训练数据, 含 `site, time, Q_obs, P, Q_sim, PET_Romanenko`
#' @param leads 预见期 (小时)
#' @param ... 透传给 `kfold_xgboost()`
#' @return list: `data_full`, `data` (有效观测行), 及各模型族的 kfold 拟合结果
#' @import xgboost
#' @importFrom kfold kfold_xgboost
#' @export
train_xgboost <- function(data_full, leads = 1:12, ...) {
  model <- function(X, Y, ...) {
    kfold_xgboost(X, Y,
      nrounds = 500, early_stopping_rounds = 30, eta = 0.05,
      ..., max_depth = 3, min_child_weight = 6,
      subsample = 0.8, gamma = 1, reg_lambda = 2
    )
  }

  X <- xgb_features(data_full, leads)
  Y <- select(X$data, Q_obs)

  listk(data_full, 
    # data = X$data,
    # MetXGB = model(X$MetXGB, Y),
    # QlagXGB = map(X$QlagXGB, \(Xi) model(Xi, Y)),
    # HydroMetXGB = model(X$HydroMetXGB, Y),
    HydroMetQlagXGB = map(X$HydroMetQlagXGB, \(Xi) model(Xi, Y))
  )
}
