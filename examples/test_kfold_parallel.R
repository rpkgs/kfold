# 验证训练入口及其封装：外部 plan 控制并行，串行与并行结果一致。
pkgload::load_all(".")
options(future.rng.onMisuse = "error")
set.seed(1)
X <- matrix(rnorm(300), 100, 3)
colnames(X) <- c("a", "b", "c")
Y <- X %*% c(1, -2, 0.5) + rnorm(100)

# 记录各折训练 PID，确认训练实际发生在两个 worker 中。
fit_probe <- function(x, y, ...) {
    fit <- kfold:::.lm2(x, y)
    fit$worker_pid <- Sys.getpid()
    fit
}

fit_all <- function() {
    list(
        probe = kfold_ml(X, Y, FUN = fit_probe, .progress = FALSE),
        xgb = kfold_xgboost(X, Y, nrounds = 5, nthread = 1, .progress = FALSE),
        rf = kfold_rf(X, Y, ntree = 10, num.threads = 1, .progress = FALSE),
        lm = kfold_lm(X, Y, .progress = FALSE)
    )
}

future::plan(future::sequential)
serial <- fit_all()
serial_gof <- lapply(serial, GOF)

future::plan(future::multisession, workers = 2)
plan_before <- future::plan()
parallel_fit <- fit_all()
stopifnot(identical(future::plan(), plan_before))
pids <- vapply(parallel_fit$probe$model, function(m) m$worker_pid, integer(1))
stopifnot(length(unique(pids)) == 2L, all(pids != Sys.getpid()))
cat("PASS: five folds trained on two workers; caller plan unchanged\n")

# 切回串行评估，单独验证并行训练结果，避免混入预测并行的影响。
future::plan(future::sequential)
for (name in names(serial)) {
    stopifnot(
        identical(serial[[name]]$index, parallel_fit[[name]]$index),
        isTRUE(all.equal(serial_gof[[name]], GOF(parallel_fit[[name]])))
    )
    cat("PASS:", name, "serial/parallel indices and GOF unchanged\n")
}
