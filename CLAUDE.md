# kfold.R — Codebase Guide

## Package Overview

`kfold` is an R package for stratified k-fold cross-validation of machine-learning models (Random Forest via `ranger`, XGBoost, linear models). It computes per-fold and aggregate GOF (Goodness of Fit) statistics for train, valid, and external test sets.

## Key Files

| File               | Role                                                                         |
| ------------------ | ---------------------------------------------------------------------------- |
| `R/kford_ml.R`     | Top-level API: `kfold_ml`, `kfold_rf`, `kfold_xgboost`, `kfold_lm`          |
| `R/kfold_calib.R`  | Per-fold calibration: `kfold_calib`                                          |
| `R/predict.R`      | S3 predict methods: `predict.kfold`, `predict.ranger`, `predict.lm2`         |
| `R/GOF.R`          | GOF S3 generic: `GOF()`, `GOF.default()`, `GOF.kfold()`, `NSE()`, `cv_coef` |
| `R/chunk.R`        | Fold splitters: `chunk_stratified()`, `chunk()`                              |
| `R/tools.R`        | Internal helpers: `listk()`, `dt_round()`, `select.matrix()`                |
| `R/oneapi.R`       | Multi-lead-time utilities: `GOT_list()`, `feature_leads()`, `add_previous()` |

---

## `kfold` S3 Object

`kfold_ml()` returns an S3 object of class `kfold`:

```r
list(
  data  = list(X, Y),          # full feature/response matrices
  index = list(fold1 = ..., ), # named list of validation index vectors
  model = list(fold1 = ..., )  # named list of fitted model objects
)
```

---

## `predict.kfold` — Three Modes

```r
predict(object, newdata = NULL, ..., mode = "test")
```

| `mode`    | Description                                                                            |
| --------- | -------------------------------------------------------------------------------------- |
| `"train"` | Each fold model predicts full `X`; own-fold indices set to `NA` → ensemble row means  |
| `"valid"` | Each fold model predicts full `X`; non-own-fold indices set to `NA` → ensemble row means |
| `"test"`  | All fold models predict `newdata` → ensemble row means (requires `newdata`)           |

Returns a named list of per-fold prediction vectors plus an `ensemble` element (row means across folds).

---

## `GOF.kfold` — Train / Valid / Test

```r
GOF(object)             # computes train + valid GOF on the full dataset
GOF(object, test = list(X = ..., Y = ...))  # computes test GOF on external data
```

Internally:

```r
# train: ensemble prediction with each fold's own observations masked → GOF on full Y
ypred_train <- predict(object, mode = "train")
gof_train   <- GOF(object$data$Y, ypred_train, mode = "train")

# valid: OOF ensemble prediction → GOF on full Y (unbiased generalisation estimate)
ypred_valid <- predict(object, mode = "valid")
gof_valid   <- GOF(object$data$Y, ypred_valid, mode = "valid")

rbind(gof_train, gof_valid)
# → data.table with columns: kfold (fold name / "ensemble"), mode ("train"/"valid"), metrics…
```

`kfold` column holds individual fold names and `"ensemble"` (the pooled OOF row).

---

## GOF Metrics (`GOF()`)

All metrics use optional per-point weights `w` (default: uniform). Invalid values (`NA`, `Inf`) are removed before all calculations.

| Metric      | Formula / Source                       | Note                                   |
| ----------- | -------------------------------------- | -------------------------------------- |
| `NSE`       | `1 - Σ(ysim−yobs)² / Σ(yobs−ȳ)²`      | Nash–Sutcliffe efficiency; 1 = perfect |
| `KGE`       | via `hydroGOF::KGE()`                  | Kling–Gupta efficiency                 |
| `RMSE`      | `√(Σw·(ysim−yobs)² / Σw)`             | Root mean square error                 |
| `MAE`       | `Σw·\|ysim−yobs\| / Σw`               | Mean absolute error                    |
| `Bias`      | `Σw·(ysim−yobs) / Σw`                 | Signed mean error                      |
| `Bias_perc` | `Bias / ȳ`                             | Relative bias                          |
| `R2`, `R`   | Pearson correlation²                   | Optional via `include.r = TRUE`        |

Default output column order: `NSE, KGE, RMSE, MAE, Bias, Bias_perc, n_sim` (plus `R2, R, pvalue` when `include.r = TRUE`).

---

## Parallelism

`kfold_ml` uses `furrr::future_map` for fold-level parallelism. Set up a plan before calling:

```r
library(future)
plan(multisession, workers = 5)
```

---

## Multi-Lead-Time API (`R/oneapi.R`)

| Function         | Description                                                                 |
| ---------------- | --------------------------------------------------------------------------- |
| `GOT_list()`     | Compute train + test GOF across a named list of `kfold` objects (one per lead time); returns a tidy `data.table` with a `lead` column |
| `feature_leads()` | Build lagged feature matrices `list(X, Y)` for each lead time from a full dataset |
| `add_previous()` | Append lagged `Q_obs` columns (`Q_t-1`, …, `Q_t-n`) to a data frame       |
