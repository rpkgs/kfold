# kfold.R — Codebase Guide

## Package Overview

`kfold` is an R package for stratified k-fold cross-validation of machine-learning models (Random Forest via `ranger`, XGBoost, linear models). It computes per-fold and aggregate GOF (Goodness of Fit) statistics for both train and test sets.

## Key Files

| File              | Role                                                                    |
| ----------------- | ----------------------------------------------------------------------- |
| `R/kford_ml.R`    | Top-level API: `kfold_ml`, `kfold_rf`, `kfold_xgboost`, `kfold_lm`      |
| `R/kfold_calib.R` | Per-fold calibration (`kfold_calib`) and result assembly (`kfold_tidy`) |
| `R/GOF.R`         | GOF metrics: `GOF()`, `NSE()`, `cv_coef()`                              |
| `R/tools.R`       | `chunk_stratified()` — stratified fold splitter                         |

---

## `gof_all` Calculation — Train vs Test

### Step 1: Fold splitting (`chunk_stratified`)

`chunk_stratified(Y, kfold)` sorts observations by Y value, then shuffles within blocks of size `kfold`. This ensures each fold has a representative distribution of Y (stratified split). Returns a list of `kfold` index vectors.

### Step 2: Per-fold calibration (`kfold_calib`)

For fold `i` with test indices `index_i`:

```r
x_train <- X[-index_i, ]   # ~(1 - 1/k) × N rows
x_test  <- X[ index_i, ]   # ~(1/k) × N rows

m <- FUN(x_train, y_train)          # train model
ypred_train <- predict(m, x_train)  # in-sample prediction
ypred_test  <- predict(m, x_test)   # out-of-fold prediction

gof = list(
    train = GOF(y_train, ypred_train),  # in-sample fit
    test  = GOF(y_test,  ypred_test)    # out-of-fold fit
)
```

### Step 3: Aggregate `gof_all` (`kfold_tidy`)

```r
gof_all <- rbind(
    # train: column-wise MEAN of each fold's train GOF
    cbind(kfold = "all", type = "train",
          gof_fold[type == "train", -(1:2)][, lapply(.SD, mean)]),

    # test: single GOF call on ALL out-of-fold predictions stacked together
    cbind(kfold = "all", type = "test",
          GOF(Y, ypred))   # ypred assembled from all folds' ypred_test
)
```

#### `gof_all` train

- **What it is**: Column-wise arithmetic mean of each fold's train-set GOF metrics.
- **Interpretation**: Average in-sample fitting performance. Reflects how well the model memorizes the training data on average across folds. Expected to be optimistic (higher NSE/KGE, lower RMSE than test).

#### `gof_all` test

- **What it is**: A **single `GOF()` call** on the full `Y` vs. globally assembled out-of-fold predictions `ypred`. Each observation's prediction comes exclusively from the fold iteration where that observation was held out.
- **Interpretation**: Unbiased estimate of generalization performance across the entire dataset. More statistically rigorous than averaging fold-level test GOFs, because it evaluates all N observations as one unit (no double-counting, no averaging of averages).

#### Key asymmetry

|                 | Aggregation method                   | Observations evaluated  |
| --------------- | ------------------------------------ | ----------------------- |
| `gof_all` train | Mean of fold-level statistics        | ~(1 − 1/k) × N per fold |
| `gof_all` test  | Single GOF on pooled OOF predictions | All N observations      |

---

## GOF Metrics (`GOF()`)

All metrics use optional per-point weights `w` (default: uniform).

| Metric      | Formula                              | Note                                   |
| ----------- | ------------------------------------ | -------------------------------------- | ----- | ------------------- |
| `NSE`       | `1 - Σ(ysim - yobs)² / Σ(yobs - ȳ)²` | Nash–Sutcliffe efficiency; 1 = perfect |
| `KGE`       | via `hydroGOF::KGE()`                | Kling–Gupta efficiency                 |
| `RMSE`      | `√(Σw·(ysim−yobs)² / Σw)`            | Root mean square error                 |
| `MAE`       | `Σw·                                 | ysim−yobs                              | / Σw` | Mean absolute error |
| `Bias`      | `Σw·(ysim−yobs) / Σw`                | Signed mean error                      |
| `Bias_perc` | `Bias / ȳ`                           | Relative bias                          |
| `R2`, `R`   | Pearson correlation²                 | Optional via `include.r = TRUE`        |

Invalid values (`NA`, `Inf`) are removed before all calculations.

---

## Parallelism

`kfold_ml` uses `furrr::future_map` for fold-level parallelism. Set up a plan before calling:

```r
library(future)
plan(multisession, workers = 5)
```
