# Baseline

## Framework
### [Data](gen.R)
- `gen_data()` – generates data using random seed
- Available sections:
  - `N(5, 1)  + mixt + N(17, 1)`
  - `U(0, 10) + mixt + U(0, 20)`
  - `exp(0.2) + mixt + exp(5)`
  - `Pareto(1, 0.5) + mixt + Pareto(1, 2)`


### [Cost Functions](cf.R)
**Signature**: `vector -> vector -> float`

- Quantile based:
  - `cf_median()` – difference between corresponding 50% quantile
  - `cf_all_q()` – aggregated diffrenece between all quantile in range 10%-90%
  - `cf_30_50_70()` - difference between corresponding 30%, 50% and 70% quantile
  - `cf_50_90_q()` - aggregated diffrenece between all quantile in range 50%-90%
- Statistical:
  - `cf_wt()` - Mann-Whitney statistics

All cost functions can be accessed via `cf` list by its name

### [Detectors](detectors.R)
**Signature**: `time series -> change point -> change interval`



- `det_cf_functor(cf_name)` – functor for constructing detector using cost function name
- `det_cf_functor(cf_name, left_cp=FALSE, right_cp=TRUE)` – boolean arguments `left_cp` and `right_cp` helps detector to decide how to split sample


### [Penalty Functions](penalty.R)
**Signature**: `true interval -> estimated interval -> float`

- `IOU` – intersection over union for two intervals
- `Hausdorff` – penalty is large when a changepoint from either `ci_true` or `ci_est` is far from every changepoint of `ci_est` or `ci_true` respectively
- `Recall | Precision` – measure the ability of the method to find the correct change times (with a tolerance of 10 samples)
- `F1-score` - harmonic mean of the `recall` and `precision`

### [Validator](validator.R)
Compare detectors using penalty functions