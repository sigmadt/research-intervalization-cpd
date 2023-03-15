# Baseline

## Framework
### [Data](gen.R)
- `gen_data()` – generates data using random seed
- Available sections:
  - `N(5, 1)  + mixt + N(17, 1)`
  - `U(0, 10) + mixt + U(0, 20)`


### [Cost Functions](cf.R)
- Quantile based:
  - `cf_median()` – difference between corresponding 50% quantile
  - `cf_all_q()` – aggregated diffrenece between all quantile in range 10%-90%
  - `cf_30_50_70()` - difference between corresponding 30%, 50% and 70% quantile
  - `cf_50_90_q()` - aggregated diffrenece between all quantile in range 50%-90%
- Statistical:
  - `cf_wt()` - Mann-Whitney statistics

All cost functions can be accessed via `cf` by its name

### [Detectors](detectors.R)
**Signature**: `time series -> change point -> change interval`



- `det_cf_functor(cf_name)` - functor for constructing detector using cost function name


### Penalty Functions
- TBD
