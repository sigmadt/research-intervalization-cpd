library("functional")

source("cf.R")
source("utils.R")
source("graph.R")
source("gen.R")


# time series -> change point -> change interval
det_cf_functor <- function(cf_name) {
  det_name <- function(ts, cp, name) {
    res <- find_right_bound(cp, ts, cf[[name]])
    res
  }
  Curry(det_name, name = cf_name)
}

det_basic <- function(ts, cp) {
  res <- find_right_bound(cp, ts, cf[['all_q']])
  res
}

det_name <- function(ts, cp, name) {
  res <- find_right_bound(cp, ts, cf[[name]])
  res
}

det_wt <- function(ts, cp) {
  res <- find_right_bound(cp, ts, cf[['wilcox']])
  res
}

# Bounds for detectors
find_right_bound <- function(left_cp_ind, df, cost_func, step = 1) {
  n <- nrow(df)
  n_steps <- (n - left_cp_ind) / step - 1
  
  it <- 1:n_steps
  cf <- c()
  
  for (iter in 1:n_steps) {
    mid  <- iter * step + left_cp_ind
    cost <- cost_func(df[mid:n, 1], df[1:left_cp_ind, 1])
    cf <- c(cf, cost)
  }
  
  data.frame(it, cf)
}


show_top_cf_changes <- function(det, ts, cp, top = 10) {
  aa <- add_abs_change_rate(det(ts, cp))
  aa[order(aa$rel), ][1:top, c(1, 3)]
}

show_top_all_cf <- function(cf_list, ts, cp, top = 10) {
  res <- NULL
  
  for (cf_name in names(cf_list)) {
    print(cf_name)
    if (is.null(res)) {
      res <- show_top_cf_changes(det_cf_functor(cf_name), ts, cp, top)
    } else {
      res <-
        cbind(res, show_top_cf_changes(det_cf_functor(cf_name), ts, cp, top))
    }
  }
  
  res
}
