library("ggplot2")
library("cowplot")
library("repr")

source("utils.R")


theme_set(theme_bw())

# SIGNATURE: vector -> vector -> int

# 1. HD-quantile based
# 1.1 Median
cf_median <- function(left, right) {
  sf <- shift_func(left, right)
  sf[5, 4]
}

# 1.2 RMSE for 50-90% quantile group
cf_50_90_q <- function(left, right) {
  sf <- shift_func(left, right)
  cost <- 0
  
  for (q in 5:9) {
    cost <- cost + sf[q, 4] ^ 2
  }
  
  sqrt(cost)
}

# 1.3 MAE for 30%, 50%, 70% quantile
cf_30_50_70 <- function(left, right) {
  sf <- shift_func(left, right)
  cost <- abs(sf[3, 4]) + abs(sf[5, 4]) + abs(sf[7, 4])
  
  cost
}

# 1.4 All quantile abs difference sum
cf_all_q <- function(left, right) {
  sf <- shift_func(left, right)
  cost <- 0
  for (q in 1:9) {
    cost <- cost + abs(sf[q, 4])
  }
  
  cost
}

# 2. Stat tests
# 2.1 Wilcox Test for comparing two samples
cf_wt <- function(left, right) {
  wilcox.test(right, left)$statistic
}


cf <- list(
  median = cf_median,
  near_median = cf_30_50_70,
  higher_q = cf_50_90_q,
  all_q = cf_all_q,
  wilcox = cf_wt
)
