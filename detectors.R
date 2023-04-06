library("functional")

source("cf.R")
source("utils.R")
source("graph.R")
source("gen.R")


# time series -> change point -> change interval
detector_cf <- function(cf_name, left_cp=TRUE, right_cp=FALSE, center_cp=FALSE) {
  det_name <- function(ts, cp, name) {
    res <- NULL
    if (left_cp) {
      res <- find_right_bound(cp, ts, cf[[name]])
    } else if (right_cp) {
      res <- find_left_bound(cp, ts, cf[[name]])
    }
    return(res)
  }
  Curry(det_name, name = cf_name)
}


# Bounds for detectors
# 1. Right
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


# 2. Left
find_left_bound <- function(right_cp_ind, df, cost_func, step = 1) {
  n <- nrow(df)
  n_steps <- right_cp_ind / step - 1
  
  it <- c() 
  cf <- c()
  
  for (iter in 1:n_steps) {
    mid  <- right_cp_ind - iter * step
    cost <- cost_func(df[right_cp_ind:n, 1], df[1:mid, 1])
    cf <- c(cf, cost)
    it <- c(it, mid)
  }
  
  data.frame(it, cf)
}


get_top_cf_changes <- function(det, ts, cp, top = 10) {
  aa <- add_abs_change_rate(det(ts, cp))
  aa[order(aa$rel), ][1:top, c(1, 3)]
}

show_cf_changes_less_than <- function(det, ts, cp, th = 0.01) {
  aa <- add_abs_change_rate(det(ts, cp))
  # filter(aa, rel <th)
  fa <- aa[aa$rel < th,]
  sa <- fa[order(fa$rel),]
  head(sa, 3)
}

show_all_cf_filtered <-
  function(cf_list,
           ts,
           cp,
           th = 0.01,
           left_cp = TRUE,
           right_cp = FALSE,
           center_cp = FALSE) {
    res <- NULL
    
    for (cf_name in names(cf_list)) {
      curr <-
        show_cf_changes_less_than(detector_cf(cf_name, left_cp, right_cp, center_cp),
                                  ts,
                                  cp,
                                  th)
      if (nrow(curr) < 3) {
        next
      }
      print(cf_name)
      if (is.null(res)) {
        res <- curr
      } else {
        res <- cbind(res, curr)
      }
    }
    
    res
  }

show_top_all_cf <- function(cf_list, ts, cp, top = 10, left_cp=TRUE, right_cp=FALSE) {
  res <- NULL
  
  for (cf_name in names(cf_list)) {
    print(cf_name)
    if (is.null(res)) {
      res <- get_top_cf_changes(detector_cf(cf_name, left_cp=left_cp, right_cp=right_cp), ts, cp, top)
    } else {
      res <-
        cbind(res, get_top_cf_changes(detector_cf(cf_name, left_cp=left_cp, right_cp=right_cp), ts, cp, top))
    }
  }
  
  res
}

# Get answer
get_change_interval <- function(cf_name, ts, cp, 
                             left_cp = TRUE,
                             right_cp = FALSE,
                             center_cp = FALSE ) {
    ans_cp <- get_top_cf_changes(detector_cf(cf_name, left_cp, right_cp, center_cp), ts, cp, 1)[1, 1]
    res <- c(0, 0)
    if (left_cp) {
      res <- c(cp:(cp + ans_cp))
    } else if (right_cp) {
      res <- c(ans_cp:cp)
    }
    res
}


