source("penalty.R")
source("cf.R")
source("utils.R")
source("graph.R")
source("gen.R")
source("detectors.R") 

# [estimated interval] -> true interval -> best estimated interval

get_ci_penalty <- function(ci_true, ci_est) {
  if (length(ci_est) <= 2) {
    print("interval should have more than 2 points")
    return("")
  }
  
  res <- data.frame(ci_prettify(ci_true), ci_prettify(ci_est))
  
  
  for (p in pen) {
    if (is.null(res)) {
      res <- data.frame(p(ci_est, ci_true))
    } else {
      res <- cbind(res, data.frame(p(ci_est, ci_true)))
    }
  }
  names(res) <- c('ci_true', 'ci_est', names(pen))
  
  res
}


get_cf_leaderboard <- function(cp, dist_name,
                               left_cp = TRUE,
                               right_cp = FALSE,
                               center_cp = FALSE) {
  ts <- get_slice_from_df(gen_data(),
                          dist_loc[[dist_name]]['from'],
                          dist_loc[[dist_name]]['to'])
  res <- NULL
  for (c in names(cf)) {
    ci_true <- ci_true_loc[[dist_name]]
    ci_est <- get_change_interval(c, ts, cp, left_cp, right_cp, center_cp)
    curr <- get_ci_penalty(ci_true, ci_est)
    if (is.null(res)) {
      res <- curr
    } else {
      res <- rbind(res, curr)
    }
  }
  
  res <- cbind(data.frame(cf=names(cf)), res)
  df_prettify(res)
}




