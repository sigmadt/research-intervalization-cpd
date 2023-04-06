source("penalty.R")
source("cf.R")
source("utils.R")
source("graph.R")
source("gen.R")
source("detectors.R") 

# [estimated interval] -> true interval -> best estimated interval

get_ci_penalty <- function(ci_true, ci_est) {
  if (length(ci_est) < 2) {
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


get_cf_leaderboard_by_name <- function(cp, dist_name,
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

get_cf_leaderboard <- function(cp, ts, ci_true,
                               left_cp = TRUE,
                               right_cp = FALSE,
                               center_cp = FALSE) {
  res <- NULL
  for (c in names(cf)) {
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

get_cf_winners_by_dist <- function(pen_name="hausdorff", left_cp=TRUE, right_cp=FALSE, center_CP = TRUE) {
  res <- NULL
  
  
  for (d in names(dist_loc)) {
    print(d)
    lb <- get_cf_leaderboard_by_name(ci_true_loc[[d]][1], d, left_cp, right_cp, center_cp)
    # decreasing penalty function
    if (!is.null(desc_pen[[pen_name]])) {
      curr_winner <- lb[which.min(lb[[pen_name]]), c("cf", "ci_true", "ci_est", pen_name)]
    } else {
      curr_winner <- lb[which.max(lb[[pen_name]]), c("cf", "ci_true", "ci_est", pen_name)]
    }
    if (is.null(res)) {
      res <- data.frame(dist=d, curr_winner)
    } else {
      res <- rbind(res, data.frame(dist=d, curr_winner))
    }
  }
  
  res
}
