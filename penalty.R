library("pracma")
library("MLmetrics")
library("comprehenr")

# intersection / union
iou_pen <- function(ci_true, ci_est) {
    if (ci_true[-1] < ci_est[1]) {
      return(0)
    } 
    if (ci_est[-1] < ci_true[1]) {
      return(0)
    }
    inter <- min(ci_true[-1], ci_est[-1]) - max(ci_true[1], ci_est[1])
    union <- max(ci_true[-1], ci_est[-1]) - min(ci_true[1], ci_est[1])
    
    inter / union
}

# hausdorff
hausdorff_pen <- function(ci_true, ci_est) {
  hausdorff_dist(ci_true, ci_est)
}


# |{ct_i | min_j{|ce_j - ct_i| < 10}}| / |ci_est|
precision_pen <- function(ci_true, ci_est, th=10) {
  min_diff_ct <- to_vec(for (ct_i in ci_true) min(abs(ci_est - ct_i)))
  less_th <- to_vec(for (p in min_diff_ct) if (p < th) p)
  
  length(less_th) / length(ci_est)
} 

# |{ct_i | min_j{|ce_j - ct_i| < 10}}| / |ci_true|
recall_pen <- function(ci_true, ci_est, th=10) {
  min_diff_ct <- to_vec(for (ct_i in ci_true) min(abs(ci_est - ct_i)))
  less_th <- to_vec(for (p in min_diff_ct) if (p < th) p)
  
  length(less_th) / length(ci_true)
} 


# f1-score
f1_pen <- function(ci_true, ci_est) {
  pr <- precision_pen(ci_true, ci_est)
  rc <- recall_pen(ci_true, ci_est)
  2 * pr * rc / (pr + rc)
}

