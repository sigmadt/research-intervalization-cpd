library("ggplot2")
library("cowplot")
library("repr")
theme_set(theme_bw())

# Harrell-Davis quantile estimator
hd <- function(data, q, clean_na=TRUE, SEED=17) {
  set.seed(SEED)
  if (clean_na) {
    data <- data[!is.na(data)]
  }
  
  n <- length(data)
  a <- q * (n + 1)
  b <- (1 - q) * (n + 1)
  vec <- seq(along = data)
  weights <- pbeta(vec / n, a, b) - pbeta((vec - 1) / n, a, b)
  
  sorted_data <- sort(data)
  hd <- sum(weights * sorted_data)
  hd
}

# Shift function
shift_func <- function(x, y) {
  m <- matrix(0,9,4)
  
  for (d in 1:9) {
    q <- d/10
    m[d,1] <- q
    m[d,2] <- hd(x, q)
    m[d,3] <- hd(y, q)
    m[d,4] <- m[d,2] - m[d,3]
  }
  
  res <- data.frame(m)
  names(res) <- c('quantile',
                  'group1','group2',
                  'emp_diff')
  res
}

# Plot
clrs <- c('right_left'   = "#001b58",
          'center_left'  = "#f42088",
          'right_center' = "#6b9080"
)

plot_diff <- function(data) {
  p <- ggplot(data, aes_string(x='quantile')) +
    geom_line(aes(y=right_left,   color='right_left'),    linetype="solid", size=1) +
    geom_line(aes(y=center_left,  color='center_left'),   linetype="solid", size=1) +
    geom_line(aes(y=right_center, color='right_center'),  linetype="solid", size=1) +
    ylab("quantile difference") +
    labs(color="Legend") +
    scale_color_manual(values=clrs)
  p
}

# Shift function for various ranges
pair_shift <- function(df, from=1, to=300, offset=100) {
  m <- matrix(0,9,7)
  
  l <- df[from:(from+offset-1),1]
  c <- df[(to-2*offset+1):(to-offset),1]
  r <- df[(to-offset+1):to,1]
  for (d in 1:9) {
    q <- d/10
    m[d,1] <- q
    m[d,2] <- hd(l, q)
    m[d,3] <- hd(c, q)
    m[d,4] <- hd(r, q)
    # right - left
    m[d,5] <- m[d,4] - m[d,2]
    # center - left
    m[d,6] <- m[d,3] - m[d,2]
    # right - center
    m[d,7] <- m[d,4] - m[d,3]
  }
  
  res <- data.frame(m)
  names(res) <- c(
    'quantile',
    'left_dist','center_dist', 'right_dist',
    'right_left','center_left', 'right_center'
  )
  res
}

plot_shift <- function(data) {
  p <- ggplot(data, aes_string(x='quantile')) +
    geom_line(aes(y=emp_diff), linetype="solid", color='#c7751e', size=0.8) +
    ylab("difference") +
    ylim(0, NA)
  p
}

non_st_rnd <- function(n, p_1, p_2) {
  ifelse(runif(n) > seq(0, 1, length.out=n), p_1, p_2)
}

N <- 100


gen_data <- function(p=0.2, SEED=17) {
  set.seed(SEED)
  # 1. Norm
  s   <- 1
  m_1 <- 5
  m_2 <- 17
  
  dn_1 <- data.frame(v=rnorm(N, mean=m_1, sd=s))
  
  mixt <- rnorm(N, mean=non_st_rnd(N, m_1, m_2), sd=s)
  dn_2 <- data.frame(v = mixt)
  dn_3 <- data.frame(v=rnorm(N, mean=m_2, sd=s))
  
  df_norm <- rbind(dn_1, dn_2, dn_3)
  
  # 2. Horizontal line + noise
  dh_1 <- data.frame(v=rep(c(30), 100) + runif(100, -1, 1))
  dh_3 <- data.frame(v=rep(c(70), 100) + runif(100, -1, 1))
  dh_2 <- data.frame(v=sample(c(
    sample(dh_1$v, N * p, replace=TRUE), 
    sample(dh_3$v, N * (1 - p), replace=TRUE))
  )) 
  
  df_hor_line <- rbind(dh_1, dh_2, dh_3)
  
  # 3. Trending line + noise
  dl_1 <- data.frame(v=seq(1,100) + rnorm(100, 0, 1))
  dl_3 <- data.frame(v=seq(100,1) + rnorm(100, 0, 1))
  dl_2 <- data.frame(v=sample(c(
    sample(dl_1$v, N * p, replace=TRUE), 
    sample(dl_3$v, N * (1 - p), replace=TRUE))
  )) 
  
  df_trend_line <- rbind(dl_1, dl_2, dl_3)
  
  df <- rbind(df_norm, df_hor_line, df_trend_line)
  
  df
}

df <- gen_data()
ggplot(df[1:300, ,drop=FALSE], aes(x=seq(1,300),y=v)) +
  geom_point()

# 1. DF norm
df_norm <- df[1:300, ,drop=FALSE]

cp_norm <- 100


plot_shift_and_dist <- function(df, df_left, df_right, cpt_pos, step) {
  ps    <- plot_shift(shift_func(df_right, df_left))
  vert_g <- ggplot(df, aes(x=seq(1,300),y=v)) +
    geom_point() + 
    geom_vline(xintercept=cpt_pos, color = "blue",  size=0.8) +
    geom_vline(xintercept=cpt_pos+step, color = "red",  linetype="dashed", size=0.8)
  
  pg <- plot_grid(ps, vert_g)
  pg
}

partition_shift <- function(df, left, right, cpt_pos, step=50) {
  options(repr.plot.width=20, repr.plot.height=10)
  cp_ind <- left+cpt_pos-1
  df_left <- df[left:cp_ind, 1]
  n_steps <- (right - cp_ind) / step - 1
  
  plots <- list()
  for (ss in 1:n_steps) {
    curr_step <- ss * step
    mid  <- cp_ind + curr_step
    pg   <- plot_shift_and_dist(df, df_left, df[mid:right, 1], cpt_pos, curr_step)
    
    plots[[ss]] <- pg
  }

  plot_grid(plotlist=plots, nrow=n_steps)

}

p_step_50 <- partition_shift(df_norm, 1, 300, cp_norm, 50)

# p_step_50
# 2. Info about differences if quantiles

plot_shift_with_params <- function(data) {
  mean_diff <- mean(data$emp_diff)
  abs_dev   <- abs(max(data$emp_diff) - min(data$emp_diff)) 
  
  p <- ggplot(data, aes_string(x='quantile')) +
    geom_line(aes(y=emp_diff), linetype="solid",  size=0.8) +
    geom_hline(yintercept=mean_diff, color="brown", size=0.4) +
    geom_hline(yintercept=abs_dev, color="purple", size=0.5) +
    ylab("difference") +
    ylim(0, NA)
  p
}

plot_shift_param_and_dist <- function(df, df_left, df_right, cpt_pos, step) {
  ps    <- plot_shift_with_params(shift_func(df_right, df_left))
  vert_g <- ggplot(df, aes(x=seq(1,300),y=v)) +
    geom_point() + 
    geom_vline(xintercept=cpt_pos, color = "blue",  size=0.8) +
    geom_vline(xintercept=cpt_pos+step, color = "red",  linetype="dashed", size=0.8)
  
  pg <- plot_grid(ps, vert_g)
  pg
}

partition_shift_param <- function(df, left, right, cpt_pos, step=50) {
  options(repr.plot.width=20, repr.plot.height=10)
  cp_ind <- left+cpt_pos-1
  df_left <- df[left:cp_ind, 1]
  n_steps <- (right - cp_ind) / step - 1
  
  plots <- list()
  for (ss in 1:n_steps) {
    curr_step <- ss * step
    mid  <- cp_ind + curr_step
    pg   <- plot_shift_param_and_dist(df, df_left, df[mid:right, 1], cpt_pos, curr_step)
    
    plots[[ss]] <- pg
  }
  
  plot_grid(plotlist=plots, nrow=n_steps)
  
}

# 2.1 Mean diff and (max - min)
p_50_mean_dev <- partition_shift_param(df_norm, 1, 300, cp_norm, 50)
p_50_mean_dev


# Algorithm

find_right_cp <- function(left_cp_ind, df, step=25) {
  res <- list()
  n <- nrow(df)
  n_steps <- (n - left_cp_ind) / step - 1
  
  cost <- Inf
  for (iter in 1:n_steps) {
    mid  <- iter*step + left_cp_ind
    sf   <- shift_func(df[mid:n, 1], df[1:left_cp_ind, 1])
    prev <- cost
    cost <- abs(max(sf$emp_diff) - min(sf$emp_diff)) 
    
    if (iter == 1) {
      prev <- cost
    }
    
    res[[iter]] <- abs(cost - prev) / prev
  }
  
  res
}


plot_cf1_right <- function(step=25) {
  res <- data.frame()


  l_1 <- find_right_cp(100, df_norm, 50)
  df_1 <- data.frame(v=matrix(unlist(l_1), nrow=length(l_1), byrow=TRUE))
  p_1 <- ggplot(df_1, aes(x=seq(1, 3),y=v)) +
      xlab("iter") +
      ylab("d_cost") +
      ylim(0, 10) +
      geom_line()


  l_2 <- find_right_cp(100, df_norm, 25)
  df_2 <- data.frame(v=matrix(unlist(l_2), nrow=length(l_2), byrow=TRUE))
  p_2 <- ggplot(df_2, aes(x=seq(1, 7),y=v)) +
    xlab("iter") +
    ylab("d_cost") +
    ylim(0, 10) +
    geom_line()
  
  l_3 <- find_right_cp(100, df_norm, 10)
  df_3 <- data.frame(v=matrix(unlist(l_3), nrow=length(l_3), byrow=TRUE))
  p_3 <- ggplot(df_3, aes(x=seq(1, 19),y=v)) +
    xlab("iter") +
    ylab("d_cost") +
    ylim(0, 10) +
    geom_line()
  
  p <- plot_grid(p_1, p_2, p_3, 
                 ncol=3,
                 labels=c("step=50", "step=25", "step=10")
                 )
  p
}

plot_cf1_right_few <- function(step=25) {
  res <- data.frame()
  
  
  l_1 <- find_right_cp_few(100, df_norm, 50)
  df_1 <- data.frame(v=matrix(unlist(l_1), nrow=length(l_1), byrow=TRUE))
  p_1 <- ggplot(df_1, aes(x=seq(1, 3),y=v)) +
    xlab("iter") +
    ylab("d_cost") +
    ylim(0, 10) +
    xlim(2, 3) +
    geom_line()
  
  
  l_2 <- find_right_cp_few(100, df_norm, 25)
  df_2 <- data.frame(v=matrix(unlist(l_2), nrow=length(l_2), byrow=TRUE))
  p_2 <- ggplot(df_2, aes(x=seq(1, 7),y=v)) +
    xlab("iter") +
    ylab("d_cost") +
    ylim(0, 10) +
    xlim(2, 7) +
    geom_line()
  
  l_3 <- find_right_cp_few(100, df_norm, 10)
  df_3 <- data.frame(v=matrix(unlist(l_3), nrow=length(l_3), byrow=TRUE))
  p_3 <- ggplot(df_3, aes(x=seq(1, 19),y=v)) +
    xlab("iter") +
    ylab("d_cost") +
    ylim(0, 10) +
    xlim(2, 20) +
    geom_line()
  
  p <- plot_grid(p_1, p_2, p_3, 
                 ncol=3,
                 labels=c("step=50", "step=25", "step=10")
  )
  p
}

plot_cf1_right()

find_right_cp(100, df_norm, 10)
find_right_cp(100, df_norm, 25)
find_right_cp(100, df_norm, 10)

l_3 <- find_right_cp(100, df_norm, 10)
df_3 <- data.frame(v=matrix(unlist(l_3), nrow=length(l_3), byrow=TRUE))



find_right_cp_few <- function(left_cp_ind, df, step=25) {
  res <- list()
  n <- nrow(df)
  n_steps <- (n - left_cp_ind) / step - 1
  
  cost <- Inf
  for (iter in 1:n_steps) {
    mid  <- iter*step + left_cp_ind
    sf   <- shift_func(df[mid:n, 1], df[1:left_cp_ind, 1])
    prev <- cost
    cost <- abs(max(sf[1:3,]$emp_diff) - min(sf[1:3,]$emp_diff)) 
    
    if (iter == 1) {
      prev <- cost
    }
    res[[iter]] <- abs(cost - prev) / prev
  }
  
  res
}


find_right_cp_few(100, df_norm, 10)



# Diff in diff 

find_right_diff_in_diff <- function(left_cp_ind, df, step=25) {
  res <- list()
  n <- nrow(df)
  n_steps <- (n - left_cp_ind) / step - 1
  
  cost <- Inf
  
  prev_sf <- NULL
  for (iter in 1:n_steps) {
    mid  <- iter*step + left_cp_ind
    sf   <- shift_func(df[mid:n, 1], df[1:left_cp_ind, 1])
    
    if (is.null(prev_sf)) {
      prev_sf <- sf
      next
    }
    cost <- sum((sf$emp_diff - prev_sf$emp_diff)^2) 
    
    res[[iter]] <- cost
  }
  
  res[[1]] <- 10
  
  res
}

find_right_diff_in_diff(100, df_norm)


plot_cf3_right <- function(step=25) {
  res <- data.frame()
  
  
  l_1 <- find_right_diff_in_diff(100, df_norm, 50)
  df_1 <- data.frame(v=matrix(unlist(l_1), nrow=length(l_1), byrow=TRUE))
  p_1 <- ggplot(df_1, aes(x=seq(1, 3),y=v)) +
    xlab("iter") +
    ylab("diff_in_diff") +
    ylim(0, NA) +
    xlim(2, 3) +
    geom_line()
  
  
  l_2 <- find_right_diff_in_diff(100, df_norm, 25)
  df_2 <- data.frame(v=matrix(unlist(l_2), nrow=length(l_2), byrow=TRUE))
  p_2 <- ggplot(df_2, aes(x=seq(1, 7),y=v)) +
    xlab("iter") +
    ylab("diff_in_diff") +
    ylim(0, NA) +
    xlim(2, 7) +
    geom_line()
  
  l_3 <- find_right_diff_in_diff(100, df_norm, 10)
  df_3 <- data.frame(v=matrix(unlist(l_3), nrow=length(l_3), byrow=TRUE))
  p_3 <- ggplot(df_3, aes(x=seq(1, 19),y=v)) +
    xlab("iter") +
    ylab("diff_in_diff") +
    ylim(0, NA) +
    xlim(2, 20) +
    geom_line()
  
  p <- plot_grid(p_1, p_2, p_3, 
                 ncol=3,
                 labels=c("step=50", "step=25", "step=10")
  )
  p
}

plot_cf3_right()

l_1 <- find_right_diff_in_diff(100, df_norm, 50)
df_1 <- data.frame(v=matrix(unlist(l_1), nrow=length(l_1), byrow=TRUE))
