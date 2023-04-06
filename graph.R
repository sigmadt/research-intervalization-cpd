library("ggplot2")
library("cowplot")
library("ggfittext")

source("gen.R")

# plot cost function behavior for every partition starting from cp with step
cf_build_plot <- function(res, title="cost function") {
  p_1 <- ggplot(res[1:300, , drop = FALSE], aes(x = it, y = cf)) +
    geom_point()
  p_2 <- ggplot(res[1:300, , drop = FALSE], aes(x = it, y = rel)) +
    geom_point()
  
  p_title <- ggdraw() + 
    draw_label(title, x = 0.05, hjust = 0, vjust = 1)
  p <- plot_grid(p_1, p_2,
                 ncol = 2,
                 labels = c("abs", "rel"))
  
  plot_grid(p_title, p, ncol=1, rel_heights = c(0.1, 1))
}

cf_build_plot_abs_rel <-
  function(df,
           cp,
           df_name = "data frame",
           cf_name = "cost function",
           left_cp = TRUE,
           right_cp = FALSE,
           center_cp = FALSE) {
    bound <- NULL
    if (left_cp) {
      bound <- find_right_bound(cp, df, cf[[cf_name]])
    } else if (right_cp) {
      bound <- find_left_bound(cp, df, cf[[cf_name]])
    }
    title <- paste(df_name, ' | ', cf_name)
    cf_build_plot(add_abs_change_rate(bound), title)
  }

# plot d1 + mixt + d2
build_plot_dist <- function(dist_name) {
  if (is.null(dist_loc[[dist_name]])) {
    print("Available distributions:    ")
    print("- N(5, 1)  + mixt + N(17, 1)")
    print("- U(0, 10) + mixt + U(0, 20)")
    print("- exp(0.2) + mixt + exp(5)  ")
    return("----------------------------")
  }
  g <- ggplot(get_slice_from_df(gen_data(),
                                dist_loc[[dist_name]]['from'],
                                dist_loc[[dist_name]]['to']),
              aes(x = seq(1, 300), y = v)) +
    geom_point()
  
  g
}

build_plot_dist_with_ci <- function(dist_name) {
  pd <- build_plot_dist(dist_name)
  pd +
    annotate(
      "rect",
      xmin = ci_true_loc[[dist_name]][1],
      xmax = tail(ci_true_loc[[dist_name]], 1),
      ymin = 0,
      ymax = Inf,
      alpha = 0.5,
      fill = "blue"
    )
}


# compare true and estimated change interval
build_plot_cmp_ci <- function(dist_name, ci_est) {
  pd <- build_plot_dist_with_ci(dist_name)
  pd +
    annotate(
      "rect",
      xmin = ci_est[1],
      xmax = tail(ci_est, 1),
      ymin = 0,
      ymax = Inf,
      alpha = 0.3,
      fill = "green"
    )
}

build_plot_labeled <- function(dist_name, ci_est) {
  pd <- build_plot_dist(dist_name)
  pd +
    annotate(
      "rect",
      xmin = ci_true_loc[[dist_name]][1],
      xmax = tail(ci_true_loc[[dist_name]], 1),
      ymin = 0,
      ymax = Inf,
      alpha = 0.4,
      fill = "blue"
    ) +
    annotate(
      "rect",
      xmin = ci_est[1],
      xmax = tail(ci_est, 1),
      ymin = 0,
      ymax = Inf,
      alpha = 0.3,
      fill = "red"
    ) + 
    geom_fit_text(
      data = data.frame(xmin = ci_est[1], xmax = tail(ci_est, 1), 
                        label = "estimated"),
      aes(xmin = xmin, xmax = xmax, ymin = 4, ymax = 5, label = label),
      inherit.aes = FALSE
    )
  
}


# given data
build_plot_given_data <- function(df, ci_true=c(100,200), N=300) {
  g <- ggplot(df, aes(x = seq(1, N), y = v)) +
    geom_point() +
    annotate(
      "rect",
      xmin = ci_true[1],
      xmax = tail(ci_true, 1),
      ymin = 0,
      ymax = Inf,
      alpha = 0.5,
      fill = "blue"
    )
  
  g
}

build_plot_given_data_cmp_ci <- function(df, ci_true=c(100,200), ci_est=c(150, 250)) {
  g <- build_plot_given_data(df, ci_true)
  
  g +
    annotate(
      "rect",
      xmin = ci_est[1],
      xmax = tail(ci_est, 1),
      ymin = 0,
      ymax = Inf,
      alpha = 0.3,
      fill = "green"
    )
}



