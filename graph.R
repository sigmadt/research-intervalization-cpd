library("ggplot2")
library("cowplot")

source("gen.R")
# plot cost function behavior for every partition starting from cp with step
cf_build_plot <- function(res) {
  p_1 <- ggplot(res[1:300, , drop = FALSE], aes(x = it, y = cf)) +
    geom_point()
  p_2 <- ggplot(res[1:300, , drop = FALSE], aes(x = it, y = rel)) +
    geom_point()
  
  p <- plot_grid(p_1, p_2,
                 ncol = 2,
                 labels = c("abs", "rel"))
  p
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
