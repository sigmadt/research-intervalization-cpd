# Harrell-Davis quantile estimator
hd <- function(data,
               q,
               clean_na = TRUE,
               SEED = 17) {
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

# shift function based on HD
shift_func <- function(x, y) {
  m <- matrix(0, 9, 4)
  
  for (d in 1:9) {
    q <- d / 10
    m[d, 1] <- q
    m[d, 2] <- hd(x, q)
    m[d, 3] <- hd(y, q)
    m[d, 4] <- m[d, 2] - m[d, 3]
  }
  
  res <- data.frame(m)
  names(res) <- c('quantile',
                  'group1', 'group2',
                  'emp_diff')
  res
}


add_abs_change_rate <- function(df) {
  rel <- c(1)
  for (i in 2:nrow(df)) {
    d <- abs(df[i, 2] - df[i - 1, 2]) / abs(df[i - 1, 2]) * 100
    rel <- c(rel, d)
  }
  
  df['rel'] <- rel
  df
}

add_growth_rate <- function(df) {
  rel <- c(0)
  for (i in 2:nrow(df)) {
    d <- (df[i, 2] - df[i - 1, 2]) / df[i - 1, 2] * 100
    rel <- c(rel, d)
  }
  
  df['rel'] <- rel
  df
}

# prettify change interval
ci_prettify <- function(ci) {
  paste('(', as.character(ci[1]), ';', as.character(tail(ci, 1)), ')')
}

# prettify long floats for df
df_prettify <- function(df, r=2) {
  data.frame(lapply(df, function(y) if(is.numeric(y)) round(y, r) else y)) 
}
