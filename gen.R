# Reference dataset
gen_data <- function(SEED = 17) {
  df_norm <- gen_normal(SEED = SEED)
  df_unif <- gen_uniform(SEED = SEED)
  
  df <- rbind(df_norm, df_unif)
  
  data.frame(df)
}

non_st_rnd <- function(n, p_1, p_2) {
  ifelse(runif(n) > seq(0, 1, length.out = n), p_1, p_2)
}


gen_normal <-
  function(m_1 = 5,
           s_1 = 1,
           m_2 = 17,
           s_2 = 1,
           N = 100,
           SEED = 17) {
    set.seed(SEED)
    
    d_1 <- data.frame(v = rnorm(N, mean = m_1, sd = s_1))
    
    mixt <- rnorm(N, mean = non_st_rnd(N, m_1, m_2), sd = s_1)
    d_2 <- data.frame(v = mixt)
    
    d_3 <- data.frame(v = rnorm(N, mean = m_2, sd = s_2))
    
    df <- rbind(d_1, d_2, d_3)
    
    df
  }

gen_uniform <-
  function(l_1 = 0,
           r_1 = 10,
           l_2 = 0,
           r_2 = 20,
           N = 100,
           SEED = 17) {
    set.seed(SEED)
    
    d_1 <- data.frame(v = runif(N, min = l_1, max = r_1))
    
    mixt <- runif(N, min = l_1, max = non_st_rnd(N, r_1, r_2))
    d_2 <- data.frame(v = mixt)
    
    d_3 <- data.frame(v = runif(N, min = l_2, max = r_2))
    
    df <- rbind(d_1, d_2, d_3)
    
    df
  }

get_slice_from_df <- function(df, from = 1, to = 10) {
  data.frame(v = df[from:to, 1])
}

# Location for distributions
dist_loc <- list(norm = c("from" = 1, "to" = 300),
                 unif = c("from" = 301, "to" = 600))
