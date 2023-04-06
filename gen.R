library("EnvStats")

# Reference dataset
gen_data <- function(SEED = 17) {
  # (0) basic
  df_norm <- gen_normal(SEED = SEED)
  df_unif <- gen_uniform(SEED = SEED)
  df_exp  <- gen_exp(SEED = SEED)
  df_pareto <- gen_pareto(SEED = SEED)
  
  # (1) different sized intervals
  # 1.1 large left | large center | large right
  df_norm_lll <-
    gen_normal(
      m_1 = 10,
      m_2 = 30,
      N_left = 500,
      N_center = 500,
      N_right = 500
    )
  
  # 1.2 large left | medium center | large right
  df_norm_lml <-
    gen_normal(
      m_1 = 14,
      m_2 = 18,
      N_left = 500,
      N_center = 100,
      N_right = 500
    )
  
  # 1.3 large left | small center | large right
  df_norm_lsl <-
    gen_normal(
      m_1 = 7,
      m_2 = 900,
      N_left = 500,
      N_center = 50,
      N_right = 450
    )
  
  # 1.4 medium left | medium center | large right
  df_unif_mml <-
    gen_uniform(
      l_1 = 5, 
      r_1 = 7, 
      l_2 = 2, 
      r_2 = 10,
      N_left = 200,
      N_center = 150,
      N_right = 550
      )
  
  # 1.5 large left | medium center | small right
  df_unif_lms <-
    gen_uniform(
      l_1 = 0, 
      r_1 = 10, 
      l_2 = 10, 
      r_2 = 20,
      N_left = 500,
      N_center = 250,
      N_right = 50
    )
  
  # 1.6 large left | small center | small right
  df_exp_lss  <- gen_exp(
    lambda_1 = 0.5,
    lambda_2 = 2,
    N_left = 400,
    N_center = 40,
    N_right = 60
  )
  
  # 1.7 medium left | large center | medium right
  df_exp_mlm <- gen_exp(
    lambda_1 = 0.1,
    lambda_2 = 3,
    N_left = 100,
    N_center = 600,
    N_right = 200
  )  
  
  # 1.8 small left | large center | small right
  df_pareto_sls <- gen_pareto(
    shape_1 = 1,
    loc_1 = 2,
    shape_2 = 3,
    loc_2 = 60,
    N_left = 50,
    N_center = 500,
    N_right = 50
  )
  
  # 2. corner cases
  # 2.1 medium left | ultra small center | medium right
  df_norm_mum <-  gen_normal(
    m_1 = 10,
    s_1 = 2,
    m_2 = 25,
    s_2 = 0.5,
    N_left = 140,
    N_center = 10,
    N_right = 150
  )
  
  # 2.2 small left | ultra small center | large right
  df_unif_sul <- gen_uniform(
    l_1 = 3, 
    r_1 = 5, 
    l_2 = 4, 
    r_2 = 10,
    N_left = 50,
    N_center = 5,
    N_right = 445
  )
  
  # concat dfs 14
  df <- rbind(df_norm,
              df_unif,
              df_exp,
              df_pareto,
              df_norm_lll,
              df_norm_lml,
              df_norm_lsl,
              df_unif_mml,
              df_unif_lms,
              df_exp_lss,
              df_exp_mlm,
              df_pareto_sls,
              df_norm_mum,
              df_unif_sul)
  
  data.frame(df)
}

non_st_rnd <- function(n, p_1, p_2, SEED=17) {
  set.seed(SEED)
  ifelse(runif(n) > seq(0, 1, length.out = n), p_1, p_2)
}

gen_normal <-
  function(m_1 = 5,
           s_1 = 1,
           m_2 = 17,
           s_2 = 1,
           N_left = 100,
           N_center = 100,
           N_right = 100,
           SEED = 17) {
    set.seed(SEED)
    
    d_1 <- data.frame(v = rnorm(N_left, mean = m_1, sd = s_1))
    
    mixt <- rnorm(N_center, mean = non_st_rnd(N_center, m_1, m_2, SEED), sd = non_st_rnd(N_center, s_1, s_2, SEED))
    d_2 <- data.frame(v = mixt)
    
    d_3 <- data.frame(v = rnorm(N_right, mean = m_2, sd = s_2))
    
    df <- rbind(d_1, d_2, d_3)
    
    df
  }

gen_uniform <-
  function(l_1 = 0,
           r_1 = 10,
           l_2 = 5,
           r_2 = 20,
           N_left = 100,
           N_center = 100,
           N_right = 100,
           SEED = 17) {
    set.seed(SEED)
    
    d_1 <- data.frame(v = runif(N_left, min = l_1, max = r_1))
    
    mixt <- runif(N_center, min = non_st_rnd(N_center, l_1, l_2), max = non_st_rnd(N_center, r_1, r_2))
    d_2 <- data.frame(v = mixt)
    
    d_3 <- data.frame(v = runif(N_right, min = l_2, max = r_2))
    
    df <- rbind(d_1, d_2, d_3)
    
    df
  }

gen_exp <-
  function(lambda_1 = 0.2,
           lambda_2 = 5,
           N_left = 100,
           N_center = 100,
           N_right = 100,
           SEED = 17) {
    set.seed(SEED)
    
    d_1 <- data.frame(v = rexp(N_left, rate=lambda_1))
    
    mixt <- rexp(N_center, rate = non_st_rnd(N_center, lambda_1, lambda_2))
    d_2 <- data.frame(v = mixt)
    
    d_3 <- data.frame(v = rexp(N_right, rate=lambda_2))
    
    df <- rbind(d_1, d_2, d_3)
    
    df
  }

gen_pareto <-
  function(loc_1 = 1,
           shape_1 = 0.5,
           loc_2 = 1,
           shape_2 = 2,
           N_left = 100,
           N_center = 100,
           N_right = 100,
           SEED = 17) {
    set.seed(SEED)
    
    d_1 <- data.frame(v = rpareto(N_left, location = loc_1, shape=shape_1))
    
    mixt <- rpareto(N_center, location = non_st_rnd(N_center, loc_1, loc_2), shape = non_st_rnd(N_center, shape_1, shape_2))
    d_2 <- data.frame(v = mixt)
    
    d_3 <- data.frame(v = rpareto(N_right, location = loc_2, shape=shape_2))
    
    df <- rbind(d_1, d_2, d_3)
    
    df
  }

get_slice_from_df <- function(df, from = 1, to = 10) {
  data.frame(v = df[from:to, 1])
}

get_df_by_name <- function(df, name="norm") {
  from <- dist_loc[[name]]["from"]
  to <- dist_loc[[name]]["to"]
  data.frame(v = df[from:to, 1])
}

# Location for distributions
dist_loc <- list(norm = c("from" = 1, "to" = 300),
                 unif = c("from" = 301, "to" = 600),
                 exp = c("from" = 601, "to" = 900),
                 pareto = c("from" = 901, "to" = 1200),
                 norm_lll = c("from" = 1201, "to" = 2700),
                 norm_lml = c("from" = 2701, "to" = 3800),
                 norm_lsl = c("from" = 3801, "to" = 4800),
                 unif_mml = c("from" = 4801, "to" = 5700),
                 unif_lms = c("from" = 5701, "to" = 6500),
                 exp_lss = c("from" = 6501, "to" = 7000),
                 exp_mlm = c("from" = 7001, "to" = 7900),
                 pareto_sls = c("from" = 7901, "to" = 8500),
                 norm_mum = c("from" = 8501, "to" = 8800),
                 unif_sul = c("from" = 8801, "to" = 9300)
                 )

ci_true_loc <- list(
  norm = c(100:200),
  unif = c(100:200),
  exp = c(100:200),
  pareto = c(100:200),
  norm_lll = c(500:1000),
  norm_lml = c(500:600),
  norm_lsl = c(500:550), 
  unif_mml = c(200:350),
  unif_lms = c(500:750),
  exp_lss = c(400:440),
  exp_mlm = c(100:700),
  pareto_sls = c(50:550),
  norm_mum = c(140:150),
  unif_sul = c(50:55)
)
