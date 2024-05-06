#' Author: Ashley LaRoque, Akira Terui
#' Description:
#' This script runs modified dispersal-observation model for predicting movement with density and body size

# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")
source("code/function.R")


# data --------------------------------------------------------------------

## format data
df_move0 <- readRDS("data_formatted/data_move.rds") %>% 
  drop_na(section0)

df_output <- readRDS("data_formatted/cjs_output.rds") %>% # comes from 'run_model_cjs_move'
  dplyr::select(para, "50%", y) %>% # uses median estimate
  filter(para == "mean.p") %>% 
  rename("medi" = "50%") %>% 
  mutate(medi = as.numeric(medi))

df_den <- readRDS("data_formatted/data_density.rds") %>% 
   left_join(df_output,
            by = c("species" = "y")) %>% 
  mutate(adj_density = (density * medi))

df_den_adj <- df_den %>% 
  pivot_wider(id_cols = c(occasion, section, area),
              names_from = species,
              values_from = c(n, adj_density))

df_h <- readRDS("data_formatted/data_habitat.rds") %>% 
  dplyr::select(-area)

## combine movement, density, and habitat
df_move <- df_move0 %>% 
  left_join(df_den_adj,
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_h,
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  mutate(intv = as.numeric(datetime1 - datetime0),
         y = ifelse(is.na(section1), 0, 1)) %>% 
  group_by(occasion0) %>% 
  mutate(intv = ifelse(is.na(intv),
                       median(intv, na.rm = TRUE),
                       intv)) %>% 
  ungroup() %>% 
  select(-c(starts_with("n_")))


# run jags ----------------------------------------------------------------

usp <- c("green_sunfish",
         "redbreast_sunfish",
         "creek_chub",
         "bluehead_chub") %>% 
  sort()

list_est <- foreach(x = usp) %do% {
  
  df_i <- filter(df_move, species == x)
  
  ## data for jags
  list_jags <- with(df_i,
                    list(Y = y,
                         X1 = section1 * 10 - 5,
                         X0 = section0 * 10 - 5, 
                         Nsample = nrow(df_i),
                         Intv = intv))
  
  ## select predictors
  X <- df_i %>% 
    dplyr::select(length0, 
                  area_ucb, 
                  adj_density_creek_chub,
                  adj_density_bluehead_chub,
                  adj_density_green_sunfish,
                  adj_density_redbreast_sunfish) %>% 
    mutate(across(.cols = c(length0, area_ucb, starts_with("adj_density")),
                  .fns = function(x) c(scale(x)))) %>% 
    model.matrix(~., data = .)
  
  list_jags$X <- X
  list_jags$K <- ncol(X)
  
  ## run.jags arguments
  ## - initial values
  n_chain <- 3
  
  inits <- replicate(n_chain,
                     list(.RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA),
                     simplify = FALSE)
  
  for (j in 1:n_chain) inits[[j]]$.RNG.seed <- 10 * j
  
  ## - parameters to be monitored
  para <- c("b")
  
  ## model files
  m <- runjags::read.jagsfile("code/model_move.R")
  
  ## run model
  post <- runjags::run.jags(model = m$model,
                            data = list_jags,
                            monitor = para,
                            burnin = 1000,
                            sample = 1000,
                            thin = 5,
                            n.chains = n_chain,
                            inits = inits,
                            method = "parallel",
                            module = "glm")
  
  MCMCvis::MCMCsummary(post$mcmc) %>% 
    as_tibble(rownames = "para") %>% 
    mutate(y = x,
           var = colnames(X)) %>% 
    relocate(var)
}

list_est[[1]]
list_est[[2]]
list_est[[3]]
list_est[[4]]

MCMCvis::MCMCplot(post$mcmc,
                  params = "b",
                  main = "MCMC Parameter Estimate",
                  xlab = "Posterior Median with CI", 
                  labels = c("intercept", "length", "area_ucb", "density_creek_chub",
                             "density_bluehead_chub", "density_green_sunfish", "density_redbreast_sunfish"),
                  col = c("black", "blue", "tan", "deeppink1" , "slateblue", "springgreen4", "firebrick2"))

