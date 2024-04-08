
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")
source("code/function.R")


# data --------------------------------------------------------------------

## format data
df_move0 <- readRDS("data_formatted/data_move.rds") %>% 
  drop_na(section0, section1)

df_den <- readRDS("data_formatted/data_density.rds")

df_h <- readRDS("data_formatted/data_habitat.rds")

## combine movement, density, and habitat
df_move <- df_move0 %>% 
  left_join(df_den,
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_h,
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  mutate(intv = as.numeric(datetime1 - datetime0))


# run jags ----------------------------------------------------------------

usp <- c("creek_chub",
         "green_sunfish",
         "redbreast_sunfish",
         "bluehead_chub") %>% 
  sort()

list_est <- foreach(x = usp) %do% {
  
  df_i <- filter(df_move, species == x)
  
  ## data for jags
  list_jags <- with(df_i,
                    list(Y1 = section1 * 10 - 5,
                         Y0 = section0 * 10 - 5, 
                         Nsample = nrow(df_i),
                         Intv = intv))
  
  ## select predictors
  X <- df_i %>% 
    dplyr::select(length0, 
                  velocity_mean, 
                  density_creek_chub) %>% 
    mutate(across(.cols = c(length0, velocity_mean, starts_with("density")),
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
  parms <- c("b")
  
  ## model files
  m <- runjags::read.jagsfile("code/model_move.R")
  
  ## run model
  post <- runjags::run.jags(model = m$model,
                            data = list_jags,
                            monitor = parms,
                            burnin = 1000,
                            sample = 1000,
                            thin = 5,
                            n.chains = n_chain,
                            inits = inits,
                            method = "parallel",
                            module = "glm")
  
  MCMCvis::MCMCsummary(post$mcmc) %>% 
    as_tibble(rownames = "parms") %>% 
    mutate(y = x,
           var = colnames(X)) %>% 
    relocate(var)
}
