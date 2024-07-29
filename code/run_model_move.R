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

df_zeta <- readRDS("data_formatted/data_detection.rds") # comes from 'run_model_cjs_move'
df_season <- readRDS("data_formatted/data_season.rds") # comes from 'run_model_cjs_move'
df_water_pres <- readRDS("data_formatted/data_water_pressure.rds")   # comes from 'format_water_level'


df_den <- readRDS("data_formatted/data_density.rds") %>% 
  left_join(df_season, by = "occasion") %>% 
  mutate(season= case_when(season == 0 ~ "winter",
                           season == 1 ~ "summer")) %>% 
   left_join(df_zeta,
            by = c("species", "season")) %>% 
  mutate(adj_density = (density * estimate)) 

df_den_adj <- df_den %>% 
  pivot_wider(id_cols = c(occasion, section, area),
              names_from = species,
              values_from = c(n, adj_density))

df_h <- readRDS("data_formatted/data_habitat.rds") %>% 
  dplyr::select(-area)

## combine movement, density, and habitat
df_move <- df_move0 %>% # movement dataframe
  left_join(df_den_adj, # add density values
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_h, # add habitat variables 
            by = c("occasion0" = "occasion",
                   "section0" = "section")) %>% 
  left_join(df_water_pres, # add water level fluctuations
            by = c("occasion0" = "occasion")) %>% 
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
         "bluehead_chub",
         "striped_jumprock") %>% 
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
    dplyr::select(length0,    # length of individual
                  area_ucb,   # area of undercut bank coverage
                  #mean_wpres, # water pressure
                  adj_density_creek_chub, # seasonally adjusted density
                  adj_density_bluehead_chub,
                  adj_density_green_sunfish,
                  adj_density_redbreast_sunfish,
                  adj_density_striped_jumprock) %>% 
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
  
  for (j in 1:n_chain) inits[[j]]$.RNG.seed <- 100 * j
  
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

## export
saveRDS(list_est, file = "data_formatted/output_move.rds")

list_est[[1]]
list_est[[2]]
list_est[[3]]
list_est[[4]]
list_est[[5]]



# Ignore --> playing with figures will move later-----------------------------------------------------------------


bhc <- output_move[[1]]
bhc <- bhc %>% 
  rename(median = "50%",
         upper = "97.5%",
         lower = "2.5%")

fig <- ggplot(bhc, aes(y = var)) +
  geom_pointrange(aes(x = median, xmax = upper, xmin = lower)) +
  geom_vline(xintercept = 0, color = "black") +
  labs(title = "MCMC Parameter Estimate",
       x = "Posterior Median with CI") +
  theme_minimal()
fig

 for(i in 1:length(output_move)) {
   
  output_move[i] <- rename(median = "50%",
          upper = "97.5%",
          lower = "2.5%")
  
   fig[i] <- ggplot(output_move[i], aes(y = var)) +
     geom_pointrange(aes(x = median, xmax = upper, xmin = lower)) +
     geom_vline(xintercept = 0, color = "black") +
     labs(title = "MCMC Parameter Estimate",
          x = "Posterior Median with CI") +
     theme_minimal()
   

  
}

fig[1]

MCMCvis::MCMCplot(post$mcmc,
                  params = "b",
                  main = "MCMC Parameter Estimate",
                  xlab = "Posterior Median with CI", 
                  labels = c("intercept", "length0", "area_ucb", "mean_level", "adj_density_creek_chub",
                             "adj_density_bluehead_chub", "adj_density_green_sunfish", "adj_density_redbreast_sunfish"),
                  col = c("black", "blue", "tan", "deeppink1" ,"orange", "slateblue", "springgreen4", "firebrick2"))

