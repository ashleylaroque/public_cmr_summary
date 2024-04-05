#' Author: Ashley LaRoque, Akira Terui
#' Description:
#' This script runs CJS model integrating imperfect detection and movement

# Setup -------------------------------------------------------------------

source("code/library.R")
source("code/setup_cjs.R")

## binary recapture record
list_recap <- with(df_y,
                   list(Y = y,# capture state  
                        Id_tag_y = tag_index, # tag id
                        Id_occ_y = occasion, # occasion
                        Nind = n_distinct(tag_index), # n unique ind
                        Nocc = n_distinct(occasion), # n unique occasions
                        Nobs = nrow(df_y),
                        L = 430, # total length
                        Nsec = 43,
                        Fc = df_fc$occasion,
                        X_mid = seq(5, 425, by = 10))
                   ) # first capture

## movement distance
df_dist <- df_dist %>% drop_na(x)
list_move <- with(df_dist,
                  list(X = x, # distance class
                       Section = section, # section
                       Id_tag_x = tag_index,
                       Id_occ_x = occasion,
                       Nx = nrow(df_dist)))

## density
list_d <- with(df_den,
               list(Density = density_redbreast_sunfish,
                    Id_occ_d = occasion,
                    Id_sec_d = section,
                    Nd = nrow(df_den)))

d_jags <- c(list_recap, list_move, list_d)

para <- c("alpha", "beta")

# mcmc setup --------------------------------------------------------------

## model file ####
mcjs <- runjags::read.jagsfile("code/model_cjs_move.R")

## mcmc setup ####
n_ad <- 1000
n_iter <- 1.0E+3
n_thin <- max(3, ceiling(n_iter / 250)) #happens second want chains to converge 
n_burn <- ceiling(max(10, n_iter/2)) #happens first and gets rid of noise 
n_sample <- ceiling(n_iter / n_thin)
n_chain <- 4

inits <- replicate(n_chain,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA,
                        mean.p = 0.5,
                        mean.phi = 0.9,
                        alpha = 3),
                   simplify = FALSE)

for (j in 1:n_chain) inits[[j]]$.RNG.seed <- (j - 1) * 10 + 1


# run ---------------------------------------------------------------------

post <- runjags::run.jags(model = mcjs$model,
                          monitor = para,
                          data = d_jags,
                          n.chains = n_chain,
                          inits = inits,
                          method = "parallel",
                          burnin = n_burn,
                          sample = n_sample,
                          adapt = n_ad,
                          thin = n_thin,
                          n.sims = n_chain,
                          module = "glm") #specific to jags doesnt need to be change

MCMCvis::MCMCsummary(post$mcmc)
