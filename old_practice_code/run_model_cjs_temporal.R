# Run CJS Model with random temporal variability


# Setup -------------------------------------------------------------------

pacman::p_load(tidyverse,
               lme4)

source("code/function.R")
source("code/setup_cjs.R")



Nind <- nrow(Y)
Nocc <- ncol(Y)
Fc <- apply(Y, MARGIN = 1, getf)

d_jags <- list(Y = Y,                # recapture 0-1 
               Nind = Nind,          # number of individuals
               Nocc = Nocc,          # occasion 1-13
               Fc = Fc)              # first occasion detected

para <- c("mean.phi", "mean.p", "sigma2")

# mcmc setup --------------------------------------------------------------

## model file ####
mcjs <- runjags::read.jagsfile("code/model_cjs_temporal.R")

## mcmc setup ####
n_ad <- 1000
n_iter <- 2.0E+3
n_thin <- max(3, ceiling(n_iter / 250)) #happens second want chains to converge 
n_burn <- ceiling(max(10, n_iter/2)) #happens first and gets rid of noise 
n_sample <- ceiling(n_iter / n_thin)
n_chain <- 4

inits <- replicate(n_chain,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA,
                        mean.p = 0.5,
                        mean.phi = 0.9),
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
