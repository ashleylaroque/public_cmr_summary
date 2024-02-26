# Run CJS Model with movement integrated


# Setup -------------------------------------------------------------------

source("code/library.R")
source("code/function.R")
source("code/setup_cjs.R")

Nind <- nrow(Y2) # length (#) of individuals
Nocc <- ncol(Y2) # width (#) of occasions
X <- Y2 * 10 - 5 # calculates midpoint of section
Y <- apply(Y2, 1, FUN = function(x) ifelse(is.na(x), 0, 1)) %>% # calculate capture state
  t()
Fc <- apply(Y, MARGIN = 1, getf) # applies function to get the first occasion 


d_jags <- list(Y = Y,                # capture state  
               X = X,                # section midpoint
               Nind = Nind,          # number of individuals
               Nocc = Nocc,          # occasion 1-14
               Fc = Fc,              # get first occasion
               L = 430)              # length of study reach

para <- c("mean.phi", "mean.p", "sd_x")

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
