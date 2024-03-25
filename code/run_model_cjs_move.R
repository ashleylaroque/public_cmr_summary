# Run CJS Model with movement integrated


# Setup -------------------------------------------------------------------

source("code/library.R")
source("code/function.R")
#source("code/setup_cjs.R")

# how to format below list now that data is no longer in matrix but vector (needs to select correct column)

list_recap <- with(Y,
                   list(Y = x,# capture state  
                        Id_tag_y = tag_index, # tag id
                        Id_occ_y = occasion, # occasion
                        Nind = n_distinct(tag_index), # n unique ind
                        Nocc = n_distinct(occasion), # n unique occasions
                        Nobs = nrow(Y),
                        L = 430, # total length
                        Fc = fc$occasion)) # first capture

Y1 <- Y1 %>% 
  drop_na(x)

list_move <- with(Y1,
                  list(X = x, # distance class
                       Section = section, # section
                       Id_tag_x = tag_index,
                       Id_occ_x = occasion,
                       Nx = nrow(Y1)))

list_d <- with(df_density_sub,
               list(Density = d, # fish density
                    Id_occ_d = occasion,
                    Id_sec_d = section,
                    Nd = nrow(df_density_sub)))

d_jags <- c(list_recap, list_move, list_d)

# # left side is model text ; right side is setup text
# d_jags <- list(Y = Y,                # capture state  
#                X = Y1,               # movement proxy
#                Density = Y2,         # density predictor
#                Size = Y3,            # size predictor
#                Nind = Nind,          # number of individuals
#                Nocc = Nocc,          # number of occasions
#                Fc = fc,              # get first occasion of capture
#                L = 430)              # length of study reach

para <- c("mean.phi", "mean.p", "alpha", "beta")

# mcmc setup --------------------------------------------------------------

## model file ####
mcjs <- runjags::read.jagsfile("code/model_cjs_move.R")

## mcmc setup ####
n_ad <- 100
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
