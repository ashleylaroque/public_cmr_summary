
# CJS Movement Model 

model {
  
  # Priors
  for (i in 1:Nind){
    for (t in Fc[i]:(Nocc - 1)){
      logit(phi[i,t]) <- mu
      p[i,t] <- mean.p
    } #t
  } #i
  
  mean.phi ~ dunif(0, 1)               # Prior for mean survival
  mu <- log(mean.phi / (1 - mean.phi)) # Logit transformation
  mean.p ~ dunif(0, 1)                 # Prior for mean recapture
  
  # Likelihood
  
  ## survival model
  for (i in 1:Nind){
    
    # Define latent state at first capture
    z[i,Fc[i]] <- 1
    
    for (t in (Fc[i]+1):Nocc){
      
      # State process
      # logit(phi[i, t]) <- alpha_phi + beta_phi * size[i, t]
      mu1[i,t] <- phi[i,t - 1] * z[i,t - 1]
      z[i,t] ~ dbern(mu1[i,t])
      
      # Observation process
      mu2[i,t] <- p[i,t - 1] * z[i,t] * xi[i,t]
      Y[i,t] ~ dbern(mu2[i,t])
    } #t
  } #i
  
  ## dispersal model
  ## prior
  
  ## if you want to model the effects of predictors,
  ## make sd_x as a function of predictors
  ## e.g., log(sd_x) <- alpha + beta * size[i, t]
  sd_x ~ dunif(0, 1000)
  tau_x <- pow(sd_x, -2)
  
  ## likelihood
  for (i in 1:Nind) {
    for (t in (Fc[i] + 1):Nocc) {
      X[i, t] ~ dnorm(X[i, t - 1], tau_x)
      xi[i, t] <- step(s[i, t] - 1.5)
      s[i, t] <- step(L - X[i, t]) + step(X[i, t])
    }#t
  }#i
  
}
