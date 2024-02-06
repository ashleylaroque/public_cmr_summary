
# CJS Movement Model 

model {
  
  # Priors
  for (i in 1:Nind){
    for (t in Fc[i]:(Nocc - 1)){
      logit(phi[i,t]) <- mu + epsilon[t]
      p[i,t] <- mean.p
    } #t
  } #i
  
  # constraints
  for (t in 1:(Nocc - 1)){
    epsilon[t] ~ dnorm(0, tau)
  }
  
  mean.phi ~ dunif(0, 1)               # Prior for mean survival
  mu <- log(mean.phi / (1 - mean.phi)) # Logit transformation
  sigma ~ dunif(0, 10)                 # Prior for standard deviation
  tau <- pow(sigma, -2)
  sigma2 <- pow(sigma, 2)              # Temporal variance
  mean.p ~ dunif(0, 1)                 # Prior for mean recapture
  
  # Likelihood
  for (i in 1:Nind){
    
    # Define latent state at first capture
    z[i,Fc[i]] <- 1
    
    for (t in (Fc[i]+1):Nocc){
      
      # State process
      mu1[i,t] <- phi[i,t - 1] * z[i,t - 1]
      z[i,t] ~ dbern(mu1[i,t])
      
      # Observation process
      mu2[i,t] <- p[i,t - 1] * z[i,t]
      Y[i,t] ~ dbern(mu2[i,t])
    } #t
  } #i
}



model{
  # Priors
  tau ~ dscaled.gamma(1000, 1)
  sigma <- sqrt(1/tau)
  mu.phi ~ dunif(0,1)
  
  # Dispersal model
  for(i in 1:Nsample){
    ## Observation
    Y[i] ~ dbern(phi[i]*z[i])
    phi[i] <- mu.phi
    
    ## Dispersal
    z[i] <- step(s[i] - 1.5)
    s[i] <- step(L - X[i]) + step(X[i])
    X[i] ~ dnorm(X0[i], tau)
  }
}