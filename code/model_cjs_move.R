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
      Ym[i,t] ~ dbern(mu2[i,t])
    } #t
  } #i
  
  
  ## movement model
  ## prior
  
 # sd_x ~ dunif(0, 1000)       # constraint for movement (1000 comes from study reach being 430 so logically the number must be larger than the absolute max value)
 # tau_x <- pow(sd_x, -2)      # variance for movement
  sd_eps ~ dunif(0, 10)        # constraint for temporal variation
  tau_eps <- pow(sd_eps, -2)   # variance for temporal variation
  alpha ~ dnorm(0, 0.01)        
  beta ~ dnorm(0, 0.01)        
  
  ## likelihood
  for (i in 1:Nind) {
    for (t in (Fc[i] + 1):Nocc) {
      Xm[i, t] ~ dnorm(Xm[i, t - 1], tau_x[i, t - 1]) # gross movement
      xi[i, t] <- step(s[i, t] - 1.5) # true emigration
      s[i, t] <- step(L - Xm[i, t]) + step(Xm[i, t]) # component to measure emigration (whether they have left up vs downstream)
    
      # density and size predictor   
      tau_x[i, t - 1] <- pow(sd_x[i, t-1], -2) # variance over time and individual from movement contraint over time and individual
      log(sd_x[i, t - 1]) <- alpha + beta * Den[Sm[i, t - 1], t - 1] + eps[i, t - 1] # integrate density and size with a temporal variation parameter
      #log(sd_x[i, t - 1]) <- alpha + eps[i, t - 1] # integrate density and size with a temporal variation parameter
      eps[i, t - 1] ~ dnorm(0, tau_eps) # eps integrates temporal variation
    
    }#t
  }#i
  
  # ## nested model to retrospectively generate missing values 
  # # density: from predicted movement, generate section so that density in that given section can be applied as a predictor
  # # size: fill in missing size values between recapture events based on predicted growth curve 
  # for (i in 1:Nind) {
  #   
  #   
  # }
  
}

data {
  ## reorganize capture-recapture Y
  for (n in 1:Nobs) {
    Ym[Id_tag_y[n], Id_occ_y[n]] <- Y[n]
  }
  
  for (i in 1:Nx) {
    Xm[Id_tag_x[i], Id_occ_x[i]] <- X[i]
    Sm[Id_tag_x[i], Id_occ_x[i]] <- Section[i]
  }
 
  for (j in 1:Nd) {
    Den[Id_sec_d[j], Id_occ_d[j]] <- Density[j]
  }
   
}
