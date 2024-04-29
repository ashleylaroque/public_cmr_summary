model {
  
  # survival model ----------------------------------------------------------
  ## priors
  for (i in 1:Nind){
    for (t in Fc[i]:(Nocc - 1)){
      logit(phi[i, t]) <- mu
      p[i, t] <- mean.p
    } #t
  } #i
  
  mean.phi ~ dunif(0, 1)               # Prior for mean survival
  mu <- log(mean.phi / (1 - mean.phi)) # Logit transformation
  mean.p ~ dunif(0, 1)                 # Prior for mean recapture
  
  ## likelihood
  for (i in 1:Nind){
    
    # define latent state at first capture
    # set one because the survival state is known
    z[i, Fc[i]] <- 1
    
    for (t in (Fc[i]+1):Nocc){
      # state process
      mu_s[i, t] <- phi[i, t - 1] * z[i, t - 1]
      z[i, t] ~ dbern(mu_s[i, t])
      
      # observation process
      mu_o[i,t] <- p[i, t - 1] * z[i, t] * xi[i,t]
      Ym[i,t] ~ dbern(mu_o[i,t])
    } #t
  } #i
  
  # movement model ----------------------------------------------------------
  ## prior
  sd_x ~ dunif(0, 1000)
  tau_x <- pow(sd_x, -2) 
  
  ## likelihood
  for (i in 1:Nind) {
    for (t in (Fc[i] + 1):Nocc) {
      Xm[i, t] ~ dnorm(Xm[i, t - 1], tau_x) # gross movement
      xi[i, t] <- step(s[i, t] - 1.5) # true emigration
      s[i, t] <- step(L - Xm[i, t]) + step(Xm[i, t]) # component to measure emigration (whether they have left up vs downstream)
      

    }#t
  }#i
  
}

# tells JAGS how to turn vector data into matrix
data {
  ## reorganize capture-recapture Y
  for (n in 1:Nobs) {
    Ym[Id_tag_y[n], Id_occ_y[n]] <- Y[n] # recapture matrix
  }
  
  for (i in 1:Nx) {
    Xm[Id_tag_x[i], Id_occ_x[i]] <- X[i] # movement matrix
  }
  
}