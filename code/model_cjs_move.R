model {
  
  # survival model ----------------------------------------------------------
  
  ## transformed parameters
  ## - zeta[1]: detection probability for winter
  ## - zeta[2]: detection probability for summer
  ## - 1 / (1 + exp(x)) is an inverse-logit function
  zeta[1] <- 1 / (1 + exp(-mu.p))
  zeta[2] <- 1 / (1 + exp(-(mu.p + alpha)))
  
  ## priors
  for (i in 1:Nind){
    for (t in Fc[i]:(Nocc - 1)){
      ## - phi_day: survival prob per day
      ## - phi = phi_day^(Interval): 
      ## -- cumulative survival from one occasion to the next
      ## -- log-transformed log(phi) = Interval * log(phi_day)
      logit(phi_day[i, t]) <- mu.phi #+ eps_phi[t]
      log(phi[i, t]) <- Intv_m[i, t] * log(phi_day[i, t])
    } #t
  } #i
  
 # for (t in 1:(Nocc - 1)) {
  #  eps_phi[t] ~ dnorm(0, tau_phi)
 # }
  
  for (i in 1:Nind) {
    for (t in (Fc[i] + 1):Nocc) {
      logit(p[i, t]) <- mu.p + alpha * Sm[i, t]
    }
  }
  
  alpha ~ dnorm(0, 0.01)

  
  mean.phi ~ dunif(0, 1)               # Prior for mean survival
  mu.phi <- log(mean.phi / (1 - mean.phi)) # Logit transformation
  mean.p ~ dunif(0, 1)                 # Prior for mean recapture
  mu.p <- log(mean.p / (1 - mean.p))
  
  tau_phi ~ dscaled.gamma(2.5, 6)
  sd_phi <- 1 / sqrt(tau_phi)
  
  ## likelihood
  for (i in 1:Nind){
    
    # define latent state at first capture
    # set one because the survival state is known
    z[i, Fc[i]] <- 1
    
    for (t in (Fc[i] + 1):Nocc){
      # state process
      mu_s[i, t] <- phi[i, t - 1] * z[i, t - 1] 
      z[i, t] ~ dbern(mu_s[i, t]) 
      
      # observation process
      mu_o[i,t] <- p[i, t] * z[i, t] * xi[i,t]
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
    Sm[Id_tag_y[n], Id_occ_y[n]] <- Season[n] # season matrix (winter/summer)
  }
  
  for (n in 1:Nint) {
    Intv_m[Id_tag_int[n], Id_occ_int[n]] <- Intv[n] # interval matrix
  }
  
  for (i in 1:Nx) {
    Xm[Id_tag_x[i], Id_occ_x[i]] <- X[i] # movement matrix
  }
  
}