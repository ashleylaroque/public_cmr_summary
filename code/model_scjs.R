model {
  
  # survival model ----------------------------------------------------------
  
  ## transformed parameters
  ## - zeta[1]: detection probability for winter
  ## - zeta[2]: detection probability for summer
  ## - 1 / (1 + exp(x)) is an inverse-logit function
  zeta[1] <- 1 / (1 + exp(-mu.p))
  zeta[2] <- 1 / (1 + exp(-(mu.p + alpha)))
  
  ## priors
  
  ## - phi_day: survival prob per day
  ## - phi = phi_day^(Interval): 
  ## -- cumulative survival from one occasion to the next
  ## -- log-transformed log(phi) = Interval * log(phi_day)
  for (i in 1:Nind){
    for (t in Fc[i]:(Nocc - 1)){
      logit(phi_day[i, t]) <- mu.phi
      log(phi[i, t]) <- Intv_m[i, t] * log(phi_day[i, t])
    } #t
  } #i
  
  ## - p: detection probability
  ## - alpha: seasonal effect
  ## - Sm: season index matrix (0 = winter, 1 = summer)
  for (i in 1:Nind) {
    for (t in (Fc[i] + 1):Nocc) {
      logit(p[i, t]) <- mu.p + alpha * Sm[i, t]
    }
  }
  
  alpha ~ dnorm(0, 0.01)
  
  mean.phi ~ dunif(0, 1) # Prior for mean survival
  mu.phi <- log(mean.phi / (1 - mean.phi)) # Logit transformation
  
  mean.p ~ dunif(0, 1) # Prior for mean recapture
  mu.p <- log(mean.p / (1 - mean.p)) # Logit transformation
  
  ## likelihood
  for (i in 1:Nind){
    ## define latent state at first capture
    ## set one because the survival state is known
    z[i, Fc[i]] <- 1
    
    for (t in (Fc[i] + 1):Nocc){
      ## state process
      mu_s[i, t] <- phi[i, t - 1] * z[i, t - 1] 
      z[i, t] ~ dbern(mu_s[i, t]) 
      
      ## observation process
      ## - Ym: recapture matrix
      ## - z: survival state
      ## - xi: stay state
      mu_o[i,t] <- p[i, t] * z[i, t] * xi[i,t]
      Ym[i,t] ~ dbern(mu_o[i,t])
    } #t
  } #i
  
  # movement model ----------------------------------------------------------
  ## prior for log-trans daily movement sd
  log_sd0 ~ dnorm(0, 0.01)
  sd0 <- exp(log_sd0)
  
  ## likelihood
  ## - Xm: location matrix
  ## - xi: stay state, 1 = stay, 0 = move out from the study section
  for (i in 1:Nind) {
    for (t in (Fc[i] + 1):Nocc) {
      Xm[i, t] ~ dnorm(Xm[i, t - 1], tau_x[i, t - 1]) # gross movement
      tau_x[i, t - 1] <- pow(sd_x[i, t - 1], -2)
      log(sd_x[i, t - 1]) <- log_sd0 + log(Intv_m[i, t - 1])
      
      # xi, indicator for true emigration
      # component to measure emigration (whether they have left up vs downstream)
      xi[i, t] <- step(s[i, t] - 1.5) 
      s[i, t] <- step(L - Xm[i, t]) + step(Xm[i, t]) 
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