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
  alpha ~ dnorm(0, 0.01)        
  beta ~ dnorm(0, 0.01)        
  
  ## likelihood
  for (i in 1:Nind) {
    for (t in (Fc[i] + 1):Nocc) {
      Xm[i, t] ~ dnorm(Xm[i, t - 1], tau_x[i, t - 1]) # gross movement
      xi[i, t] <- step(s[i, t] - 1.5) # true emigration
      s[i, t] <- step(L - Xm[i, t]) + step(Xm[i, t]) # component to measure emigration (whether they have left up vs downstream)
      
      ## regression on sd parameter
      tau_x[i, t - 1] <- pow(sd_x[i, t - 1], -2) # variance over time and individual from movement contraint over time and individual
      log(sd_x[i, t - 1]) <- alpha + beta * den[i, t - 1]
      
      ## w: latent indicator; w = 1 if the deviation from the section mid point is < 5 m
      ## sn: latent section number for individual `i` and occation `t - 1`
      ## nu: latent indicator; nu = 1 if individual i stay in the study section
      ## sm: latent section number; when nu = 0, dummy one will be inserted to make the code work
      ## - `sn` cannot be used directly because sn = 0 when an individual moves out the study section
      ## - `sm` insert dummy one for those individuals just to make the code work
      w[i, t - 1, 1:Nsec] <- step(5 - abs(X_mid[] - Xm[i, t - 1]))
      
      sn[i, t - 1] <- sum(w[i, t - 1, ] * 1:Nsec)
      nu[i, t - 1] <- step(sum(w[i, t - 1, ] * 1:Nsec) - 0.5)
      sm[i, t - 1] <- nu[i, t - 1] * sn[i, t - 1] + (1 - nu[i, t - 1])
      
      ## den: density for individual i and occasion t - 1
      ## - if nu = 0 (individual emigrate from the study section), use the mean density across the sections
      den[i, t - 1] <- nu[i, t - 1] * Den[sm[i, t - 1], t - 1] +
        (1 - nu[i, t - 1]) * mean(Den[, t - 1])
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
    Sm[Id_tag_x[i], Id_occ_x[i]] <- Section[i] # section matrix
  }
  
  for (j in 1:Nd) {
    Den[Id_sec_d[j], Id_occ_d[j]] <- Density[j] # density matrix 
  }
  
}
