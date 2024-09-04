model {
  
  for (k in 1:K) {
    b[k] ~ dnorm(0, 0.1)
  }
  
  p ~ dunif(0, 1)
  
  for (i in 1:Nsample) {
    ## observation model for recaptured or not
    z[i] <- step(430 - X1[i]) + step(X1[i])
    phi[i] <- step(z[i] - 1.5)
    Y[i] ~ dbern(phi[i] * p)
    
    ## movement model
    X1[i] ~ dnorm(X0[i], tau[i])
    tau[i] <- pow(sd_m[i], -2)
    
    log(sd_m[i]) <- inprod(b[], X[i,]) + log(Intv[i])
  }
  
}