model {
  
  for (k in 1:K) {
    b[k] ~ dnorm(0, 0.1)
  }
  
  for (i in 1:Nsample) {
    Y1[i] ~ dnorm(Y0[i], tau[i])
    tau[i] <- pow(sd_m[i], -2)
    
    log(sd_m[i]) <- inprod(b[], X[i,]) + log(sqrt(Intv[i]))
  }
  
}