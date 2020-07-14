ring_model_t_pdbh_nc_NOCOVAR_sigd <- function() {

  beta0     ~ dnorm(0, sd = 1.0/0.00001)
  #sig_d_obs ~ dunif(0, 1000)
  sig_x_obs ~ dunif(0, 2.0)
  
  sig_x     ~ dunif(0, 1000)
  beta_sd   ~ dunif(0, 1000)
  beta_t_sd ~ dunif(0, 1000)

  # build cov matrices	
  for (i in 1:2) {
    cov2[i,i] <- sig_x_obs * sig_x_obs
  }
  cov2[1,2] <- 0 
  cov2[2,1] <- 0 
  
  # ring-width likelihood
    
  # one core trees
  for (i in 1:n1cores) {
    logXobs[i1core2m[i]] ~ dnorm(log(X[meas2x[i1core2m[i]]]), sd=sig_x_obs)
  }
  
  # two core trees
  for (i in 1:n2cores) {
    mn_expr1[1:2,i] <- log(X[meas2x[i2core2m[i]]])*ones[1:2]
    logXobs[i2core2m[i]:(i2core2m[i]+1)] ~ dmnorm(mn_expr1[1:2,i], cov=cov2[1:2,1:2])
  }

  for (i in 1:N_pdbh){   
   # tmp <- D[pdbh2d[i]-1]
    #check_idx[i] ~ dunif(D[pdbh2d[i]-1]-.001, D[pdbh2d[i]-1]+.001)
    logPDobs[i] ~ dt(log(D[pdbh2d[i]-1] + X[pdbh2d[i]]/10.0),  1/(sig_d_obs * sig_d_obs), 3)
  }
  
  # process evolution
  for (i in 1:N_trees){
    D0[i] ~ dunif(-30, 50)
    D[first_ti[i]] <- D0[i] + 2.0 * X[first_ti[i]] / 10.0
    for (t in (first_ti[i]+1):cs_last_ti[i]){
      D[t] <- D[t-1] + 2.0 * X[t] / 10.0
    }
  }

  for(i in 1:N_X) {
    X[i] ~ dlnorm(beta[x2tree[i]] + beta_t[x2year[i]], sd = sig_x)
  }
  
  for(i in 1:N_trees) {
    beta[i] ~ dnorm(beta0, sd = beta_sd)
  }
  
  for(j in 1:N_years) {
    beta_t[j] ~ dnorm(0, sd = beta_t_sd)
  }
}

