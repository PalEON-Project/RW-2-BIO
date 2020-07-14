ring_model_t_date_size_pdbh_avg <- function() {
  
  beta0      ~ dnorm(0, sd = 1.0/0.00001)
  beta_slope ~ dunif(0, 0.3)	

  sig_d_obs ~ dunif(1e-6, 1000)
  sig_x_obs ~ dunif(1e-6, 2.0)#0.6
  sig_x     ~ dunif(1e-6, 1000)
  
  beta_sd   ~ dunif(0, 1000)
  beta_t_sd ~ dunif(0, 1000)

  b0 ~ dnorm(0, 1)
  b1 ~ dunif(5, 25)
  
  # ring-width likelihood
  
  # one core trees
  for (i in 1:N_inc) {
    logXobs[i] ~ dnorm(log(X[meas2x[i]]), sd=sig_x_obs)
  }
  

  # process evolution
  for (i in 1:N_trees){
    D0[i] ~ dunif(-200, 300)
    D[first_ti[i]] <- D0[i] + 2.0 * X[first_ti[i]] / 10.0
    for (t in (first_ti[i]+1):cs_last_ti[i]){
      D[t] <- D[t-1] + 2.0 * X[t] / 10.0
    }
  }
  
  # dbh likelihood
  for(j in 1:N_dbh) {
    logDobs[j] ~ dt(log(D[meas2d[j]-1] + expit(b0 + b1*(dbh_day_id[j]-0.5)) * X[meas2d[j]]/10.0),  1/(sig_d_obs * sig_d_obs), 3)
  }


  # make a vector of last pdbh last ti for dbh; this will change first_ti and cs_last_ti
  # make logpDobs vector
  # make an index vector of pdbh2d
  for (i in 1:N_pdbh){ 
    logPDobs[i] ~ dt(log(D[pdbh2d[i]-1] + expit(b0 + b1*(pdbh_day_id[i]-0.5)) * X[pdbh2d[i]]/10.0),  1/(sig_d_obs * sig_d_obs), 3)
  }

  for(i in 1:N_trees){
    mnX[first_ti[i]] <- beta[i] + beta_slope * (D0[i] - open_dbh) * (D0[i] > open_dbh) + beta_t[1]
   for (j in (first_ti[i]+1):cs_last_ti[i]){
     mnX[j] <- beta[i] + beta_slope * (D[j-1] - open_dbh) * (D[j-1] > open_dbh) + beta_t[x2year[j]]
   }
  }

 for (i in 1:N_X){
    X[i] ~ dlnorm(mnX[i], sd = sig_x)
  }

  for(i in 1:N_trees) {
    beta[i] ~ dnorm(beta0, sd = beta_sd)
  }
  
  for(j in 1:N_years) {
    beta_t[j] ~ dnorm(0, sd = beta_t_sd)
  }
}