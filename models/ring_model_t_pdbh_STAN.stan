// RW + CENSUS MODEL
data {
  int<lower=0> N_C;                // number of trees between census and RW data
  int<lower=0> N_Dobs;            // number of census diameter measurements
  int<lower=0> N_Tr;     // number of trees with rw measurements 
  int<lower=0> N_years;    // number of years
  int<lower=0> N_X_C;    // number of values to estimate
  int<lower=0> N_Xobs;     // number of increments measured
  int<lower=0> Tr2X_C [N_Tr];    // connects meta diameters to values 
  int<lower=0> X2C [N_X_C];    // connects values to trees 
  int<lower=0> X2year_C [N_X_C];    // connects values to years 
  real logTr[N_Tr];  // log(DBH from meta data file)
  real logXobs[N_Xobs]; // increments
  real logDobs[N_Dobs]; // diameter measurements
  int<lower=0> idx_C [N_C, 3]; // points to the start and stop point in values for each tree
  int<lower=0> Xobs2X_C[N_Xobs]; // index to relate measurements to estimated X vals
  int<lower=0> Dobs2X [N_Dobs]; // connects census diameter measurements to vals
}
transformed data {
}
parameters {
  real beta0; // prior for beta(tree)
  real<lower=0> beta_sd; // prior for beta(tree)
  real<lower=0> beta_t_sd; // prior for beta(year) 
  real beta[N_C]; // in likelihood of X vals 
  real beta_t[N_years]; // in likelihood of X vals 
  real<lower=1e-6> sig_x; // in likelihood of X vals 
  real<lower=1e-6> sig_x_obs; // in likelihood for increments
  real<lower=0> sig_d_obs; // in likelihood for diameters 
  real<lower=1e-6, upper=80> D0[N_C]; // starting diameter values 
  vector<lower=1e-6>[N_X_C] X; // estimated annual incrememnts 
}
transformed parameters {
    // process evolution
  vector<lower=1e-6>[N_X_C] D;

    // increment diameters from D0 according to estimated Xs
  for (tree in 1:N_C){
    D[idx_C[tree,2]] = D0[tree] + 2.0 * X[idx_C[tree,2]] / 10.0;
    for (val in (idx_C[tree,2]+1):(idx_C[tree,3])){
      D[val] = D[val-1] + 2.0 * X[val] / 10.0;
    }
  }
}

model{

  // priors
  beta0     ~ normal(0, 1.0/0.00001);
  sig_x_obs ~ uniform(1e-6, 2.0);
  sig_d_obs ~ uniform(1e-6, 1000);
  
  sig_x     ~ uniform(1e-6, 1000);
  beta_sd   ~ uniform(1e-6, 1000);
  beta_t_sd ~ uniform(1e-6, 1000);
    
  for(tree in 1:N_C) {
    D0[tree] ~ uniform(-3,80);
    beta[tree] ~ normal(beta0, beta_sd);
  }
  
  for(year in 1:N_years) {
    beta_t[year] ~ normal(0, beta_t_sd);
  }
  
  // likelihood
  // looping through all estimated values
  for (val in 1:N_X_C){
     X[val] ~ lognormal(beta[X2C[val]] + beta_t[X2year_C[val]], sig_x);
   }
   
  // looping through all measured increments
  for (inc in 1:N_Xobs){
     logXobs[inc] ~ normal(log(X[Xobs2X_C[inc]]), sig_x_obs);
  }
  
  // looping through all diameters measured in RW meta data 
  for (tree in 1:N_Tr){
    if (logTr[tree] == -999){
    } else{
      logTr[tree] ~ student_t(3, log(D[Tr2X_C[tree]]), sig_d_obs);
    }
  }
  
  // looping through all census diameter measurements  
  for (tree in 1:N_Dobs){
    logDobs[tree] ~ student_t(3, log(D[Dobs2X[tree]]), sig_d_obs);
  }
  
}
