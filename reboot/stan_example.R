seaice <- read.csv("seaice.csv", stringsAsFactors = F)


#is sea ice extent declining in the northern hemisphere over time?
plot(extent_north ~ year, pch = 20, data = seaice)
lm1 = lm(extent_north ~ year, data = seaice)
abline(lm1, col = 2, lty =2, lw =3)



library(rstan)
library(gdata)
library(bayesplot)



x <- I(seaice$year - 1978)
y <- seaice$extent_north
N <- length(seaice$year)
lm1 <- lm(y ~ x)
summary(lm1)

lm_alpha <- summary(lm1)$coeff[1]  # the intercept
lm_beta <- summary(lm1)$coeff[2]  # the slope
lm_sigma <- sigma(lm1)  # the residual error

stan_data = list(N=N, x=x, y=y)


write("// Stan model for simple linear regression
      data{
      int < lower = 1 > N; // Sample size
      vector[N] x; // Predictor
      vector[N] y; // Outcome
      }
parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 y ~ normal(alpha + x * beta , sigma);
}

generated quantities {
} // The posterior predictive distribution",

"stan_model1.stan")

stanc("stan_model1.stan")

stan_model1 = 'stan_model1.stan'

fit <- stan(file = stan_model1, data = stan_data,
            warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)
fit

posterior = extract(fit)
str(posterior)

plot(y~x, pch= 20)
abline(lm1, col = 2, lty = 2, lw = 3)
abline( mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2)


