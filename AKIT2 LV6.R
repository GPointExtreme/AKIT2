rbinom(20, size=10, prob = 0.5)

hist(rbinom(1000, size=10, prob = 0.5))

dbinom(x = 5, size = 10, prob = 0.5)

wahrsch = dbinom(0:100, size=100, prob=0.5)
plot(wahrsch, type = 'h')

theta = seq(0,1, length.out = 200)
likely = dbinom(60, size = 100, prob = theta)
plot(theta, likely, type = 'l')

# Neues Beispiel
# Anwendung der Bayes- Regel
theta = seq(0,1, length.out = 100)
likley = dbinom(60, size = 100, prob = theta)
prior = dbeta(theta, 3, 3)
# wie sieht der prior aus
plot(theta, prior, type = 'l')

posterior = likley*prior
posterior = posterior / sum(posterior)
plot(theta, posterior, type='l')

# Neues Beispiel

library(runjags)
library(coda)
library(akit2)

x = rnorm(100, mean = 150, sd = 10)
y = x + rnorm(100, mean = 0, sd = 20)
plot(x, y)
summary(lm(y ~ x))

modell = "
model {
  # y ~ x
  # y = intc + beta*x + fehler
  for(i in 1:100) {
    y[i] ~ dnorm(intercept + beta*x[i], 1/sigma^2)
  }
  # Prior
  intercept ~ dnorm(0, 1/200^2)
  beta ~ dnorm(0, 1/5^2)
  sigma ~ dexp(3/50)
}
"

modell = run.jags(modell, data = list(y=y, x=x), monitor = c('intercept', 'beta', 'sigma'))

view(modell)
#samplefehler sollte unter 1% sein bzw. zumindest unter 3%.

diagMCMC(modell$mcmc, 'beta') # schlecht
diagMCMC(modell$mcmc, 'intercept') # schlecht
diagMCMC(modell$mcmc, 'sigma') # gut

x.transf = x - mean(x)
hist(x.transf)

modell = run.jags(modell, data = list(y=y, x=x.transf), monitor = c('intercept', 'beta', 'sigma'))
view(modell)

diagMCMC(modell$mcmc, 'beta')

y.z = zscale(y) # Transf: mean->0, sd->1
x.z = zscale(x)
zscale

modell = run.jags(modell, data = list(y=y.z, x=x.z), monitor = c('intercept', 'beta', 'sigma'))
view(modell)
diagMCMC(modell$mcmc, 'beta')

# Rücktransformation
# 0.423 ... beta des z-transf. Modells
0.4234929527 / sd(x)*sd(y)
