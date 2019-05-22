# Modell mit korrigierten Beta-Faktoren
model.space = "
data {
  N <- length(Salary[])
  N.comp <- max(CompanySize.int[])
  N.spc <- max(TabSpaces.int[])
  salary.sd = sd(Salary[])
}

model {
  for (i in 1:N) {
    Salary[i] ~ dnorm(mu[i], 1/sigma^2)
    mu[i] <- beta.company[CompanySize.int[i]] +
             beta.space[TabSpaces.int[i]] +
             beta.yearProg * YearsProgram.num[i]
  }
  
  for (c in 1:N.comp) {
    beta.company[c] ~ dnorm(betac.mu, 1/betac.sigma^2)
  }
  betac.mu ~ dnorm(0, 1/100000^2)
  betac.sigma ~ dexp(3/50000)
  
  for (s in 1:N.spc) {
    beta.space[s] ~ dnorm(0, 1/25000^2)
  }
  
  beta.yearProg ~ dnorm(0, 1/10000^2)
  sigma ~ dexp(3/(salary.sd*2))

  # Nachbereitung
  for (c in 1:N.comp) {
    for (s in 1:N.spc) {
      mtx[c, s] <- beta.company[c] + beta.space[s]
    }
  }
  
  intercept <- mean(mtx[1:N.comp, 1:N.spc])
  for (c in 1:N.comp) {
    beta2.company[c] <- mean(mtx[c, 1:N.spc]) - intercept
  }
  for (s in 1:N.spc) {
    beta2.space[s] <- mean(mtx[1:N.comp, s]) - intercept
  }
}
"

fit.space = run.jags(model.space,
                data = de,
                monitor = c("intercept", "beta2.company", "beta2.space",
                            "beta.yearProg", "sigma"),
                n.chains = 3,
                method="parallel")

view(fit.space)
plotcoef(fit.space)
