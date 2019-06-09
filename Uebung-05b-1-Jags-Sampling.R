# AKIT2, Arno Hollosi
# Ãœbung: Bayes-Sampling mit JAGS

library(runjags)
rjags::load.module("glm")
library(akit2)

body = read.csv("C:\\Users\\Dominik\\Downloads\\body-dimensions.csv")
gruppeF = body$Height[body$Gender=="Female"]
gruppeM = body$Height[body$Gender=="Male"]

# 1) Vollziehen Sie das Skript Schritt für Schritt für die Gruppe der Frauen nach.
#    Überlegen Sie sich, wozu die einzelnen Schritte notwendig sind.
#    Prüfen Sie bei den Diagnose-Funktionen, ob die Bedingungen
#    eingehalten sind.

bodyModell = "
  data {
    # Berechnen einiger Daten
    N <- length(y[])
    height.mean <- mean(y)
    height.sd <- sd(y)
  }
  
  model {
    # Likelihood
    for (i in 1:N) {
      y[i] ~ dnorm(mu, 1/sigma^2)
      # JAGS verwendet statt der Standardabweichung sigma die Präzision: 1/sigmaÂ²
    }

    # Prior für Mittelwert
    mu ~ dnorm(height.mean, 1/(height.sd*2)^2)

    # Prior für Standardabweichung: wir orientieren uns an der Standardabeweichung
    # der Daten und geben etwas Spielraum hinzu
    sigma ~ dexp(3/(height.sd*2))
}
"

# 1a) run.jags() ausfÃ¼hren -- siehe Source-Code des Arbeitsblatts
model.F = run.jags(
  model = bodyModell,
  data = list(y = gruppeF),
  monitor = c("mu", "sigma"),
  n.chains = 3,
  burnin = 1000,
  sample = 10000,
  method = "parallel"
)
view(model.F)
posterior.F = as.matrix(model.F$mcmc)

# 1b) diagMCMC() der beiden Parameter -- siehe Source-Code des Arbeitsblatts

diagMCMC(model.F$mcmc, parName = "mu")
diagMCMC(model.F$mcmc, parName = "sigma")

# 1c) plotPost() der beiden Parameter -- siehe Source-Code des Arbeitsblatts

par(mfrow = c(1,2))
plotPost(posterior.F[, "mu"], main = "Gruppe F, Mittelwert", xlab = bquote(mu), cenTend = "median")
plotPost(posterior.F[, "sigma"], main = "Gruppe F, Std.abw.", xlab = bquote(sigma), cenTend = "median")

# 1d) Beispiel: andere HDI berechnen (aus akit2-Bibliothek)
HDIofMCMC(posterior.F[,"mu"], credMass = 0.5)
HDIofMCMC(posterior.F[,"sigma"], credMass = 0.89)

# 1e) Beispiel: MÃ¶gliche errechnete Verteilungen
par(mfrow=c(1,1))
hist(gruppeF, col="lightblue", xlim = c(145,185))
curves = sample.int(30000, 20) # Wir wÃ¤hlen zufällig 20 Werte aus dem Posterior aus
for (i in curves) {
  # *170, damit auf Diagram die Verteilung selbe Höhe wie Histogramm hat
  curve(dnorm((x-posterior.F[i,"mu"])/posterior.F[i,"sigma"])*170,
        add=TRUE, col=rgb(0.5,0.5,1,0.4))
}
curve(dnorm((x-mean(posterior.F[,"mu"]))/mean(posterior.F[,"sigma"]))*170,
      add=TRUE, col="darkblue", lwd=2)


# 2) Führen Sie die selbe Berechnung sowie Diagnose und Diagramme für die Gruppe der Männer durch.
#    Speichern Sie den Posterior als posterior.M

model.M = run.jags(
  model = bodyModell,
  data = list(y = gruppeM),
  monitor = c("mu", "sigma"),
  n.chains = 3,
  burnin = 1000,
  sample = 10000,
  method = "parallel"
)
view(model.M)
posterior.M = as.matrix(model.M$mcmc)

# 3) Schauen Sie sich mit plotPost() die Differenz der beiden Posteriors an.
#    Wie interpretieren Sie das Ergebnis?

par(mfrow=c(1,2))
plotPost(posterior.F[, "mu"]-posterior.M[, "mu"], main = "Differenz Mittelwert A/B", xlab = bquote(mu), cenTend = "median")
plotPost(posterior.F[, "sigma"]-posterior.M[, "sigma"], main = "Differenz Std.abw. A/B", xlab = bquote(sigma), cenTend = "median")

# 4) Schreiben Sie ein Modell, in dem beide Gruppen gleichzeitig berechnet werden,
#    aber nur eine Standardabweichung für beide benützt wird.
#    Die Modell-Parameter sind also: mu.F, mu.M und sigma; Daten: gruppeF, gruppeM

model = run.jags(
  model = "
  data {
  # Berechnen einiger Daten
  N.F <- length(yf[])
  N.M <- length(ym[])
  height.mean.F <- mean(yf)
  height.mean.M <- mean(ym)
  height.sd <- sd(ym)
  }
  
  model {
  for (i in 1:N.F) {
  yf[i] ~ dnorm(mu.F, 1/(sigma^2))
  }
  for (j in 1:N.M) {
  ym[j] ~ dnorm(mu.M, 1/(sigma^2))
  }
  mu.F ~ dnorm(height.mean.F, 1/(height.sd*2)^2)
  mu.M ~ dnorm(height.mean.M, 1/(height.sd*2)^2)
  sigma ~ dexp(3/(height.sd*2))
  }
  ",
  data=list(
    yf = gruppeF,
    ym = gruppeM
  ),
  monitor=c("mu.F", "mu.M", "sigma"),
  n.chains=3,
  burnin = 1000,
  sample = 10000,
  method="parallel"
)
view(model)

diagMCMC(model$mcmc, parName = "mu.F")
diagMCMC(model$mcmc, parName = "mu.M")
diagMCMC(model$mcmc, parName = "sigma")

posterior = as.matrix(model$mcmc)
par(mfrow=c(2,2))
plotPost(posterior[,"mu.F"], main="Gruppe Frauen, Mittelwert", xlab=bquote(mu), cenTend = "median")
plotPost(posterior[,"mu.M"], main="Gruppe MÃ¤nner, Mittelwert", xlab=bquote(mu), cenTend = "median")
plotPost(posterior[,"sigma"], main="Std.abw.", xlab=bquote(sigma), cenTend = "median")
plotPost(posterior[,"mu.F"]-posterior[,"mu.M"], main="Differenz Mittelwert A/B", xlab=bquote(mu), cenTend = "median")
