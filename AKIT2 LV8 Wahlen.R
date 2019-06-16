library(car)
library(corrplot)
library(runjags)
library(akit2)

# Daten von EU-Wahl 2014
w2014 = read.csv('C:\\Users\\Dominik\\Downloads\\Endgueltiges_Ergebnis_mit_Briefwahl_2014.csv', stringsAsFactors = F)
w2014 = w2014[,c(1:3,5:8,10,12,14,16,18,20,22,24)] # ohne Prozentspalten
colnames(w2014)[3:11] = c('Wahlberechtigte', 'abgegeben', 'ungueltig', 'gueltig',
                          'OEVP', 'SPOE', 'FPOE', 'GRUENE', 'BZOE')

w2014 = within(w2014, {
  abgegeben = as.integer(abgegeben)
  ungueltig = as.integer(ungueltig)
  gueltig = as.integer(gueltig)
  nicht.waehler = Wahlberechtigte - abgegeben
  kleine = BZOE + REKOS + ANDERS + EUSTOP
})
# kleine Parteien weg
w2014 = w2014[,c(-11,-13,-14,-15)]

omit2014 = c(1, # Kommentarzeile
             grep('^G.0000', w2014$GKZ),  # Bundesländer
             grep('^G.[A-Z]', w2014$GKZ), # Bezirke
             grep('^G...99', w2014$GKZ),  # Wahlkarten (sind in Summen der Städte enthalten)
             grep('^G9..01', w2014$GKZ),  # Wien ohne Wahlkarten
             grep('^G[1-7]0101', w2014$GKZ), # Hauptstädte ohne Wahlkarten
             grep('G10201|G30301|G40201|G30401|G40301|G20201', w2014$GKZ) # letzte Überbleibsel
             )

at2014 = w2014[2,]
w2014 = w2014[-omit2014,] # ohne Summenzeilen


# Daten von EU-Wahl 2019
w2019 = read.csv('C:\\Users\\Dominik\\Downloads\\Vorlaeufiges_Ergebnis_mit_Briefwahl_2019.csv', stringsAsFactors = F)
w2019 = w2019[,c(1:7,9,11,13,15,17,19)] # ohne Prozentspalten
colnames(w2019)[3:12] = c('Wahlberechtigte', 'abgegeben', 'ungueltig', 'gueltig',
                          'OEVP', 'SPOE', 'FPOE', 'GRUENE', 'NEOS', 'KPOE')

w2019 = within(w2019, {
  abgegeben = as.integer(abgegeben)
  ungueltig = as.integer(ungueltig)
  gueltig = as.integer(gueltig)
  nicht.waehler = Wahlberechtigte - abgegeben
  kleine = KPOE + EUROPA
})
# kleine Parteien weg
w2019 = w2019[,c(-12,-13)]

omit2019 = c(1, # Kommentarzeile
             grep('^G.0000', w2019$GKZ),  # Bundesländer
             grep('^G.[A-Z]', w2019$GKZ), # Bezirke
             grep('^G...99', w2019$GKZ),  # Wahlkarten (sind in Summen der Städte enthalten)
             grep('^G9..01', w2019$GKZ),  # Wien ohne Wahlkarten
             grep('^G[1-7]0101', w2019$GKZ), # Hauptstädte ohne Wahlkarten
             grep('G10201|G30301|G40201|G30401|G40301|G20201', w2019$GKZ) # letzte Überbleibsel
)

at2019 = w2019[2,]
w2019 = w2019[-omit2019,] # ohne Summenzeilen

# JOIN
wahlen = merge(w2014, w2019, by='GKZ', suffixes=c('14','19'))

# Steiermark
stmk = wahlen[grep('^G6', wahlen$GKZ),]
stmk$groesse = factor(ifelse(stmk$Wahlberechtigte14 < 1200, 'klein',
                      ifelse(stmk$Wahlberechtigte14 < 5000, 'mittel', 'gross')))

# Aufteilen in 2 Gruppen
set.seed(12)
bekannt = sample.int(nrow(stmk), 100)
# JAGS verträgt keine Variablen, die Zeichenketten sind
data.columns = grep('^([^G]|GR)', colnames(stmk))
stmk.bek = stmk[bekannt, data.columns]
stmk.unbek = stmk[-bekannt, data.columns]
stmk.bek.name = stmk$Gebietsname19[bekannt]
stmk.unbek.name = stmk$Gebietsname19[-bekannt]

# Erstes Modell
modell1 = "
data {
  N <- length(Wahlberechtigte14[])
  sdOEVP = sd(OEVP14)
  sdSPOE = sd(SPOE14)
  sdFPOE = sd(FPOE14)
  sdGRUENE = sd(GRUENE14)
  sdNEOS = sd(NEOS14)
  sdKleine = sd(kleine14)
  sdNW = sd(nicht.waehler14)

  sdOEVP19 = sd(OEVP19)
  maxOEVP19 = max(OEVP19)
}
model {
  for (i in 1:N) {
    OEVP19[i] ~ dnorm(mu[i], 1/sigma^2)
    mu[i] <- intercept +
             beta.oevp*OEVP14[i] +
             beta.spoe*SPOE14[i] +
             beta.fpoe*FPOE14[i] +
             beta.gruene*GRUENE14[i] +
             beta.neos*NEOS14[i] +
             beta.kleine*kleine14[i] +
             beta.nw*nicht.waehler14[i]
  }

  sigma ~ dexp(3/(2*sdOEVP19))
  intercept ~ dnorm(0, 1/(3*maxOEVP19)^2)
  beta.oevp ~ dnorm(0, 1/(2*sdOEVP19/sdOEVP)^2)
  beta.spoe ~ dnorm(0, 1/(2*sdOEVP19/sdSPOE)^2)
  beta.fpoe ~ dnorm(0, 1/(2*sdOEVP19/sdFPOE)^2)
  beta.gruene ~ dnorm(0, 1/(2*sdOEVP19/sdGRUENE)^2)
  beta.neos ~ dnorm(0, 1/(2*sdOEVP19/sdNEOS)^2)
  beta.kleine ~ dnorm(0, 1/(2*sdOEVP19/sdKleine)^2)
  beta.nw ~ dnorm(0, 1/(2*sdOEVP19/sdNW)^2)
}
"

m1 = run.jags(modell1,
              monitor=c('intercept', 'sigma',
                        'beta.oevp', 'beta.spoe', 'beta.fpoe', 'beta.gruene',
                        'beta.neos', 'beta.kleine', 'beta.nw'),
              data=stmk.bek,
              n.chains = 3)
view(m1)
diagMCMC(m1$mcmc, 'beta.neos')

#z-transformieren und gruppenvariablen in integer verwanden - ALL IN ONE Befehl :)
stmk.bek.z = prepare.df.bayes(stmk.bek, drop.originals = TRUE)

m1z = run.jags(modell1,
              monitor=c('intercept', 'sigma',
                        'beta.oevp', 'beta.spoe', 'beta.fpoe', 'beta.gruene',
                        'beta.neos', 'beta.kleine', 'beta.nw'),
              data=stmk.bek.z,
              n.chains = 3)

view(m1z)
#Schaut immer noch nicht gut aus!

stmk.bek.lz = zscale.df(log2(stmk.bek[, -23] + 1), drop.originals = TRUE)

m1lz = run.jags(modell1,
               monitor=c('intercept', 'sigma',
                         'beta.oevp', 'beta.spoe', 'beta.fpoe', 'beta.gruene',
                         'beta.neos', 'beta.kleine', 'beta.nw'),
               data=stmk.bek.lz,
               n.chains = 3)

view(m1lz)
diagMCMC(m1lz$mcmc, 'beta.neos')


# Zweites Modell
modell2 = "
model {
for (i in 1:100) {
OEVP19[i] ~ dnorm(mu[i], 1/sigma^2)
mu[i] <- beta.oevp*OEVP14[i] +
beta.spoe*SPOE14[i] +
beta.fpoe*FPOE14[i] +
beta.gruene*GRUENE14[i] +
beta.neos*NEOS14[i] +
beta.kleine*kleine14[i] +
beta.nw*nicht.waehler14[i]
}

for (i in 101:171) {
OEVP19.hat[i-100] ~ dnorm(mu[i], 1/sigma^2)
mu[i] <- beta.oevp*OEVP14[i] +
beta.spoe*SPOE14[i] +
beta.fpoe*FPOE14[i] +
beta.gruene*GRUENE14[i] +
beta.neos*NEOS14[i] +
beta.kleine*kleine14[i] +
beta.nw*nicht.waehler14[i]
}

sigma ~ dexp(3/2)
beta.oevp ~ dnorm(0, 1/2^2)
beta.spoe ~ dnorm(0, 1/2^2)
beta.fpoe ~ dnorm(0, 1/2^2)
beta.gruene ~ dnorm(0, 1/2^2)
beta.neos ~ dnorm(0, 1/2^2)
beta.kleine ~ dnorm(0, 1/2^2)
beta.nw ~ dnorm(0, 1/2^2)
}
"
#Neuer Befehl um Daten zu transformieren wie einen anderen Datensatz!
stmk.unbek.lz = zscale.df.other(log2(stmk.unbek[, -23]+1), stmk.bek.lz, drop.originals = TRUE)

m2lz = run.jags(modell2,
                monitor=c('sigma',
                          'beta.oevp', 'beta.spoe', 'beta.fpoe', 'beta.gruene',
                          'beta.neos', 'beta.kleine', 'beta.nw', 'OEVP19.hat'),
                data=rbind(stmk.bek.lz, stmk.unbek.lz),
                n.chains = 3)

samples2 = as.matrix(m2lz$mcmc)
dim(samples2)
stmk.samples = samples2[, -(1:8)]
#L�schen sigma, beta.oevp, etc. raus weil wir nur Gemeinden (OEVP19.hat) haben wollen

gemeinde = 37
gem.samples = stmk.samples[, gemeinde]
stmk.unbek.name[gemeinde]
#Zur�cktransformieren und es muss der Datensatz genommen werden den wir zum Umrechnen 
#schon rein gegeben haben.
gem.orig = inv.zscale.other(gem.samples, stmk.bek.lz$OEVP19)
gem.orig = 2^gem.orig - 1
plotPost(gem.orig, compVal = stmk.unbek$OEVP19[gemeinde])
#Wir sagen 598 vorraus und in wirklichkeit waren 535.