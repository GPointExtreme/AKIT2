# AKIT2 SS18, Hauptklausur, 15.6.2018
library(ggplot2)
library(car)
library(corrplot)
library(effects)
library(pwr)
library(ROCR)
library(runjags)
library(coda)
rjags::load.module("glm")
library(akit2)

df <- read.csv('C:\\Users\\Dominik\\Downloads\\sport-blut.csv')

# Wir haben Daten von über 200 SportlerInnen erhoben. Wir stellen uns die Frage, wovon
# die Hemoglobin-Konzentration der SportlerInnen abhängt.
#
# Relevante Daten im Datensatz:
# - hg ... Hemoglobin-Konzentration (Einheit: g/dl)
# - wcc ... Anzahl weiße Blutkörperchen (Einheit: 1/nl)
# - ht ... Größe (cm)
# - sex ... Frau (f) oder Mann (m)
# - lbm ...  Magermasse des Körpers, auch fettfreie Masse genannt (kg)
# - pcbfat ... Anteil Körperfett (%)
# - sport ... Sportart

summary(df)
describe(df)

#y = hg
#Varianz = sex

mapply(hist,as.data.frame(df),main=colnames(df))
#wcc aufpassen wegen den paar höheren Werten.
#pcbfat eventuell log-transformieren.
plot(df$pcbfat, df$hg)

#z-Transformieren (machen wir bei Bayes immer!)
dfz = prepare.df.bayes(df, drop.originals = TRUE)
summary(dfz)


#-------------------Vorlage fuer 2 Gruppen und 4 Variablen----------------------------#

#Modell mit 2 Gruppen und vier Variablen
#Wenn Interaktion gefragt ist
#bei den Variablen hinzufuegen.
#zwei Variablen: beta.interaktion*wcc*variable 2 -> interkation von wcc und variable 2
#einer Variable und einer Gruppe:  beta[gruppe[i]]*wcc[i]  -> nicht ganzs sicher ob das stimmt

#Folgende Variablen sind zum ersetzen

#sex              = ist die Gruppenvariable nach der die Varianz gefragt ist was später auch Simga im Monitor darstellt  
#sport              = 2 Gruppenvariable
#hg  = stellt die abhaengige Variable dar 
#wcc            = erste metrische Variable
#pcbfat            = zweite metrische Varibale
#lbm            = dritte metrische Varibale
#ht            = vierte metrische Variable



#--------------------------Beginn von Modell definition---------------------------------#
modell = "
data {
N <- length(hg[])
Nsex <- max(sex)
Nsport <- max(sport)
}


model {

for (i in 1:N) {


hg[i] ~ dnorm(mu[i], 1/sigma[sex[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptsport[sport[i]] +           		# Gruppe fuer sport / intercept
interceptsex[sex[i]] +                        # Gruppe fuer sex /intercept
beta.wcc*wcc[i] +
beta.pcbfat*pcbfat[i] +
beta.lbm*lbm[i] +
beta.ht*ht[i]

#----------------------------Beginn von Vorhersage-----------------------------------#

#hg.hat[i] ~ dnorm(mu[i], 1/sigma[sex[i]]^2) #ist gleich erste Zeile im Modell
}
#-----------------------------Ende der Vorhersage------------------------------------#


#-----------------------------------Priors-------------------------------------------#


for(l in 1:Nsex){
sigma[l]~dexp(3/1)
}
#--------------------------ACHTUNG Partial Pooling-----------------------------------#
#interceptsex[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#------------------------------------------------------------------------------------#


beta.wcc ~ dnorm(0,1/1^2)
beta.pcbfat ~ dnorm(0,1/1^2)
beta.lbm ~ dnorm(0,1/1^2) 
beta.ht ~ dnorm(0,1/1^2)


for(l in 1:Nsex) 
{
  interceptsex[l] ~ dnorm(0, 1/1^2)
}


# sex hat nur 2 Werte: kein Pooling
# alle sexs bekommen das selbe sgima & intercept
#VT: wenn 7 sexs wären, dann muesste man sonst 7 Zeilen fuer intercept und 7 fuer sigma schreiben

#--------------------------------Ende von Priors-------------------------------------#

#---------------------------Partial-Pooling fuer sport-------------------------------#


interceptsport.mu ~ dnorm(0,1/1^2)
interceptsport.sigma ~ dexp(1)

for (d in 1:Nsport) {
interceptsport[d] ~ dnorm(interceptsport.mu, 1/interceptsport.sigma^2)
}


# stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNsporteitig)

for (l in 1:Nsex) {
for (d in 1:Nsport) {
mtx[l,d] <- interceptsex[l] +interceptsport[d]

}
}

intercept <- mean(mtx[1:Nsex,1:Nsport])
for(l in 1:Nsex)
{
  alphasex[l] <- mean(mtx[l,1:Nsport]) - intercept
}
for (d in 1:Nsport) 
{
  Gammasport[d] <- mean(mtx[1:Nsex,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}

#------------------------Ende Partial-Pooling fuer sport-----------------------------#
}
"


#Modell fuer Variablen aufrufen
modell.fit = run.jags(model=modell,
                      data=dfz,
                      burnin = 5000,
                      monitor = c("intercept", "alphasex", "Gammasport", "sigma",
                                  "beta.wcc", "beta.pcbfat", "beta.lbm", "beta.ht"),
                      n.chains = 3,
                      sample= 10000,
                      thin=2,
                      inits = list(list(.RNG.name="base::Mersenne-Twister", .RNG.seed=456),
                                   list(.RNG.name="base::Super-Duper", .RNG.seed=123),
                                   list(.RNG.name="base::Wichmann-Hill", .RNG.seed=789)),
                      method = "parallel")
summary(df)
fit.samples = as.matrix(modell.fit$mcmc)
fit.summary = view(modell.fit)


#Wenn MC%ofSD um 1 dann mcmc machen bzw wenn SSeff unter 10%
diagMCMC(modell.fit$mcmc,"beta.lbm")
diagMCMC(modell.fit$mcmc,"beta.ht")
#Schauen noch gut aus.

#------------------------------------Interpretation----------------------------------#

#Verzeichnis wo unsere abhaengige Variable, etc. drinnen steckt!

#"N" oder "hg" oder
#"Nsex" oder "interceptsex" oder "alphasex"
#"Nsport" oder "interceptsport" oder "Gammasport" oder "mu"
#"beta.wcc" oder "wcc"
#"beta.wcc" oder "wcc"
#"beta.wcc" oder "wcc"
#"beta.wcc" oder "wcc"

#Warum ist sigma die Varianz? Ist das immer so?
#Wann schauen wir den intercept an?
#"intercept" "sigma"




# Frage 1: Wie groß ist der Unterschied zwischen Männern und Frauen?
#          Haben die beiden Gruppen auch eine unterschiedliche Varianz?
plotcoef(modell.fit, c("sex"))
table(df$sex) #herrausfinden welche Nummer was ist

diff = (fit.samples[,"alphasex[1]"]-fit.samples[,"alphasex[2]"])
plotPost(diff, compVal = 0)

diff_z = (diff*sd(df$hg))
plotPost(diff_z, compVal = 0)
#Umgerechnet liegen sie -1.51 auseinander
#Schließt 0 nicht mit ein = also signifikant!
#In 95% der Fälle liegt der Unterschied zwischen Männern und Frauen zwischen einen HDI 
  #von -1.99 und -0.979


diff = (fit.samples[,"sigma[1]"]-fit.samples[,"sigma[2]"])
plotPost(diff, compVal = 0)

diff_z = (diff*sd(df$hg))
plotPost(diff_z, compVal = 0)
#Varianz hat einen unterschied von -0.137.
#0 ist mit eingeschlossen also nicht signifikant.


# Frage 2: Welchen Einfluss haben weiÃŸe BlutkÃ¶rperchen, die GrÃ¶ÃŸe, die Magermasse und
#          der KÃ¶rperfettanteil auf die Hemoglobin-Konzentration?
plotPost((fit.samples[, "beta.wcc"]*sd(df$hg)/sd(df$wcc)), compVal = 0)
#Im 95%-Level pro 1 erhöhung von wcc, verringert sich hg auf das 0.096-fache.
mean(fit.samples[,"beta.wcc"]*sd(df$hg))
sd(df$wcc)
#Pro sd erhöhung von wcc (1.8 1/nl), erhöt sich hg um 0.17 g/dl.
#0 ist nicht mit eingeschlossen = also signifikant.

plotPost((fit.samples[, "beta.ht"]*sd(df$hg)/sd(df$ht)), compVal = 0)
#Im 95%-Level pro 1 erhöhung von ht, verringert sich hg auf das 0.0188-fache.
mean(fit.samples[,"beta.ht"]*sd(df$hg))
sd(df$ht)
#Pro sd erhöhung von ht (9.7cm), verringert sich hg um -0.16 g/dl.
#0 ist mit eingeschlossen = also nicht signifikant.

plotPost((fit.samples[, "beta.lbm"]*sd(df$hg)/sd(df$lbm)), compVal = 0)
#Im 95%-Level pro 1 erhöhung von lbm, erhöht sich hg auf das 0.0213-fache.
mean(fit.samples[,"beta.lbm"]*sd(df$hg))
sd(df$lbm)
#Pro sd erhöhung von lbm (13kg), verringert sich hg um +0.3 g/dl.
#0 ist mit eingeschlossen = also nicht signifikant.

plotPost((fit.samples[, "beta.pcbfat"]*sd(df$hg)/sd(df$pcbfat)), compVal = 0)
#Im 95%-Level pro 1 erhöhung von pcbfat, verringert sich hg auf das -0.00103-fache.
mean(fit.samples[,"beta.pcbfat"]*sd(df$hg))
sd(df$pcbfat)
#Pro sd erhöhung von pcbfat (6.2%), verringert sich hg um +0.00916 g/dl.
#0 ist mit eingeschlossen = also nicht signifikant.

#Signifikant ist nur beta.wcc in einem 95%-Level. beta.lbm > beta.wcc/beta.ht/beta.pcbfat

# Frage 3: Gibt es eine Sportart, bei der die Hemoglobin-Konzentration im
#          75%-Signifikanzniveau von den anderen abweicht?
plotcoef(modell.fit, c("sport"))
table(df$sport)

diff = (fit.samples[,"Gammasport[1]"]-fit.samples[,"Gammasport[4]"])
plotPost(diff, compVal = 0, credMass = 0.75)
diff_z = (diff*sd(df$hg))
plotPost(diff_z, compVal = 0, credMass = 0.75)
#1=B_Ball - 4=Netball = 0.46
#B_Ball um 0.46 g/dl (75%-HDI: [0.232, 0.77]) höhere HG-Konzentration als Netball
#0 ist nicht eingeschlossen also signifikant

diff = (fit.samples[,"Gammasport[8]"]-fit.samples[,"Gammasport[2]"])
plotPost(diff, compVal = 0, credMass = 0.75)
diff_z = (diff*sd(df$hg))
plotPost(diff_z, compVal = 0, credMass = 0.75)
#2=Field - 8=T_Sprnt = 0.148
#Field um 0.148 g/dl (75%-HDI: [-0.217, 0.441]) höhere HG-Konzentration als T_Sprnt
#0 ist eingeschlossen also nicht signifikant


# Frage 4: Hat das Modell eine gute Vorhersagekraft bzw. ist das Modell eine gute
#          Beschreibung des vorhandenen Datensatzes?

#--------------------------Beginn von Modell definition---------------------------------#
modellp = "
data {
N <- length(hg[])
Nsex <- max(sex)
Nsport <- max(sport)
}


model {

for (i in 1:N) {


hg[i] ~ dnorm(mu[i], 1/sigma[sex[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptsport[sport[i]] +           		# Gruppe fuer sport / intercept
interceptsex[sex[i]] +                        # Gruppe fuer sex /intercept
beta.wcc*wcc[i] +
beta.pcbfat*pcbfat[i] +
beta.lbm*lbm[i] +
beta.ht*ht[i]

#----------------------------Beginn von Vorhersage-----------------------------------#

hg.hat[i] ~ dnorm(mu[i], 1/sigma[sex[i]]^2) #ist gleich erste Zeile im Modell
}
#-----------------------------Ende der Vorhersage------------------------------------#


#-----------------------------------Priors-------------------------------------------#


for(l in 1:Nsex){
sigma[l]~dexp(3/1)
}
#--------------------------ACHTUNG Partial Pooling-----------------------------------#
#interceptsex[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#------------------------------------------------------------------------------------#


beta.wcc ~ dnorm(0,1/1^2)
beta.pcbfat ~ dnorm(0,1/1^2)
beta.lbm ~ dnorm(0,1/1^2) 
beta.ht ~ dnorm(0,1/1^2)


for(l in 1:Nsex) 
{
  interceptsex[l] ~ dnorm(0, 1/1^2)
}


# sex hat nur 2 Werte: kein Pooling
# alle sexs bekommen das selbe sgima & intercept
#VT: wenn 7 sexs wären, dann muesste man sonst 7 Zeilen fuer intercept und 7 fuer sigma schreiben

#--------------------------------Ende von Priors-------------------------------------#

#---------------------------Partial-Pooling fuer sport-------------------------------#


interceptsport.mu ~ dnorm(0,1/1^2)
interceptsport.sigma ~ dexp(1)

for (d in 1:Nsport) {
interceptsport[d] ~ dnorm(interceptsport.mu, 1/interceptsport.sigma^2)
}


# stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNsporteitig)

for (l in 1:Nsex) {
for (d in 1:Nsport) {
mtx[l,d] <- interceptsex[l] +interceptsport[d]

}
}

intercept <- mean(mtx[1:Nsex,1:Nsport])
for(l in 1:Nsex)
{
  alphasex[l] <- mean(mtx[l,1:Nsport]) - intercept
}
for (d in 1:Nsport) 
{
  Gammasport[d] <- mean(mtx[1:Nsex,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}

#------------------------Ende Partial-Pooling fuer sport-----------------------------#
}
"


#Modell fuer Variablen aufrufen
modell.pred = run.jags(model=modellp,
                      data=dfz,
                      burnin = 5000,
                      monitor = c("hg.hat"),
                      n.chains = 3,
                      sample= 10000,
                      thin=2,
                      inits = list(list(.RNG.name="base::Mersenne-Twister", .RNG.seed=456),
                                   list(.RNG.name="base::Super-Duper", .RNG.seed=123),
                                   list(.RNG.name="base::Wichmann-Hill", .RNG.seed=789)),
                      method = "parallel")
summary(df)
pred.samples = as.matrix(modell.pred$mcmc)
pred.summary = view(modell.pred)

sum(pred.summary[, "MC%ofSD"] >= 1)
#Keine MC%ofSD Werte über 1. Gut!

# Frage 5: Welche Hemoglobin-Konzentration wird fÃ¼r eine Tennis-Spielerin mit 175cm GrÃ¶ÃŸe,
#          60 kg Magermasse und ansonsten mittleren Werten vorhergesagt?
#          Geben Sie auch ein 75%-Intervall an.

# Relevante Daten im Datensatz:
# - hg ... Hemoglobin-Konzentration (Einheit: g/dl)
# - wcc ... Anzahl weiße Blutkörperchen (Einheit: 1/nl)
# - ht ... Größe (cm)
# - sex ... Frau (f) oder Mann (m)
# - lbm ...  Magermasse des Körpers, auch fettfreie Masse genannt (kg)
# - pcbfat ... Anteil Körperfett (%)
# - sport ... Sportart