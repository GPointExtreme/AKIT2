# AKIT2 SS18, Nachklausur (2. Termin), 19.9.2018
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

df <- read.csv('C:\\Users\\Dominik\\Downloads\\real-estate.csv')

# Wir wollen eine neue Dienstleistung im Immobiliensektor anbieten: online den Preis
# eines Hauses schätzen lassen. Wir starten in den USA und haben folgende Informationen
# von Häusern:

# - price ... Verkaufspreis in US$
# - bedrooms ... Anzahl der Schlafzimmer
# - bathrooms ... Anzahl der Badezimmer bzw. WCs (gelten als 1/2 Badezimmer :o)
# - garage ... Anzahl der Garagenplätze
# - area ... Wohnfläche in m^2
# - location ... Rural (=Land), Suburb (=Vorort), Urban (=Stadt)
# - decade ... Baujahr des Hauses (in den USA ist es Üblich auch sehr alte Häuser zu kaufen)

#abhängige Variable = price
#Gruppenvariablen1 = location
#Gruppenvariablen2 = decade

view(df)
mapply(hist,as.data.frame(df),main=colnames(df))
#price ist linkslastig. Sollten wir genauer anschauen.
1500000/89900 #=17
465/53 #=8.7
#Das Verhältnis von price hebt sich von den anderen Variablen ab.
hist(df$price)
hist(log2(df$price))
#log-transformiert sehen Daten nun Normalverteilt aus.
df$price.log = log2(df$price)

#z-Transformieren
dfz = prepare.df.bayes(df, drop.originals = TRUE)
summary(dfz)

# Frage 1: Verwenden Sie nur die ersten 750 Datensätze.
#          Welchen Einfluss haben die verschiedenen Variablen auf den Preis?
#          (z.B. um wie viel steigt der Preis, wenn ich 1 Schlafzimmer mehr haben will?)
dfzn = dfz[1:750,]

#location              = ist die Gruppenvariable nach der die Varianz gefragt ist was später auch Simga im Monitor darstellt  
#decade              = 2 Gruppenvariable
#price.log  = stellt die abhaengige Variable dar 
#bedrooms            = erste metrische Variable
#bathrooms            = zweite metrische Varibale
#garage            = dritte metrische Varibale
#area            = vierte metrische Variable


#--------------------------Beginn von Modell definition---------------------------------#

modell = "
data {
N <- length(price.log[])
Nlocation <- max(location)
Ndecade <- max(decade)
}


model {

for (i in 1:N) {


price.log[i] ~ dnorm(mu[i], 1/sigma[location[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptdecade[decade[i]] +           		# Gruppe fuer decade / intercept
interceptlocation[location[i]] +                        # Gruppe fuer location /intercept
beta.bedrooms*bedrooms[i] +
beta.bathrooms*bathrooms[i] +
beta.garage*garage[i] +
beta.area*area[i]

#----------------------------Beginn von Vorhersage-----------------------------------#

#price.log.hat[i] ~ dnorm(mu[i], 1/sigma[location[i]]^2) #ist gleich erste Zeile im Modell

#-----------------------------Ende der Vorhersage------------------------------------#
}

#-----------------------------------Priors-------------------------------------------#


for(l in 1:Nlocation){
sigma[l]~dexp(3/1)
}
#--------------------------ACHTUNG Partial Pooling Gruppe 1-----------------------------------#
#interceptlocation[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#------------------------------------------------------------------------------------#


beta.bedrooms ~ dnorm(0,1/1^2)
beta.bathrooms ~ dnorm(0,1/1^2)
beta.garage ~ dnorm(0,1/1^2) 
beta.area ~ dnorm(0,1/1^2)


for(l in 1:Nlocation) 
{
  interceptlocation[l] ~ dnorm(0, 1/1^2)
}


# location hat nur 3 Werte: kein Pooling
# alle locations bekommen das selbe sgima & intercept
#VT: wenn 7 locations wären, dann muesste man sonst 7 Zeilen fuer intercept und 7 fuer sigma schreiben

#--------------------------------Ende von Priors-------------------------------------#

#-------------------------Partial-Pooling fuer decade-------------------------------#


interceptdecade.mu ~ dnorm(0,1/1^2)  #entfernen wenn kein partial pooling
interceptdecade.sigma ~ dexp(1)      #entfernen wenn kein partial pooling

for (d in 1:Ndecade) {
interceptdecade[d] ~ dnorm(interceptdecade.mu, 1/interceptdecade.sigma^2) #wenn kein Partial Pooling dann nach: .mu, 1 #/....
}


#--------stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNdecadeeitig)

for (l in 1:Nlocation) {
for (d in 1:Ndecade) {
mtx[l,d] <- interceptlocation[l] +interceptdecade[d]

}
}

intercept <- mean(mtx[1:Nlocation,1:Ndecade])
for(l in 1:Nlocation)
{
  alphalocation[l] <- mean(mtx[l,1:Ndecade]) - intercept
}
for (d in 1:Ndecade) 
{
  Gammadecade[d] <- mean(mtx[1:Nlocation,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}

#------------------------Ende Partial-Pooling fuer decade-----------------------------#
}
"

#Modell fuer Variablen aufrufen
modell.fit = run.jags(model=modell,
                      data=dfzn,
                      burnin = 5000,
                      monitor = c("intercept", "alphalocation", "Gammadecade", "sigma",
                                  "beta.bedrooms", "beta.bathrooms", "beta.garage", "beta.area"),
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

max(fit.summary[,"MC%ofSD"])
sum(fit.summary[,"SSeff"]<=((modell.fit$sample*3)/100)*10)
#MC%ofSD ist unter 1. Sehg gut.
#SSeFF < 10%. Sehr gut.

diagMCMC(modell.fit$mcmc,"beta.bathrooms")
diagMCMC(modell.fit$mcmc,"beta.area")

#          Welchen Einfluss haben die verschiedenen Variablen auf den Preis?
#          (z.B. um wie viel steigt der Preis, wenn ich 1 Schlafzimmer mehr haben will?)
plotcoef(modell.fit, "beta")
#area und bathrooms sind im + Bereich und garage und bedrooms eher im - Bereich.

#Ist der Einfluss von mehreren metrischen Variablen auf die abhängige Variable gefragt, dann
#müssen diese auf eine vergleichbare Ebene gebracht werden. Pro sd() schafft dies!
mean(fit.samples[,"beta.metrischeVariable"]*sd(df$abhängigeVariable))
sd(df$metrischeVariable)
#AW: Pro sd erhöhung von metrische Variable (sd von metrischeVariable), erhöt/verringert sich 
#abhängigeVariable um mean(Zeile224).



# Frage 2: Ist der Unterschied zwischen Suburb und Urban preislich relevant?
#          Geben Sie jeweils Wahrscheinlichkeiten für Ihre Einschätzungen an.
#"intercept", "alphalocation", "Gammadecade", "sigma",
#"beta.bedrooms", "beta.bathrooms", "beta.garage", "beta.area"

plotcoef(modell.fit, c("alphalocation")) #Hat nur Nummern
table(df$location)                       #herrausfinden welche Nummer was ist
diff = (fit.samples[,"alphalocation[2]"]-fit.samples[,"alphalocation[3]"])
plotPost(diff, compVal = 0, credMass = 0.95)
#Umrechnen wenn Daten z- und die abhängige Variable log-transformiert sind!
diff_z_log = 2^(diff*sd(df$price.log)) #Keine Division mit sd() bei nominalen Daten!
plotPost(diff_z_log, compVal = 1, credMass = 0.95)
#In 95% der Fälle liegt der Unterschied zwischen Suburb und Urban zwischen einem HDI von 
#0.885 und 1.09 (Durchschnittswert = 0.988)
#Da die 1 (wie null wenn Log transformiert) eingeschlossen ist, ist der Unterschied nicht 
#signifikant

#          Sind Häuser aus den 1950er signifikant teurer als jene aus den
#          1960ern oder 1970ern?
plotcoef(modell.fit, c("Gammadecade")) #Hat nur Nummern
table(df$decade)                       #herrausfinden welche Nummer was ist
diff = (fit.samples[,"Gammadecade[7]"]-fit.samples[,"Gammadecade[8]"])
plotPost(diff, compVal = 0, credMass = 0.95)
#Umrechnen wenn Daten z- und die abhängige Variable log-transformiert sind!
diff_z_log = 2^(diff*sd(df$price.log)) #Keine Division mit sd() bei nominalen Daten!
plotPost(diff_z_log, compVal = 1, credMass = 0.95)
#In 95% der Fälle liegt der Unterschied zwischen 1950ern und 1960ern zwischen einem HDI von 
#1.02 und 1.35 (Durchschnittswert = 1.16)
#Da die 1 (wie null wenn Log transformiert) nicht eingeschlossen ist, ist der Unterschied 
#signifikant

plotcoef(modell.fit, c("Gammadecade")) #Hat nur Nummern
table(df$decade)                       #herrausfinden welche Nummer was ist
diff = (fit.samples[,"Gammadecade[7]"]-fit.samples[,"Gammadecade[9]"])
plotPost(diff, compVal = 0, credMass = 0.95)
#Umrechnen wenn Daten z- und die abhängige Variable log-transformiert sind!
diff_z_log = 2^(diff*sd(df$price.log)) #Keine Division mit sd() bei nominalen Daten!
plotPost(diff_z_log, compVal = 1, credMass = 0.95)
#In 95% der Fälle liegt der Unterschied zwischen 1950ern und 1970ern zwischen einem HDI von 
#1.1 und 1.5 (Durchschnittswert = 1.28)
#Da die 1 (wie null wenn Log transformiert) nicht eingeschlossen ist, ist der Unterschied 
#signifikant

# Frage 3: Hat das Modell eine gute Vorhersagekraft bzw. ist das Modell eine gute
#          Beschreibung des vorhandenen Datensatzes?

modellp = "
data {
N <- length(price.log[])
Nlocation <- max(location)
Ndecade <- max(decade)
}


model {

for (i in 1:N) {


price.log[i] ~ dnorm(mu[i], 1/sigma[location[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptdecade[decade[i]] +           		# Gruppe fuer decade / intercept
interceptlocation[location[i]] +                        # Gruppe fuer location /intercept
beta.bedrooms*bedrooms[i] +
beta.bathrooms*bathrooms[i] +
beta.garage*garage[i] +
beta.area*area[i]

#----------------------------Beginn von Vorhersage-----------------------------------#

price.log.hat[i] ~ dnorm(mu[i], 1/sigma[location[i]]^2) #ist gleich erste Zeile im Modell

#-----------------------------Ende der Vorhersage------------------------------------#
}

#-----------------------------------Priors-------------------------------------------#


for(l in 1:Nlocation){
sigma[l]~dexp(3/1)
}
#--------------------------ACHTUNG Partial Pooling Gruppe 1-----------------------------------#
#interceptlocation[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#------------------------------------------------------------------------------------#


beta.bedrooms ~ dnorm(0,1/1^2)
beta.bathrooms ~ dnorm(0,1/1^2)
beta.garage ~ dnorm(0,1/1^2) 
beta.area ~ dnorm(0,1/1^2)


for(l in 1:Nlocation) 
{
  interceptlocation[l] ~ dnorm(0, 1/1^2)
}


# location hat nur 3 Werte: kein Pooling
# alle locations bekommen das selbe sgima & intercept
#VT: wenn 7 locations wären, dann muesste man sonst 7 Zeilen fuer intercept und 7 fuer sigma schreiben

#--------------------------------Ende von Priors-------------------------------------#

#-------------------------Partial-Pooling fuer decade-------------------------------#


interceptdecade.mu ~ dnorm(0,1/1^2)  #entfernen wenn kein partial pooling
interceptdecade.sigma ~ dexp(1)      #entfernen wenn kein partial pooling

for (d in 1:Ndecade) {
interceptdecade[d] ~ dnorm(interceptdecade.mu, 1/interceptdecade.sigma^2) #wenn kein Partial Pooling dann nach: .mu, 1 #/....
}


#--------stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNdecadeeitig)

for (l in 1:Nlocation) {
for (d in 1:Ndecade) {
mtx[l,d] <- interceptlocation[l] +interceptdecade[d]

}
}

intercept <- mean(mtx[1:Nlocation,1:Ndecade])
for(l in 1:Nlocation)
{
  alphalocation[l] <- mean(mtx[l,1:Ndecade]) - intercept
}
for (d in 1:Ndecade) 
{
  Gammadecade[d] <- mean(mtx[1:Nlocation,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}

#------------------------Ende Partial-Pooling fuer decade-----------------------------#
}
"
#------------------------------------Vorherasage-------------------------------------#

modell.pred = run.jags(model=modellp,
                       data=dfzn,
                       burnin = 5000,
                       monitor = c("price.log.hat"),
                       n.chains = 3,
                       sample= 3000,
                       #thin=2,
                       inits = list(list(.RNG.name="base::Mersenne-Twister", .RNG.seed=456),
                                    list(.RNG.name="base::Super-Duper", .RNG.seed=123),
                                    list(.RNG.name="base::Wichmann-Hill", .RNG.seed=789)),
                       method = "parallel")
summary(df)
pred.samples = as.matrix(modell.pred$mcmc)
pred.summary = view(modell.pred)

#Wenn MC%ofSD >2 dann nehmen wir die raus und sehen uns diese genauer an 
#selbes gilt für Werte von SSeFF <10% von n.chain*sample_size sind

sum(pred.summary[,"MC%ofSD"]>=2)
sum(pred.summary[,"SSeff"]<=((modell.pred$sample*3)/100)*10)

diagMCMC(modell.pred$mcmc,"price.log.hat[2]")
diagMCMC(modell.pred$mcmc,"price.log.hat[95]")
diagMCMC(modell.pred$mcmc,"price.log.hat[165]")
#Sieht alles gut aus.

#-----------------------------Vergleich

# Fragestellung:  Hat das Modell eine gute Vorhersagekraft bzw. ist das Modell eine gute
#                 Beschreibung des vorhandenen Datensatzes?

rmin = apply(pred.samples, 1, min)
rmax = apply(pred.samples, 1, max)
rmed = apply(pred.samples, 1, median)
plotPost(rmin, compVal = min(dfzn$price.log)) #Sieht gut aus.
plotPost(rmax, compVal = max(dfzn$price.log)) #Sieht gut aus.
plotPost(rmed, compVal = median(dfzn$price.log)) #Schließt 0 mit ein. Nicht signifikant.
#Schließt es 0 mit ein ist es nicht signifikant!

# graphischer Check
breaks = seq(-6,6, length.out = 40)
par(mfrow=c(3,3))
hist(dfzn$price.log, col="darkblue", breaks=breaks)
for (i in 1:8) {
  hist(pred.samples[sample.int(nrow(pred.samples),1),], col="lightblue", breaks=breaks)
}
#Man kann hier schon unterschiede sehen. Median wird nicht so gut vorhergesagt.

#-------------------------------Ende Vorherasage-------------------------------------#

# Frage 4: Berechnen Sie für die letzten 143 Datensätze eine Preis-Vorhersage.
#          Wie viele Vorhersagen enthalten im 50%-Intervall den Echtpreis?
#          Wie groß (=US$) ist das 50%-Intervall der Vorhersagen im Schnitt?
#          Hat das Modell also eine gute Vorhersagekraft?
#
#          Tipp: ein Dataframe mit allen Daten Übergeben, aber zwei Likelihood-Schleifen:
#          einmal über die "Echtdaten" (750 Datensätze) und einmal über die Testdaten (143 Datensätze).
#          Auf passenden Array-Index aufpassen!
modell1 = "
data {
N <- length(price.log[])
Nlocation <- max(location)
Ndecade <- max(decade)
}


model {

for (i in 1:750) {


price.log[i] ~ dnorm(mu[i], 1/sigma[location[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptdecade[decade[i]] +           		# Gruppe fuer decade / intercept
interceptlocation[location[i]] +                        # Gruppe fuer location /intercept
beta.bedrooms*bedrooms[i] +
beta.bathrooms*bathrooms[i] +
beta.garage*garage[i] +
beta.area*area[i]

#----------------------------Beginn von Vorhersage-----------------------------------#

#price.log.hat[i] ~ dnorm(mu[i], 1/sigma[location[i]]^2) #ist gleich erste Zeile im Modell

#-----------------------------Ende der Vorhersage------------------------------------#
}
for (i in 751:893) {


price.log.hat[i-750] ~ dnorm(mu[i], 1/sigma[location[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptdecade[decade[i]] +           		# Gruppe fuer decade / intercept
interceptlocation[location[i]] +                        # Gruppe fuer location /intercept
beta.bedrooms*bedrooms[i] +
beta.bathrooms*bathrooms[i] +
beta.garage*garage[i] +
beta.area*area[i]
}

#-----------------------------------Priors-------------------------------------------#


for(l in 1:Nlocation){
sigma[l]~dexp(3/1)
}
#--------------------------ACHTUNG Partial Pooling Gruppe 1-----------------------------------#
#interceptlocation[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#------------------------------------------------------------------------------------#


beta.bedrooms ~ dnorm(0,1/1^2)
beta.bathrooms ~ dnorm(0,1/1^2)
beta.garage ~ dnorm(0,1/1^2) 
beta.area ~ dnorm(0,1/1^2)


for(l in 1:Nlocation) 
{
  interceptlocation[l] ~ dnorm(0, 1/1^2)
}


# location hat nur 3 Werte: kein Pooling
# alle locations bekommen das selbe sgima & intercept
#VT: wenn 7 locations wären, dann muesste man sonst 7 Zeilen fuer intercept und 7 fuer sigma schreiben

#--------------------------------Ende von Priors-------------------------------------#

#-------------------------Partial-Pooling fuer decade-------------------------------#


interceptdecade.mu ~ dnorm(0,1/1^2)  #entfernen wenn kein partial pooling
interceptdecade.sigma ~ dexp(1)      #entfernen wenn kein partial pooling

for (d in 1:Ndecade) {
interceptdecade[d] ~ dnorm(interceptdecade.mu, 1/interceptdecade.sigma^2) #wenn kein Partial Pooling dann nach: .mu, 1 #/....
}


#--------stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNdecadeeitig)

for (l in 1:Nlocation) {
for (d in 1:Ndecade) {
mtx[l,d] <- interceptlocation[l] +interceptdecade[d]

}
}

intercept <- mean(mtx[1:Nlocation,1:Ndecade])
for(l in 1:Nlocation)
{
  alphalocation[l] <- mean(mtx[l,1:Ndecade]) - intercept
}
for (d in 1:Ndecade) 
{
  Gammadecade[d] <- mean(mtx[1:Nlocation,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}

#------------------------Ende Partial-Pooling fuer decade-----------------------------#

}
"

#Modell fuer Variablen aufrufen
modell.fit1 = run.jags(model=modell1,
                      data=dfz,
                      burnin = 5000,
                      monitor = c("intercept", "alphalocation", "Gammadecade", "sigma",
                                  "beta.bedrooms", "beta.bathrooms", "beta.garage", "beta.area",
                                  "price.log.hat"),
                      n.chains = 3,
                      sample= 10000,
                      thin=2,
                      inits = list(list(.RNG.name="base::Mersenne-Twister", .RNG.seed=456),
                                   list(.RNG.name="base::Super-Duper", .RNG.seed=123),
                                   list(.RNG.name="base::Wichmann-Hill", .RNG.seed=789)),
                      method = "parallel")
summary(df)
fit1.samples = as.matrix(modell.fit1$mcmc)
fit1.summary = view(modell.fit1)

max(fit1.summary[,"MC%ofSD"])
sum(fit1.summary[,"SSeff"]<=((modell.fit1$sample*3)/100)*10)
#MC%ofSD ist unter 1. Sehr gut.
#SSeFF < 10%. Sehr gut.

diagMCMC(modell.fit1$mcmc,"price.log.hat[1]")
diagMCMC(modell.fit1$mcmc,"price.log.hat[143]")
#Sieht gut aus unsere predictions.

#          Wie viele Vorhersagen enthalten im 50%-Intervall den Echtpreis?
dim(fit1.samples)
fit1.summary.hat = fit1.samples[, -(1:23)]


#          Wie groß (=US$) ist das 50%-Intervall der Vorhersagen im Schnitt?
#          Hat das Modell also eine gute Vorhersagekraft?


# Allgemeine Tipps:
# - Lesen Sie die Angabe genau durch und setzen Sie die geforderten Punkte um.
#   FleiÃŸaufgaben kosten Zeit, bringen aber keine zusÃ¤tzlichen Punkte.
# - Die Interpretation (wo gefordert) muss so geschrieben sein, dass jede/jeder sie
#   verstehen kann (auch ohne mathematischen Hintergrund). Bedeutet unter anderem:
#   passend rÃ¼cktransformieren falls transformiert wurde.
# - Die "ersten X DatensÃ¤tze" meint wirklich *die ersten*. Keine Zufallsauswahl!
