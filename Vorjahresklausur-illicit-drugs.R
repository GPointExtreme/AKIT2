# AKIT2 SS18, Nachklausur (3. Termin), 25.10.2018
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

df <- read.csv('C:\\Users\\Dominik\\Downloads\\illicit-drugs.csv')

# Wir sind in den 1980ern bei Miami Vice angestellt, um den Drogenmarkt zu Überwachen.
# Folgende Daten zu Verkaufspreisen sind uns bekannt:

# - avg.usd ... Durchschnittlicher Verkaufspreis in US$
# - avg.purity ... Reinheitsgrad der Drogen (0..100 in Prozent)
# - quantity ... Verkaufte Menge (in Gramm)
# - year ... Jahr
# - drug ... Drogentyp

# Schritt 0:
# Wir sind am Preis pro Gramm interessiert, nicht am Gesamtpreis.
# Berechnen Sie daher eine zusätzliche Variable die Preis (US$) je Gramm angibt.
df$price = df$avg.usd/df$quantity
hist(df$price)
#Sehr viele Datensätze im niedrigen Bereich. 
#Erster Hinweis das wir eventuell log-transformieren müssen.

#Verhältnisse prüfen
view(df)
1800/0.4 #price max-min
#4500
1701/0.025 #quantity max-min
#68040
100/1 #avg.purity max-min
#100
10.814/-1.322 #price.log max-min
#-8.18003
#Verhältnisse sehen nicht gut aus. Modell rechnen ohne Log war schlecht. Nun rechnen wir es
#zum zweiten Mal mit Log.

hist(log2(df$price))
df$price.log = log2(df$price)

# Zudem ist der Reinheitsgrad für Haschisch/Marihuana immer 100% und verzerrt uns
# das Regressionsmodell. Daher: setzen Sie den Reinheitsgrad für Haschisch/Marihuana
# auf 0 bzw. falls Sie z-Skalieren, setzen Sie den z-Wert(!) auf 0.
# Betrifft alle Drogendatensätze für die gilt:
# drug %in% c("Hashish", "Marijuana-Domestic", "Marijuana-Imported","Marijuana-Sinsemilla")

dfz = prepare.df.bayes(df, drop.originals = TRUE)
summary(dfz)
dfz$avg.purity[dfz$drug %in% c(3,8,9,10)]=0

# Schritt 1: Erstellen Sie ein Modell welches den Preis pro Gramm vorhersagt.
#            Wir gehen davon aus, dass sich Drogen auch ihre Varianz betreffend unterscheiden.
#            Konvergiert das Modell?

#drug              = ist die Gruppenvariable nach der die Varianz gefragt ist was später auch Simga im Monitor darstellt  
#year              = 2 Gruppenvariable
#price.log  = stellt die abhaengige Variable dar 
#quantity            = erste metrische Variable
#avg.purity            = zweite metrische Varibale


#--------------------------Beginn von Modell definition---------------------------------#

modell = "
data {
N <- length(price.log[])
Ndrug <- max(drug)
Nyear <- max(year)
}


model {

for (i in 1:N) {


price.log[i] ~ dnorm(mu[i], 1/sigma[drug[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptyear[year[i]] +           		# Gruppe fuer year / intercept
interceptdrug[drug[i]] +                        # Gruppe fuer drug /intercept
beta.quantity*quantity[i] +
beta.avg.purity*avg.purity[i]

#----------------------------Beginn von Vorhersage-----------------------------------#

#price.log.hat[i] ~ dnorm(mu[i], 1/sigma[drug[i]]^2) #ist gleich erste Zeile im Modell

#-----------------------------Ende der Vorhersage------------------------------------#
}

#-----------------------------------Priors-------------------------------------------#


for(l in 1:Ndrug){
sigma[l]~dexp(3/1)
}
#--------------------------ACHTUNG Partial Pooling Gruppe 1-----------------------------------#
#interceptdrug[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#------------------------------------------------------------------------------------#


beta.quantity ~ dnorm(0,1/1^2)
beta.avg.purity ~ dnorm(0,1/1^2)


for(l in 1:Ndrug) 
{
  interceptdrug[l] ~ dnorm(0, 1/1^2)
}


# drug hat nur 3 Werte: kein Pooling
# alle drugs bekommen das selbe sgima & intercept
#VT: wenn 7 drugs wären, dann muesste man sonst 7 Zeilen fuer intercept und 7 fuer sigma schreiben

#--------------------------------Ende von Priors-------------------------------------#

#-------------------------Partial-Pooling fuer year-------------------------------#


interceptyear.mu ~ dnorm(0,1/1^2)  #entfernen wenn kein partial pooling
interceptyear.sigma ~ dexp(1)      #entfernen wenn kein partial pooling

for (d in 1:Nyear) {
interceptyear[d] ~ dnorm(interceptyear.mu, 1/interceptyear.sigma^2) #wenn kein Partial Pooling dann nach: .mu, 1 #/....
}


#--------stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNyeareitig)

for (l in 1:Ndrug) {
for (d in 1:Nyear) {
mtx[l,d] <- interceptdrug[l] +interceptyear[d]

}
}

intercept <- mean(mtx[1:Ndrug,1:Nyear])
for(l in 1:Ndrug)
{
  alphadrug[l] <- mean(mtx[l,1:Nyear]) - intercept
}
for (d in 1:Nyear) 
{
  Gammayear[d] <- mean(mtx[1:Ndrug,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}

#------------------------Ende Partial-Pooling fuer year-----------------------------#
}
"


#Modell fuer Variablen aufrufen
modell.fit = run.jags(model=modell,
                      data=dfz,
                      burnin = 5000,
                      monitor = c("intercept", "alphadrug", "Gammayear", "sigma",
                                  "beta.quantity", "beta.avg.purity"),
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

#Wenn MC%ofSD >2 dann nehmen wir die raus und sehen uns diese genauer an 
#selbes gilt für Werte von SSeFF <10% von n.chain*sample_size sind

diagMCMC(modell.fit$mcmc,"beta.avg.purity")
diagMCMC(modell.fit$mcmc,"alphadrug[8]")
diagMCMC(modell.fit$mcmc,"beta.quantity")
#Ohne price.log waren die Werte sehr schlecht. Nun konvertiert das Modell gut!

#Schritt 2: Beantworten Sie folgende Fragen (geben Sie auch Intervalle und Wahrscheinlichkeiten an):
#   - Waren Drogen im Jahr 1989 deutlich billiger als 1991?

plotcoef(modell.fit, c("Gammayear")) #Hat nur Nummern
table(df$year)                       #herrausfinden welche Nummer was ist
diff = (fit.samples[,"Gammayear[6]"]-fit.samples[,"Gammayear[7]"])
plotPost(diff, compVal = 0, credMass = 0.95)
#Das HDI von 95% [-0.103 bis 0.0326] zeigt das die 0 eingeschlossen ist und daher ist unser 
#Unterschied nicht signifikant.

#Umrechnen wenn Daten z- und die abhängige Variable log-transformiert sind!
diff_z_log = 2^(diff*sd(df$price.log)) #Keine Division mit sd() bei nominalen Daten!
plotPost(diff_z_log, compVal = 1, credMass = 0.95)
#In 95% der Fälle liegt der Unterschied zwischen Year 1989 und Year 1991 zwischen 
#einem HDI von 0.84 und 1.06 (Durchschnittswert = 0.933).
#Da die 1 (wie null wenn Log transformiert) eingeschlossen ist, ist der Unterschied  
#nicht signifikant.

#   - Gibt es eine Heroin-Sorte, die deutlich teurer ist als die anderen?

plotcoef(modell.fit, c("alphadrug")) #Hat nur Nummern
table(df$drug)                       #herrausfinden welche Nummer was ist
diff = (fit.samples[,"alphadrug[6]"]-fit.samples[,"alphadrug[7]"])
plotPost(diff, compVal = 0, credMass = 0.95)
#Das HDI von 95% [-0.218 bis 0.0237] zeigt das die 0 nicht eingeschlossen ist und daher ist unser 
#Unterschied signifikant.

#Umrechnen wenn Daten z- und die abhängige Variable log-transformiert sind!
diff_z_log = 2^(diff*sd(df$price.log)) #Keine Division mit sd() bei nominalen Daten!
plotPost(diff_z_log, compVal = 1, credMass = 0.95)
#In 95% der Fälle liegt der Unterschied zwischen Heroin-White und Heroin-Tar zwischen 
#einem HDI von 0.689 und 0.957 (Durchschnittswert = 0.811).
#Da die 1 (wie null wenn Log transformiert) nicht eingeschlossen ist, ist der Unterschied  
# signifikant.

#   - Gibt es einen Mengen-Rabatt (quantity), wenn ja, wie groß ist dieser?

#Umrechnen, wenn Daten z-transformiert sind und die abhängige Variable log-transformiert ist
plotPost((2^(fit.samples[, "beta.quantity"]*sd(df$price.log)
            /sd(df$quantity)))^100, credMass = 0.95)
#Wir bemerken einen minimalen unterschied. Muss hoch 100 genommen werden um Unterschied zu
#sehen.
#Bei erhöhung der Menge um 100g verringert sich der Preis auf das 0.899-fache.
#Das wäre 11% Rabatt pro 100g.

#   - Welche Drogen haben die größte Varianz im Verkaufswert?

plotcoef(modell.fit, c("sigma")) #Hat nur Nummern
table(df$drug)                   #herrausfinden welche Nummer was ist
#Wir sehen das die Varianz bei 4 = Heroin am größten ist.

# Schritt 3: Hat das Modell eine gute Vorhersagekraft bzw. ist das Modell eine gute
#            Beschreibung des vorhandenen Datensatzes?

modellp = "
data {
N <- length(price.log[])
Ndrug <- max(drug)
Nyear <- max(year)
}


model {

for (i in 1:N) {


price.log[i] ~ dnorm(mu[i], 1/sigma[drug[i]]^2)  #!!Grup-pe1  ACHTUNG: hier könnte sein das die Gruppe 2 genommen werden muss jenachdem wir man oben die Gruppe mit der Varianz definiert

mu[i] <- interceptyear[year[i]] +           		# Gruppe fuer year / intercept
interceptdrug[drug[i]] +                        # Gruppe fuer drug /intercept
beta.quantity*quantity[i] +
beta.avg.purity*avg.purity[i]

#----------------------------Beginn von Vorhersage-----------------------------------#

price.log.hat[i] ~ dnorm(mu[i], 1/sigma[drug[i]]^2) #ist gleich erste Zeile im Modell

#-----------------------------Ende der Vorhersage------------------------------------#
}

#-----------------------------------Priors-------------------------------------------#


for(l in 1:Ndrug){
sigma[l]~dexp(3/1)
}
#--------------------------ACHTUNG Partial Pooling Gruppe 1-----------------------------------#
#interceptdrug[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#------------------------------------------------------------------------------------#


beta.quantity ~ dnorm(0,1/1^2)
beta.avg.purity ~ dnorm(0,1/1^2)


for(l in 1:Ndrug) 
{
  interceptdrug[l] ~ dnorm(0, 1/1^2)
}


# drug hat nur 3 Werte: kein Pooling
# alle drugs bekommen das selbe sgima & intercept
#VT: wenn 7 drugs wären, dann muesste man sonst 7 Zeilen fuer intercept und 7 fuer sigma schreiben

#--------------------------------Ende von Priors-------------------------------------#

#-------------------------Partial-Pooling fuer year-------------------------------#


interceptyear.mu ~ dnorm(0,1/1^2)  #entfernen wenn kein partial pooling
interceptyear.sigma ~ dexp(1)      #entfernen wenn kein partial pooling

for (d in 1:Nyear) {
interceptyear[d] ~ dnorm(interceptyear.mu, 1/interceptyear.sigma^2) #wenn kein Partial Pooling dann nach: .mu, 1 #/....
}


#--------stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNyeareitig)

for (l in 1:Ndrug) {
for (d in 1:Nyear) {
mtx[l,d] <- interceptdrug[l] +interceptyear[d]

}
}

intercept <- mean(mtx[1:Ndrug,1:Nyear])
for(l in 1:Ndrug)
{
  alphadrug[l] <- mean(mtx[l,1:Nyear]) - intercept
}
for (d in 1:Nyear) 
{
  Gammayear[d] <- mean(mtx[1:Ndrug,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}

#------------------------Ende Partial-Pooling fuer year-----------------------------#
}
"


#Modell fuer Variablen aufrufen
modell.pred = run.jags(model=modellp,
                      data=dfz,
                      burnin = 1000,
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
#Keine MC%ofSD Werte über 2 oder SSeff unter 10%.

diagMCMC(modell.pred$mcmc,"price.log.hat[159]")
diagMCMC(modell.pred$mcmc,"price.log.hat[1123]")
#Beide Modelle sehen gut aus!

# Fragestellung:  Hat das Modell eine gute Vorhersagekraft bzw. ist das Modell eine gute
#                 Beschreibung des vorhandenen Datensatzes?

rmin = apply(pred.samples, 1, min)
rmax = apply(pred.samples, 1, max)
rmed = apply(pred.samples, 1, median)
plotPost(rmin, compVal = min(dfz$price.log)) #Sieht gut aus
plotPost(rmax, compVal = max(dfz$price.log)) #Sieht gut aus
plotPost(rmed, compVal = median(dfz$price.log)) #Ist hier etwas außerhalb des 95% HDI! 2.4% haben
#einen erhöhten median.
#Keiner hat 0 eingeschlossen also sind alle signifikant.

# graphischer Check
breaks = seq(-4,4, length.out = 40)
par(mfrow=c(3,3))
hist(dfz$price.log, col="darkblue", breaks=breaks)
for (i in 1:8) {
  hist(pred.samples[sample.int(nrow(pred.samples),1),], col="lightblue", breaks=breaks)
}
#Schaut richtig geil aus. Vorhersagekraft ist also sehr gut.

# Schritt 4: Welcher Preis je Gramm wird für 1kg Heroin-White im Jahr 1988 vorhergesagt, wenn
#            die Reinheit 100%, 50% und 20% beträgt?
#            Welcher Preis je Gramm wird für 100g Kokain im Jahr 1988 vorhergesagt, wenn
#            die Reinheit 100%, 50% und 20% beträgt?
#
# Auf Basis dieser Zahlen: würden Sie als Dealer Ihre Drogen strecken oder rein verkaufen?
# Falls strecken: wie weit würden Sie diese strecken? Untermauern Sie Ihre Antwort mit
# Wahrscheinlichkeiten.

#Vorhersage bauen
mypredict = function(drug.int, year.int, quantity, avg.purity, Variable3, Variable4)
{
  price.log.z.mu = fit.samples[,"intercept"] +
    fit.samples[,"beta.quantity"]*quantity +
    fit.samples[,"beta.avg.purity"]*avg.purity +
    fit.samples[,1+drug.int]+ #1 wie folgt ersetzen  
    #Schritt1: colnames(fit.samples) auf rufen und ansehen an welcher Stelle die drug beginnt 
    #Schritt2: die erhaltene Stelle -1 rechen Bsp: drug beginnt an Position 2 somit würde stehen: 1+drug.int
    fit.samples[,12+year.int]#4 wie folgt ersetzen  
  #Schritt1: colnames(fit.samples) auf rufen und ansehen an welcher Stelle die year beginnt 
  #Schritt2: die erhaltene Stelle -1 rechen Bsp: year beginnt an Position 5 somit würde stehen: 4+year.int
  price.log.z.sigma = fit.samples[,19+drug.int] #19 wie folgt ersetzen  
  #Schritt1: colnames(fit.samples) auf rufen und ansehen an welcher Stelle Sigma beginnt 
  #Schritt2: die erhaltene Stelle -1 rechen Bsp: Sigma beginnt an Position 20 somit würde stehen: 19+drug.int
  price.log.z = rnorm(nrow(fit.samples), price.log.z.mu, price.log.z.sigma)
  price.log = inv.zscale.other(price.log.z, dfz$price.log)
  return(price.log)
}

#Welcher Preis je Gramm wird für 1kg Heroin-White im Jahr 1988 vorhergesagt, wenn
#            die Reinheit 100%, 50% und 20% beträgt?

#ACHTUNG: Für Gruppen müssen Integar eingegeben werden (weil z Transformiert) levels(df$year)
#Wert durch gefragte Werte ersetzen
price.log_predict = mypredict(7, 5, zscale.other(1000, dfz$quantity), 
                              zscale.other(100, dfz$avg.purity))
price_predict = 2^price.log_predict #ACHTUNG wenn price.log log Transformiert ist einkommentieren
plotPost(price_predict, credMass = 0.95)

#100% = 69.1

# Tipp: Sie können weitere Datensätze dem Datenframe wie folgt hinzufügen, z.B. 1001. Zeile:
# df[1001,] = list(wert_spalte1, wert_spalte2, wert_spalte3, wert_spalte4, ...)
# Passend z-Skalieren nicht vergessen (falls Sie mit skalierten Daten arbeiten.)



# Quelle der Daten:
# Caulkins, JP, and Padman, R: Quantity discounts and quality premia for illicit drugs.
# JASA Sept 1993, Vol. 88, #423, pp. 748-757.
