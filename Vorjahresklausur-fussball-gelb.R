# AKIT2 SS17, Hauptklausur, 23.6.2017
library(ggplot2)
library(arm)
library(car)
library(corrplot)
library(effects)
library(lmtest)
library(psych)
library(pwr)
library(ROCR)
library(runjags)
library(coda)
library(VIM)
rjags::load.module("glm")
library(akit2)

df <- read.csv('C:\\Users\\Dominik\\Downloads\\fussball-gelb.csv')

# Aus vier verschiedenen FuÃŸball-Liegen liegen Ergebnisse fÃ¼r gelbe Karten je Spielposition vor.
#
# Der Datensatz enthÃ¤lt:
# - position ... Spielposition auf dem FuÃŸballfeld
# - league ... FuÃŸball-Liga
# - height ... durchschnittliche GrÃ¶ÃŸe der Spieler
# - weight ... durchschnittliches Gewicht der Spieler
# - games ... Anzahl von Spielen
# - yellow ... Anzahl der gelben Karten

# Frage 1: Welche Spielposition erhält am meisten, welche am wenigsten gelbe Karten?
#          Ist der Unterschied relevant? Oder gibt es Positionen mit Ähnlich vielen 
#          Karten?

# Frage 2: Unterschiedliche Liegen haben eine unterschiedliche Kultur in Bezug auf Karten.
#          In welcher Liga ist die Varianz der vergebenen Karten die grÖßte?

# Frage 3: Die Spielfeldpositionen sind teils in links/rechts aufgeschlüsselt:
#          Gibt es Unterschiede zwischen linken und rechten Spielern auf derselben Position?

# Frage 4: Wie interpretieren Sie den Einfluss von Größe und Gewicht der Spieler?
#          In welchem Zusammenhang stehen gelbe Karten zu Anzahl an Spielen?

# Frage 5: Wie stark ist der Einfluss der Liga auf Anzahl der gelben Karten?
#          (Hinweis: Beantwortung dieser Frage macht Modell deutlich komplexer.)

#-------------------- Analyse -------------------#
summary(df)
#6 Variablen davon haben 2 Gruppenvariablen

table(df$position)
#Gruppenvariable position hat 12 Levels

table(df$league)
#Gruppenvariable league hat 4 Levels

#Notiz: Frage 1 abhängige Varibale ist gelbe Karte
#Notiz: Frage 2 es wird nach der Varianz von Liga gefragt

hist(df$height) #verteilung sieht normal aus
hist(df$weight) #verteilung sieht normal aus
hist(df$games)  #bedarf genauerere Analyse
hist(df$yellow) #bedarf genauerere Analyse

describe(df)
#Die Variablen games und yellow weisen einen großen sd auf.
#Bei beiden gibt es großen min max unterschied. Bedarf weiterer Analyse.
#ACHTUNG bei transformation von yellow da es eine 0 beinhaltet muss es +1 gerechnet werden.

plot(df$games, df$yellow)
#Auch hier sehen wir das sich viele Werte in der linken untere Ecke befinden.
plot(log2(df$games), log2(df$yellow+1))
#Daten sind nun besser verteilt.

df$games.log = log2(df$games+1)
df$yellow.log = log2(df$yellow+1)

plot(df$games.log, df$yellow.log)
#ACHTUNG bei Interpretation muss zurückgerechnet werden!

dfz = prepare.df.bayes(df, drop.originals = TRUE)
summary(dfz)
#sieht gut aus!


#-------------------- Modell aufstellen -------------------#

#Modell mit 2 Gruppen und vier Variablen
#Wenn Interaktion gefragt ist
#bei den Variablen hinzufügen.
#zwei Variablen: beta.interaktion*height*variable 2 -> interkation von height und variable 2
#einer Variable und einer Gruppe:  beta[gruppe[i]]*height[i]  -> nicht ganzs sicher ob das stimmt


#Gru-ppe1             = league       
#Gru-ppe2             = position
#abhaengige-Variable  = yellow.log
#Vari-able1           = 
#Vari-able2           =
#Vari-able3           =
#Vari-able4           =

modell = "
data {
N <- length(yellow.log[])     # generelle Lafuvariable über alle Werte
Nleague <- max(league)      # Anzahl der league ->4
Nposition <- max(position)  # Anzahl der position -> 12
}


model {

for (i in 1:N) {


yellow.log[i] ~ dnorm(mu[i], 1/sigma[league[i]]^2)  # ACHTUNG: Je nachdem nach welcher Gruppe gefragt wird, muss hier auch die richtige Gruppe eingetragen werden. 
#für das Pooling verwendet wird. Unterschiedliche sd fÃ¼r league

mu[i] <- interceptposition[position[i]] +           # Gruppe fÃ¼r position / intercept
interceptleague[league[i]] +                        # Gruppe fÃ¼r league /intercept
beta.height*height[i] +
beta.weight*weight[i] +
beta.games*games.log[i]                             #-----------ACHTUNG hier stand zuerst games.log*games.log nach dem ersetzen
}

#----------------------------------------------------------------------------------------------
#Vorhersage
#yellow.log.hat[i] ~ dnorm(mu[i], 1/sigma[league[i]]^2) #ist gleich erste Zeile im Modell
#----------------------------------------------------------------------------------------------



# Priors

for(l in 1:Nleague){
sigma[l]~dexp(3/1)
}
#--------------------------------------------------------------------------
#interceptleague[l]~dnorm(0,1)
#sigma[l]~dexp(3/1) 
#wenn in der Fragestellung nach der Gruppe gefragt wird die Pooling verlangt, dann gehört
#diese Funktion in die forschleife des Partial-Poolings
#--------------------------------------------------------------------------


beta.height ~ dnorm(0,1/1^2)
beta.weight ~ dnorm(0,1/1^2)
beta.games ~ dnorm(0,1/1^2) #<---------- achtung war vorher log

#--------------------------------------------------------------------------
for(l in 1:Nleague) 
{
  interceptleague[l] ~ dnorm(0, 1/1^2)
}
#--------------------------------------------------------------------------

# league hat nur 3 Werte: kein Pooling
# alle leagues bekommen das selbe sgima & intercept
#VT: wenn 7 leagues wären, dann müsste man sonst 7 Zeilen für intercept und 7 für sigma schreiben



# Partial-Pooling für position

interceptposition.mu ~ dnorm(0,1/1^2)
interceptposition.sigma ~ dexp(1)

for (d in 1:Nposition) {
interceptposition[d] ~ dnorm(interceptposition.mu, 1/interceptposition.sigma^2)
}


# stabilere Faktoren ausrechnen (Gruppen beeinflussen sich gegeNpositioneitig)

for (l in 1:Nleague) {
for (d in 1:Nposition) {
mtx[l,d] <- interceptleague[l] +interceptposition[d]

}
}

intercept <- mean(mtx[1:Nleague,1:Nposition])
for(l in 1:Nleague)
{
  alphaleague[l] <- mean(mtx[l,1:Nposition]) - intercept
}
for (d in 1:Nposition) 
{
  Gammaposition[d] <- mean(mtx[1:Nleague,d]) - intercept  # mtx ist definiert mit mtx[l,d]
}


}
"


#Modell für Variablen aufrufen
modell.fit = run.jags(model=modell,
                      data=dfz,
                      burnin = 5000,
                      monitor = c("intercept", "alphaleague", "Gammaposition", "sigma",
                                  "beta.height", "beta.weight", "beta.games"),
                      n.chains = 3,
                      sample= 10000,
                      thin=2,
                      inits = list(list(.RNG.name="base::Mersenne-Twister", .RNG.seed=456),
                                   list(.RNG.name="base::Super-Duper", .RNG.seed=123),
                                   list(.RNG.name="base::Wichmann-Hill", .RNG.seed=789)),
                      method = "parallel")

fit.samples = as.matrix(modell.fit$mcmc)
fit.summary = view(modell.fit)
#MC%ofSD alles unter 2 also können wir weiter machen

#Weil SSeff bei diesen Variablen rund um 10000 sind schauen wir das genauer an!
diagMCMC(modell.fit$mcmc, "beta.height")
diagMCMC(modell.fit$mcmc, "beta.weight")

#----------------- Interpretation -------------------#

# Frage 1: Welche Spielposition erhÃ¤lt am meisten, welche am wenigsten gelbe Karten?
#          Ist der Unterschied relevant? Oder gibt es Positionen mit Ähnlich vielen Karten?
plotcoef(modell.fit, c("Gammaposition"))
#Position 5 hat am meisten Gelbe Karten
#Position 6 erhält am wenigsten Karten

table(df$position)
#Position 5 = Defensive Midfielder!
#Position 6 = Goalkeeper!

diff_pos5v6_z_log = (fit.samples[,"Gammaposition[5]"]-fit.samples[,"Gammaposition[6]"])
plotPost(diff_pos5v6_z_log, compVal = 0)
#Nullwert ist nicht eingeschlossen also ist es signifikant.
#Ist aber aktuell noch transformiert.

diff_pos5v6_z_log = 2^(diff_pos5v6_z_log*sd(df$yellow.log))
plotPost(diff_pos5v6_z_log, compVal = 0)
#Nullwert ist nicht eingeschlossen also ist es noch immer signifikant.


# Frage 2: Unterschiedliche Liegen haben eine unterschiedliche Kultur in Bezug auf Karten.
#          In welcher Liga ist die Varianz der vergebenen Karten die grÃ¶ÃŸte?

plotcoef(modell.fit, c("sigma"))
table(df$league)
#Auf den ersten Blick sieht Position 2 also Frankreich so aus als ob es die größte Varianz hat.
#Dicht gefolgt von Position 1 was England ist.

diff_pos2v1_z_log = (fit.samples[,"sigma[2]"]-fit.samples[,"sigma[1]"])
plotPost(diff_pos2v1_z_log, compVal = 0)
#Schließt Nullwert mit ein ist also nicht signifikant.


# Frage 3: Die Spielfeldpositionen sind teils in links/rechts aufgeschlüsselt:
#          Gibt es Unterschiede zwischen linken und rechten Spielern auf derselben Position?

plotcoef(modell.fit, c("Gammaposition"))
table(df$position)

#7-10
diff_pos7v10_z_log = (fit.samples[,"Gammaposition[7]"]-fit.samples[,"Gammaposition[10]"])
plotPost(diff_pos7v10_z_log, compVal = 0)

#8-11

#9-12
#Machen wir jetzt nicht.

# Frage 4: Wie interpretieren Sie den Einfluss von Größe und Gewicht der Spieler?
#          In welchem Zusammenhang stehen gelbe Karten zu Anzahl an Spielen?

plotPost(2^(fit.samples[, "beta.height"]*sd(df$yellow.log)/sd(df$height)))
#Pro cm das ein Spieler größer ist verringert sich die Chance auf Gelbe Karten um das 0.99fache.

plotPost(2^(fit.samples[, "beta.weight"]*sd(df$yellow.log)/sd(df$weight)))
#Pro kg das ein Spieler mehr wiegt erhöht sich die Chance auf Gelbe Karten um das 1.02fache.

plotPost(2^(fit.samples[, "beta.games"]*sd(df$yellow.log)/sd(df$games.log)))
#Pro game das ein Spieler mehr spielt erhöht sich die Chance auf Gelbe Karten um das 1.89fache.


# Frage 5: Wie stark ist der Einfluss der Liga auf Anzahl der gelben Karten?
#          (Hinweis: Beantwortung dieser Frage macht Modell deutlich komplexer.)

plotcoef(modell.fit, c("alphaleague"))
table(df$league)
#Spanien mit Position 4 hat am meisten Gelbe Karten

diff_pos4v2_z_log = (fit.samples[,"alphaleague[4]"]-fit.samples[,"alphaleague[2]"])
plotPost(diff_pos4v2_z_log, compVal = 0)

plotPost(2^(fit.samples[, "alphaleague"]*sd(df$yellow.log)))
#Umrechnen geht hier nicht?!?!?!?!