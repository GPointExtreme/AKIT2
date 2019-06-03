# AKIT2 SS17, Hauptklausur, 23.6.2017
library(ggplot2)
library(arm)
library(car)
library(coin)
library(corrplot)
library(dplyr)
library(effects)
library(lme4)
library(lmtest)
library(psych)
library(pwr)
library(ROCR)
library(runjags)
library(coda)
library(VIM)
library(akit2)

df <- read.csv('C:\\Users\\Dominik\\Downloads\\kaese.csv')

# Eine Marktstudie gepaart mit Umfragedaten untersucht, was einen Spitzen-Käse ausmacht.
#
# Der Datensatz enthält:
# - visuell1..visuell4 ... 4 Fragen/Antworten auf einer Likert-Scala zum visuellen Eindruck
# - gesamt1..gesamt4 ... 4 Fragen/Antworten auf einer Likert-Scala zum Geschmackseindruck insgesamt
# - milchzucker ... Wert für Milchzucker-Gehalt
# - enzyme ... Wert für Enzym-Gehalt
# - ausgezeichnet ... ob der Käse prämiert worden ist oder nicht
#

# Schritt 1: Erstellen Sie aus den Likert-Skalen passende latente Faktoren
#
#            [eigentliche Analyse ausgelassen, stattdessen: ]
#
#            Verwenden Sie den Mittelwert von visuell1..visuell4 als latente Variable visuell
#            und verwenden Sie den Mittelwert gesamt1..gesamt3 (ohne gesamt4)
hist(df$enzyme)
hist(df$milchzucker)
#Beide haben einige Ausreißer. Müssen wir beachten.

likert = df[,1:8]
corrplot(cor(likert))
#Noch nie gesehen...

KMO(cor(likert))
#Noch nie gesehen...

fa.parallel(likert)
#Noch nie gesehen...

sum(principal(likert)$values > 1)
#Noch nie gesehen...

#ganzer Abschnitt ist nur dafür da das wir gesamt 4 weg lassen.
df$gesamt = rowMeans(df[,1:3]) #Wir lassen gesamt4 weg
df$visuell = rowMeans(df[,5:8])
#Fragebogenskalen werden typischerweise zTransformiert.
df$gesamt.z = zscale(df$gesamt)
df$visuell.z = zscale(df$visuell)

# Schritt 2: Erstellen Sie ein Vorhersagemodell, ob ein Käse ausgezeichnet ist oder nicht
#            Verifizieren & interpretieren Sie das Modell.
model = glm(ausgezeichnet ~ gesamt.z + visuell.z + milchzucker + enzyme, data = df, family = binomial(link = 'logit'))
summary(model)
describe(df$gesamt)
#wenn sich bei gesamt der sd um 1.52 ändert dann sinkt der logit um -0.94
describe(df$visuell)
#wenn sich bei visuell der sd um 0.76 ändert dann sinkt der logit um -0.11

#wenn sich bei milchzucker der Wert um 1 ändert dann steigt der logit um 0.01
#wenn sich bei enzyme der Wert um 1 ändert dann steigt der logit um 0.069

#Nun können wir mit Modelltests anfangen:
deviance(model)/model$df.residual
#Kleiner als 1. Sieht also gut aus.

hinkley(model)
#y.hat^2 ist ok. Sieht gut aus.

log.linearity.test(model)
#Kann nicht ausgeführt werden weil es Werte 0 gibt.

outlierTest(model)
#No Studentized residuals with Bonferonni p < 0.05. Also keine Ausreißer.

plot(model, which=4)
#Kein Wert über 0.5

plot(model, which=5)
#Auch nichts das weit außerhalb wer Linien liegt.
#Nur der Datensatz 772 sticht herraus.

#Richtige Interpretation:
summary(model)

cf.intcpt = coef(model)[1]
inv.logit(cf.intcpt) #hier berechnen wir die Wahrscheinlichkeit verpackt in einer Funktion
#0.001692311 = 0,17%
#Käse ohne Milchzucker und Enzymen, der einen durchschnittlich Gesammteindruck und
#visuellen Eindruck hat ist mit 0,17% Wahrscheinlichkeit ein Prämiumkäse.

#Gesamteindruck ist Signifikant
exp(coef(model)[2])
#0.3884617 = Wenn sich der Gesamteindruck um eine sd erhöht dann sinkt die Chance
#auf eine prämierung um das 0.39 fache. Chance sinkt um 61%.

#Enzyme ist Signifikant
exp(coef(model)[5])
#1.071156 = Wenn sich die Enzyme um 1 Wert erhöht dann steigt die Chance
#auf eine prämierung um das 1.07 fache. Chance erhöt sich um 7%.

#Einfluss der Variablen:
Anova(model)
#gesamt.z hat wohl den größten Einfluss.

#Ausreißer anschauen:
model2 = update(model, data=df[c(-145, -772),])
summary(model2)
cbind(data.frame(coef(model), coef(model2)))
#cbind gibt bessere übersicht.
exp(cbind(data.frame(coef(model), coef(model2))))
#enzyme: 7% auf 9%
#gesamt: -61% auf -64%
#Ausreißer haben wohl doch einen Einfluss.
#Es müsste überprüft werden ob Daten richtig erhoben wurden und auch richtig
#transkribiert worden sind.

# Schritt 3: Erstellen Sie ein weiteres Modell ohne den Gesamteindruck-Faktor.
#            Verifizieren & interpretieren Sie das Modell.

# Schritt 4: Conclusio:
#            Was schließen Sie aus den beiden Modellen?
#            Wenn Sie KÃ¤sehersteller wÃ¤ren, was bedeuten die Ergebnisse fÃ¼r Neukunden-Gewinnung und Bestandskunden?
#            Welche "confounding"/"versteckte" Eigenschaften kÃ¶nnen das Resultat verfÃ¤lschen?
#            Welche Empfehlung geben Sie fÃ¼r die nÃ¤chste Studie ab? (soll nochmal selbe Fragen analysieren)
