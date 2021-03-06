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

# Eine Marktstudie gepaart mit Umfragedaten untersucht, was einen Spitzen-K�se ausmacht.
#
# Der Datensatz enth�lt:
# - visuell1..visuell4 ... 4 Fragen/Antworten auf einer Likert-Scala zum visuellen Eindruck
# - gesamt1..gesamt4 ... 4 Fragen/Antworten auf einer Likert-Scala zum Geschmackseindruck insgesamt
# - milchzucker ... Wert f�r Milchzucker-Gehalt
# - enzyme ... Wert f�r Enzym-Gehalt
# - ausgezeichnet ... ob der K�se pr�miert worden ist oder nicht
#

# Schritt 1: Erstellen Sie aus den Likert-Skalen passende latente Faktoren
#
#            [eigentliche Analyse ausgelassen, stattdessen: ]
#
#            Verwenden Sie den Mittelwert von visuell1..visuell4 als latente Variable visuell
#            und verwenden Sie den Mittelwert gesamt1..gesamt3 (ohne gesamt4)
hist(df$enzyme)
hist(df$milchzucker)
#Beide haben einige Ausrei�er. M�ssen wir beachten.

likert = df[,1:8]
corrplot(cor(likert))
#Noch nie gesehen...

KMO(cor(likert))
#Noch nie gesehen...

fa.parallel(likert)
#Noch nie gesehen...

sum(principal(likert)$values > 1)
#Noch nie gesehen...

#ganzer Abschnitt ist nur daf�r da das wir gesamt 4 weg lassen.
df$gesamt = rowMeans(df[,1:3]) #Wir lassen gesamt4 weg
df$visuell = rowMeans(df[,5:8])
#Fragebogenskalen werden typischerweise zTransformiert.
df$gesamt.z = zscale(df$gesamt)
df$visuell.z = zscale(df$visuell)

# Schritt 2: Erstellen Sie ein Vorhersagemodell, ob ein K�se ausgezeichnet ist oder nicht
#            Verifizieren & interpretieren Sie das Modell.
model = glm(ausgezeichnet ~ gesamt.z + visuell.z + milchzucker + enzyme, data = df, family = binomial(link = 'logit'))
summary(model)
describe(df$gesamt)
#wenn sich bei gesamt der sd um 1.52 �ndert dann sinkt der logit um -0.94
describe(df$visuell)
#wenn sich bei visuell der sd um 0.76 �ndert dann sinkt der logit um -0.11

#wenn sich bei milchzucker der Wert um 1 �ndert dann steigt der logit um 0.01
#wenn sich bei enzyme der Wert um 1 �ndert dann steigt der logit um 0.069

#Nun k�nnen wir mit Modelltests anfangen:
deviance(model)/model$df.residual
#Kleiner als 1. Sieht also gut aus.

hinkley(model)
#y.hat^2 ist ok. Sieht gut aus.

log.linearity.test(model)
#Kann nicht ausgef�hrt werden weil es Werte 0 gibt.

outlierTest(model)
#No Studentized residuals with Bonferonni p < 0.05. Also keine Ausrei�er.

plot(model, which=4)
#Kein Wert �ber 0.5

plot(model, which=5)
#Auch nichts das weit au�erhalb wer Linien liegt.
#Nur der Datensatz 772 sticht herraus.

#Richtige Interpretation:
summary(model)

cf.intcpt = coef(model)[1]
inv.logit(cf.intcpt) #hier berechnen wir die Wahrscheinlichkeit verpackt in einer Funktion
#0.001692311 = 0,17%
#K�se ohne Milchzucker und Enzymen, der einen durchschnittlich Gesammteindruck und
#visuellen Eindruck hat ist mit 0,17% Wahrscheinlichkeit ein Pr�miumk�se.

#Gesamteindruck ist Signifikant
exp(coef(model)[2])
#0.3884617 = Wenn sich der Gesamteindruck um eine sd erh�ht dann sinkt die Chance
#auf eine pr�mierung um das 0.39 fache. Chance sinkt um 61%.

#Enzyme ist Signifikant
exp(coef(model)[5])
#1.071156 = Wenn sich die Enzyme um 1 Wert erh�ht dann steigt die Chance
#auf eine pr�mierung um das 1.07 fache. Chance erh�t sich um 7%.

#Einfluss der Variablen:
Anova(model)
#gesamt.z hat wohl den gr��ten Einfluss.

#Ausrei�er anschauen:
model2 = update(model, data=df[c(-145, -772),])
summary(model2)
cbind(data.frame(coef(model), coef(model2)))
#cbind gibt bessere �bersicht.
exp(cbind(data.frame(coef(model), coef(model2))))
#enzyme: 7% auf 9%
#gesamt: -61% auf -64%
#Ausrei�er haben wohl doch einen Einfluss.
#Es m�sste �berpr�ft werden ob Daten richtig erhoben wurden und auch richtig
#transkribiert worden sind.

# Schritt 3: Erstellen Sie ein weiteres Modell ohne den Gesamteindruck-Faktor.
#            Verifizieren & interpretieren Sie das Modell.
model3 = glm(ausgezeichnet ~ visuell.z + milchzucker + enzyme, data = df, family = binomial(link = 'logit'))

deviance(model3) / model3$df.residual
#1.009908 - Sollte kleiner 1 sein - Knapp �ber 1 ist noch kein Problem.

hinkley(model3)
#y.hat^2 ist ok.

log.linearity.test(model3)
#Funktioniert nicht weil 0 Werte drinnen sind.

outlierTest(model3)
#No Studentized residuals with Bonferonni p < 0.05

plot(model3, which = 4)
#Andere Ausrei�er aber keiner �ber 0.5

plot(model3, which = 5)
#Sieht auch gut aus. 110 sticht heraus.

#Interpretation pipapo
summary(model3)
summary(model)
#enzyme ist nun negativ. Visuell.z und milchzuker sind nun signifikant.

inv.logit(coef(model3)[1])
#0.009690989 = 1% Wenn Milchzucker und enzyme 0 w�ren dann g�be es eine 1%
#Warscheinlichkeit das es ein Pr�miumk�se ist. 6x h�her als vorher.

#Visueller Eindruck
odds.visuell = exp(coef(model3)[2])
odds.visuell
1-0.7899023
#0.2100977 = 21% Chance wenn sich der visuelle Eindruck um 1-sd Punkt auf der 
#likert-Skala erh�ht dann sinkt die Chance auf einen Pr�miumk�se um das 0.79-fache.

#Enzyme
odds.visuell = exp(coef(model3)[4])
odds.visuell
1-0.9413493
#0.0586507 = 6% Chance wenn sich die Enzyme um 1 Wert erh�hen dann sinkt die Chance
#auf einen Pr�miumk�se um das 0.94-fache.

Anova(model3)
#Einfluss von enzyme ist am gr��ten jedoch dicht gefolgt von visuell.z und milchzucker.

anova(model, model3)
#model besser weil es einen kleinere Werte hat.

AIC(model, model3)
#Auch hier ist model besser als model3.

par(mfrow=c(2,1))
ROC(model)
ROC(model3)
#Auch hier sieht man die Kurve von model zeigt mehr Erkl�rungskraft an.

# Schritt 4: Conclusio:
#            Was schlie�en Sie aus den beiden Modellen?
#model mit gesamt.z ist Besser und Erkl�rt mehr.
#Die Ausrei�er darin sollten wir nochmal mit Domainenwissen anschauen/kontrollieren!

#Au�erdem �ndert enzyme das vorzeichen. Beudeut das hier wohl eine Korellation vorliegt?
cor(df$gesamt, df$enzyme)
#Ja liegt mit 0.6 vor.

#Im model �berschattet der gesamteindruck den visuelleneindruck.

#            Wenn Sie K�sehersteller w�ren, was bedeuten die Ergebnisse f�r Neukunden-Gewinnung und Bestandskunden?
#Die Anwendung des ersten Modells ist eher f�r Bestandskunden da diese den
#Gesamteindruck kennen.
#model2 ist eher f�r Neukunden da diese noch keinen Gesamteindruck haben.

#Sowohl Gesamteindruck als auch Visueller Eindruck sind gegen�ber Pr�miumk�se
#negativ. Also als Herstellen m�ssen wir darauf nicht so viel Wert legen da
#die Meinung von Endverbraucher und Schiedsrichter auseinander gehen.

#            Welche "confounding"/"versteckte" Eigenschaften k�nnen das Resultat verf�lschen?
#Wir wissen nicht welche Personen befragt wurden bzw. welche Sorten K�se alle
#miteinbezogen wurden. Somit k�nnen wir nicht auf die Gesamtheit schlie�en.

#            Welche Empfehlung geben Sie f�r die n�chste Studie ab? (soll nochmal selbe Fragen analysieren)
#K�nnen wir weitere Variablen mit rein nehmen die Hilfreich w�ren?
#Mag Person generell K�se? K�sevorlieben oder K�se-"Wissen" erheben?
#Wie regelm��ig essen die Personen K�se?
#Ziel der Studie sollte klar sein: wollen wir die K�sehersteller beraten,
#wie sie m�glichst zum perfekten K�se kommen oder machen wir eine Marktstudie?
#Bei zweiterem m�sste das Modell ganz anders aufgestellt sein. Statt Pr�miumk�se
#m�sste der Gesamteindruck als abh�ngige Variable hergenommen werden.