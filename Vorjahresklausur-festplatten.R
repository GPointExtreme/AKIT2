# AKIT2, Klausur, 13.9.2017

library(car)
library(psych)
library(ROCR)
library(VIM)
library(ggplot2)
library(corrplot)
library(effects)
library(pwr)
library(runjags)
library(coda)
rjags::load.module("glm")
library(akit2)

# Die Betreiberin eines Rechenzentrums Überlegt ihre Festplattenkapazität aufzurüsten.
# Eine kosteneffiziente Lösung ist gefragt, weshalb Daten der letzten Jahre
# herangezogen werden, um auszuwerten, welche Festplatten zukünftig angeschafft
# werden sollen.
# 
# Der Datensatz enthält:
#   
# - marke ... Name des Herstellers
# - groesze ... Größe der Festplatte in TB
# - bauart ... 2.5" oder 3.5"
# - drehzahl ... Umdrehungsgeschwindigkeit in RPM
# - lautstaerke ... zuletzt gemessene Lautstärke (in dB) der Festplatte im Betrieb
# - leistung ... Leistungsaufnahme in W
# - fehler ... 0 = kein Fehler, 1 = Festplatte ist im Beobachtungszeitraum ausgefallen


df = read.csv(file="C:\\Users\\Dominik\\Downloads\\festplatten.csv")

summary(df)
view(df)

hist(df$groesze)
hist(df$drehzahl)
hist(df$lautstaerke)
hist(df$leistung)
hist(df$fehler)
table(df$bauart)
df$bauart[df$bauart=="2.5in"]

# Fragestellung

# - Erstellen Sie Vorhersagemodelle getrennt für 2.5" und 3.5"-Festplatten,
#   ob eine Festplatte ausfällt (Fehler=1) oder nicht.
#   Verifizieren Sie die beiden Modelle auf Gültigkeit.
df25 = df[df$bauart=="2.5in",]
model1=glm(fehler ~ marke + groesze + drehzahl + lautstaerke + leistung, data = df25, family = binomial(link = "logit"))
df35 = df[df$bauart=="3.5in",]
model2=glm(fehler ~ marke + groesze + drehzahl + lautstaerke + leistung, data = df35, family = binomial(link = "logit"))

summary(model1)
summary(model2)

#----------------------------------Overdisperion

deviance(model1)/model1$df.residual
#Wert liegt leicht über 1. Noch ok.
deviance(model2)/model2$df.residual
#Wert liegt leicht über 1. Noch ok.

#------------------------------------Logit Test

hinkley(model1)
hinkley(model2)
#Sieh gut aus: keine Overdispersion, und der Hinkley-Test bestaetigt uns, dass Logit 
#passend ist und wohl keine unbekannte Variable das Ergebnis (stark) beeinflusst.

#--------------------------------Lineare Abhängigkeit

log.linearity.test(model1)
log.linearity.test(model2)
#Sieht alles ok aus.

vif(model1)
#Alle Werte sind unter 2 außer drehzahl mit 2.3. Alles gut.
vif(model2)
#Alle unter 2. Gut.

#-----------------------------------Einflussreiche Werte / Ausreißer

outlierTest(model1)
plot(model1, which=4)
plot(model1, which=5)
#Alle Werte sehen gut aus weshalb wir hier mit unserem Model weiter machen können.
outlierTest(model2)
plot(model2, which=4)
plot(model2, which=5)
#Schaut auch gut aus.

#--------------------------------------ROC Kurve

ROC(model1)
ROC(model2)
#Je größer die Fläche unter der Kurve, desto besser ist das Modell. Perfekt wäre wenn die 
#Linie den gesamten Bereich 0.0 und 1.0 einschließen. ROC Kurve gibt Einblick wie gut das 
#Modell die Werte vom Datenset vorhersagt.
#Generell sehen beide Kurven nicht gerade super aus.

#--------------------------------------Variablentest
Anova(model1)
#marke und drehzahl hat den größten Einfluss auf unser model1
Anova(model2)
#marke und drehzahl hat den größten Einfluss auf unser model2

drop1(model1)[order(drop1(model1)[,3]),]
drop1(model2)[order(drop1(model2)[,3]),]
#In model2 verschlechtern die Variablen groesze und lautstaerke sogar das Modell.
#Ansonsten gleiches Ergebniss wie bei Anova.

############################################################################################
# - Interpetieren Sie die beiden Modelle, insbesondere die Koeffizienten.
#   Zeigen Sie konkrete Werte auch anhand selbst gewählter Beispieldaten.

summary(model1)
coef(model1)
coef(model2)
#markeSeagate und lautstaerke ändert sich das Vorzeichen. Deutet auf einen Zusammenhang hin!

logisticR2(model1)
logisticR2(model2)
#Model2 sagt etwas Besser vorher aber es sind bei beiden Modellen niedrige Werte.

#----------------------------Interpretation:

intcp = inv.logit(coef(model1)[1]) #--> diese liefert direkt die absolute Wahrscheinlichkeit
#Ergebniss: 0.018 = 2% Wahrscheinlichkeit
#Basisfall Beispiel: Eine 2.5in der Marke HGST wo alle anderen metrischen Variablen 0 sind
#(was unsinnig ist) hat eine 2% Wahrscheinlichkeit einen Fehler zu haben.

intcp = inv.logit(coef(model2)[1]) #--> diese liefert direkt die absolute Wahrscheinlichkeit
#Ergebniss: 0.05 = 5% Wahrscheinlichkeit
#Basisfall Beispiel: Eine 3.5in der Marke HGST wo alle anderen metrischen Variablen 0 sind
#(was unsinnig ist) hat eine 5% Wahrscheinlichkeit einen Fehler zu haben.

#Ganzes Modell zurückrechnen:
exp(coef(model1))
#Wenn man die marke Western Digital nimmt erhöht sich die Chance auf das 4.3-fache einen Fehler zu haben.
#Wenn man die marke Seagate nimmt senkt sich die Chance auf das 0.97-fache einen Fehler zu haben.
#Wenn die drehzahl um 1RPM steigt dann erhögt sich die Chance auf das 1.00015-fache einen Fehler zu haben.
(exp(coef(model1)[6])^1000)
#Wenn die drehzahl um 1000RPM steigt dann erhögt sich die Chance auf das 1.16-fache einen Fehler zu haben.

exp(coef(model2))
#Wenn man die marke Western Digital nimmt erhöht sich die Chance auf das 2.9-fache einen Fehler zu haben.
#Wenn man die marke Toshiba nimmt senkt sich die Chance auf das 3.5-fache einen Fehler zu haben.
#Wenn die drehzahl um 1RPM steigt dann erhögt sich die Chance auf das 1.00018-fache einen Fehler zu haben.
(exp(coef(model2)[6])^1000)
#Wenn die drehzahl um 1000RPM steigt dann erhögt sich die Chance auf das 1.19-fache einen Fehler zu haben.

predict(model1, newdata = data.frame(
  marke=c("Seagate", "Toshiba", "Western Digital", "HGST"), 
  groesze=8, 
  drehzahl=4200, 
  lautstaerke=mean(df25$lautstaerke), 
  leistung=mean(df25$leistung)),
type='response')
#"Seagate",  "Toshiba",   "Western Digital",  "HGST"
#0.1760580,  0.2937397,   0.3903119,          0.1807298
#Seagate hat die geringste Wahrscheinlichkeit einen Fehler zu haben in Model1.

predict(model2, newdata = data.frame(
  marke=c("Seagate", "Toshiba", "Western Digital", "HGST"), 
  groesze=10, 
  drehzahl=7278, 
  lautstaerke=mean(df35$lautstaerke), 
  leistung=mean(df35$leistung)),
type='response')
#"Seagate",  "Toshiba",   "Western Digital",  "HGST"
#0.2791701,  0.5728412,   0.6225752,          0.2769739
#HGST hat die geringste Wahrscheinlichkeit einen Fehler zu haben in Model2.

#Drehzahl könnten wir noch probieren mit einmal min, mean und max um den Unterschied zu sehen.

############################################################################################
# - Beurteilen Sie die Vorhersagekraft beider Modelle im Vergleich.
#   Geben Sie weiters die Eigenschaften des 3.5"-Modells an, wenn es eine
#   True-Positive-Rate von 0.8 hat. Welche Schlussfolgerungen ziehen Sie daraus?
#vorhersage des kompletten Models mit den jeweiligen Testdaten

p1 = predict(model1, df25)
p2 = predict(model2, df35)
#echte Daten - vorhersage daten rechnen quadrieren wegen sum of squares
ssq1 = sum((df25$fehler - p1)^2)
ssq2 = sum((df35$fehler - p2)^2)
c(ssq1, ssq2)
#Ist in model2 besser weil die Vorhersagen weniger abweichen.

#---------------------------------Cutoff bestimmen
ROC(model1)
cutoff = 0.26
#Heißt das wir eine Festplatte ab einer Fehlerwarscheinlichkeit von 26% als 
#fehlerhaft einstufen.

############################################################################################
# - Neuanschaffung: wir wollen 500 2.5"-Festplatten anschaffen.
#   Welches Festplattenmodell soll gewählt werden?
#   Welches ist in Summe[^1] am günstigsten[^2]?
#   Folgende Modelle stehen zur Auswahl:
#     - HGST, 6TB, 5940rpm, 6.5W, 22dB, Preis: Euro254,-
#     - Seagate, 6TB, 7200rpm, 7W, 26dB, Preis: Euro230,-
#     - Toshiba, 6TB, 10000rpm, 10.5W, 32dB, Preis: Euro158,-
#     - Western Digital, 6TB, 10000rpm, 10W, 30dB, Preis: Euro180,-
p1_25 = predict(model1, newdata = data.frame(
  marke="HGST", groesze=6, drehzahl=5940, lautstaerke=22, leistung=6.5), type='response')
p2_25 = predict(model1, newdata = data.frame(
  marke="Seagate", groesze=6, drehzahl=7200, lautstaerke=26, leistung=7), type='response')
p3_25 = predict(model1, newdata = data.frame(
  marke="Toshiba", groesze=6, drehzahl=10000, lautstaerke=32, leistung=10.5), type='response')
p4_25 = predict(model1, newdata = data.frame(
  marke="Western Digital", groesze=6, drehzahl=10000, lautstaerke=30, leistung=10), type='response')
c(p1_25, p2_25, p3_25, p4_25)
p1_25*254 #51.19
p2_25*230 #60.67
p3_25*158 #103.37
p4_25*180 #129.55

#Wir können sehen das die HGST am günstigsten sein würde.

# [^1]: Ohne Berücksichtigung von Strom- und Personalkosten; gehen Sie davon aus,
#       dass fehlerhafte Festplatten nur ein einziges Mal nachgekauft werden mÃ¼ssen.
# [^2]: Sie sollten nicht nur Punktwerte berechnen, sondern (so weit möglich)
#       gleich Konfidenzintervalle.


# Der Datensatz enthält:
#   
# - marke ... Name des Herstellers
# - groesze ... Größe der Festplatte in TB
# - bauart ... 2.5" oder 3.5"
# - drehzahl ... Umdrehungsgeschwindigkeit in RPM
# - lautstaerke ... zuletzt gemessene Lautstärke (in dB) der Festplatte im Betrieb
# - leistung ... Leistungsaufnahme in W
# - fehler ... 0 = kein Fehler, 1 = Festplatte ist im Beobachtungszeitraum ausgefallen