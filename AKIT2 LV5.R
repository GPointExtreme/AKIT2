# AKIT2, Klausur, 13.9.2017

library(car)
library(psych)
library(ROCR)
library(VIM)
library(akit2)

# Die Betreiberin eines Rechenzentrums überlegt ihre Festplattenkapazität aufzurüsten.
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


df = read.csv('C:\\Users\\Dominik\\Downloads\\festplatten.csv')

summary(df)
hist(df$drehzahl)
df$drehzahl = df$drehzahl/1000 #Erhöhen wir damit wir Umdrehungen pro Minute haben (Besser für interpretieren)

# Fragestellung

# - Erstellen Sie Vorhersagemodelle getrennt für 2.5" und 3.5"-Festplatten,
#   ob eine Festplatte ausfällt (Fehler=1) oder nicht.
#   Verifizieren Sie die beiden Modelle auf Gültigkeit.

df25 = df[df$bauart == '2.5in', ]
df35 = df[df$bauart == '3.5in', ]

m25 = glm(fehler ~ marke + groesze + drehzahl + lautstaerke + leistung, data = df25, family = binomial(link='logit'))
m35 = update(m25, data=df35) # Kopiert alles von m25

summary(m25)
inv.logit(-3.95444) # Intercept: 1.9%
exp(coef(m25))

# drehzahl:
# 1000rpm mehr == 15% höhere Chance (=odds) für Fehler
# 5000rpm mehr == ^5 == Verdoppeltung

# markeSeagate :: Chance auszufallen ist um 3.1% weniger als bei vergleichbarer HGST-Festplatte

predict(m25, newdata = data.frame(
  marke='Seagate',
  groesze=4,
  drehzahl=c(4,6,8,10),
  lautstaerke=mean(df$lautstaerke),
  leistung=mean(df$leistung)
), type = "response")

# abs. Ausfallwahrscheinlichkeit: 30%

lautst = seq(min(df25$lautstaerke), max(df25$lautstaerke), length.out = 200)

pred.laut = predict(m25, newdata = data.frame(
  marke='Seagate',
  groesze=4,
  drehzahl=8,
  lautstaerke=lautst,
  leistung=mean(df$leistung)
), type = "response")

plot(lautst, pred.laut, type='l', ylim=c(0.17, 0.5))

pred.laut2 = predict(m25, newdata = data.frame(
  marke='Western Digital',
  groesze=4,
  drehzahl=8,
  lautstaerke=lautst,
  leistung=mean(df$leistung)
), type = "response")

lines(lautst, pred.laut2, col='blue')

#overdispersion
deviance(m25) / m25$df.residual

#Hinkley
hinkley(m25)

#Variable linear mit y?
log.linearity.test(m25)
 
# - Interpetieren Sie die beiden Modelle, insbesondere die Koeffizienten.
#   Zeigen Sie konkrete Werte auch anhand selbst gewählter Beispieldaten.
# 
# - Beurteilen Sie die Vorhersagekraft beider Modelle im Vergleich.
#   Geben Sie weiters die Eigenschaften des 3.5"-Modells an, wenn es eine
#   True-Positive-Rate von 0.8 hat. Welche Schlussfolgerungen ziehen Sie daraus?

ROC(m25)

kosten.true.negativ = 0
kosten.true.positiv = 100
kosten.false.negativ = 500
kosten.false.positiv = 120

y.hat = predict(m25, type = "response")
y = df25$fehler
cutoff = 0.33

kosten = function(cutoff) {
anz.true.negativ = sum(y == 0 & y.hat < cutoff)
anz.true.positiv = sum(y == 1 & y.hat >= cutoff)
anz.false.negativ = sum(y == 1 & y.hat < cutoff)
anz.false.positiv = sum(y == 0 & y.hat >= cutoff)

return (kosten.false.negativ * anz.false.negativ +
  kosten.false.positiv * anz.false.positiv +
  kosten.true.negativ * anz.true.negativ +
  kosten.true.positiv * anz.true.positiv)
}

cutoff.seq = seq(0.11, 0.67, length.out = 200)
kosten.seq = sapply(cutoff.seq, kosten)

plot(cutoff.seq, kosten.seq, type='l')

# - Neuanschaffung: wir wollen 500 2.5"-Festplatten anschaffen.
#   Welches Festplattenmodell soll gewählt werden?
#   Welches ist in Summe[^1] am günstigsten[^2]?
#   Folgende Modelle stehen zur Auswahl:
#     - HGST, 6TB, 5940rpm, 6.5W, 22dB, Preis: ??? 254,-
#     - Seagate, 6TB, 7200rpm, 7W, 26dB, Preis: ??? 230,-
#     - Toshiba, 6TB, 10000rpm, 10.5W, 32dB, Preis: ??? 158,-
#     - Western Digital, 6TB, 10000rpm, 10W, 30dB, Preis: ??? 180,-
# 
# [^1]: Ohne Berücksichtigung von Strom- und Personalkosten; gehen Sie davon aus,
#       dass fehlerhafte Festplatten nur ein einziges Mal nachgekauft werden müssen.
# [^2]: Sie sollten nicht nur Punktwerte berechnen, sondern (so weit möglich)
#       gleich Konfidenzintervalle.

