# Übung AKIT2
# Arno Hollosi
#
library(dplyr)

# Daten laden
# Idee für Datenset von: DOI: 10.1186/1742-7622-5-2
# http://ete-online.biomedcentral.com/articles/10.1186/1742-7622-5-2
blutdruck <- read.csv("C:\\Users\\Dominik\\Downloads\\blutdruck.csv")
summary(blutdruck)

# Plot kann auch mit Formel angegeben werden
plot(blood.pressure ~ current.weight, data=blutdruck)

# In dieser Uebung wollen wir sehen, wie sich Koeffizienten einer Regressionsgleichung
# mit der Stichprobe Aendern koennen.
#
# Zu diesem Zweck nehmen wir an, dass das Datenset die *Gesamtpopulation* darstellt.
# Die Koeffizienten ueber das gesamte Datenset stellen also die "echten/richtigen"
# Werte fuer die Gesamtpopulation dar:
population.modell = lm(blood.pressure ~ current.weight, data=blutdruck)

# (1) Sehen Sie sich das Ergebnis des Modells an und interpretieren Sie es.
summary(population.modell)
#Bei Erhoehung von Pressure um 1 Einheit erhoeht sich Weight um 0.39699
#1Q und 3Q sind symetrisch um 0 = gut
#min und max �bersteigen jedoch die doppelten Interquartilsabstaende = nicht gut


# Nehmen wir nun an, wir machen eine Forschungsstudie mit einer Stichprobe
# von z.B. 50 Personen
# die Funktion sample_n() aus der dplyr-Bibliothek wählt zufällig Zeilen (Datensätze)
# aus der Population aus:
stichprobe = sample_n(blutdruck, 50)
modell = lm(blood.pressure ~ current.weight, data=stichprobe)

# (2) Welche Werte fuer die Koeffizienten erhalten Sie?
koeff = coef(modell)
#Intercept = 81.9199460
#current.weight = 0.5613792

# (3) Enthaelt das Konfidenz-Intervall der Stichprobe die Werte der Gesamtpopulation?
summary(modell)
#Nein, min und max sind kleiner. Passen nun.
#1Q und 3Q sind nicht mehr ganz so symetrisch.
#Bei Erhoehung von Pressure um 1 Einheit erhoeht sich Weight um 0.5614

# (4) Fuehren Sie obige Zeilen (Stichprobe auswählen, Modell berechnen und anzeigen)
# mehrfach aus. Sehen Sie sich an, wie sich die Werte ändern.
#Es kommen immer andere Werte raus. Verschriebt sich schon bemerktbar.
#Stichprobe von 50 ist wohl zu klein!


# Annahme: wir fuehren 100 Studien durch, jeweils mit einer Stichprobengroe�e von 50

# (5) Überlegen Sie sich vorher:
# - Wie weit werden die Koeffizienten von den Populationswerten abweichen?
# - Welche Verteilung werden die Koeffizienten haben?
# - Wie häufig werden die 95%-Konfidenz-Intervalle die Populationswerte enthalten?

n = 100
# wir definieren den Dataframe vorab:
# intc = Intercept, cwe = current.weight
# .lwr = lower bound, .upr = upper bound
coefs = data.frame(intc=numeric(n), cwe=numeric(n),
                   intc.lwr=numeric(n), intc.upr=numeric(n),
                   cwe.lwr=numeric(n), cwe.upr=numeric(n))
# Wir fixieren den Startwert des Zufallsgenerators, damit alle
# dieselben Zahlen haben
set.seed(2345)
for (i in 1:n) {
  stichprobe = sample_n(blutdruck, 50)
  modell = lm(blood.pressure ~ current.weight, data=stichprobe)
  coefs$intc[i] = coef(modell)[1]
  coefs$cwe[i] = coef(modell)[2]
  ci = confint(modell)
  coefs$intc.lwr[i] = ci[1,1]
  coefs$intc.upr[i] = ci[1,2]
  coefs$cwe.lwr[i] = ci[2,1]
  coefs$cwe.upr[i] = ci[2,2]
}

# (6) Schauen Sie sich nun die Histogramme für coefs$intc und coefs$cwe an
# Welche Verteilung weisen die Koeffizienten auf?

# (7) Berechnen Sie den Mittelwert von coefs$intc und coefs$cwe
# Wie weit sind die Mittelwerte von den Werten der Gesamtpopulation entfernt?

# Mit der folgenden Berechnung können Sie nachzählen, wie viele
# Konfidenzintervalle die Werte der Gesamtpopulation enthalten.
# (8) Welche Werte erhalten Sie?
sum(coefs$intc.lwr<coef(population.modell)[1] & coef(population.modell)[1]<coefs$intc.upr)
sum(coefs$cwe.lwr<coef(population.modell)[2]  & coef(population.modell)[2]<coefs$cwe.upr)

# Wir können auch einen Plot machen, der uns alle Konfidenzintervalle und
# den Populationswert anzeigt (nur für current.weight (cwe)):

plot(NA, xlim=c(min(coefs$cwe.lwr), max(coefs$cwe.upr)), ylim=c(1,n))
for (i in 1:n) {
  segments(coefs$cwe.lwr[i], i, coefs$cwe.upr[i], i)
  segments(coefs$cwe[i], i-0.25, coefs$cwe[i], i+0.25)
}
abline(v=coef(population.modell)[2], col="blue")

# (9) Was schließen Sie aus den obigen Summen bzw. dem Plot?
