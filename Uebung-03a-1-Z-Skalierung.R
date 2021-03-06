# z-Skalierung
# AKIT2 �bung
# Arno Hollosi

# Eine der typischen Transformationen von Variablen - neben der log-Transformation - ist die
# z-Skalierung.
# Wir wollen uns in dieser Übung ansehen, was die z-Skalierung macht.

# Laden Sie als erstes das Body-Datenset (setwd() nicht vergessen bzw. Pfad anpassen)
body = read.csv("C:\\Users\\Dominik\\Downloads\\body-dimensions.csv")

# 1) Schauen Sie sich das Histogramm folgender Variable an:
# - Knee.diameter
hist(body$Knee.diameter)
# - Forearm.girth
hist(body$Forearm.girth)
# - Weight
hist(body$Weight)

# 2) Berechnen Sie mean() und sd() der drei Variable
library(psych)
describe(body$Knee.diameter)  #mean: 18.81  sd: 1.35
describe(body$Forearm.girth)  #mean: 25.94  sd: 2.83
describe(body$Weight)         #mean: 69.15  sd: 13.35

# 3) Stellen Sie ein Regressionsmodell auf:
# "Gewicht als abh�ngige Variable der anderen beiden Variable"
# und interpretieren Sie das Modell:
model = lm(Weight ~ Knee.diameter + Forearm.girth, data = body)
summary(model)

# - Was genau sagt der Intercept aus? Ist der Intercept praktisch m�glich? Was also sagt er aus?
#Intercept ist -65.6772. Man kann aber nicht ein negatives Gewicht haben
#Zeigt die Grenzen des Modells auf
#Wenn Kree und Forearm 0 sind hat man theoretisch -65.68 weight im modell

# - Was sagen die Koeffizienten f�r Knee.diameter und Forearm.girth?
#Wenn Kneeumfang um 1cm erh�ht wird erh�ht sich das Gewicht um 2.8722kg
#Wenn Unterarmumfang um 1cm erh�ht wird erh�ht sich das Gewicht um 3.1144

# - Wie viel Prozent der Varianz im Gewicht wird durch das Modell erkl�rt?
#Multiple R-squared: 0.7966 = 79% wird erkl�rt.

# Welcher der beiden Variable hat mehr Einfluss auf das Gewicht?
library(car)
Anova(model)
drop1(model)
#Bei drop1 sehen wir das Forearm mit 18900.9 viel gr��er als Knee mit 3643.7 ist.
#Au�erdem kommt AIC von 1825.2 durch Knee auf 1915.0 und durch Forearm auf 2182.3.

# Diese Frage ist nur durch Blick auf die Koeffizienten nicht zu beantworten:
# die zu Grunde liegenden Zahlen haben unterschiedliche Maximum- & Minimumwerte,
# man kann sie also nicht direkt vergleichen.
# (Tipp: greifen Sie auf die Anova-Rechnung und deren "Sum of Squares" zur�ck)


# z-Skalierung
# 4) Berechnen Sie eine neue Spalte body$knee.diameter.z
# und eine neue Spalte body$forearm.girth.z
# in dem Sie zuerst den Mittelwert der jeweiligen Spalte abziehen und
# dann durch die Standardabweichung der jeweiligen Spalte dividieren
body$Knee.diameter.z = (body$Knee.diameter-mean(body$Knee.diameter))/sd(body$Knee.diameter)
body$Forearm.girth.z = (body$Forearm.girth-mean(body$Forearm.girth))/sd(body$Forearm.girth)

# 5) Schauen Sie sich das Histogramm der beiden Variable an
# und vergleichen Sie es mit den Histogrammen der Original-Variablen.
# Berechnen Sie auch Mittelwert und Standardabweichung
# Welche Auswirkung hatte die Transformation?
par(mfrow=c(2,2))

hist(body$Knee.diameter)
hist(body$Knee.diameter.z)

hist(body$Forearm.girth)
hist(body$Forearm.girth.z)
#Wertebereiche wurden Verkeleinert.

# 6) Zeichnen Sie folgende Scatterplots
# a) Knee.diameter vs. Weight
plot(body$Knee.diameter, body$Weight)
# b) knee.diameter.z vs. Weight
plot(body$Knee.diameter.z, body$Weight)
# c) Forearm.girth vs. Weight
plot(body$Forearm.girth, body$Weight)
# d) forearm.girth.z vs. Weight
plot(body$Forearm.girth.z, body$Weight)
# Wie unterscheiden sich die Plots a-b und c-d?
#Nur auf der x Achse durch kleinere Werte zu vorher.

# e) Knee.diameter vs. knee.diameter.z
plot(body$Knee.diameter, body$Knee.diameter.z)
# f) Forearm.girth vs. forearm.girth.z
plot(body$Forearm.girth, body$Forearm.girth.z)
# Was sagen die Plots e-f aus?
#Verh�ltnise bleiben die gleichen. 

# 7) Stellen Sie ein zweites lineares Modell auf:
# "Gewicht als abh�ngige Variable der beiden z-transformierten Variable"
# und interpretieren Sie das Modell:
model.z = lm(Weight ~ Knee.diameter.z + Forearm.girth.z, data = body)
summary(model.z)

# - Was genau sagt der Intercept aus? Ist der Intercept praktisch m�glich? Was also sagt er aus?
#Er ist nun positiv mit 69.1475 und theoretisch m�glich.

# - Was sagen die Koeffizienten f�r knee.diameter.z und forearm.girth.z?
#Wenn Knieumfang um 1 Standardabweichung (1.35cm) erh�ht wird, steigt das
#Gewicht um 3.87kg

# - Wie viel Prozent der Varianz im Gewicht wird durch das Modell erkl�rt?
# Vergleichen Sie die Werte mit dem urspr�nglichen Modell!
#Immer noch das gleiche mit 79%.
#Beide Modelle erkl�ren gleich viel.
#Liegt daran das wir das Modell nicht manipuliert sondern nur skaliert haben.

# 8) Nehmen Sie den ersten Fall aus dem Datensatz und sagen Sie das Gewicht vorher anhand
# a) des ersten Modells - und der unskalierten Variablen
fall1 = body[1,]
predict(model, newdata = data.frame(fall1), type = 'response')
#69.29447
# b) des skalierten Modells - auf Basis der z-Werte
predict(model.z, newdata = data.frame(fall1), type = 'response')
#69.29447

# �berdenken Sie nochmal Ihre Interpretation der Koeffizienten.
# Was bedeutet eine �nderung von +-1 bei den Variablen?
# Hinweise:
# - mit coef() erhalten Sie die Koeffizienten des Modells
# - Ergebnis sollte beide Male ca. 69.3kg ergeben
#Was soll ich �berdenken? Sagt der Intercept einfach nix aus?


# 9) Schauen Sie sich f�r das z-Modell die erkl�rte Varianz an und die Anova-Rechnung an.
# Unterscheiden sich die Werte f�r das Modell?
Anova(model.z)
drop1(model.z)
#Sind noch immer die gleichen Werte

# 10) z-Skalieren Sie nun auch das Gewicht selbst (als body$weight.z)
# Ben�tzen Sie statt manueller Umrechnung  die scale()-Funktion.
body$Weight.z = scale(body$Weight)
#scale() retourniert keinen Vektor, sondern eine Matrix mit einer Spalte.
#Dies kann zu unerwarteten Problemen f�hren.

# Schauen Sie sich wieder Histogramm, Mittelwert und Standardabweichung an.
hist(body$Weight)
hist(body$Weight.z)
describe(body$Weight)
describe(body$Weight.z)
#Nach zTransformation ist mena 0 und sd 1.

# Setzen Sie dann ein neues Modell auf und interpretieren Sie wieder die Koeffizienten.
model.z2 = lm(Weight.z ~ Knee.diameter.z + Forearm.girth.z, data = body)
summary(model.z)

# Was hat sich ge�ndert? Was bleibt gleich?
#Intercept ist wieder negativ. Eigentlich �ndert sich alles au�er die 79%.

# Wenn ich eine Forearm.girth=26 und eine Knee.diameter=18.8 habe, welches Gewicht sagt das Modell vorher?
# (Einige Umrechnungen sind notwendig)
knee.z = (18.8-mean(body$Knee.diameter))/sd(body$Knee.diameter)
Forearm.z = (26-mean(body$Forearm.girth))/sd(body$Forearm.girth)

Weight.z = predict(model.z2, newdata = data.frame(Knee.diameter.z=knee.z, Forearm.girth.z=Forearm.z),type = 'response')
#Es Ergibt 0.01100979 was auf unserer Linie liegt.
#Dies ist aber der zTranformierte Wert. Muss umgerechnet werden.
Weight.z*sd(body$Weight)+mean(body$Weight)
#Ergibt wieder 69.29447.