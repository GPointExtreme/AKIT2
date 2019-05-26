#1
body = read.csv('C:\\Users\\Dominik\\Downloads\\blutdruck.csv')

#2
describe(body)
#mean blood.pressure = 129.78
#mean current.weight = 82.69
#sd blood.pressure = 11.14
#sd current.weight = 14.61

#3
par(mfrow=c(2,2))
#Befehl um mehrere Diagramme anzuzeigen.
hist(body$blood.pressure)
hist(body$current.weight)

#4
plot(body$current.weight, body$blood.pressure)

#5
model = lm(blood.pressure ~ current.weight, data = body)

#5a
koeff = coef(model)
abline(a=koeff[1], b=koeff[2], col = "blue")
summary(model)
confint(model)
#Bei erhöhung des Gewichts um 1 Einheit erhöht sich der Blutdruck um 0,39699
#95% alle Werte würden zwischen 93,56 und 100,35 liegen.

#5b
y55 = koeff[1]+koeff[2]*55
points(55, y55, col = "Green")

y75 = koeff[1]+koeff[2]*75
points(75, y75, col = "Green")

y95 = koeff[1]+koeff[2]*95
points(95, y95, col = "Green")
#Alle drei liegen auf der Linie

#5c
predict(model, newdata = data.frame(current.weight = 55, type='response'))
#118.7873
predict(model, newdata = data.frame(current.weight = 75, type='response'))
#126.7271
predict(model, newdata = data.frame(current.weight = 95, type='response'))
#134.667

#Die Prediction liefert die gleichen Ergebnisse wie die Regressionsgleichung