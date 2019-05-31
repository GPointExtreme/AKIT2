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
#Bei erhöhung des Gewichts um 1 kg erhöht sich der Blutdruck um 0,39699
#Wenn wir unendlich oft das vorliegende Experiment wiederholen
#dann schlössen 95% der Konfidenzintevalle den tatsächlichen
#(Populations)-Wert ein.
#Wir können aus dem vorliegenden Intervall nicht heraus lesen
#ob wir ein gutes Modell haben oder nicht.

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

#Neues Beispiel!

#1
body = read.csv('C:\\Users\\Dominik\\Downloads\\software-estimation.csv')

#2
hist(body$Function.point)
hist(body$Work.hours)
#Sind beide nicht gut verteilt

#3
plot(body$Function.point, body$Work.hours)
#Viele Datensätze sind in der linken unteren Ecke

#4 ohne Log:
model = lm(Work.hours ~ Function.point, data = body)
koeff = coef(model)
abline(a=koeff[1], b=koeff[2], col = "blue")

#4 mit Log:
#Log anwenden um Punkte aus linken unterer Ecke zu bekommen
model.log=lm(log2(Work.hours) ~ log2(Function.point), data = body)
#log2 verwenden weil Interpretieren dann einfacher ist.
hist(resid(model.log))

library(car)
qqp(resid(model.log))

plot(log2(body$Function.point),log2(body$Work.hours))
#plot sieht nun schon viel Besser aus

koeff.log = coef(model.log)
exp(koeff.log)
exp(confint(model.log))
abline(model.log)
summary(model.log)
#Ändert sich workhours.log um 1.002, d.h. auch work.hours verdoppelt sich
#Pro 1 functionpoint.log (== functionpoint verdoppelt sich)
#Was heißt das jetzt genau?

#4a
summary(model)
confint(model)
#ohne Log: Bei erhöhung um 1 hour kommen 15,124 functionpoints dazu.

confint(model.log, level = 0.95)
2^confint(model.log, level = 0.95)[2,1]
#Lower Bound:
#Wenn sich functionpoints verdoppelt dann ver-x-facht sich hours um 1.76

2^confint(model.log, level = 0.95)[2,2]
#Upper Bound:
#Wenn sich functionpoints verdoppelt dann ver-x-facht sich hours um 2.276

#4b
#ohne Log:
predict(model, newdata = data.frame(Function.point = 100, type='response'))
#2098.09
predict(model, newdata = data.frame(Function.point = 200, type='response'))
#3610.516
predict(model, newdata = data.frame(Function.point = 400, type='response'))
#6635.369
predict(model, newdata = data.frame(Function.point = 800, type='response'))
#12685.07
predict(model, newdata = data.frame(Function.point = 1600, type='response'))
#24784.48
predict(model, newdata = data.frame(Function.point = 3200, type='response'))
#48983.3

#Mit Log:
#Aufpassen hier! Variable in Dataframe darf nicht die mit .log sein!
2^predict(model.log, newdata = data.frame(Function.point=100),type="response")
#1249.634
2^predict(model.log, newdata = data.frame(Function.point=200),type="response")
#2503.406
2^predict(model.log, newdata = data.frame(Function.point=400),type="response")
#5015.103
2^predict(model.log, newdata = data.frame(Function.point=800),type="response")
#10046.81
2^predict(model.log, newdata = data.frame(Function.point=1600),type="response")
#20126.9
2^predict(model.log, newdata = data.frame(Function.point=3200),type="response")
#40320.45