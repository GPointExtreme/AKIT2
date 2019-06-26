# AKIT2 SS18, Nachklausur (4. Termin), 9.1.2019
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

df <- read.csv('data.csv')

# In der norwegischen Stadt Oslo wurden im Stadtteil Alnabru im Zeitraum
# Oktober 2001 bis August 2003 der Feinstaub (PM10-Partikel) gemessen.
# Folgende Daten wurden erhoben:

# - pm10 ... ob der Feinstaub-Grenzwert überschritten worden ist
# - car ... Anzahl der Autos pro Stunde (auf Straße neben Messstation)
# - temp.2m ... Temperatur 2m über dem Boden
# - temp.25m ... Temperaturdifferenz zu 25m über dem Boden
# - wind ... Windgeschwindigkeit (Meter/Sekunde)
# - wind.direction ... Windrichtung (grob in 4 Richtungen: N, W, S, O)

#daten mal analysieren und gefühl bekommen

View(df)
view(df) #car und temp.2m hohe sds->könnte für eine log-Transformation interessant sein
hist(df$car)
mean(df$car) #im Schnitt 1700 Autos pro h
max(df$car)/min(df$car) #94.2
hist(df$temp.25m)
mean(df$temp.25m) #im Schnitt 0.13
hist(df$temp.2m)
mean(df$temp.2m) #im Schnitt 0.9 Einheit "Celsius"
hist(df$wind)
max(df$wind)/min(df$wind) #33 die Verhältnisse (94.2 und 33) von wind und car sind nicht allzugroß daher mal ohne log-Transformation
mean(df$wind) #im Schnitt 3.2m/Sek
table(df$wind.dirction) #verteilung könnte besser sein. W im vergleich weniger daten->kann sich auf die Aussagekraft wirken
sum(df$pm10==1)/length(df$pm10) #11% der Datensätze haben den Feinstaub Überschritte

#Das Modell wird mal ohne log ausgeführt

# Schritt 1:
#   Erstellen Sie ein Modell für die Vorhersage der Überschreitung des Feinstaubgrenzwerts.
#   Validieren Sie das Modell.
#   Beantworten Sie folgende Fragen (und geben Sie verständliche Zahlen dazu an)
#     - Was ist der Basisfall und welche Wahrscheinlichkeit einer Grenzwertüberschreitung
#       hat dieser?
#     - Welche Auswirkung haben Autos, Wind und Temperatur?


###Modell aufsetzen####
model = glm(df$pm10 ~  car + temp.2m + temp.25m + wind + wind.direction, data=df, family = binomial(link='logit'))
summary(model)
#Basisterm (=wind.directionE) ist signifikant. variable car, wind und wind.directionN auch sig.


#- Was ist der Basisfall und welche Wahrscheinlichkeit einer Grenzwertüberschreitung hat dieser?
#Logit-Werte können interpretiert werden, daher alles exp()
koeff = coef(model)
intcp = inv.logit(koeff[1])
intcp # Wenn alle Koeffizienten = 0 sind und die Windrichtung von Osten käme, dann wäre die absolute Wahrscheinlichkeit (signifikant) für eine Grenzwertüberschreitung bei 3.1%


#     - Welche Auswirkung haben Autos, Wind und Temperatur?
koeff
odds = exp(koeff)
odds["car"] #erhöht sich die Anzahl pro Stunden um 1 Auto , so erhöht sich die Chance für eine Überschreitung signifikant auf den Faktor 1.0007 bzw. um den Faktor 0,07%?
odds[3] #erhöht sich die temp 2m über den Boden um 1 Einheit, so erhöht sich die Chance auf den Faktor 1.02 bzw um 2.3 %
1-odds[4] #erhöht sich die Temp.differenz um eine Einheit, so veringert sich die Chance auf den Faktor 0.25
1-odds[5] #erhöht sich die windgeschwindigkeit um 1m/s mehr so verringert sich die Chance signifikant um den Faktor 0.21
odds[6] #die Windrichtung N erhöht die Chance signifikant für eine Überschreitung um 138,3% gegenüber den Basistermin wind.directionE
odds[7] #die Windrichtung S erhöht die Chance für eine Überschreitung um 34.1% gegenüber den Basistermin wind.directionE
odds[8] #die Windrichtung W erhöht die Chance für eine Überschreitung um 68,9% gegenüber den Basistermin wind.directionE

deviance(model) / model$df.residual # ok, da unter 1

#Henkley-test
hinkley(model) # y.hat2 ("ok") ist nicht sig. somit fehlen keine Variablen bzw. die logit-funktion ist für unser modell passend

Anova(model)# den größten einfluss (sig!) hat car > wind (sig!) >temp.25 > temp.2m

outlierTest(model) #gut da No Studentized residuals with Bonferonni p < 0.05

plot(model, which=4) #gut, Ausreisser (80,213 und 456), aber keine Auswirkung auf das model, da unter 1
plot(model,which=5)  #auch gut alles unter 0.5


ROC(model) #
#ROC-kurve  könnte aber besser sein bzw. die fläche unter der kurve größer. Vrhersagekraft dadurch nicht so gut

drop1(model)[order(drop1(model)[,3]),]
#durch weglassen von Variablen kann das modell verbessert werden in diesem Fall "temp.2m" und wind.direction->AIC dadurch geringer(besser).
#da die Verbesserung gering ist, mal so lassen (bzw. für Frage 2 aufheben)



# Schritt 2:
#   Reduzieren Sie das Modell aus Schritt 1 auf möglichst wenige Variable.
#   (Die Messstationen sollen so einfach wie möglich sein.)
#   Validieren Sie das so entstandene Modell.
#   Vergleichen Sie das so neue Modell mit dem Modell aus Schritt 1.

drop1(model)[order(drop1(model)[,3]),] #die variablen temp.2m und wind.direction werden entfernt, da der AIC  dadurch geringer(besser) wird.
model2 = glm(df$pm10 ~  car + temp.25m + wind , data=df, family = binomial(link='logit'))

summary(model2) #alle variablen sig. ->gut erstmal

deviance(model2) / model2$df.residual #  Overdisposion gut, da unter 1

#Henkley-test
hinkley(model2) #gut

outlierTest(model2) #gut da No Studentized residuals with Bonferonni p < 0.05
plot(model2, which=4) #gut, ausreisser (106, 180 und 208), aber keine auswirkung auf das model, da unter 1
plot(model2,which=5)  #auch gut
ROC(model2) 
ROC(model)
#ROC- könnte besser sein. verglichen mit 1  ein bisschen kleiner und sagt dadruch schlechter vorher, aber nicht ganz klar ersichtlich

drop1(model2)[order(drop1(model2)[,3]),] #entfernen einer variable nicht nötig da, AIC nicht besser wird
Anova(model2) # auch hier car mit größtem einfluss (sig!)  gefolgt von wind (sig!) und temp.25

AIC(model, model2) # AIC bei modell 2 besser, da AIC niedriger ist
# modell 2 in summe besser
anova(model, model2) # model2 besser aber nicht sig. besser zu model
cbind(logisticR2(model), logisticR2(model2)) #model1 ein wenig besser bzw. geringfügig höhere Werte als modell2 und erklärt besser (je näher bei 1 desto besser)


# Schritt 3:
#   Wie gut sind die beiden Modelle in der Vorhersage?
#   Wählen Sie selbständig eine Strategie für den Vergleich und begründen Sie
#   Ihre Wahl der Strategie und weiterer Parameter.
summary(df)

# - pm10 ... ob der Feinstaub-Grenzwert überschritten worden ist
# - car ... Anzahl der Autos pro Stunde (auf Straße neben Messstation)
# - temp.2m ... Temperatur 2m über dem Boden
# - temp.25m ... Temperaturdifferenz zu 25m über dem Boden
# - wind ... Windgeschwindigkeit (Meter/Sekunde)
# - wind.direction ... Windrichtung (grob in 4 Richtungen: N, W, S, O)

# ich habe mich für die durchschnittliche Berechnung entschieden, da es von der Witterung und von der Geschwindigkeit des Windes
#sich zwischen den Zeiten (Winter, Herbst,..) unterscheiden kann. 

#mit der ersten predict will ich überprüfen, ob sich die Erhöhung der Anzahl der Autos sich wirklich auf die Grenzüberschreitung auswirkt. 
#Es heißt nicht umsonst das Autos (mit Kraftstoff Diesel oder Benzin) sich negativ auf die Umwelt auswirken. 
#Für die Windrichtung habe ich "N" genommen, da es laut Datensatz und ich die Vermutung (kein Domänenwissen!) aufstelle, dass es in Oslo häufig der Wind aus dem Norden kommt.
describe(df)
levels(df$wind.direction)
###Model1###
pred1 = predict(model, data.frame(car = c(0, min(df$car) ,mean(df$car), max(df$car)),temp.2m = mean(df$temp.2m), 
                                  wind = mean(df$wind), temp.25m =mean(df$temp.25m) ,wind.direction="N"), type ="response")


pred2 = predict(model2, data.frame(car = c(0, min(df$car) ,mean(df$car), max(df$car)),temp.2m = mean(df$temp.2m), 
                                  wind = mean(df$wind), temp.25m =mean(df$temp.25m) ,wind.direction="N"), type ="response")
pred2
cbind(pred1, pred2)

#hier sind man, dass die erste model eine viel höhere WSL für eine Grenzüberschreitung darstellt. Aber sowohl pred1 und pred2 zeigen, dass je mehr 
#Autos sich auf den Straßen befinden, desto höher ist die WSK, dass die Überschreitung eintritt.



# Schritt 4:
#   Politiker überlegen, auf Basis Ihres ersten Modells temporäre Fahrverbote einzuführen.
#   Angenommen der wirtschaftliche Schaden (Zeitverlust) durch ein falsch verhängtes Fahrverbot
#   (== Modell sagt Überschreitung des Grenzwerts vorher, aber tatsächlich keine Überschreitung)
#   beträgt 100.000 norwegische Kronen; der wirtschaftliche Gewinn (= Gesundheit)
#   für ein zurecht verhängtes Fahrverbot 500.000 Kronen; die Kosten für ein unterlassenes
#   Fahrverbot (== Modell sagt keine Überschreitung vorher, aber Grenzwert wird überschritten)
#   betragen 300.000 Kronen.
#   Wie sollte Ihr Modell parametrisiert werden, um einen möglichst großen Gesamtnutzen zu erzielen?
#FRAGE: Können Sie mir bitte zur Zeile 176 mitteilen, was genau da gewollt war inkl. Bsp?
#   Würden Sie auf Basis Ihres Modells Fahrverbote verhängen? Warum? Warum nicht?

##Model1##

pred3 = predict(model, type ="response")
cutoff = seq(0, 1, length.out = 1000)

wert = numeric(1000)
for(i in 1:1000)
{
  kosten1 = sum(pred3 > cutoff[i]) *100000 #TNR
  einnahmen = sum(pred3 > cutoff[i] & df$pm10 == 1) *500000 #TPR
  kosten2 =  sum(pred3 < cutoff[i] & df$pm10 == 1) *300000 #FPR
  wert[i] = einnahmen - kosten1 - kosten2
}
plot(cutoff, wert, type='l')
#recht schnell das Gewinnmaximum
cutoff[which.max(wert)]
#Gewinnmaximum liegt bei einem Cutoff von 0.15


###Model2
pred4 = predict(model2, type ="response")
cutoff = seq(0, 1, length.out = 1000)

wert = numeric(1000)
for(i in 1:1000)
{
  kosten1.2 = sum(pred4 > cutoff[i]) *100000 #TNR
  einnahmen1.2 = sum(pred4 > cutoff[i] & df$pm10 == 1) *500000 #TPR
  kosten2.2 =  sum(pred4 < cutoff[i] & df$pm10 == 1) *300000 #FPR
  wert[i] = einnahmen1.2 - kosten1.2 - kosten2.2
}
plot(cutoff, wert, type='l')
cutoff[which.max(wert)]
#CGewinnmaximum liegt bei einem Cutoff 0.14 für model2


#Ich würde mich für ein Fahrverbot entscheiden, denn beide predicts für model1 und 2
#haben gezeigt, dass die Erhöhung der Autos zu höhere Grenzüberschreitung führen. 




# Quelle: Norwegian Public Roads Administration.
# Submitted by Magne Aldrin (magne.aldrin@nr.no). [28/Jul/04]
# http://lib.stat.cmu.edu/datasets/
# Daten für Klausur modifiziert.
