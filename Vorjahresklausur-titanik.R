# AKIT2 SS18, Nachklausur (2. Termin), 19.9.2018
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

df <- read.csv('C:\\Users\\Dominik\\Downloads\\titanik.csv')

# Die RMS Titanic versank am 15. April 1912 im Meer. Von den über 2000 Personen an Bord
# starben mehr als 1500. Der vorliegende Datensatz enthält folgende Angaben zu den Personen:

# - survived ... ob eine Person Überlebt hat oder nicht
# - class ... ob es sich um ein Crew-Mitglied oder eine PassagierIn der 1./2./3. Klasse handelt
# - age ... Kind oder Erwachsene
# - sex ... Mann/Frau

# Schritt 1: Verwenden Sie nur die ersten 1800 Datensätze.
#            Erstellen Sie ein Modell, um den Einfluss zu berechnen, den die
#            verschiedenen Variable auf das Überleben haben.
#            Validieren Sie Ihr Modell.
dfn=df[1:1800,]

model=glm(survived ~ class + age + sex, data = dfn, family = binomial(link = "logit"))

########################### Model Tests ##########################

#------------------------------------Overdisperion
deviance(model)/model$df.residual
#Wert unter 1. Sehr gut.

#-------------------------------------Logit Test
hinkley(model)
#y.hat^2 ist not ok. Deutet auf fehlende Variable bzw. 
#eine nicht lineare Beziehung oder andere Probleme im Modell hin.

#--------------------------------------------Lineare Abhängigkeit
vif(model)
#Alle Werte befinden sich unter 2 daher ist nur eine Geringe Abhängigkeit zu bemerken

########################### Ausreißer Tests ###############################
outlierTest(model)
#No Studentized residuals with Bonferonni p < 0.05
plot(model, which=4)
#Keine Werte über 0.5. Gut.
plot(model, which=5)
#Unsere Werte liegen unter 0.5 daher sind die Ausreißer nicht so schlimm 
#das kann man auch im Leverage test sehen, weshalb wir hier mit unserem Model 
#weiter machen können.

#######_Berechnen der Bedeutsamkeit der Variablen innerhalb meines Modells_########
Anova(model, type = 2)
#sex trägt am meisten zur Erklärkraft des modells bei.

drop1(model) #AIC interpretiern und größer ist besser
#Auch hier sehen wir den Einfluss auf das Modell: sex > class > age


# Schritt 2: Erstellen Sie ein zweites Modell, das zusätzlich einen Interaktionsterm
#            class:sex enthält. Validieren Sie Ihr Modell.
#            Vergleichen Sie Modell 1 und Modell 2. Zu welchem Schluss kommen Sie?
model2=glm(survived ~ class + age + sex + class:sex, data = dfn, family = binomial(link = "logit"))

########################### Model Tests ##########################

#------------------------------------Overdisperion
deviance(model2)/model2$df.residual
#Wert unter 1. Sehr gut.

#-------------------------------------Logit Test
hinkley(model2)
#y.hat^2 ist ok.

#--------------------------------------------Lineare Abhängigkeit
vif(model2)
#Da wir einen Interaktionsterm hinzugefügt haben sind die Werte dieses Terms und der
#enhaltenen Variablen erhöht, was uns aber nicht weiter wundert. Rest sieht gut aus.

########################### Ausreißer Tests ###############################
outlierTest(model2)
#No Studentized residuals with Bonferonni p < 0.05
plot(model2, which=4)
#Keine Werte über 0.5. Gut.
plot(model2, which=5)
#Unsere Werte liegen unter 0.5 daher sind die Ausreißer nicht so schlimm 
#das kann man auch im Leverage test sehen, weshalb wir hier mit unserem Model 
#weiter machen können.

#######_Berechnen der Bedeutsamkeit der Variablen innerhalb meines Modells_########
Anova(model2, type = 2)
#Wir sehen den Einfluss auf das Modell: sex > class > class:sex > age

drop1(model2) #AIC interpretiern und größer ist besser
#Denkt sich soweit mit Anova. Wir sehen: class:sex > age > '<none>'


########################_Modellvergleiche_############################
#--------------------------------------------anova 
anova(model, model2)
#model2 ist etwas kleiner mit 1791 (Resid. Df) als model mit 1794 und somit geringfügig Besser.

#---------------------------------------------AIC 
#auch hier können wir mehrere Modelle miteinander vergleichen
AIC(model, model2)
#Auch hier sehen wir Bessere Werte bei model2

#---------------------------------------------ROC
ROC(model, main="Modell")
ROC(model2, main="Modell 2")
#Anhand der ROC Kurve ist kein großer Unterschied zu erkennen.

cbind(data.frame(logisticR2(model)), logisticR2(model2))
#Wir sehen das model2 auf grund der Werte am besten vorhersagt.

#Anhand der Tests kann gesagt werden das model2 geringfügig Besser ist als model.
#Der Interaktionsterm dürfte sich also positiv auf unser Modell auswirken.

# Schritt 3: Interpretieren Sie das zweite Modell.
#            Welche Aussage können Sie insbesondere zu Frauen & Männern in den drei
#            Klassen machen? Gibt es Auffälligkeiten?

######################## Zurückrechnen und Interpretieren ###########################
intcp = inv.logit(coef(model2)[1])
#Basisfall: Eine erwachsene Frau die zur Crew gehört hat eine 88% Wahrscheinlichkeit zu überleben.

#Ganzes Modell zurückrechnen:
exp(coef(model2))
#Wäre die erwachsene Frau in der ersten Klasse, würden sich die Chance zu überleben auf das 
  #4.85-fache erhöhen.
#Wäre die erwachsene Frau in der zweiten Klasse, würden sich die Chance zu überleben auf das 
  #0.89-fache senken.
#Wäre die erwachsene Frau in der dritten Klasse, würden sich die Chance zu überleben auf das 
  #0.09-fache senken.

summary(model2)
exp(coef(model2)["classthird:sexmale"]+coef(model2)["sexmale"]+coef(model2)["(Intercept)"])

dfn$sex = relevel(dfn$sex, ref = 'male')
model2b=glm(survived ~ class + age + sex + class:sex, data = dfn, family = binomial(link = "logit"))

intcp = inv.logit(coef(model2b)[1])
#Basisfall: Ein erwachsener Mann der zur Crew gehört hat eine 22% Wahrscheinlichkeit zu überleben.

#Ganzes Modell zurückrechnen:
exp(coef(model2b))
#Wäre der erwachsene Mann in der ersten Klasse, würden sich die Chance zu überleben auf das 
#1.95-fache erhöhen.
#Wäre der erwachsene Mann in der zweiten Klasse, würden sich die Chance zu überleben auf das 
#0.53-fache senken.
#Wäre der erwachsene Mann in der dritten Klasse, würden sich die Chance zu überleben auf das 
#0.57-fache senken.

#Männer haben tendenziell eine schlechtere Wahrscheinlichkeit zu überleben als Frauen.
#In der ersten Klasse hat man die besten überlebenschancen.
#Bei Frauen sinkt die Überlbenschance stark von Klasse zu Klasse.
#Auffällig ist das Männer in der dritten Klasse eine viel höhere Chance haben zu überleben
#als Frauen und sogar eine höhere als andere Männer in der zweiten Klasse.

# Schritt 4: Nehmen Sie die verbleibenden 401 Datensätze und berechnen Sie absolute
#            Überlebenswahrscheinlichkeiten für jede Person für beide Modelle. 
#            Wie viele falsche Vorhersagen ergeben sich? (Grenze für Überleben: 50%)
#            Stimmt das mit dem Vergleich aus Schritt 2 Überein?
#            Wo läge der optimale Cutoff-Point? (=am wenigsten falsche Vorhersagen)

########################_Vorhersagen_###########################
dfp = df[1801:2201,]

#--------------------------------------------cutoff
cutoff = 0.5 #könnte eine Angabe sein
pred1 = predict(model, dfp, type='response')
predict_true = sum(pred1 > cutoff & dfp$survived == 'yes') #wenn über cutoff, dann 1, sonst 0
tpr = predict_true / sum(pred1 >cutoff)
#tpr = 0.7241379
1-tpr
#fpr = 0.2758621
401-(401*0.27)

cutoff = 0.5 #könnte eine Angabe sein
pred2 = predict(model2, dfp, type='response')
predict_true2 = sum(pred2 > cutoff & dfp$survived == 'yes') #wenn über cutoff, dann 1, sonst 0
tpr2 = predict_true2 / sum(pred2 >cutoff)
#tpr2 = 0.9074074
1-tpr2
#fpr2 = 0.09259259

#model2 ist besser weil es weniger Werte falsch vorhersagt.


#------------------------------------optimalen cutoff berechnen

pred1=predict(model,newdata=dfp,"response")
wirklich = dfp$survived == "yes" #wo die Testdaten wirklich TRUE ergeben

fehler = function(pred, cutoff, wirklich) {
  p1 = pred > cutoff
  anzahl_fehler = sum(wirklich != p1)
  return(anzahl_fehler)
}

cutoff = seq(0, 1, length.out = 1000)

fehler_m1 = numeric(1000)
for (i in 1:1000) {
  fehler_m1[i] = fehler(pred1, cutoff[i], wirklich)
}

#als Diagramm dargestellt
plot(cutoff, fehler_m1, type='l', ylab='Falsche Vorhersagen', main="Modell 1")
#Tiefster Punkt in der Grafik zeigt ungefähr den Bereich in dem der tatsaechliche Cutoff 
#liegen wird.

#Cutoff Bereichsgrenzen berechnen
untergrenze=cutoff[which.min(fehler_m1)] 
#AW: optimaler Cutoff fuer model von 0.5595596
obergrenze=rev(cutoff)[which.min(rev(fehler_m1))] 
#AW: optimaler Cutoff fuer model von 0.6026026

fehleranz=min(fehler_m1) #AW: Es liegen 93 Fehler vor
#AW: d.h. bei model: liegt der Cutoff für minimale Fehleranzahl irgendwo 
#zwischen 0.56 bis 0.60.


# Allgemeine Tipps:
# - Lesen Sie die Angabe genau durch und setzen Sie die geforderten Punkte um.
#   FleiÃŸaufgaben kosten Zeit, bringen aber keine zusÃ¤tzlichen Punkte.
# - Die Interpretation (wo gefordert) muss so geschrieben sein, dass jede/jeder sie
#   verstehen kann (auch ohne mathematischen Hintergrund). Bedeutet unter anderem:
#   passend rÃ¼cktransformieren falls transformiert wurde.
# - Die "ersten X DatensÃ¤tze" meint wirklich *die ersten*. Keine Zufallsauswahl!
