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

# Die RMS Titanic versank am 15. April 1912 im Meer. Von den �ber 2000 Personen an Bord
# starben mehr als 1500. Der vorliegende Datensatz enth�lt folgende Angaben zu den Personen:

# - survived ... ob eine Person �berlebt hat oder nicht
# - class ... ob es sich um ein Crew-Mitglied oder eine PassagierIn der 1./2./3. Klasse handelt
# - age ... Kind oder Erwachsene
# - sex ... Mann/Frau

# Schritt 1: Verwenden Sie nur die ersten 1800 Datens�tze.
#            Erstellen Sie ein Modell, um den Einfluss zu berechnen, den die
#            verschiedenen Variable auf das �berleben haben.
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

#--------------------------------------------Lineare Abh�ngigkeit
vif(model)
#Alle Werte befinden sich unter 2 daher ist nur eine Geringe Abh�ngigkeit zu bemerken

########################### Ausrei�er Tests ###############################
outlierTest(model)
#No Studentized residuals with Bonferonni p < 0.05
plot(model, which=4)
#Keine Werte �ber 0.5. Gut.
plot(model, which=5)
#Unsere Werte liegen unter 0.5 daher sind die Ausrei�er nicht so schlimm 
#das kann man auch im Leverage test sehen, weshalb wir hier mit unserem Model 
#weiter machen k�nnen.

#######_Berechnen der Bedeutsamkeit der Variablen innerhalb meines Modells_########
Anova(model, type = 2)
#sex tr�gt am meisten zur Erkl�rkraft des modells bei.

drop1(model) #AIC interpretiern und gr��er ist besser
#Auch hier sehen wir den Einfluss auf das Modell: sex > class > age


# Schritt 2: Erstellen Sie ein zweites Modell, das zus�tzlich einen Interaktionsterm
#            class:sex enth�lt. Validieren Sie Ihr Modell.
#            Vergleichen Sie Modell 1 und Modell 2. Zu welchem Schluss kommen Sie?
model2=glm(survived ~ class + age + sex + class:sex, data = dfn, family = binomial(link = "logit"))

########################### Model Tests ##########################

#------------------------------------Overdisperion
deviance(model2)/model2$df.residual
#Wert unter 1. Sehr gut.

#-------------------------------------Logit Test
hinkley(model2)
#y.hat^2 ist ok.

#--------------------------------------------Lineare Abh�ngigkeit
vif(model2)
#Da wir einen Interaktionsterm hinzugef�gt haben sind die Werte dieses Terms und der
#enhaltenen Variablen erh�ht, was uns aber nicht weiter wundert. Rest sieht gut aus.

########################### Ausrei�er Tests ###############################
outlierTest(model2)
#No Studentized residuals with Bonferonni p < 0.05
plot(model2, which=4)
#Keine Werte �ber 0.5. Gut.
plot(model2, which=5)
#Unsere Werte liegen unter 0.5 daher sind die Ausrei�er nicht so schlimm 
#das kann man auch im Leverage test sehen, weshalb wir hier mit unserem Model 
#weiter machen k�nnen.

#######_Berechnen der Bedeutsamkeit der Variablen innerhalb meines Modells_########
Anova(model2, type = 2)
#Wir sehen den Einfluss auf das Modell: sex > class > class:sex > age

drop1(model2) #AIC interpretiern und gr��er ist besser
#Denkt sich soweit mit Anova. Wir sehen: class:sex > age > '<none>'


########################_Modellvergleiche_############################
#--------------------------------------------anova 
anova(model, model2)
#model2 ist etwas kleiner mit 1791 (Resid. Df) als model mit 1794 und somit geringf�gig Besser.

#---------------------------------------------AIC 
#auch hier k�nnen wir mehrere Modelle miteinander vergleichen
AIC(model, model2)
#Auch hier sehen wir Bessere Werte bei model2

#---------------------------------------------ROC
ROC(model, main="Modell")
ROC(model2, main="Modell 2")
#Anhand der ROC Kurve ist kein gro�er Unterschied zu erkennen.

cbind(data.frame(logisticR2(model)), logisticR2(model2))
#Wir sehen das model2 auf grund der Werte am besten vorhersagt.

#Anhand der Tests kann gesagt werden das model2 geringf�gig Besser ist als model.
#Der Interaktionsterm d�rfte sich also positiv auf unser Modell auswirken.

# Schritt 3: Interpretieren Sie das zweite Modell.
#            Welche Aussage k�nnen Sie insbesondere zu Frauen & M�nnern in den drei
#            Klassen machen? Gibt es Auff�lligkeiten?

######################## Zur�ckrechnen und Interpretieren ###########################
intcp = inv.logit(coef(model2)[1])
#Basisfall: Eine erwachsene Frau die zur Crew geh�rt hat eine 88% Wahrscheinlichkeit zu �berleben.

#Ganzes Modell zur�ckrechnen:
exp(coef(model2))
#W�re die erwachsene Frau in der ersten Klasse, w�rden sich die Chance zu �berleben auf das 
  #4.85-fache erh�hen.
#W�re die erwachsene Frau in der zweiten Klasse, w�rden sich die Chance zu �berleben auf das 
  #0.89-fache senken.
#W�re die erwachsene Frau in der dritten Klasse, w�rden sich die Chance zu �berleben auf das 
  #0.09-fache senken.

summary(model2)
exp(coef(model2)["classthird:sexmale"]+coef(model2)["sexmale"]+coef(model2)["(Intercept)"])

dfn$sex = relevel(dfn$sex, ref = 'male')
model2b=glm(survived ~ class + age + sex + class:sex, data = dfn, family = binomial(link = "logit"))

intcp = inv.logit(coef(model2b)[1])
#Basisfall: Ein erwachsener Mann der zur Crew geh�rt hat eine 22% Wahrscheinlichkeit zu �berleben.

#Ganzes Modell zur�ckrechnen:
exp(coef(model2b))
#W�re der erwachsene Mann in der ersten Klasse, w�rden sich die Chance zu �berleben auf das 
#1.95-fache erh�hen.
#W�re der erwachsene Mann in der zweiten Klasse, w�rden sich die Chance zu �berleben auf das 
#0.53-fache senken.
#W�re der erwachsene Mann in der dritten Klasse, w�rden sich die Chance zu �berleben auf das 
#0.57-fache senken.

#M�nner haben tendenziell eine schlechtere Wahrscheinlichkeit zu �berleben als Frauen.
#In der ersten Klasse hat man die besten �berlebenschancen.
#Bei Frauen sinkt die �berlbenschance stark von Klasse zu Klasse.
#Auff�llig ist das M�nner in der dritten Klasse eine viel h�here Chance haben zu �berleben
#als Frauen und sogar eine h�here als andere M�nner in der zweiten Klasse.

# Schritt 4: Nehmen Sie die verbleibenden 401 Datens�tze und berechnen Sie absolute
#            �berlebenswahrscheinlichkeiten f�r jede Person f�r beide Modelle. 
#            Wie viele falsche Vorhersagen ergeben sich? (Grenze f�r �berleben: 50%)
#            Stimmt das mit dem Vergleich aus Schritt 2 �berein?
#            Wo l�ge der optimale Cutoff-Point? (=am wenigsten falsche Vorhersagen)

########################_Vorhersagen_###########################
dfp = df[1801:2201,]

#--------------------------------------------cutoff
cutoff = 0.5 #k�nnte eine Angabe sein
pred1 = predict(model, dfp, type='response')
predict_true = sum(pred1 > cutoff & dfp$survived == 'yes') #wenn �ber cutoff, dann 1, sonst 0
tpr = predict_true / sum(pred1 >cutoff)
#tpr = 0.7241379
1-tpr
#fpr = 0.2758621
401-(401*0.27)

cutoff = 0.5 #k�nnte eine Angabe sein
pred2 = predict(model2, dfp, type='response')
predict_true2 = sum(pred2 > cutoff & dfp$survived == 'yes') #wenn �ber cutoff, dann 1, sonst 0
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
#Tiefster Punkt in der Grafik zeigt ungef�hr den Bereich in dem der tatsaechliche Cutoff 
#liegen wird.

#Cutoff Bereichsgrenzen berechnen
untergrenze=cutoff[which.min(fehler_m1)] 
#AW: optimaler Cutoff fuer model von 0.5595596
obergrenze=rev(cutoff)[which.min(rev(fehler_m1))] 
#AW: optimaler Cutoff fuer model von 0.6026026

fehleranz=min(fehler_m1) #AW: Es liegen 93 Fehler vor
#AW: d.h. bei model: liegt der Cutoff f�r minimale Fehleranzahl irgendwo 
#zwischen 0.56 bis 0.60.


# Allgemeine Tipps:
# - Lesen Sie die Angabe genau durch und setzen Sie die geforderten Punkte um.
#   Fleißaufgaben kosten Zeit, bringen aber keine zusätzlichen Punkte.
# - Die Interpretation (wo gefordert) muss so geschrieben sein, dass jede/jeder sie
#   verstehen kann (auch ohne mathematischen Hintergrund). Bedeutet unter anderem:
#   passend rücktransformieren falls transformiert wurde.
# - Die "ersten X Datensätze" meint wirklich *die ersten*. Keine Zufallsauswahl!
