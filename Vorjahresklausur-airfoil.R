# AKIT2 SS18, Nachklausur (3. Termin), 25.10.2018
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

df <- read.csv('C:\\Users\\Dominik\\Downloads\\airfoil.csv')

# Wir arbeiten bei der NASA an der Untersuchung des Eigenlärms von Flugzeugflügeln bzw.
# Hubschrauber-Rotorblättern. Lärm ist nicht nur störend, sondern kann auch auf Probleme
# im Flügelprofil hinweisen. Aus Windkanal-Tests haben wir folgende Messwerte erhalten:

# - sound.pressure ... Schalldruck in Dezibel [db]
# - frequency ... gemessene Frequenz [Hz]
# - angle ... Anstellwinkel des Flügelprofils [Grad]
# - velocity ... Windgeschwindigkeit [m/s]
# - chord.length ... Referenzlänge des Flügelprofils [m]
# - displacement.thickness ... Verdrängungsdicke [m] (Dicke der entstehenden (Luft-)Grenzschicht 
      #zur FlügeloberflÃ¤che)

# Anmerkung: die Daten sind meist parallel gemessen worden (z.B. mehrere Frequenzen bei einem
#            Flügelprofil), wir behandeln sie aber als unabhängige Datensätze.
#            Überlegen Sie sich auch, ob Variable transformiert werden sollen.
summary(df)
view(df)
#Aufpassen auf frequency!

mapply(hist,as.data.frame(df),main=colnames(df))
#thickness und frequency ist auch nicht Normalverteilt
hist(df$frequency)
#sieht nicht Normalverteilt aus! Eventuell log-transformieren?
hist(log2(df$frequency))
#Viel Besser.
df$frequency.log = log2(df$frequency)


# Schritt 1: Erstellen Sie ein Modell für den Schalldruck und validieren Sie das Modell.
#            Verwenden Sie nur die _ersten_ 1300 Datensätze für Ihr Modell.
#            Interpretieren Sie die Abhängigkeit Schalldruck (db) zu Frequenz (Hz).
dfn=df[1:1300,]

model=lm(sound.pressure ~ frequency.log + angle + velocity + chord.length + displacement.thickness ,data=dfn)

###########################_Model Tests_###############################
vif(model)
#Alle Werte befinden sich unter 5 daher ist nur eine Geringe Abhängigkeit zu bemerken
cor(dfn$angle, dfn$displacement.thickness)
#Positive Korrelation von 0.76 ist nicht gut.

par(mfrow=c(2,2))
plot(model)
#Es ist schon eine leichte Bananenform zu erkenn. Nicht so gut.

hist(model$residuals)
#sieht normalverteilt aus
qqp(model)
#sieht gar nicht gut aus!

crPlots(model)
#frequency.log weicht ab.
#AW: Weicht der Pinke Wert z.B. bananenförmig von der blauen Linie ab, dann ist 
#das ein Hinweis, dass die Variable z.B. auch als Quadrat (x²) mit in das Modell 
#aufgenommen werden sollte.

###########################_Außreiser Tests_###############################

#7 Nach abschluss der eigentlichen Modell Teset müssen wir noch die Wirkung der 
#Außreise sofern es welche gibt darstellen
outlierTest(model)
plot(model, which=4) #sieht gut aus.
plot(model, which=5) #sieht gut aus.

#######_Berechnen der Bedeutsamkeit der Variablen innerhalb meines Modells_########

m1.aov = Anova(model, type=2) #--> type3 würde den Intercept mitanzeigen
ssq = m1.aov[,1]
names(ssq) = rownames(m1.aov) # Ergebnis passende Namen geben
ssq / m1.aov["Residuals",1]
#AW: unsere in der Forschungsfrage gestellte Variable frequency.log ehöht RSS um 39%
#Die übrigen unabhängigen Variablen ehöhen RSS um: 
#chord.length=28%
#die anderen sind noch kleiner

Anova(model, type = 3)
#Auch hier sehen wir das frequency.log den größten Einfluss hat.
#Zweitmeisten einfluss hat chord.length.

#8.1 Drop1 --> alternativ zu Anova können wir den Drop1 machen
drop1(model) #AIC interpretiern und größer ist besser
#AW: #Bei einen Drop1 interpretieren wir die AIC Werte
drop1(model)[order(drop1(model)[,4]),]

#Interpretieren Sie die Abhängigkeit Schalldruck (db) zu Frequenz (Hz).

summary(model)
2^-2.15200
#Wenn sich die Frequenz um 1Hz erhöht dann steigt auch der Schalldruck um 0.23db.

# Schritt 2: Das erste Modell weist wahrscheinlich eine hohe Kolinearität auf.
#            Entfernen Sie eine der Variablen um die Kolineratität zu verrringern und
#            begründen Sie Ihre Wahl.
#            Verwenden Sie zudem anstatt der Windgeschwindigkeit und der Flügellänge 
#            (chord.length) die sogenannte Reyonoldsnummer: reynr = velocity*chord.length
#            Validieren Sie das Modell.
#            Interpretieren Sie die geänderten Koeffizienten (angle, frequency,
#            displacement.thickness) - sofern nicht vorher entfernt - und deren
#            Konfidenzintervalle in Vergleich zum ersten Modell.

#Die Variable angle wird entfernt weil sie am wenigsten zum Modell beiträgt und im vif()
  #den höchsten Wert hat.

dfn$reynr = dfn$velocity*dfn$chord.length

model1=lm(sound.pressure ~ frequency.log + reynr + displacement.thickness ,data=dfn)

###########################_Model Tests_###############################
vif(model1)
#Alle Werte befinden sich unter 2 daher ist nur eine Geringe Abhängigkeit zu bemerken

par(mfrow=c(2,2))
plot(model1)
#Sieht besser aus.

hist(model1$residuals)
#sieht normalverteilt aus
qqp(model1)
#sieht Besser aus aber ein paar Werte verlassen noch immer die Grenzen.

crPlots(model1)
#frequency.log weicht noch immer ab.
#AW: Weicht der Pinke Wert z.B. bananenförmig von der blauen Linie ab, dann ist 
#das ein Hinweis, dass die Variable z.B. auch als Quadrat (x²) mit in das Modell 
#aufgenommen werden sollte.

###########################_Außreiser Tests_###############################

#7 Nach abschluss der eigentlichen Modell Teset müssen wir noch die Wirkung der 
#Außreise sofern es welche gibt darstellen
outlierTest(model1)
plot(model1, which=4) #sieht gut aus.
plot(model1, which=5) #sieht gut aus.

#######_Berechnen der Bedeutsamkeit der Variablen innerhalb meines Modells_########

m1.aov = Anova(model1, type=2) #--> type3 würde den Intercept mitanzeigen
ssq = m1.aov[,1]
names(ssq) = rownames(m1.aov) # Ergebnis passende Namen geben
ssq / m1.aov["Residuals",1]
#displacement.thickness ehöht RSS um 26%
#Die übrigen unabhängigen Variablen ehöhen RSS um: 
#frequency.log=24%
#reynr=10%

Anova(model1, type = 3)
#Nun hat displacement.thickness den größten Einlfuss auf das Modell
#Dicht gefolgt von frequency.log.

#8.1 Drop1 --> alternativ zu Anova können wir den Drop1 machen
drop1(model1) #AIC interpretiern und größer ist besser
#AW: #Bei einen Drop1 interpretieren wir die AIC Werte
drop1(model1)[order(drop1(model)[,4]),]
#Gleichen Erkenntnisse wie bei Anova.

#            Interpretieren Sie die geänderten Koeffizienten (angle, frequency,
#            displacement.thickness) - sofern nicht vorher entfernt - und deren
#            Konfidenzintervalle in Vergleich zum ersten Modell.

summary(model1)
2^-1.76686
#Wenn sich die Frequenz um 1Hz erhöht dann steigt auch der Schalldruck um 0.29db.
#Wenn sich die Verdrängungsdicke um 1m erhöht dann sinkt der Schalldruck um -270db.

confint(model)
confint(model1)
#frequency.log ändert sich von [-2.34 bis -1.96] auf [-1.97 bis -1.57].
#displacement.thickness ändert sich von [-210 bis -131] auf [-298 bis -241].

#frequency.log wurde höher während displacement.thickness gesunken ist.

# Schritt 3: Erstellen Sie auf Basis des ersten Modells ein drittes Modell, bei dem Sie
#            einen quadratischen Term für die Frequenz aufnehmen.
#            Validieren Sie das Modell (Kolinearität zwischen linearem und quadratischem Term
#            kann ignoriert werden).
#            Interpretieren Sie die Abhängigkeit Schalldruck (db) zu Frequenz (Hz).
#            (Tipp: Diagramm ist ein guter Weg.)

dfn$frequency.2 = dfn$frequency^2

model2=lm(sound.pressure ~ frequency.log + angle + velocity + chord.length + displacement.thickness + frequency.2 ,data=dfn)

###########################_Model Tests_###############################
vif(model2)
#angle sieht mit 3.6 gerade noch ok aus.

par(mfrow=c(2,2))
plot(model2)
#Ist etwas gebogen.

hist(model2$residuals)
#sieht normalverteilt aus
qqp(model2)
#Franzt oben und unten etwas aus.

crPlots(model2)
#frequency.2 und frequency.log sehen nicht gut aus.

###########################_Außreiser Tests_###############################

#7 Nach abschluss der eigentlichen Modell Teset müssen wir noch die Wirkung der 
#Außreise sofern es welche gibt darstellen
outlierTest(model2)
plot(model2, which=4) #sieht gut aus.
plot(model2, which=5) #sieht gut aus.

#######_Berechnen der Bedeutsamkeit der Variablen innerhalb meines Modells_########

m1.aov = Anova(model2, type=2) #--> type3 würde den Intercept mitanzeigen
ssq = m1.aov[,1]
names(ssq) = rownames(m1.aov) # Ergebnis passende Namen geben
ssq / m1.aov["Residuals",1]
#chord.length ehöht RSS um 32%
#Die übrigen unabhängigen Variablen ehöhen RSS um: 
#frequency.log=14%
#Die anderen sind viel kleiner.

Anova(model2, type = 3)
#Nun hat chord.length den größten Einlfuss auf das Modell
#Gefolgt von frequency.log.

#8.1 Drop1 --> alternativ zu Anova können wir den Drop1 machen
drop1(model2) #AIC interpretiern und größer ist besser
#AW: #Bei einen Drop1 interpretieren wir die AIC Werte
drop1(model2)[order(drop1(model2)[,4]),]
#Gleichen Erkenntnisse wie bei Anova.

#            Interpretieren Sie die Abhängigkeit Schalldruck (db) zu Frequenz (Hz).
#            (Tipp: Diagramm ist ein guter Weg.)
summary(model2)
-1.542e+00
2^-1.542
#Wenn sich die Frequenz um 1Hz erhöht dann steigt auch der Schalldruck um 0.34db.

# Schritt 4: Vergleichen Sie die drei Modelle. Was kÃönnen Sie über die drei Modelle aussagen?
#            Wie gesichert sind Ihre Aussagen?
AIC(model, model1, model2)#niedrieger AIC ist besser
#model2 hat den kleinsten AIC ist somit am Besten.

summary(model)$adj.r.squared
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
#model2 erklärt am meisten die Varianz weil es den höchsten Wert hat.
#Da sie alle über 30% liegen sind unsere Aussagen gesichert.

#Wir haben 1300 Datensätze was auf den ersten Blick gut ist. Wir müssten natürlich wissen
#wie groß die Grundgesamtheit ist um weitere Schlüsse ziehen zu können. Es kommt auch auf
#den Effekt an den wir messen. Ob dieser Groß oder Klein ist.

# Schritt 5: Sagen Sie für die drei Modelle den Schalldruck der verbleibenden 203 Datensätze
#            vorher. Welches Modell hat einen größeren Vorhersagefehler? Stimmt das mit
#            dem Vergleich aus Schritt 4 Überein?

#vorhersage des kompletten Models mit den jeweiligen Testdaten
dfp=df[1301:1503,]
dfp$reynr = dfp$velocity*dfp$chord.length
dfp$frequency.2 = dfp$frequency^2
summary(dfp)

p0 = predict(model, dfp)
p1 = predict(model1, dfp)
p2 = predict(model2, dfp)
#echte Daten -vorhersage daten rechnen quadrieren wegen sum of squares
ssq0 = sum((dfp$sound.pressure - p0)^2)
ssq1 = sum((dfp$sound.pressure - p1)^2)
ssq2 = sum((dfp$sound.pressure - p2)^2)
c(ssq0, ssq1, ssq2)# in diesem Fall ist niedriger besser
#model ist hier am niedrigsten was gut ist.


# - sound.pressure ... Schalldruck in Dezibel [db]
# - frequency ... gemessene Frequenz [Hz]
# - angle ... Anstellwinkel des Flügelprofils [Grad]
# - velocity ... Windgeschwindigkeit [m/s]
# - chord.length ... Referenzlänge des Flügelprofils [m]
# - displacement.thickness ... Verdrängungsdicke [m] (Dicke der entstehenden (Luft-)Grenzschicht 
#zur FlügeloberflÃ¤che)


# Quelle: T.F. Brooks, D.S. Pope, and A.M. Marcolini.: Airfoil self-noise and prediction.
#              Technical report, NASA RP-1218, July 1989.