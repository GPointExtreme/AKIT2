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

df <- read.csv('C:\\Users\\Dominik\\Downloads\\bank-promo.csv')
df$month = factor(df$month, c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
df$poutcome = factor(df$poutcome, c("unknown", "other", "failure", "success"))

# Eine portugiesische Bank f¸hrt eine Telefonmarketing-Kampagne durch, um eine
# langlaufende Sparform zu bewerben. Zwei konkurrierende Teams wollen die Trefferquote
# des Marketings verbessern. Folgende Daten sind vorhanden:

# - y ... ob die Person in die Sparform investiert hat (= Marketing erfolgreich): "yes"/"no"
#
# F¸r Team 1 (konzentriert sich auf Meta-Daten):
# - month ... Monat des letzten Kontaktes
# - duration ... Dauer des letzen Anrufes in Sekunden
# - campaign ... Anzahl get‰tigter Anrufe in der laufenden Kampagne
# - poutcome ... Ergebnis einer fr¸heren Marketing-Kampagne
#
# F¸r Team 2 (konzentriert sich auf Kunden-Daten):
# - age ... Alter der Kundin/des Kunden
# - job ... Kategorie f¸r Arbeitsverh‰ltnis
# - marital ... Heiratsstatus
# - education ... Grad der Ausbildung
# - default ... Ist ein Kredit in Verzug bzw. nicht mehr einbringbar?
# - balance ... durchschnittlicher Kontostand
# - housing ... Kredit f¸r das Eigenheim aufgenommen?
# - loan ... persˆnlichen (Konsum-)Kredit aufgenomen?


# Schritt 1: Verwenden Sie nur die ersten 5000 Datens‰tze.
#            Erstellen Sie ein Modell f¸r Team 1 und eines f¸r Team 2.
#            Validieren Sie die Modelle.
dfn = df[1:5000,]

#------------ ‹berblick verschaffen -------------#

view(dfn)
#age, mean = 40, min = 19, max = 93
hist(dfn$age) #sieht ok aus
#balance, mean = 1389, min = -3372, max = 98417
hist(dfn$balance) #verteilung nicht gut - Log-transformieren?
#duration, mean = 250sek, min = 1sek, max = 3322sek
hist(dfn$duration) #verteilung nicht gut - Log-transformieren?
#campaign, mean = 2.8, min = 1, max = 46
hist(dfn$campaign) #verteilung nicht gut - Log-transformieren?

table(dfn$job) #30 unkown - Aufpassen
table(dfn$marital) #meisten sind married
table(dfn$education) #221 unkown - Aufpassen
table(dfn$poutcome) #4102 unkown - Aufpassen!!!

#--------------- Modelle erstellen ------------------#

model1 = glm(y ~ month + duration + campaign + poutcome, data = dfn, family = binomial(link = "logit"))
summary(model1)

model2 = glm(y ~ age + job + marital + education + default + balance + housing + loan, data = dfn, family = binomial(link = "logit"))
summary(model2)

#--------------- Modelle validieren ------------------#

deviance(model1)/model1$df.residual #ist unter 1. Also ok.
deviance(model2)/model2$df.residual #ist unter 1. Also ok.

y.hat1 = predict(model1)
y2.hat1 = y.hat1^2
model1.test = glm(y ~ y.hat1 + y2.hat1, data = dfn, family = binomial(link = "logit"))
summary(model1.test)
#y.hat1 ist signifikant was gut ist aber y2.hat1 ist auch signifikant = nicht gut!

y.hat2 = predict(model2)
y2.hat2 = y.hat2^2
model2.test = glm(y ~ y.hat2 + y2.hat2, data = dfn, family = binomial(link = "logit"))
summary(model2.test)
#y.hat2 ist signifikant was gut ist und y2.hat2 ist nicht signifikant, so wie es sein soll!

vif(model1)
#Alle werte unter 2. Also gut.
vif(model2)
#Alle werte unter 2 auﬂer job hat fast 4. Bis 5 ist aber noch ok.

outlierTest(model1) #Zeigt keine Ausreiﬂer an
plot(model1, which=4) #Alles sogar unter 0.05. Sehr gut.
plot(model1, which=5) #Sieht generell gut aus.
outlierTest(model2) #Zeigt keine Ausreiﬂer an
plot(model2, which=4) #Alles sogar unter 0.05. Sehr gut.
plot(model2, which=5) #Sieht generell gut aus.
#Keine Ausreiﬂer in beiden Modellen.

ROC(model1)
#Sieht gut aus f¸r model1
ROC(model2)
#Sieht flacher aus als bei model1.

Anova(model1)
drop1(model1)
Anova(model2)
drop1(model2)

# Schritt 2: Beantworten Sie folgende Fragen (und geben Sie verst‰ndliche Zahlen dazu an):
#   - Team 1:
#       - Was ist der Basisfall und welche Erfolgsrate hat dieser?
#       - Welche Auswirkung hat die Anzahl der get‰tigten Anrufe?
#       - Welche Monate sind besonders erfolgreich?
summary(model1)
inv.logit(coef(model1)[1])
#Intercept = 0.02116173
#2% Chance das der derzeitige Basisfall kommt: Monat J‰nner und poutcome unkown

exp(coef(model1)[14]) #14 weil campaign die Anrufzahl beinhaltet
#Pro Sexanruf mehr den wir t‰tigen verringert sich die Chance um 10%

(exp(coef(model1)[14])^10)
#Nach 10 Anrufen mehr die wir t‰tigen verringert sich die Chance um 35%

exp(coef(model1))
#bei Umfragen die im Monat M‰rz gef¸hrt wurden erhˆht sich die Chance f¸r einen Erfogreichen abschluss auf das ca.27-fache
#bei Umfragen die im Monat Oktober gef¸hrt wurden erhˆht sich die Chance f¸r einen Erfogreichen abschluss auf das ca.14-fache
#bei Umfragen die im Monat September gef¸hrt wurden erhˆht sich die Chance f¸r einen Erfogreichen abschluss auf das ca.7-fache

#   - Team 2:
#       - Was ist der Basisfall und welche Erfolgsrate hat dieser?
#       - Welche Rolle spielt das Alter der Person?
#       - Personen mit welchem Arbeitsverh‰ltnis sprechen besonders an?
#       - Macht es einen Unterschied ob die Person einen Kredit aufgenommen hat (Eigenheim oder Konsum)?
summary(model2)
inv.logit(coef(model2)[1])
#Intercept = 0.09547266
#Basisfall hat ca eine Erfolgswahrscheinlichkeit von 10%
#Basisfall = Admin, divorced, primary, keinen Kredit und die anderen sind auf 0 (Unsinnig)

exp(coef(model2))
#Mit jedem Jahr ‰lter erhˆht sich die Chance um da 1.0026-fache (0.26%)

#retired mit 2.08-fache (108%) und student mit 2.11-fache (111%) sprechen besonders daurauf an.


# Schritt 3: Vergleichen Sie die beiden Modelle.
#            Zu welchem Schluss kommen Sie?
AIC(model1, model2)

logisticR2(model1)
logisticR2(model2)
#Was sagen diese aus?

# Schritt 4: Sagen Sie die Erfolgswahrscheinlichkeiten f√ºr beide Modelle auf Basis der
#            zweiten 5000 Datens√§tze vorher. F√ºr die Bestimmung des Cutoff-Points
#            zeichnen Sie folgende Diagramme (je Modell):
#            - Anteil der tats‰chlich erfolgreichen (=yes) und als erfolgreich vorhergesagten
#              Kampagnen an allen als erfolgreich vorhergesagten Kampagnen (= Relation Aufwand
#              zu Erfolg)
#            - Anteil der tats‰chlich erfolgreichen (=yes) und als erfolgreich vorhergesagten
#              Kampagnen an allen tats‰chlich erfolgreichen Kampagnen (= Anteil der 
#              "yes"-KundInnen die erreicht werden)
#            Interpetieren Sie diese Diagramme. Welches Modell ist besser? Welche 
#             Cutoff-Points empfehlen Sie?
#
#            Geben Sie eine Empfehlung (je Modell) f√ºr einen Cutoff-Point, unter der Annahme,
#            dass der Gewinn aus dem Sparvertrag 10x den Marketingskosten f√ºr eine Person 
#            entspricht.
#            Ziel: Gewinnmaximierung.
dfp = df[5001:10000,]
summary(dfp)

#Zahlen gesch‰tzt aus ROC Kurve
cutoff1 = 0.1
cutoff2 = 0.16

y.vorhergesagt=ifelse(predict(model1, newdata = data.frame(dfp), type="response")>=cutoff1, 1, 0) 
true_positive   = sum(y.vorhergesagt == 1 & as.integer(dfp$y)-1 == 1) 
true_negative   = sum(y.vorhergesagt == 0 & as.integer(dfp$y)-1 == 0) 
false_positive  = sum(y.vorhergesagt == 1 & as.integer(dfp$y)-1 == 0) 
false_negative  = sum(y.vorhergesagt == 0 & as.integer(dfp$y)-1 == 1) 
true_positive_rate = true_positive / (true_positive + false_negative) 
false_positive_rate = false_positive / (false_positive + true_negative) 
true_positive_rate
#0.7775891
false_positive_rate
#0.1858989



# Quelle:  S. Moro, R. Laureano and P. Cortez: Using Data Mining for Bank Direct Marketing:
#          An Application of the CRISP-DM Methodology. In P. Novais et al. (Eds.),
#          Proceedings of the European Simulation and Modelling Conference - ESM'2011,
#          pp. 117-121, Guimar√£es, Portugal, October, 2011. EUROSIS.