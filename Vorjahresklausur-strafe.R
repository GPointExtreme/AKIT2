# AKIT2 SS18, Hauptklausur, 15.6.2018
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

df <- read.csv('C:\\Users\\Dominik\\Downloads\\strafe.csv')
# Referenz-Gruppe √§ndern
df$race = relevel(df$race, ref = 'Caucasian')
df$sex = relevel(df$sex, ref = 'Male')


# In Amerika werden Algorithmen eingesetzt, um vorherzusagen, ob eine straff√§llige Person
# wieder r¸ckf‰llig wird. Diese Algorithmen liefern einen Risiko-Wert, der zum Groﬂteil
# auf Basis eines Fragebogens errechnet wird. Es stellt sich die Frage, ob diese
# Algorithmen eine versteckte Tendenz haben, ob sie also einzelne Personengruppen 
# benachteiligen.
# 
# Der Datensatz stammt aus einem einzelnen Bundesstaat der USA und enth√§lt neben
# einigen Angaben auch, ob eine Person tats√§chlich r√ºckf√§llig geworden ist. Datenfelder:
#
# - age ... Alter der Person in Jahren
# - risk_score ... Risiko-Wert (1-10), ob eine Person r√ºckf√§llig wird (berechnet von Algorithmus)
# - priors_count ... Anzahl an Vorstrafen
# - sex ... Geschlecht
# - is_recid ... ob eine Person im Zeitraum von 2 Jahren r√ºckf√§llig geworden ist (yes) oder nicht (no)
# - charge_degree ... Schwere der begangenen Straftat
# - race ... Rasse (aus amerikanischer Sicht)


# Schritt 1: Erstellen Sie beschreibende Statistiken zu wichtigsten Werten des Datensatzes,
#            vor allem in Hinblick auf Rasse, Geschlecht, Risikowert und R¸ckfallquote.
#            Zu welchen ersten Hypothesen gelangen Sie?
summary(df)
describe(df)
#age: mean 35, min 18, max 96
#sex: male 4997, female 1175
#priors_count: Durschnittlicher gefangener hat 5 Vorstrafen.
#risk_score: Liegt durchschnittlich bei 4.4 von 10.
#is_recid: ca. 50% werden wieder Straff‰llig in den n‰chsten 2 Jahren.

table(df$race)
#Meisten sind:
#Caucasian:         2103
#African-American:  3175
#Hispanic:          509
#Alle anderen sind kleiner als Hispanic.

############################################################

#Rasse vs. R¸ckf‰llig
plot(df$race, df$is_recid)
#R¸clf‰llig wurden:
#Caucasian:         ca. 40%
#African-American:  ca. 60%
#Hispanic:          ca. 40%
#Asia:              ca. 35%
#Native American:   ca. 65%
#Other:             ca. 40%

#Rasse vs. Algorytmus
plot(df$race, df$risk_score)
#Einstufung:
#Caucasian:         ca. 3
#African-American:  ca. 5
#Hispanic:          ca. 3
#Asia:              ca. 2
#Native American:   ca. 7
#Other:             ca. 2

#African-American und Native American werden h‰ufiger als hˆheres Risiko eingestuft und
#werden auch h‰ufiger R¸ckf‰llig als die anderen.

plot(df$is_recid, df$race)
(100/6173)*3175
#Den grˆﬂten Teil der R¸ckf‰lligen insgesamt sind die African-American wobei sie auch die
#grˆﬂte Personenanzahl von 51% haben.

#########################################################

#Geschlecht vs. R¸ckf‰llig
plot(df$sex, df$is_recid)
#Durchschnitt:
#male:    ca. 50%
#female:  ca. 40%

#Geschlecht vs. Algorythmus
plot(df$sex, df$risk_score)
#Durchschnitt:
#male:    ca. 4 passt ca. weil 50% R¸ckf‰llig werden
#female:  ca. 4 wird gut vorrausgesagt mit 40% R¸ckf‰lligkeit

#############################################################

#Hypothesen:
#H1: M‰nner begehen ˆfter Straftaten als Frauen und werden auch h‰ufiger R¸ckf‰llig.
#H2: African-American und Native American begehen ˆfter Straftaten und werden auch
  #h‰ufiger R¸ckf‰llig.
#H3: Asiaten werden im Verh‰ltnis am wenigsten R¸ckf‰llig.

##############################################################

# Schritt 2: Erstellen Sie ein Vorhersagemodell, ob eine Person r¸ckf‰llig wird.
#            Verifizieren & interpretieren Sie das Modell und den Einfluss der Variablen.
#            Welche Variable w√§ren bei einem perfekten Algorithmus signifikant?
#
#            Berechnen Sie auch anhand einiger typischer Datens√§tze tats√§chliche
#            Wahrscheinlichkeiten und diskutieren deren Unterschiede.
describe(df)
model1 = glm(is_recid ~ age + charge_degree + race + sex + priors_count + risk_score, 
             data = df, family = binomial(link = "logit"))
summary(model1)

###########################_Model Tests_###############################
deviance(model1)/model1$df.residual
#Wert liegt mit 1.2 knapp ¸ber 1. Ist noch ok.

hinkley(model1)
#sieht nicht ok aus. Deutet auf eine fehlende Variable bzw. 
#eine nicht lineare Beziehung oder andere Probleme im Modell hin.

dfn = df
dfn$priors_count=dfn$priors_count+1
model1b = glm(is_recid ~ age + charge_degree + race + sex + priors_count + risk_score, 
             data = dfn, family = binomial(link = "logit"))
log.linearity.test(model1b)
#age und priors_count sind nicht ok. Kˆnnten also eine lineare Abh‰ngigkeit haben!

cor(df$age, df$age*as.integer(df$is_recid))
#Korrelation von 0.6
cor(df$priors_count, df$priors_count*as.integer(df$is_recid))
#Korrelation von 0.96

vif(model1)
#Alle Werte befinden sich unter 5, daher ist nur eine Geringe Abh‰ngigkeit zu bemerken

#Einflussreiche Werte / Ausreiﬂer
outlierTest(model1)
#Keine Ausreiﬂer gefunden.
plot(model1, which=4)
#Keine Werte ¸ber 0.05
plot(model1, which=5) 
#Alle Datens‰tze innerhalb der roten Werte +-0.5 und +-1 ist gut.

ROC(model1)
#Die Erkl‰rkraft unseres Modells kˆnnte Besser sein. Eventuell h‰ngt es mit der Kolinearit‰t
#zusammen.

logisticR2(model1)

########################_Zur¸ckrechnen und Interpretieren_###########################
summary(model1)

#Interpretation:
intcp = inv.logit(coef(model1)[1]) #--> diese liefert direkt die absolute Wahrscheinlichkeit
#Ergebniss: 0.50 = 50% Wahrscheinlichkeit

summary(df)
#Basisfall ist m‰nnlich, Caucasian, charge_degree mit F, mit einer 50% Wahrscheinlichkeit.
#Basisfall hat aber age = 0 was nicht sinnvoll w‰re!

exp(coef(model1))
#Wenn man eine Frau ist verringert sich die Chance -35% R¸ckf‰llig zu werden.
#Wenn man ein Asia ist verringert sich die Chance -23% R¸ckf‰llig zu werden.
#F¸r jedes Jahr das man ‰lter ist verringert sich die Chance um 3% R¸ckf‰llig zu werden.
#F¸r jeden priors_count erhˆht sich die Chance um 13% R¸ckf‰llig zu werden.
#F¸r jede risk_score erhˆht sich die Chance um 17% R¸ckf‰llig zu werden.
#F¸r charge_degree M sinkt die Chance um -11% R¸ckf‰llig zu werden.
#Bei allen Rassen sinkt die Chance auﬂer bei African-American, dort steigts um 1%.

###########################Einfluss der Variablen################################
Anova(model1)
#priors_count hat hˆchten Einfluss.
#risk_score und age sagen auch viel aus.
#charge_degree, race und sex eher weniger.

drop1(model1)
#race verschl‰chtert sogar das Modell.

#risk_score m¸sste signifikant sein und andere Variablen nicht wenn risk_score richtig gut w‰re.

################################Predict###########################################

table(df$race)
predict(model1, newdata = data.frame(sex=c("Male", "Female"), charge_degree ="F", age=35, race="African-American", priors_count=3, risk_score=4), type='response')
#African-American: male = 0.5, female = 0.40
predict(model1, newdata = data.frame(sex=c("Male", "Female"), charge_degree ="F", age=35, race="Caucasian", priors_count=3, risk_score=4), type='response')
#Caucasian: male = 0.5, female = 0.4
predict(model1, newdata = data.frame(sex=c("Male", "Female"), charge_degree ="F", age=35, race="Hispanic", priors_count=3, risk_score=4), type='response')
#Hispanic: male = 0.45, female = 0.35
predict(model1, newdata = data.frame(sex=c("Male", "Female"), charge_degree ="F", age=35, race="Other", priors_count=3, risk_score=4), type='response')
#Other: male = 0.47, female = 0.37

#Rasse sind alle ca. gleich.
#Frauen werden weniger R¸ckf‰llig obwohl sie gleich eingestuft werden.

# Schritt 3: Erstellen Sie ein weiteres Modell, das aus allen vorhandenen Variablen
#            (inkl. is_recid aber exkl. risk_score) versucht, vorherzusagen, ob der
#            Risikowert gr√∂√üergleich 7 ist oder nicht.
#            Verifizieren & interpretieren Sie das Modell und den Einfluss der Variablen.
#
# Hinweis: Sie erstellen also ein Modell, das versucht anhand der bekannten Variablen
#          und der -- zum Zeitpunkt der Berechnung des Risikowerts zuk√ºnftigen -- R√ºckfallquote,
#          das Ergebnis des Algorithmus vorherzusagen. Welche Variable w√§ren bei einem
#          perfekten Algorithmus signifikant?
df$risk_score.new = ifelse(df$risk_score >= 7, 1, 0) #Grˆﬂergleich 7 soll 1 sein sonst 0!

model2 = glm(risk_score.new ~ is_recid + age + charge_degree + race + sex + priors_count, 
             data = df, family = binomial(link = "logit"))
summary(model2)

###########################_Model Tests_###############################
deviance(model2)/model2$df.residual
#Wert liegt unter 1. Das ist gut.

hinkley(model2)
#sieht ok aus.

vif(model2)
#Alle Werte befinden sich unter 5, daher ist nur eine Geringe Abh‰ngigkeit zu bemerken

#Einflussreiche Werte / Ausreiﬂer
outlierTest(model2)
#Keine Ausreiﬂer gefunden.
plot(model2, which=4)
#Keine Werte ¸ber 0.05
plot(model2, which=5) 
#Alle Datens‰tze innerhalb der roten Werte +-0.5 und +-1 ist gut.

ROC(model1)
ROC(model2)
#model2 hat einen grˆﬂeren Bereich und sagt somit besser Vorraus.

########################_Zur¸ckrechnen und Interpretieren_###########################
summary(model2)
#W‰re der Algorythmus perfekt dann w‰re nur is_recid signifikant!

#Interpretation:
intcp = inv.logit(coef(model2)[1]) #--> diese liefert direkt die absolute Wahrscheinlichkeit
#Ergebniss: 0.64 = 64% Wahrscheinlichkeit

summary(df)
#Basisfall ist m‰nnlich, Caucasian, charge_degree mit F, mit einer 50% Wahrscheinlichkeit.
#Basisfall hat aber age = 0 was nicht sinnvoll w‰re!

exp(coef(model2))
#Die Chance bei female sinkt um 5% das sie 7+ eingestuft wird.
#is_recid = yes steigt die Chance um 90% das man als 7+ eingestuft wird.
#Pro Jahr veringert sich die Chance um 10%.
#Wenn man African-American ist erhˆht sich die Chance um 75% als 7+ eingestuft wird.
#priors_count als jede weitere Straftat erhˆht die Chance um 24%.

# Schritt 4: Conclusio:
#            Was schlie√üen Sie aus den beiden Modellen?
#            Ist der Algorithmus (implizit) rassistisch bzw. sexistisch?
#            Falls nein, warum nicht? Falls ja, warum?
#            Gibt es Einschr√§nkungen f√ºr die Interpretation?

cbind(coef(model1), coef(model2))


# Tipp f√ºr Interpretation: sehen Sie sich strafe-perfekter-score.R an,
# bevor Sie sich die Musterl√∂sung ansehen. Das sollte bei Interpretation helfen.


# Bei Interesse:
# Artikel: https://www.propublica.org/article/machine-bias-risk-assessments-in-criminal-sentencing
# Methode: https://www.propublica.org/article/how-we-analyzed-the-compas-recidivism-algorithm
# Daten: https://github.com/propublica/compas-analysis/blob/master/Compas%20Analysis.ipynb
