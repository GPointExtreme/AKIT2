# AKIT2 SS17, Hauptklausur, 23.6.2017 (leicht modifiziert)
library(car)
library(effects)
library(lmtest)
library(psych)
library(pwr)
library(akit2)

df <- read.csv('C:\\Users\\Dominik\\Downloads\\heart-child.csv')
# Nur vollst�ndige Datens�tze
df = df[complete.cases(df),]

# Eine Studie untersucht die Herzwerte von ProbandInnen und zielt dabei auf die
# Frage ab, ob & wie eine Kinderkrankheit die Herzwerte beeinflusst.
# Die Personen sind "self-selected", soll heißen, ob jemand eine Kinderkrankheit hatte
# oder nicht, wurde der Person _nicht_ im Rahmen des Experiments aufgezwungen :o) 
#
# Der Datensatz enth�lt:
# - heart ... Herzwerte (Loveen's heart index); je h�her, desto besser
# - age ... Alter (Jahre)
# - weight ... Gewicht (kg)
# - sex ... männlich/weiblich
# - child.illness ... hatte die Kinderkrankheit ja/nein
#

# Schritt 1: Machen Sie eine Power-Analyse, um die Anzahl der notwendigen ProbandInnen zu ermitteln.
#            Sie wollen min. Effekte der Gr��e Cohens f²=0.04, mit Power=80% und Signifikanz-Niveau=5% messen.
#            Nehmen Sie dann mittels df[1:___,] nur so viele Eintr�ge aus dem Datensatz,
#            wie die Power-Analyse ergibt.
#
#            Falls Sie diesen Schritt auslassen wollen:
#            Verwenden Sie 240 ProbandInnen.
f2 = pwr.f2.test(u = 1, f2 = 0.04, power = 0.8)
#Ergebnis: v=196
n = round(f2$v+4+1)
#Ergebnis: n=201

df.real = df[1:201,]
#Anzahl der Probantinnen betr�gt 201.

# Schritt 2: (Imputation [nicht relevant])

# Schritt 3: Erstellen Sie ein Modell mit allen Variablen und ohne Interaktionen
#            F�hren Sie eine erste Analyse & Interpretation durch.
model = lm(heart ~ child.illness + sex + age + weight, data = df.real)
summary(model)
#Intercept ist negativ. Kann in der Praxis nicht sein. Zeigt uns die Grenzen des Modells auf.
#Wenn child.illnessyes ja ist dann senkt sich heart um -4.67.
#sexmale erh�ht heart um 2.89.
#age erh�ht heart um 1.76.
#weight erh�ht heart um 4.3.
#Erkl�rungskraft = 65%.

#Modelltests:
#1.Metrische Variablen 
#heart ist metrisch.

#2.Unabh�ngige Variablen m�ssen eine Varianz aufweisen 

#3.Beobachtungen (Messwerte) der abh�ngigen Variablen sind unabh�ngig voneinander 

#4.Wenn mehrere unabh�ngige Variablen in der Regressionsgleichung enthalten sind, 
#d�rfen diese sich nicht gegenseitig linear berechnen lassen (??? Kolinearit�t) 
vif(model)
#Werte befinden sich alle deutlich unter 5 und somit k�nnen wir weiter machen.

#5.Unabh�ngige Variablen d�rfen nicht mit externen Variablen korrelieren. 
#(externe = die nicht im Regressionsmodell sind) 

#6.Residuen haben eine homogene Varianz
parold = par(mfrow=c(2,2))
plot(model)
par(parold)
#Es ist kein Muster zu erkennen deswegen d�rfte eine Varianz vorliegen.

#7.Residuen sollen voneinander unabh�ngig sein, also keine Autokorrelation aufweisen
cor(resid(model)[1:200], resid(model)[2:201])
#Ergebnis: -0.0325102
#Gut oder was?

#8.Residuen sind normalverteilt (eine wichtige Anforderung) 
qqp(model)
#Sieht gut aus.

#9.Die abh�ngige Variable h�ngt linear von den unabh�ngigen Variablen ab 
plot(model, which=4)
#Keine Werte �ber 0.1
plot(model, which=5)
#Keine Werte sind �ber 0.5 oder unter -0.5. Gut!
outlierTest(model)
#Nur Datensatz 42 sticht raus.

#Erkl�rungskraft der Variablen:
ml.aov = Anova(model, type = 2)
ssq = ml.aov[,1]
names(ssq) = rownames(ml.aov)
ssq / ml.aov["Residuals",1]
#child.illness hat 2.5% Erkl�rungskraft.
#sex hat 2% Erkl�rungskraft.
#age hat 10% Erkl�rungskraft.
#weight hat 72% Erkl�rungskraft.

drop1(model)
#Gleiche Erkenntnisse wie bei Anova.

model_ohne_child_ilness=update(model,~.-child.illness)
r2.model=summary(model)$r.squared
r2.model_ohne_child_ilness=summary(model_ohne_child_ilness)$r.squared

f2.model = (r2.model - r2.model_ohne_child_ilness) / (1 - r2.model)
#f2=0.025 ist nicht gut da wir eigentlich 0.04 brauchen.

# Schritt 4: Hat die Kinderkrankheit unterschiedlich starke Auswirkung auf M�nner und Frauen?
#            Erstellen Sie ein passendes Modell, interpretieren Sie das neue Modell und 
#            vergleichen Sie das neue Modell zum Modell aus Schritt 3.
#
#            Tipp: qnf Zbqryy oraögvtg nyfb rvara Vagrenxgvbafgrez "frk:puvyq.vyyarff"
#                  (fvrur qnmh nhpu Ivqrb nhs Zbbqyr)
#            (Entsch�sseln auf rot13.com)
model2 = lm(heart ~ child.illness + sex + age + weight + sex:child.illness, data = df.real)

qqp(model2)

summary(model2)
#Der Interaktionsterm ist nicht signifikant.
#Frauen: child.illnessyes = -6.4858
#M�nner: child.illnessyes:sexmale = 5.3830
confint(model2)

anova(model, model2)
#model ist besser weil h�herer RSS.
AIC(model, model2)
#model ist besser weil niedrigerer AIC.

# Schritt 5: Conclusio:
#            Welche Auswirkungen hat die Kinderkrankheit?
#
#            Ist der Effekt relevant?
#            Welche "confounding"/"versteckte" Eigenschaften können das Resultat verfälschen?
#            Welche Empfehlung geben Sie für die nächste Studie ab? (soll nochmal selbe Fragen analysieren)
