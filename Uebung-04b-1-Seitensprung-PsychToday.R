# AKIT2, Arno Hollosi
# Übung
# Logistische Regression
# Die Tipps sind mit "rot13" verschlüsselt. EntschlÃ¼sselung z.B. mithilfe von www.rot13.com

# SeitensprÃ¼nge (Fair, 1978)
affairs = read.csv("C:\\Users\\Dominik\\Downloads\\Fair-JPE-1978_PsychToday.csv")
affairs$had.affair = affairs$affairs > 0
# Faktoren aus den (Integer-)Zahlen machen
affairs$gender = as.factor(ifelse(affairs$gender == 0, 'female', 'male'))
affairs$child = as.factor(ifelse(affairs$child == 0, 'no', 'yes'))
affairs$occup = as.factor(affairs$occup) # Details: siehe PDF
affairs$religious = as.factor(c('antireligious', 'not at all', 'slightly', 'somewhat', 'very')[affairs$religious])
# "happy" ist 5-teilige Skala, aber wir tun so, als sei es eine nummerische Variable

library(car)
library(akit2)

# Setzen Sie zwei Modelle an:
# Eines mit gender + age + married + child + religious + happy
# und ein zweites, bei dem child nicht verwendert wird
model1 = glm(had.affair ~ gender + age + married + child + religious + happy, data = affairs, family = binomial(link = 'logit'))
model2 = glm(had.affair ~ gender + age + married + religious + happy, data = affairs, family = binomial(link = 'logit'))
summary(model1)
summary(model2)

plotcoef(model1)
plotcoef(model2)

# Welches Modell ist "besser"?
# Tipp 1: Jrypurf ung qra xyrvarera NVP-Fpber?
# Tipp 2: Fvr xÃ¶aara nhpu zvg nabin(zbqryy, zbqryy2, grfg="Puvfd") ceÃ¼sra, bo qre Hagrefpuvrq fvtavsvxnag vfg)
AIC(model1, model2)
#model 2 hat kleineren AIC. Erklärt also besser.

anova(model1, model2, test = "Chisq")
#Prüft ob Ergebniss signifikant ist.
#Auch hier ist Model 2 Besser weil höherer Resid. Df.

# Testen Sie für beide Modelle, ob eine Overdispersion vorliegt
deviance(model1) / model1$df.residual
#1.017093 - Ist der Wert deutlich größer als 1 dann liegt eine Overdispersion vor.
deviance(model2) / model2$df.residual
#1.018025 - Sieht hier gerade noch gut aus.

# Prüfen Sie für das erste Modell, ob logit() passend ist oder andere Variablen fehlen bzw.
# ob der Zusammenhang mit den numerischen Variablen linear ist.
hinkley(model1)
#y.hat^2 ist ok - Passt also gut.

log.linearity.test(model1)
#Hier ist auch alles ok.

# Erstellen Sie ein drittes Modell, bei dem child+happy fehlen
model3 = glm(had.affair ~ gender + age + married + religious, data = affairs, family = binomial(link = 'logit'))

# Ist das Modell besser?
AIC(model1, model2, model3)
#model 3 ist schlechter als model1 und 2.

# Prüfen Sie für dieses dritte Modell, ob logit() passend ist oder andere Variablen fehlen
hinkley(model3)
#y.hat^2 ist ok.

# Gibt es Ausreißer im ersten Modell?
outlierTest(model1)
#No Studentized residuals with Bonferonni p < 0.05
#Also passt alles - Keine Ausreißer

# Zeichnen Sie auch das Cook-Distance-Diagramm auf
plot(model1, which = 4)
#Keine Werte über 0.5. Also alles gut.

# Geben Sie folgende Wahrscheinlichkeiten (%) und Chancen (Odds) an:
# - Seitensprung einer Person mit gender=male, age=30, married=7, child=no, religious=slightly, happy=5
Wahrscheinlichkeit = predict(model1, newdata=data.frame(gender="male",age=30,married=7,child='no',religious='slightly',happy=5),type = "response")
#0.1977581 = die Wahrscheinlichkeit

logit = predict(model1, newdata=data.frame(gender="male",age=30,married=7,child='no',religious='slightly',happy=5))
chance = exp(logit)
#0.2465068 = die Chance

# - Seitensprung einer Person mit gender=male, age=35, married=12, child=no, religious=slightly, happy=5
# - Seitensprung einer Person mit gender=female, age=30, married=7, child=no, religious=slightly, happy=3
# - Seitensprung einer Person mit gender=female, age=30, married=7, child=no, religious=antireligious, happy=3
# Tipp: Qvr cerqvpg-Shaxgvba vfg uvyservpu, rvazny zvg Cnenzrgre glcr="erfcbafr"
