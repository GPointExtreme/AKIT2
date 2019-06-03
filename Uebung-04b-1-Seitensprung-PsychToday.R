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
model1 = glm(had.affair ~ gender + age + married + child + religious + happy, data = affairs)
model2 = glm(had.affair ~ gender + age + married + religious + happy, data = affairs)
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

# Testen Sie für beide Modelle, ob eine Overdispersion vorliegt
deviance(model1) / model1$df.residual
#0.1675609
deviance(model2) / model2$df.residual
#0.1675511

# Prüfen Sie für das erste Modell, ob logit() passend ist oder andere Variablen fehlen bzw.
# ob der Zusammenhang mit den numerischen Variablen linear ist.
hinkley(model1)


# Erstellen Sie ein drittes Modell, bei dem child+happy fehlen

# Ist das Modell besser?

# PrÃ¼fen Sie fÃ¼r dieses dritte Modell, ob logit() passend ist oder andere Variablen fehlen


# Gibt es AusreiÃŸer im ersten Modell?

# Zeichnen Sie auch das Cook-Distance-Diagramm auf

# Geben Sie folgende Wahrscheinlichkeiten (%) und Chancen (Odds) an:
# - Seitensprung einer Person mit gender=male, age=30, married=7, child=no, religious=slightly, happy=5
# - Seitensprung einer Person mit gender=male, age=35, married=12, child=no, religious=slightly, happy=5
# - Seitensprung einer Person mit gender=female, age=30, married=7, child=no, religious=slightly, happy=3
# - Seitensprung einer Person mit gender=female, age=30, married=7, child=no, religious=antireligious, happy=3
# Tipp: Qvr cerqvpg-Shaxgvba vfg uvyservpu, rvazny zvg Cnenzrgre glcr="erfcbafr"
