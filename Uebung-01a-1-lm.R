# Daten von:
# R.J. Gladstone (1905). "A Study of the Relations of the Brain to 
# to the Size of the Head", Biometrika, Vol. 4, pp105-123
#
# Gewicht des Gehirns (Gramm) und Kopfgröße (cm³) für 237 Erwachsene
# Kein repräsentatives Sample!

brain = read.csv('C:\\Users\\Dominik\\Downloads\\brainiac.csv')

# 1) Mittelwert Größe + Std.abweichung (gesamt, getrennt nach Alter)
mean(brain$HeadSize)
sd(brain$HeadSize)

levels(brain$AgeRange) # Schauen welche Altersklassen es gibt.
mean(brain$HeadSize[brain$AgeRange=="Age20_46"])
mean(brain$HeadSize[brain$AgeRange=="Age46plus"])

# 2) Mittelwert Gewicht + Std.abweichung (gesamt, getrennt nach Alter)
mean(brain$BrainWeight)
sd(brain$BrainWeight)

# Nochmal das gleiche...
mean(brain$BrainWeight[brain$AgeRange=="Age20_46"])
mean(brain$BrainWeight[brain$AgeRange=="Age46plus"])

# 3) Histogramm Größe (gesamt, getrennt nach Geschlecht)
hist(brain$HeadSize)
levels(brain$Gender)
hist(brain$HeadSize[brain$Gender=="Female"])
hist(brain$HeadSize[brain$Gender=="Male"])

# 4) Histogramm Gewicht (gesamt, getrennt nach Geschlecht)
hist(brain$BrainWeight)
hist(brain$BrainWeight[brain$Gender=="Female"])
hist(brain$BrainWeight[brain$Gender=="Male"])

# 5) Scatterplot: Gewicht ~ Größe
plot(brain$BrainWeight~brain$HeadSize)

# 6) Regression: Gewicht ~ Größe
#    Interpretieren Sie die Koeffizienten
#    Berechnen Sie für eine Person mit 3456cm³ Kopfgröße das Gehirngewicht anhand der Regressionsgleichung
model=lm(brain$BrainWeight~brain$HeadSize)
summary(model)
koeff=coef(model)
abline(koeff[1], koeff[2], col='red')
# y=kx+d
y=koeff[2]*4500+koeff[1]
points(4500, y, col='blue') # Nachschauen ob der Punkt auf der Linie ist!

# 7) 2. Modell: Gewicht ~ Größe + Alter
#    Interpretieren Sie die Koeffizienten
model1=lm(brain$BrainWeight~brain$HeadSize+brain$AgeRange)
summary(model1)
koeff2=coef(model1)
koeff2

# 8) Rechnen Sie zwei neue Regressionsmodelle (Gewicht ~ Größe):
#    Daten getrennt für Personen unter und über 46 Jahre, d.h. splitten Sie den Datensatz auf
#    Wie unterscheiden sich die neuen Modelle mit jenem aus Punkt 7?
df_under46=brain[brain$AgeRange=="Age20_46",]
df_over46=brain[brain$AgeRange=="Age46plus",]
show(df_over46)
show(df_under46)

model_under46=lm(df_under46$BrainWeight~df_under46$HeadSize)
summary(model_under46)
koeff_under46=coef(model_under46)
abline(koeff_under46[1], koeff_under46[2], col='blue')

model_over46=lm(df_over46$BrainWeight~df_over46$HeadSize)
summary(model_over46)
koeff_over46=coef(model_over46)
abline(koeff_over46[1], koeff_over46[2], col='green')
# Gr�ne Linie steigt st�rker an als Blaue. Hei�t das �ltere Gehirne wachsen schneller?