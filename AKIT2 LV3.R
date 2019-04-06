df = read.csv('C:\\Users\\Dominik\\Downloads\\housing-prices-ge19.txt')

df$price.log = log10(df$Price) # deswegen gleichen wir es mit einem log aus
m2 = lm(price.log ~ Age + Land.Value + Living.Area + Pct.College + Bedrooms, data=df)
# Daten von gestern geladen

mod0 = lm(price.log ~ 1, data=df)
summary(mod0)
rss0 = sum(resid(mod0)^2) # ^2 ist einfach so

mod1 = lm(price.log ~ Living.Area, data=df)
summary(mod1)
sum(resid(mod1)^2)

rss1 = sum(resid(mod1)^2)
(rss0-rss1)/rss0

mod2 = update(mod1, ~ . + Age)
summary(mod2)

rss2 = sum(resid(mod2)^2)
(rss1 - rss2)/rss0

mod3 = update(mod2, ~ . + Pct.College)
summary(mod3)

rss3 = sum(resid(mod3)^2)
(rss2 - rss3)/rss0

# Nun automatisieren wir alles was wir vorher per Hand gemacht haben.

library(car)
Anova(mod3)
# Sum sq sagt aus wieviel jede Variable beiträgt.
# Anova sortiert automatisch schon welche Variable am meisten aussagt.

drop1(mod3)
# drop1 macht ca. das gleiche wie Anova.
# Gibt uns aber auch noch AIC. Ergebnisse sind negativ.
# none Wert ist der Gesamtwert - Würde man Living.Area weg lassen dann würde es auf -5792 sinken.

AIC(mod0, mod1, mod2, mod3)
#Man kann sehen wie viel mehr Ausgesagt werden kann je mehr Variablen im Modell sind.

# Jetzt probieren wir die Effektgröße einer Variable zu finden.
mod4 = update(mod3, ~ . - Living.Area)
summary(mod4)

r_voll = summary(mod3)$r.squared
r_voll
r_ohneArea = summary(mod4)$r.squared
r_ohneArea

f2 = (r_voll - r_ohneArea)/(1- r_voll)
f2

library(pwr)
pwr.f2.test(f2=0.768, sig.level = 0.05, power = 0.8, u = 1)
# u sagt wieviele Variablen sollen diesen Effekt beschreiben

# 14 Samples brauchen wir jetzt
library(dplyr)
sample_1 = sample_n(df, 14)
sample_modell = lm(price.log ~ Age + Living.Area + Pct.College, data=sample_1)
summary(sample_modell)

sign = numeric(1000)
for(i in 1:1000) {
  sample_i = sample_n(df, 15)
  sample_modell_i = lm(price.log ~ Age + Living.Area + Pct.College, data=sample_i)
  sign[i] = coef(summary(sample_modell_i))[3,4]
}
sum(sign<0.05)

# Nehmen wir nun eine andere Variable weil mit Living.Area ist es too easy peasy lemon squeeze!
summary(mod3)
mod5 = update(mod3, ~ . - Age) # Age weg tun
r_ohneAge = summary(mod5)$r.squared
f2_Age = (r_voll - r_ohneAge)/(1 - r_voll) # Formel die ich Aufgeschreiben habe - LV3
f2_Age

pwr.f2.test(power=0.8, sig.level = 0.05, f2 = 0.023, u=1)

sign = numeric(1000)
for(i in 1:1000) {
  sample_i = sample_n(df, 345)
  sample_modell_i = lm(price.log ~ Age + Living.Area + Pct.College, data=sample_i)
  sign[i] = coef(summary(sample_modell_i))[2,4] # p-Wert für Age
}
sum(sign<0.05)

pwr.f2.test(sig.level = 0.05, f2 = 0.02, u=1, v = 200)
# Können power Funktion auch nutzen um andere Sachen zu Berechnen.
# Bei 200 Versuchen würden wir nur einen power von 0.516 bekommen.

sign = numeric(1000)
faktor = numeric(1000)
for(i in 1:1000) {
  sample_i = sample_n(df, 345)
  sample_modell_i = lm(price.log ~ Age + Living.Area + Pct.College, data=sample_i)
  sign[i] = coef(summary(sample_modell_i))[2,4] # p-Wert für Age
  faktor[i] = coef(summary(sample_modell_i))[2,1] # Einflusswert von Age
}
sum(sign<0.05)
summary(mod3)

hist(faktor) # Würde eher nicht pupliziert werden.
hist(faktor[sign<0.05]) # das schon eher weil signifikant
mean(faktor[sign<0.05]) # jetzt sind wir aber weit weg vom eigentlichen Mittelwert von 0.0007326

# Zeigt das wenn wir zu wenig power haben verlieren wir so oder so.

