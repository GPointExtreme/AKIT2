kry = read.csv('C:\\Users\\Dominik\\Downloads\\KRY-Benotung-WS18_19.csv')
summary(kry)

model1 = lm(gesamt ~ frage9 + frage10, data=kry)
summary(model1)
confint(model1)

x = rnorm(n = 300)
y = rnorm(n = 300)
cor(x, y)
m = lm (y ~ x)
summary(m)


#Weiterer Versuch
model2 = lm(gesamt ~ gruppe + frage9 + frage10, data=kry)
summary(model2)
plot(kry$gesamt, resid(model2))
plot(model2, which=1)
hist(resid(model2))

library(car)
qqp(model2)

vif(model2)
# variance inflation factor - wenn Werte zweistellig werden dann ist das sehr schlecht

plot(model2, which=4)
# Cockdistace sagt aus wieviel die einzelenen Werte von unserer Schaukel entfernt sind.

plot(model2, which=5)
# Sollte in den roten Linien bleiben (0,5)

outlierTest(model2)
# Sagt uns ob Daten abweichen

model3 = lm(gesamt ~ gruppe + frage9 + frage10, data=kry[-17,])
# Nehmen hier Wert mit -17 raus und schauen wie sich die Werte verändern.
summary(model3)
summary(model2)

# Neue Daten
cat = read.csv('C:\\Users\\Dominik\\Downloads\\cats\\cat-experiment-4.csv')
summary(cat)

mean(cat$response[cat$word=='noun1'])
mean(cat$response[cat$word=='noun2'])
mean(cat$response[cat$word=='noun3'])
mean(cat$response[cat$word=='noun4'])
mean(cat$response[cat$word=='cat_name'])

catmodel = lm(response ~ word, data=cat)
summary(catmodel)

# Neues Beispiel
df = read.csv('C:\\Users\\Dominik\\Downloads\\housing-prices-ge19.txt')
m1 = lm(Price ~ Age + Land.Value + Living.Area + Pct.College + Bedrooms, data=df)
summary(m1)

plot(m1, which=1)

library(car)
qqp(m1) # sieht nicht gut aus oben hin
vif(m1)
plot(m1, which=4)
plot(m1, which=5)


hist(df$Price)

df$price.log = log10(df$Price) # deswegen gleichen wir es mit einem log aus
hist(df$price.log)

m2 = lm(price.log ~ Age + Land.Value + Living.Area + Pct.College + Bedrooms, data=df)
summary(m2)

qqp(m2) # sieht nun besser aus da price.log verwendet wird
outlierTest(m2) # Mehrere Werte nahe an 10 ist schlecht - Ausreißer

diff_y = 10^(-8.648e-04) # Heißt wenn wir um ein x rüber gehen steigt es um 0.998 im y.
10000*diff_y # nach 1 Jahr
10000*diff_y*diff_y # nach 2 Jahren
10000*(diff_y^7) # nach 7 Jahren ist die Wohnung so viel wert.
# Wichtig für Prüfung!

# Für Bedrooms bedeutet das 
bed_diff = 10*coef(m2)[6]
100000*(bed_diff^2)
