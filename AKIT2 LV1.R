body = read.csv('C:\\Users\\Dominik\\Downloads\\body-dimensions.csv')

# Überblick über Daten
summary(body)

# Histogramme
hist(body$Age)
hist(body$Weight)

# Scatterplot
plot(body$Height, body$Weight)

# Regression
model = lm(Weight ~ Height, data=body)
summary(model)

# Scatterplot
plot(body$Height, body$Weight)
abline(a=-105, b=1.0176, col='blue')

# Damit greifen wir auf die Werte zu des Koeffizienten
koeff = coef(model)
# In R beginnen Arrays bei 1 und nicht bei 0!
abline(a=koeff[1], b=koeff[2], col='blue')

# Eigenes Gewicht
koeff[1] + koeff[2]*170

# Zufallszahlen von Normalverteilung
zufall=rnorm(n=30)
hist(zufall)
mean(zufall)

# Konfidenzintervall
confint(model)

# Nochmal kopiert um mehrere Geraden einzuzeichnen
plot(body$Height, body$Weight)
intercept100=rnorm(n=100, mean=koeff[1], sd=7.53941)
hist(intercept100)
height100 = rnorm(n=100, mean=koeff[2], sd=0.04399)
for (i in 1:100) {
  abline(a=intercept100[i], b=height100[i], col="skyblue")
}

# Neues Modell
model2 = lm(Weight ~ Height + Age, data=body)
summary(model2)

koeff2 = coef(model2)
koeff2[1]+koeff2[2]*170 + koeff2[3]*28

# Erweiterung von Modell2
model3 = lm(Weight ~ Height + Age + Gender, data=body)
summary(model3)

plot(body$Height, body$Weight)
abline(a=koeff[1], b=koeff[2], col="blue")
koeff3=coef(model3)
abline(a=koeff3[1], b=koeff3[2], col="purple")
abline(a=koeff3[1]+koeff3[4], b=koeff3[2], col="green")

hist(body$Age[body$Gender=='Male'])
hist(body$Age[body$Gender=='Female'])