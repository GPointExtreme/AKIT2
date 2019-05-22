library(runjags)
library(coda)
rjags::load.module("glm")
library(akit2)

so = read.csv('C:\\Users\\Dominik\\Downloads\\StackOverflow-LV\\StackOverflow-survey.csv')

# YearsProgram als metrische Zahl
levels(so$YearsProgram)

so$YearsProgram.num = strtoi(sub(" .*", "", so$YearsProgram)) + 0.5
so$YearsProgram.num[is.na(so$YearsProgram.num)] = 0.5

table(so$YearsProgram.num)

hist(so$YearsProgram.num)

hist(so$Salary)

# Levels der FirmengrÃ¶ÃŸe in hÃ¼bsche Reihenfolge bringen
levels(so$CompanySize)
so$CompanySize = factor(so$CompanySize, levels = c(
  "I don't know",
  "I prefer not to answer",
  "Fewer than 10 employees",
  "10 to 19 employees",
  "20 to 99 employees",
  "100 to 499 employees",
  "500 to 999 employees",
  "1,000 to 4,999 employees",
  "5,000 to 9,999 employees",
  "10,000 or more employees"))

de= so[so$Country=='Germany',]
hist(de$Salary)

# Wir entfernen alle Werte die zu niedrig sind
de = de[de$Salary > 15000,]

# Salary ~ YearsProgram
modell1 = "
model {
for (i in 1:590) {
Salary[i] ~ dnorm(intercept + beta.year * YearsProgram.num[i], 1/sigma^2)
}
# intercept kann auch negativ sein
# Abschätzung: view(de$Salary) max Wert = 140000
# Doppelte davon also nehmen wir 300000
intercept~dnorm(0, 1/300000^2)
# Abschätzung: sd(de$Salary) / sd(de$YearsProgram.num)
beta.year~dnorm(0, 1/3000^2)
# Abschätzung: sd(de$Salary)
sigma~ dexp(3/25000)
}
"

fit1 = run.jags(modell1, data=de, monitor=c('intercept', 'beta.year', 'sigma'), n.chains=3)

view(fit1)

# MC%ofSD sind der Fehlerprozent und ist hier weit unter 2% die wird erreichen sollten - ALso gut!

diagMCMC(fit1$mcmc, 'beta.year')
# Ist eine sehr schöne Diagnose.

samples1=as.matrix(fit1$mcmc)
dim(samples1)

dimnames(samples1)

hist(samples1[,'beta.year'])
#Da es ein Vektor ist funktioniert $ nicht. Wir müssen mit [] arbeiten.

plotPost(samples1[,'beta.year'])
#Das sind echte Vorhersagen nun.

mean(samples1[,'beta.year'] > 1500)
# Chance das ein Wert über 1500 liegt ist also 7%

# Neues Modell2

levels(de$CompanySize)
# Haben 10 verschiedene

de$CompanySize.int=as.integer(de$CompanySize)
#Müssen die Gruppen in integer umwandeln damit wir durch eine for Schleife die Werte verarbeiten können.

modell2 = "
model {
for (i in 1:590) {
Salary[i] ~ dnorm(mu[i], 1/sigma^2)
mu[i] <- int.company[CompanySize.int[i]] + beta.year * YearsProgram.num[i]
}
for (i in 1:10) {
int.company[i] ~ dnorm(0, 1/100000^2)
}
intercept~dnorm(0, 1/300000^2)
# Abschätzung: sd(de$Salary) / sd(de$YearsProgram.num)
beta.year~dnorm(0, 1/3000^2)
# Abschätzung: sd(de$Salary)
sigma~ dexp(3/25000)
}
"

fit2=run.jags(modell2, data=de, monitor = c('int.company', 'beta.year', 'sigma'))

view(fit2)
# Wir haben nun 10 intercept Werte!
# MC%ofSD ist wieder weit unter 2% Sehr gut!

samples2=as.matrix(fit2$mcmc)
plotPost(samples2[,'beta.year'], cenTend = 'mean')
plotcoef(fit2)
# Hier sehen wir auf einem Blick wo ca. alle Werte sind

plotcoef(fit2, 'company')
# Nur unsere 10 intercepts die wir genommen haben.

levels(de$CompanySize)
# Gruppe 1-10

diff.klein.gross=samples2[,'int.company[10]'] - samples2[,'int.company[5]']
plotPost(diff.klein.gross, compVal = 10000)
# Wenn man von einer kleinen Firma in eine große wechselt liegt die Chance mehr als 10000 zu verdienen liegt bei 27,8%!

# Neues Modell3

modell3 = "
model {
for (i in 1:590) {
Salary[i] ~ dnorm(mu[i], 1/sigma^2)
mu[i] <- int.company[CompanySize.int[i]] + beta.year * YearsProgram.num[i]

# Vorhersage (predict)
y.hat[i] ~ dnorm(mu[i], 1/sigma^2)
}
for (i in 1:10) {
int.company[i] ~ dnorm(0, 1/100000^2)
}
# Abschätzung: sd(de$Salary) / sd(de$YearsProgram.num)
beta.year~dnorm(0, 1/3000^2)
# Abschätzung: sd(de$Salary)
sigma~ dexp(3/25000)
}
"

fit3=run.jags(modell3, data=de, monitor = c('y.hat'), n.chains=3)

samples3=as.matrix(fit3$mcmc)
dim(samples3)

plotPost(samples3[,100], compVal = de$Salary[100])

# posterior predictiv check
par(mfrow=c(3,3))
hist(de$Salary, col="darkblue")
for (i in 1:8) {
hist(samples3[i*1000,], col="lightblue")
}
# Unsere 8 Zufalls generierten sind Gleichverteilt weil wir das im Modell so angegeben haben.
# Sieht man in Zeile 116

sample3.means = rowMeans(samples3)
plotPost(sample3.means, compVal = mean(de$Salary))
# In unserem Sample drinnen haben wir 30000 Datensets. Wir rechnen hier von allen die Mittelwerte aus und vergleichen diesen mit dem Originalen.
# Diese zwei stimmen überein. Das ist sehr gut für uns.

sample3.mins = apply(samples3, 1, min)
# Hier rechnen wir uns alle minimum Werte aus.
plotPost(sample3.mins, compVal = min(de$Salary))
# Das haben wir nicht getroffen.
plotPost(sample3.mins, compVal = 0)

sample3.maxs = apply(samples3, 1, max)
plotPost(sample3.maxs, compVal = max(de$Salary))
# Sieht auch nicht gut.

###########Tabs oder Space ################

table(de$TabSpaces)
# wir nehmen einen weiteren Gruppen INtercept mit auf
de$TabSpaces.int= as.integer(de$TabSpaces)


modell4="
model
{
  for(i in 1:590)
  {
  Salary[i]~dnorm(mu[i],1/sigma^2)
  
  mu[i]<-  int.company[CompanySize.int[i]] +
  int.space[TabSpaces.int[i]]+
  beta.year*YearsProgram.num[i]
  }
  
  
  for(i in 1:10)
  {
  int.company[i] ~ dnorm(0,1/100000^2)
  }
  
  for(i in 1:3)
  {
  int.space[i] ~ dnorm(0,1/100000^2)
  }
  #Abschätzung beta.year = sd(de$Salary)/sd/de$YearsProgram.num *001
  beta.year ~ dnorm(0,1/3000^2)
  #Abschätzung: sd(de$Salary)
  sigma ~ dexp(3/25000) #3 ist eine Regel bei der man sagen kann man hat 95% der WErte bei einer exponential verteilung
}
"

fit4=run.jags(modell4, data=de, monitor=c('int.company','int.space','beta.year','sigma'), n.chains = 3)

view(fit4)
diagMCMC(fit4$mcmc, int.space[2])

samples4 = as.matrix(fit4$mcmc)
diff.tab.space = samples4[,'int.space[2]'] - samples4[,'int.space[3]']
plotPost(diff.tab.space, compVal = 0)