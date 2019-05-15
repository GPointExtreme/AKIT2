df = read.csv('C:\\Users\\Dominik\\Downloads\\operationen.csv')

table(df$Team)
table(df$Resultat)
addmargins(table(df$Team, df$Resultat))
148/502 # 29% Team B
188/498 # 37% Team A

table(df$Schwierigkeit)
table(df$Team, df$Resultat, df$Schwierigkeit)

# Team A, leicht: 20%
56/(214+56)
# Team a, schwer: 58%
132/(132+96)

# Team B, leicht: 22%
89/(89+318)
# Team B, schwer: 62%
59/(59+36)


# Neues Beispiel
df = read.csv('C:\\Users\\Dominik\\Downloads\\dichotomize.csv')
dfi = read.csv('C:\\Users\\Dominik\\Downloads\\dichotomize-icecream.csv')

summary(lm(cost ~ work + complexity, data=df))
df$complexFactor = ifelse(df$complexity < 13.5, 'low', 
                          ifelse(df$complexity > 18.5, 'high', 'normal'))
table(df$complexFactor)

summary(lm(cost ~ work + complexFactor, data=df))

# Beispiel Eis

summary(lm(deaths ~ icecream, data=dfi))
summary(lm(deaths ~ icecream + temperature, data=dfi))
table(dfi$heat)
plot(dfi$heat, dfi$temperature)

summary(lm(deaths ~ icecream + heat, data=dfi))
exp(coef(model))
# Neues Beispiel!!!

df = read.csv('C:\\Users\\Dominik\\Downloads\\Fair-JPE-1978_PsychToday.csv')

df$had.affair = ifelse(df$affairs > 0, 1, 0)
table(df$had.affair)

model = glm(had.affair ~ age, data=df, family = binomial(link='logit'))
summary(model)
exp(coef(model))

predict(model, newdata=data.frame(age=46), type='response')
# 28,5%

-1.557192 + 46*0.013920 # logit
odds = exp(-1.557192 + 46*0.013920) # odds (Wettchance) 0.4 == 2/5
odds/(1+odds) # absolute Wahrscheinlichkeit: 28.6%

model2 = glm(had.affair ~ age + married, data=df, family = binomial(link='logit'))
summary(model2)
exp(coef(model2))
# married ist 1.1070306 heiﬂt das es um 10% immer steigt.
# age ist 0.967634 nimmt immer um 3% ab pro Jahr.

age = seq(30, 100, length.out = 200)
married = seq(0, 70, length.out = 200)

vorhersage = predict(model2, newdata = data.frame(age=age, married=married), type = 'response')

plot(age, vorhersage, type='l')                     

df$sex = ifelse(df$gender==0, 'femlae', 'male')
model3 = glm(had.affair ~ age + married + sex, data=df, family = binomial(link='logit'))
summary(model3)
exp(coef(model3))

predict(model3, newdata = data.frame(age=35, married=5, sex=c('male', 'femlae')), type='response')

predict(model3, newdata = data.frame(age=35, married=15, sex=c('male', 'femlae')), type='response')

predict(model3, newdata = data.frame(age=65, married=25, sex=c('male', 'femlae')), type='response')

df$religious = as.factor(df$religious)
model4 = glm(had.affair ~ age + married + sex + religious, data=df, family = binomial(link='logit'))
summary(model4)
# Im Intercept ist female und reli1 drinnen.
exp(coef(model4))
