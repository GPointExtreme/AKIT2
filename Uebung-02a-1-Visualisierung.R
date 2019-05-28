# Übung, AKIT2
# Visualisierung von Regressionsgeraden
# Arno Hollosi

# Wir starten mit dem Blutdruckdatensatz:
df = read.csv('C:\\Users\\Dominik\\Downloads\\blutdruck.csv')

# Ein Boxplot gibt uns eine schnelle Ãœbersicht Ã¼ber die Verteilung einzelner Variable:
old_settings = par(mfrow=c(1,3), mar=c(1,2,3,1))
boxplot(df$blood.pressure, main="Blood pressure")
boxplot(df$birth.weight, main="Birth weight")
boxplot(df$current.weight, main="Current weight")
par(old_settings)

# "Lesen" eines Boxplots:
# - die dicke Linie in der Mitte ist der Median (=50% Quantil)
# - die Box entspricht der Interquartile Range (IQR; 25%-75% Quantil)
# - die Schnurrhaare (engl. Whiskers) entsprechen:
#   - oben: jener Wert, der gerade noch innerhalb von 75%-Quantil + 1.5*IQR liegt
#   - unten: jener Wert, der gerade noch innerhalb von 25%-Quantil - 1.5*IQR liegt
# - Werte außerhalb werden als "Ausreißer" mit Punkten dargestellt

# Wir stellen unser Modell auf
model = lm(blood.pressure ~ current.weight + birth.weight, data=df)

# (1) Sehen Sie sich nochmal die Koeffizienten an und interpretieren Sie diese
summary(model)

# Wie nun eine Regressionsgerade darstellen? SchlieÃŸlich haben wir zwei Variable...
# Wir könnten so tun, als ob die jeweils andere Variable nicht existiert:
# Zuerst einen Scatterplot der Variable mit Blutdruck machen:
plot(df$birth.weight, df$blood.pressure,
     col=rgb(0,0,0,0.3), main="Birth weight vs Blood pressure")

# Mit abline(a=, b=) können wir eine Linie mit Steigung einzeichnen:
# ... a=Intercept, b=Steigung
# Wir verwenden die Koeffizienten unseres Modells
abline(a=coef(model)[1], b=coef(model)[3], col="blue")

# Das ist unbefriedigend: die Linie, welche nur knapp sichtbar ist entspricht
# jener Linie, wenn current.weight=0.
# Machen wir es besser: zeichnen wir 5 Linien ein, bei denen current.weight
# die 2.5%, 25%, 50%, 50%, 75% und 97.5% Quantil-Werte hat:
cwq = quantile(df$current.weight, c(0.025, 0.25, 0.5, 0.75, 0.975))

# Aus Sicht unseres Diagramms veränderen current.weight-Werte nur den Offset:
# schlieÃŸlich ist die X-Achse ja birth.weight!
intercept = coef(model)[1]
cw.coef = coef(model)[2]
bw.coef = coef(model)[3]
colors = rainbow(5)
for (i in 1:5) {
  abline(a=intercept+cwq[i]*cw.coef, b=bw.coef, col=colors[i])
}
# Wir können auch noch eine Legende einfügen:
legend(x='bottomleft', legend=round(cwq,1), col=colors, lwd=2, title="Curr. weight")


# (2) Zeichnen Sie einen Ähnlichen Plot für blood.pressure vs. current.weight
plot(df$current.weight, df$blood.pressure,
     col=rgb(0,0,0,0.3), main="Current weight vs Blood pressure")

bwq = quantile(df$birth.weight, c(0.025, 0.25, 0.5, 0.75, 0.975))

intercept = coef(model)[1]
cw.coef = coef(model)[2]
bw.coef = coef(model)[3]
colors = rainbow(5)
for (i in 1:5) {
  abline(a=intercept+bwq[i]*bw.coef, b=cw.coef, col=colors[i])
}

# Wir könnten die Linien auch mithilfe von predict() erzeugen:
plot(df$birth.weight, df$blood.pressure,
     col=rgb(0,0,0,0.3), main="Birth weight vs Blood pressure")

# Wir erzeugen eine Folge von Werten für die X-Achse (hier: birth weigth),
# der den für uns interessanten Wertebereich abdeckt, z.B. von 1.5 bis 5.3:
bw = seq(1.5, 5.3, length.out = 200)
# Nun lassen wir uns die Werte vorhersagen:
bp = predict(model, data.frame(birth.weight = bw,
                               current.weight = mean(df$current.weight)))

# und dann zeichnen wir die Punkte mit Liniensegmenten verbunden ein:
lines(bw, bp, col="blue")
# diese Variante ist insoferne "einfacher", als dass wir nicht die Regressionsgleichung
# von Hand nachprogrammieren müssen. Zudem können damit auch nicht-lineare Zusammenhänge
# gezeichnet werden


# (3) Zeichnen Sie mithilfe einer Schleife wieder alle 5 Linien wie im vorigen Plot ein.
for (i in 1:5) {
  lines()
}
#Wie?

# (4) Machen Sie dasselbe auch für blood.pressure vs. current.weight
#Wie?

# Wir könnten uns auch veranschaulichen, wie die Verteilung von current.weight
# aussieht, indem wir den Punkten unterschiedliche Farben geben:
pcolors = topo.colors(100, alpha=0.75)
# Wir müssen nun den Wertebereich von current.weight auf 1..100 transformieren:
cw.index = round((df$current.weight - min(df$current.weight)) / (max(df$current.weight) - min(df$current.weight)) * 99 + 1)
plot(df$birth.weight, df$blood.pressure, col=pcolors[cw.index], main="Birth weight vs Blood pressure")
# Wir könnten nun wieder die verschiedenen Regressionsgeraden einzeichnen
#Wie? So wie in Frage 4?

# (5) Machen Sie auch einen entsprechendes Plot für blood.pressure vs. current.weight
#Kann erst gemacht werden wenn Frage 4 geklärt ist.

# Wenn Sie die Bibliothek threejs installieren, kÃ¶nnen Sie sich auch einen 3d-Plot ansehen
library(threejs)
# Wir verwenden nur 300 Punkte des Datensets, damit es Ã¼bersichtlicher bleibt
df2 = df[1:300,]

# Wieder verschiedene Farben, dieses Mal für blood.pressure:
bp = df2$blood.pressure
bp.index = round((bp - min(bp))/(max(bp)-min(bp))*99+1)
pcolors = topo.colors(100)[bp.index]
graph = scatterplot3js(x=df2$birth.weight,
                       y=df2$current.weight,
                       z=df2$blood.pressure,
                       color=pcolors, size=0.2, flip.y = F)
graph
# Sie können mit der linken Maustaste nun den Graphen drehen
# Wenn Sie in RStudio die Grafik mit "Zoom" vergrößern, dann können Sie auch die
# rechte Maustaste verwenden, um den Graphen neu zu positionieren

# Einzeichnen der Regressions-Ebene:
xseq = seq(min(df2$birth.weight), max(df2$birth.weight), length.out = 6)
yseq = seq(min(df2$current.weight), max(df2$current.weight), length.out = 6)
planexy1 = data.frame(birth.weight=rep(xseq, 2),
                      current.weight=c(rep(yseq[1],6), rep(yseq[6],6)))
planexy2 = data.frame(birth.weight=c(rep(xseq[1],6), rep(xseq[6],6)),
                      current.weight=rep(yseq, 2))
planexy = rbind(planexy1, planexy2)
planexy$blood.pressure = predict(model, planexy)
graph2 = points3d(graph, as.matrix(planexy), col="darkblue", size=0.01)
lines3d(graph2, c(1:6, 13:18)+300, c(7:12, 19:24)+300)
