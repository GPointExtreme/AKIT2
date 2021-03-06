library(car)

diamonds = read.csv("C:\\Users\\Dominik\\Downloads\\diamonds.csv")
summary(diamonds)
#Die Variablen Colour, Clarity, und Certification sind in Gruppen unterteilt
#Wenn der Diamant zu der jeweiligen Gruppe z�hlt, dann ist der Wert in dieser Gruppe 1, ansonsten 0

plot(Price ~ Carat, data = diamonds)

#Modell erstellen
model = lm(Price ~ Carat, data = diamonds)
summary(model)
abline(model, col = 'blue')
qqp(resid(model)) #Wann nehmen wir nur das Model oder die Residuen?

#2. Modell mit allen Variablen aufsetzen
model2 = lm(Price ~ Carat + Colour + Clarity, data = diamonds)
summary(model2)

qqp(model2) #hier sieht man, dass die Residuen nicht sch�n normalverteilt sind
plot(model2, which=1)
#hier ist eine deutliche Form zu erkennen. Diese weist darauf hin, dass unser Modell
#von der Beschreibung her nicht optimal ist.
#Einerseits weil bei den niedrigen Werten und bei den hohen Werten falsche Vorhersagen machen
#Wir m�ssen uns also �berlegen, ob wir bei diesem Modell irgendetwas ver�ndern k�nnen,
#damit es mehr den Erfordernissen der linearen Regression entspricht


#--> die erste Idee, da es sich um einen Preis handelt, ist, dass wir den Preis logarithmisch
#transformieren, da der Preis eher etwas multiplikatives ist

model3 = lm(log(Price) ~ Carat + Colour + Clarity, data = diamonds) #hier nimmt er auch nicht log2 oder log10 und er logarithmisiert auch direkt im Modell
plot(model3, which=1)
#jetzt hat sich zwar die Struktur ver�ndert, also die Kr�mmung zeigt nun nach oben und nicht mehr wie vorher nach unten,
#aber wir haben immer noch das Problem, dass es �berhaupt so eine starke Kr�mmung gibt
#es sind jetzt aber die Ausrei�er im hohen Bereich ein bisschen besser geworden


#--> Dreiteilung in ein einziges Modell hineinbringen


#Daten in drei unterschiedliche Gruppen aufteilen

diamonds$size = factor(ifelse(diamonds$Carat < 0.5, 'small',
                              ifelse(diamonds$Carat < 0.95, 'medium', 'large')))
  #neue Variable einf�gen
summary(diamonds$size)


#im neuen Modell wird die zus�tzliche Variable miteingebaut
model4 = lm(log(Price) ~ Carat + Colour + Clarity + size, data = diamonds) #hier nimmt er auch nicht log2 oder log10 und er logarithmisiert auch direkt im Modell
summary(model4) #Achtung beim interpretieren, da ja der Preis log transformiert wurde
#in der Summary kann man schon sehen, dass die gerade eingef�hrte Variable size Einfluss auf das Ergebnis hat
#das wesentliche ist aber, dass "size" eine Verschiebung des Intercepts darstellt


#Beispieldiamant: Colour F, Clarity VS1, size=medium, Carat=0.6
cf4 = coef(model4)

#zugeh�rige Gleichung, um den Preis zu berechnen
price1 =
  cf4[1] + #Intercept
  cf4[2] * 0.6 + #Carat = 0.6
  cf4[4] + #Colour F
  cf4[8] + #Clarity VS1
  cf4[12] #size medium
exp(price1) #Ergebnis: 4.087,831

#Das size bedeutet eine Verschiebung des Intercepts
#Die Steigung der drei unterschiedlichen Geraden bleibt immer gleich, sie werden nur anhand des Intercepts verschoben
#im Plot sieht man auch, dass aber auch die Steigungen der Geraden nicht optimal sind, sprich nicht optimal in die vorhanden Datenpunkte hinein passen
plot(log(Price) ~ Carat, data = diamonds)
lines(x<-c(0,0.5), y=pricing1(x, cf4[13]), col='darkgreen', lwd=2)
lines(x<-c(0.5,0.95), y=pricing1(x, cf4[12]), col='darkred', lwd=2)
lines(x<-c(0.95,1.1), y=pricing1(x, 0), col='darkblue', lwd=2)
pricing1 = function(carat, size){
  cf4[1] + #Intercept
    cf4[2] * carat +
    cf4[4] +
    cf4[8] +
    size 
}


#wir m�chten die Steigung der einzelnen Geraden auch noch besser anpassen, so dass die einzelnen Linien auch noch unterschiedliche Steigungen haben
#die �berlegung ist also die Gr��e in Carat und die Gr��e in den Gruppen small, medium und large miteinander zu multiplizieren
#daf�r erhalten wir dann wieder eigene Koeffizienten

model5 = lm(log(Price) ~ Carat + Colour + Clarity + size + size:Carat, data = diamonds)
#size:Carat --> sagt aus, dass diese beiden Werte miteinander multipliziert werden
#Interaktionsterm wird mit ":" geschrieben, dann sind bei beiden Variablen die verwendet werden auch im Modell enthalten
#alternativ kann auch "*" verwendet werden, dann m�ssen die beiden Variablen nicht extra im Modell angef�hrt werden
#Interaktionsterm ist eine Multiplikation der beiden Variablen

summary(model5)
#hier sehen wir, dass noch zus�tzliche Koeffizienten f�r die Interaktion haben

#zugeh�riger Plot
plot(log(Price) ~ Carat, data = diamonds)
lines(x<-c(0,0.5), y=pricing2(x, cf5[13], cf5[15]), col='darkgreen', lwd=2)
lines(x<-c(0.5,0.95), y=pricing2(x, cf5[12], cf5[14]), col='darkred', lwd=2)
lines(x<-c(0.95,1.1), y=pricing2(x, 0, 0), col='darkblue', lwd=2)
cf5 = coef(model5)
pricing2 = function(carat, size, interact){
  cf5[1] + #Intercept
    cf5[2] * carat +
    cf5[4] +
    cf5[8] +
    size +
    interact*carat
}
#hier sehen wir jetzt, dass die einzelnen Geraden, neben einem unterschiedlichen Intercept auch unterschiedliche Steigungen aufweisen

plot(model5, which=1) #Residuen beim neuen Modell
#hier sind die Residuen ohne gr��ere Struktur sichtbar
#links vl noch eine Varianz die ein bisschen gr��er ist als recht, aber im Prinzip ist es recht gleichm��ig verteilt

qqp(model5) #jetzt sind die Residuen gut normalverteilt

#Eine M�glichkeit um dies zu erreichen ist somit, einen Interaktionsterm in das Modell mithinein zu nehmen
#Ein Hinweis auf eine andere M�glichkeit ist, dass diese Kr�mmung vom Anfang �hnlich aussieht wie eine quadratische Kurve
#das hei�t man k�nnte, obwohl wir beim linearen Modell sind, eine quadratische Variable hineinbringen

#neue Variable in unseren Datensatz hinzuf�gen und in das Modell mit aufnehmen:
diamonds$Carat.sq = diamonds$Carat^2

model6 = lm(log(Price) ~ Carat + Carat.sq + Colour + Clarity, data = diamonds)
#aus Sicht des linearen Modells ist es noch immer linear, da wir nur die einzelnen Variablen zusammenz�hlen
#in der Realtit�t ist Carat.sq aber ein quadratischer Term
summary(model6)
plot(model6, which = 1) #auch hier sehen die Residuen gut aus
qqp(model6) #nicht ganz so sch�n, wie im Modell mit der size

plot(log(Price) ~ Carat, data = diamonds)
lines(x<-seq(0.2,1,0.01), y=pricing3(x), col='darkgreen', lwd=2)
cf6 = coef(model6)
pricing3 = function(carat, size, interact){
  cf6[1] + #Intercept
    cf6[2] * carat +
    cf6[3] * (carat^2) + 
    cf6[4] +
    cf6[8]
}
#hier sehen wir, dass die Regressionslinie nun gebogen ist, da der quadratische Term mit ber�cksichtigt wird
#auch hier bildet diese Linie das Verh�ltnis zwischen den Diamanten sehr gut ab
#unser Modell beschreibt jetzt einigerma�en gut, wie sich unsere Preise entwickeln

#Frage: ist das Modell mit dem quadratischen Term, oder das Modell mit dem zus�tzlichen Interaktionsterm besser?
#um das herauszufinden, kann man sich anschauen, wie viel Varianz im Modell erkl�rt wird
#--> beim quadratischen Modell sind es 99,5%
#--> beim Modell mit Interaktionsterm sind es 99,55%
#von dem her w�re eventuell das Modell 5 vorzuziehen
#man sollte sich aber bei der Interpretation nicht auf das verlassen, das kann nur ein erster Anhaltspunkt sein
#es gibt gute Argumente beide Modelle f�r richtig zu halten

#--> in der Praxis w�rden wir uns wahrscheinlich auf Basis von zus�tzlichem Wissen, das
#wir �ber Diamanten haben, unsere Entscheidung f�r ein Modell treffen

#ZUSAMMENFASSUNG:
#Bestimmte Datens�tze k�nnen auf eine Art und Weise strukturiert sein k�nnen,
#dass die Grundvoraussetzungen und Annahmen, die ein lineares Regressionsmodell erlauben
#nicht vorhanden sind. In diesem Beispiel war das insbesondere (nicht nur dass die Residuen nicht normalverteilt sind)
#das, dass die Residuen eine sehr starke Struktur aufweisen (Kr�mmung im which=1)
