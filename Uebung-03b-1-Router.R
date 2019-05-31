# AKIT2, Arno Hollosi
# Übung

# Aufgabenstellung:
# Wir wollen in unserem Netzwerk den Jitter von IP-Packeten reduzieren.
# Wir testen dazu das Verhalten eines neuen Routers im Vergleich zum alten.
# Wir haben kein Vorwissen, ob der neue Router schlechter oder besser ist.
# Im Datenfile finden Sie 300 Messpunkte (in ms):
# 150 Jitter-Werte vom alten, 150 Jitter-Werte vom neuen Router.
router = read.csv("C:\\Users\\Dominik\\Downloads\\network.csv")

# 1) Berechnen Sie, welchen Effekt wir mit 85% Wahrscheinlichkeit messen können.
library(pwr)
pwr.t.test(n = 150, power = 0.85)
#Wir finden ein d=0.35 mit 85% Wahrscheinlichkeit.

pwr.f2.test(u = 1, v = 300-2, power = 0.85)
#Wir finden ein f2=0.03 mit 85% Wahrscheinlichkeit.

sd.split=(sd(router$jitter[router$gruppe=="alt"])+sd(router$jitter[router$gruppe=="neu"]))/2
mean.split=(mean(router$jitter[router$gruppe=="alt"])-mean(router$jitter[router$gruppe=="neu"]))
d=mean.split/sd.split
#d=0.2 ist somit zu Klein. Wir haben wohl zu wenig Daten.

# 2) Ist der neue Router besser?
model = lm(jitter ~ gruppe, data = router)
summary(model)
confint(model)
#Konfidenzinteval jitter -2.45 bis 0.17 schließt 0 mit ein.
#Keine eindeutige Aussage im 95% Level möglich.
#Tendenziell könnte man aber eine verbesserung vermuten.

f2 = summary(model)$r.squared / (1 - summary(model)$r.squared)
#f2=0.01 ist nur ein sehr kleiner Effekt und wir unterschreiten die
#vorberechnete Power. In der Praxis müssten wir nun wohl weitere Daten
#erheben.

boxplot(jitter ~ gruppe, data = router)
#Daten sind Nahezu gleich. Beim alten gibt es jedoch Ausreißer nach oben
#und beim neuen Ausreißer nach unten.
oldpar = par(mfrow=c(1,2))
hist(router$jitter[router$gruppe=="alt"], main="alt", col="blue")
hist(router$jitter[router$gruppe=="neu"], main="neu", col="blue")
par(oldpar)

# 3) Wenn wir uns in 9 von 10 Fällen richtig entscheiden wollen,
#    ist der neue Router dann besser bzw. immer noch besser?
confint(model, level=0.9)
#Nun schließen wir 0 nicht mehr mit ein.
#Im 90% Level ist eine veringerung des Jitter am neuen Router um
#-2.24 bis -0.05 erkennbar.
#Dies Ändert aber nichts daran das der Effekt im Verhältnis immer noch
#zu klein ist! Hier müssen wir vorsichtig sein beim Interpretieren.

# 4) Wie würden Sie als IT-Leiter entscheiden? Welche Information fehlt Ihnen noch für eine Entscheidung?
# Notieren Sie Ihre Vorgehensweise und Argumente für/wider eine Verbesserung.
#Mit diesen Daten können keine eindeutigen Beurteilungen gemacht werden.
#Für eine Entscheidung fehlen und noch die gewünschte Verbesserung des
#jitters und wieviel die Router kosten würden.
#Geht es nur um einen Austausch würden wir diesen machen da der neue Router
#anscheinend nicht schlechter als der alte ist.
#Falls es um eine verbesserung der Jitter geht dann würden wir mehr Daten
#einholen um Bessere Aussagen treffen zu können.

# Die Tipps sind mit "rot13" verschlÃ¼sselt. Entschlüsselung z.B. mithilfe von www.rot13.com
# Sie sollten die Tipps nur schrittweise entschlüsseln, nicht alle gleichzeitig
#
# Tipp 1: Orerpuara Fvr qvr Cbjre zvg cbjre.g.grfg haq cbjre.s2.grfg.
# Tipp 2: Orerpuara Fvr q naunaq qre Zvggryjregr haq Fgnaqneqnojrvpuhatra. Trora Fvr rvar
#         refgr RvafpuÃ¤gmhat no.
# Tipp 3: Frgmra Fvr qnf yvarner Zbqryy na haq fpunhra Fvr fvpu nhpu qnf pbasvag() na.
# Tipp 4: Mrvpuara Fvr Uvfgbtenzzr & Obkcybg (uÃ¤ggr tyrvpu nz Nasnat trznpug jreqra fbyyra)
# Tipp 5: Orv Sentr (3) trug rf hz qnf Xbasvqramvagreinyy, avpug hz qvr Cbjre.
# Tipp 6: SÃ¼e rvar Ragfpurvqhat sruyra h.n. abpu Xbfgra qrf arhra Ebhgref, trjÃ¼afpugr Ireorffrehat qrf Wvggre rgp.
