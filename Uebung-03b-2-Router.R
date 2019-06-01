# AKIT2, Arno Hollosi
# ‹bung

# Aufgabenstellung (analog zu ‹bung 03b-1):
# Wir wollen in unserem Netzwerk den Jitter von IP-Packeten reduzieren.
# Wir testen dazu das Verhalten eines neuen Routers im Vergleich zum alten.
# Im Datenfile finden Sie Messpunkte zu altem und neuem Router.
# Damit sich die Umstellung lohnt, muss der neue Router mindestens 5ms kleineren Jitter haben.
# Wir wollen zumindest einen Unterschied von 2ms zuverl‰ssig (=zu 90%) messen kˆnnen.
router = read.csv("C:\\Users\\Dominik\\Downloads\\network2.csv")

# 1) Bestimmen Sie, wie viele Samples Sie brauchen.
hist(router$jitter)
#Daten sehen Normalverteilt aus. Keine Auff‰lligkeiten.

plot(router$gruppe, router$jitter)
#Hier erkennen wir schon das "neu" geringere Werte wie "alt" hat.
#Es gibt einige Ausreiﬂer im oberen bereich jedoch sind diese noch im 75% Teil.

library(pwr)
sd(router$jitter[router$gruppe=="alt"])
sd(router$jitter[router$gruppe=="neu"])
#Wir runden auf 10 auf.

pwr.t.test(d=2/10, power = 0.9)
#Hier verwenden wir 2 weil wir 2ms messen wollen.
#Den Befehl "round" verwenden wir weil es im Tipp steht. Kˆnnten aber 
#auch 10 stattdessen einsetzen wir in Zeile 23 beschrieben.
#Wir brauchen 527 pro Gruppe.

pwr.f2.test(u=1, v=527*2-2, power=0.9)
#zu 90% finden wir einen Effekt mit f2=0.01

# 2) Holen Sie sich exakt diese Anzahl an Samples zuf‰llig aus dem Datensatz. (Siehe Tipp 3)
#    Verwenden Sie set.seed(1234) bevor Sie das Zufallssample ziehen.
library(dplyr)
set.seed(1234)
samples = sample_n(tbl = router, size = 527*2)
show(samples$gruppe)

# 3) Stellen Sie ein Modell mit dem ausgew‰hlten Sample auf und interpretieren Sie das Ergebnis.
model = lm(jitter ~ gruppe, data = samples)
summary(model)

f2 = summary(model)$r.squared / (1-summary(model)$r.squared)
#Unser Effekt ist 0.12, also deutlich ¸ber das f2=0.01!

confint(model)
#Von -7.895075 bis -5.513807

# 4) Wiederholen Sie obigen Vorgang (2 & 3) 1000x (set.seed nicht in der Schleife aufrufen!).
#    Wie viele Konfidenzintervalle schlieﬂen 5ms ein?
#    Was leiten Sie daraus ab?
lower.ci = numeric(1000)
higher.ci = numeric(1000)
for (i in 1:1000) {
  daten = sample_n(router, 527*2, replace=TRUE)
  fit = lm(jitter ~ gruppe, data = daten)
  lower.ci[i] = confint(fit)[2,1]
  higher.ci[i] = confint(fit)[2,2]
}
#Wie viel CI schlieﬂen 5ms mit ein?
mean(lower.ci < -5 & higher.ci > -5)
#Wir bekommen hier 85% raus. 
lower.ci[2] #-6.653063
higher.ci[2] #-4.197304
#-6.65 < -5 > -4.19

# 5) Bestimmen Sie den echten Wert des Gruppenunterschieds in unserer Population (=3000 Datens‰tze)
#    Was schlieﬂen Sie daraus? In wiefern kˆnnen wir also von einer einzigen Stichprobe auf einen
#    Unterschied >5ms schlieﬂen?
diff.pop = mean(router$jitter[router$gruppe=="neu"])-mean(router$jitter[router$gruppe=="alt"])

#Wie h‰ufig ist der echte Populationswert eingeschlossen?
mean(lower.ci < diff.pop & higher.ci > diff.pop)
#Hier bejomme ich 95% raus.

# 6) Z‰hlen Sie in der Gruppe des neuen Routers bei den Jitter-Werten 5ms hinzu.
#    Wiederholen Sie Schritt 2 mit diesem ge√§nderten Datensatz. (inkl. set.seed())
#    Was kˆnnen Sie nun schlieﬂen?
set.seed(1234)
messung = sample_n(router, 527*2)
messung$jitter[messung$gruppe=="neu"] = messung$jitter[messung$gruppe=="neu"]+5
#Bekomme hier einen Fehler?
model = lm(jitter ~ gruppe, data = messung)
summary(model)
confint(model)

# Die Tipps sind mit "rot13" verschl√ºsselt. Entschl√ºsselung z.B. mithilfe von www.rot13.com
# Sie sollten die Tipps nur schrittweise entschl√ºsseln, nicht alle gleichzeitig
#
# Tipp 1: Mrvpuara Fvr Uvfgbtenzzr haq Obkcybg. Tvog rf Nhss√§yyvtxrvgra?
# Tipp 2: Orfgvzzra Fvr qvr Fgnaqneqnojrvpuhat, ehaqra nhs qvr a√§pufgr tnamr zf, haq
#         orerpuara qnaa q haq va Sbytr cje.g.grfg()
# Tipp 3: Va qre Ovoyvbgurx "qcyle" tvog rf qvr Shaxgvba fnzcyr_a(), qvr qnorv uvysg.
# Tipp 4: Va qre Fpuyrvsr: fnzcyr_a(), yz(), pbasvag(); ibeno Inevnoyr vavgvnyvfvrera zvg
#         m.O. ybjre = ahzrevp(1000) haq uvture = ahzrevp(1000), qnaa va orvqr zvg
#         ybjre[v] = ..., uvture[v] = ... qvr Retroavffr rvagentra.
#         Fvr fbyygra orv fnzcyr_a() aha mhf√§gmyvpu qra Cnenzrgre "ercynpr=GEHR" irejraqra.
