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
sd.split=(sd(router$jitter[router$gruppe=="neu"])+sd(router$jitter[router$gruppe=="alt"]))/2
mean.split=(mean(router$jitter[router$gruppe=="neu"])-mean(router$jitter[router$gruppe=="alt"]))
d=mean.split/sd.split

library(pwr)
pwr.t.test(d=d, sig.level = 0.05, power = 0.85, alternative = "less")

# 2) Ist der neue Router besser?
#Neuer ist wohl ein bischen Besser. Da wir in Zeile 13 und 14 "neu" vorne haben.
#"neu" ist vorne und es kommt -0.1984052 raus. Negativ ist Besser.

# 3) Wenn wir uns in 9 von 10 FÃ¤llen richtig entscheiden wollen, ist der neue Router dann
#    besser bzw. immer noch besser?
pwr.t.test(n=300, sig.level = 0.1, power = 0.85, alternative = "less")
#d = -0.1893957
#Router ist immer noch Besser auch wenn der Effekt ein bischen kleiner ist.

# 4) Wie würden Sie als IT-Leiter entscheiden? Welche Information fehlt Ihnen noch für eine Entscheidung?
#
# Notieren Sie Ihre Vorgehensweise und Argumente für/wider eine Verbesserung.

router = read.csv("daten/network.csv")

# Die Tipps sind mit "rot13" verschlÃ¼sselt. EntschlÃ¼sselung z.B. mithilfe von www.rot13.com
# Sie sollten die Tipps nur schrittweise entschlÃ¼sseln, nicht alle gleichzeitig
#
# Tipp 1: Orerpuara Fvr qvr Cbjre zvg cbjre.g.grfg haq cbjre.s2.grfg.
# Tipp 2: Orerpuara Fvr q naunaq qre Zvggryjregr haq Fgnaqneqnojrvpuhatra. Trora Fvr rvar
#         refgr RvafpuÃ¤gmhat no.
# Tipp 3: Frgmra Fvr qnf yvarner Zbqryy na haq fpunhra Fvr fvpu nhpu qnf pbasvag() na.
# Tipp 4: Mrvpuara Fvr Uvfgbtenzzr & Obkcybg (uÃ¤ggr tyrvpu nz Nasnat trznpug jreqra fbyyra)
# Tipp 5: Orv Sentr (3) trug rf hz qnf Xbasvqramvagreinyy, avpug hz qvr Cbjre.
# Tipp 6: SÃ¼e rvar Ragfpurvqhat sruyra h.n. abpu Xbfgra qrf arhra Ebhgref, trjÃ¼afpugr Ireorffrehat qrf Wvggre rgp.
