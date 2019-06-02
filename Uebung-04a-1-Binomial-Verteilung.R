# AKIT2, Arno Hollosi
# Übung
# Binomial-Verteilung
# Die Tipps sind mit "rot13" verschlÃ¼sselt. EntschlÃ¼sselung z.B. mithilfe von www.rot13.com

# Wie hoch ist die Chance, ...
# bei 5 Münzwürfen mit fairer Münze, genau 4x Kopf zu haben?
# Tipp: Irejraqra Fvr qovabz()

dbinom(x = 4, size = 5, prob = 0.5, log = FALSE)
#0.15625 

# bei 5 Münzwürfen mit fairer Münze, genau 3x Kopf zu haben?
dbinom(3, 5, 0.5) #Ist das gleiche wie oben nur mit 3 statt 4 als x
#0.3125

# bei 5 Münzwürfen mit fairer Münze, *maximal* 2x Kopf zu haben?
# Tipp: Fvr xÃ¶aara angÃ¼eyvpu qvr rvamryara qovabz()-Jregr nhfhzzvrera (0 avpug iretrffra)
#       rssrxgvire trug rf nore zvg qre Shaxgvba covabz()
dbinom(0, 5, 0.5) + dbinom(1, 5, 0.5) + dbinom(2, 5, 0.5)
#0.5
#Kann aber sich aber sparen mit pbinom()
pbinom(2, 5, 0.5)
#0.5

# bei 5 Münzwürfen mit Münze mit p=0.75, *maximal* 2x Kopf zu haben?
pbinom(2, 5, 0.75)
#0.1035156 = 10%

# bei 5 Münzwürfen mit Münze mit p=0.75, *mindestens* 2x Kopf zu haben?
# Tipp: Fvr xÃ¶aara rvasnpu <1 - Jnuefpurvayvpuxrvg> erpuara, rvasnpure
#       vfg rf, qra Cnenzrgre "ybjre.gnvy=SNYFR" mh irejraqra.
#       Npughat: zhff aha qvr Teramr a=2 bqre a=1 frva?
1 - pbinom(1, 5, 0.75)
#0.984375
pbinom(1, 5, 0.75, lower.tail = FALSE)
#0.984375