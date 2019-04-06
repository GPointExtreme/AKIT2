# Daten einlesen
body = read.csv('C:\\Users\\Dominik\\Downloads\\body-dimensions.csv')

# Mittelwert wird mit mean() berechnet, Standardabweichung mit sd()
# Beispiel:
mean(body$Weight)
sd(body$Weight)

# (1) Berechnen Sie Mittelwert und Standardabweichung für das Alter
mean(body$Age)
sd(body$Age)

# Anzahl der Spalten und Anzahl der Zeilen eines Dataframes ermitteln:
nrow(body)
ncol(body)

# Namen der Spalten abfragen:
colnames(body)

# Alle Kategorien eines Faktors (=kategorische Variable) auflisten:
levels(body$Gender)

# Ein Faktor kann auch in Zahlen umgewandelt werden:
as.numeric(body$Gender) # bzw.:
as.integer(body$Gender)

# Zugriff auf Daten, wie bei zwei-dimensionalem Array
# body[zeilen, spalten]
body[4, 2]   # Wert in Zeile 4, Spalte 2
body$Age[3]  # dritte Zeile von body$Age

# (2) Geben Sie die fünfte Zeile von Weight aus
body$Age[5]

# (3) Geben Sie den Wert von Zeile 7, Spalte 4 aus
body[7, 4]

# Will man *alle* Zeilen oder Spalten auswählen, kann man die Angabe weglassen,
# z.B. alle Spalten der Zeile 7 (achten Sie auf den Beistrich):
body[7, ]
# z.B. alle Zeilen der Spalte 3
body[, 3]

# (4) Ermitteln Sie anhand obiger Syntax den Mittelwert aller Werte der Spalte 2
mean(body[, 2])

# R arbeitet Vektoren-basiert, mann kann also ohne Schleifen Vektoren addieren, multiplizieren etc.:
# Beispiel: Brustumfang (Chest.girth) und Schulterumfang (Shoulder.girth) addieren
# (wir verwenden zwei Arten, um auf Spalten zuzugreifen):
body$Chest.girth + body$Shoulder.girth
body[, 11] + body[, 10]  # Tipp: wenn Sie mit colnames(body) nachsehen, dann sehen Sie den Index der Spalten

# (5) Machen Sie aus der Gewicht in Pfund um (*2.2 multiplizieren) und weisen Sie das
#     Ergebnis der neuen Variable body$Weight_lbs zu.
body$Weight_lbs = body$Weight*2.2

# Um selber einen Vektor zu erstellen gibt es unterschiedliche Möglichkeiten:
# a) Zahlen miteinander verbinden mit c():
c(1, 3, 8)
# Beispiel:
mean(c(1,3,8))

# b) falls Sie die Integerliste von z.B. 1-20 haben wollen:
1:20
# rückwärts geht auch
20:1

# c) Eine Sequenz erstellen, z.B. 30 Werte zwischen 0 und 1:
seq(0, 1, length.out = 30)
# oder Schrittgröße angeben, z.B. Werte zwischen 0 und 100, Schrittweite 3
seq(0, 100, 3)

# (6) Wählen Sie alle Spalten der Zeilen 4, 7, 8 aus dem body-Datensatz aus
body[c(4,7,8),]

# (7) Wählen Sie die Spalten 3 und 4 der Zeilen 15-21 aus dem body-Datensatz aus
body[seq(15:21),c(3,4)]

# Man kann auch Vergleiche mit einem Vektor machen:
body$Height > 175
# Da Boolean-Variable auch als Zahlen behandelt werden können und als 0/1 interpretiert werden,
# kann man Booleanwerte z.B. auch Summieren, um die Anzahl der Datensätze zu erhalten,
# die eine bestimmte Eigenschaft erfüllen:
sum(body$Height > 175)

# Um Gleichheit bei einem Faktor abzufragen, verwenden Sie "==" und einen String
body$Gender == "Female"

# Eine Liste von Bool-Werten kann auch dazu verwendet werden, Zeilen oder Spalten auszuwählen:
# Beispiel: Gewicht jener Personen, die exakt 30 Jahre alt sind:
body$Weight[body$Age==30]
# (Hinweis: hier war kein Beistrich am Ende, da body$Weight nur ein Vektor (eine Spalte) ist, kein Dataframe.)
# Alternativ könnte man auch so auf die Weight-Spalte zugreifen:
body[body$Age==30, "Weight"] # oder
body[body$Age==30, 23]

# (8) Ermitteln Sie Mittelwert und Standardabweichung des Alters für Frauen (body$Gender)
mean(body$Age[body$Gender=="Female"])
sd(body$Age[body$Gender=="Female"])

# (9) Selbes für Männer
mean(body$Age[body$Gender=="Male"])
sd(body$Age[body$Gender=="Male"])

# Logische Verknüpfungen macht man mit einfachen "&" und "|"
TRUE & FALSE
TRUE | FALSE

# (10) Berechnen Sie die Standardabweichung des Gewichts für jene Datensätze,
# auf die Gender=='Male' UND Alter größer 30 Jahre zutrifft.
sd(body$Weight[body$Gender=="Male" & body$Age > 30])
