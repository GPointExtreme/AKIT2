# AKIT2 SS17, Hauptklausur, 23.6.2017 (leicht modifiziert)
library(car)
library(effects)
library(lmtest)
library(psych)
library(pwr)
library(akit2)

df <- read.csv('heart-child.csv')
# Nur vollständige Datensätze
df = df[complete.cases(df),]

# Eine Studie untersucht die Herzwerte von ProbandInnen und zielt dabei auf die
# Frage ab, ob & wie eine Kinderkrankheit die Herzwerte beeinflusst.
# Die Personen sind "self-selected", soll heißen, ob jemand eine Kinderkrankheit hatte
# oder nicht, wurde der Person _nicht_ im Rahmen des Experiments aufgezwungen :o) 
#
# Der Datensatz enthält:
# - heart ... Herzwerte (Loveen's heart index); je höher, desto besser
# - age ... Alter (Jahre)
# - weight ... Gewicht (kg)
# - sex ... männlich/weiblich
# - child.illness ... hatte die Kinderkrankheit ja/nein
#

# Schritt 1: Machen Sie eine Power-Analyse, um die Anzahl der notwendigen ProbandInnen zu ermitteln.
#            Sie wollen min. Effekte der Größe Cohens f²=0.04, mit Power=80% und Signifikanz-Niveau=5% messen.
#            Nehmen Sie dann mittels df[1:___,] nur so viele Einträge aus dem Datensatz,
#            wie die Power-Analyse ergibt.
#
#            Falls Sie diesen Schritt auslassen wollen:
#            Verwenden Sie 240 ProbandInnen.

# Schritt 2: (Imputation [nicht relevant])

# Schritt 3: Erstellen Sie ein Modell mit allen Variablen und ohne Interaktionen
#            Führen Sie eine erste Analyse & Interpretation durch.

# Schritt 4: Hat die Kinderkrankheit unterschiedlich starke Auswirkung auf Männer und Frauen?
#            Erstellen Sie ein passendes Modell, interpretieren Sie das neue Modell und 
#            vergleichen Sie das neue Modell zum Modell aus Schritt 3.
#
#            Tipp: qnf Zbqryy oraögvtg nyfb rvara Vagrenxgvbafgrez "frk:puvyq.vyyarff"
#                  (fvrur qnmh nhpu Ivqrb nhs Zbbqyr)
#            (Entschüsseln auf rot13.com)

# Schritt 5: Conclusio:
#            Welche Auswirkungen hat die Kinderkrankheit?
#            Ist der Effekt relevant?
#            Welche "confounding"/"versteckte" Eigenschaften können das Resultat verfälschen?
#            Welche Empfehlung geben Sie für die nächste Studie ab? (soll nochmal selbe Fragen analysieren)
