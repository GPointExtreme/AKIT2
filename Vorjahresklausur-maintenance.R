# AKIT2 SS17, 2. Nachklausur (3. Termin), 7.12.2017
library(ggplot2)
library(arm)
library(car)
library(coin)
library(corrplot)
library(dplyr)
library(effects)
library(lme4)
library(lmtest)
library(psych)
library(pwr)
library(ROCR)
library(runjags)
library(coda)
library(VIM)
rjags::load.module("glm")
source('utilities.R')
library(akit2)

df <- read.csv('C:\\Users\\Dominik\\Downloads\\maintenance.csv')

# Wir entwickeln ein Smart-Service f�r "predictive maintenance", d.h. wir wollen
# absch�tzen, wie wahrscheinlich es ist, ob in den n�chsten Wochen ein Fehler beim
# Produkt auftritt. Das Produkt ist ein professioneller Gro�blatt-Plotter (A0).

# Unsere Datens�tze enthalten Aufzeichnungen aus der Vergangenheit: Daten eines jeden Plotters und
# ob er in den folgenden 4 Wochen mit einem Fehler ausgefallen ist oder nicht.
# Jeder Datensatz enth�lt:
# - stunden ... Betriebsstunden des Plotters
# - blaetter ... Anzahl Bl�tter, die geplottet wurden
# - vibration ... Vibration w�hrend des Plottens
# - strom ... Stromaufnahme w�hrend des Plottens
# - eindruck1..eindruck5 ... Eindruck der Technikerin/des Technikers bei letzter Wartung (Likert-Skala)
# - fehler ... 0/1 - hatte einen Fehler im folgenden Beobachatungszeitraum

# Hinweis: Stunden und Blätteranzahl korrellieren stark. Es ist daher wohl kontraproduktiv,
#          beide Variable in ein einziges Modell aufzunehmen.

# Schritt 0: Erzeugen Sie eine weitere Variable: Blätter pro Stunde, welches Sie in die Modelle aufnehmen.

# Schritt 1: Erstellen Sie aus der Likert-Skala den passenden latenten Faktor
#            Ist die Eindruck-Likert-Skala gut geeicht oder sollen Änderungen vorgenommen werden?
#
#            Falls Sie diesen Schritt auslassen wollen:
#            Verwenden Sie den Mittelwert aus eindruck1..eindruck5 als Faktor

# Schritt 2: Erstellen Sie zwei Vorhersagemodelle, ob ein Fehler bevorsteht:
#            Einmal mit Stunden, einmal mit Blätteranzahl; jeweils plus alle anderen Variablen
#            Verifizieren & interpretieren Sie die Modelle.

# Schritt 3: Vergleichen Sie beide Modelle
#            Welches ist aussagekräftiger? Was schließen Sie aus Ihrem Vergleichs-Ergebnis?
#            Können Sie eine Aussage treffen, ob Stunden oder Blätter die bessere "Ursache" ist? Warum?
