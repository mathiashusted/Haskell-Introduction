-- Mathias Husted, Nourolhoda Bonyani
-- Aufgabe 1
-- 09.01.2023

module Aufgabe1 where

-- TEILAUFGABE a)
-- 29 Knuts = 1 Sickel => 17 Sickel = 1 Galleone
type Knuts = Integer; type Sickel = Integer; type Galleonen = Integer;

data ZauberGeld = Preis Knuts Sickel Galleonen deriving (Show, Eq, Ord)

-- TEILAUFGABE b)
-- Ms. Granger will 2 Galleonen abheben
-- 2 Galleonen = 34 Sickel = 986 Knuts (2*17*29)
-- Besitzt jedoch nur 30 Sickel (=870 Knuts) und 123 Knuts (870+123=993)
-- 2 Galleonen sind 986 Knuts, Ms. Granger besitzt 993 Knuts, also kann sie die
-- gefragte Geldmenge abheben.



-- HILFSFUNKTION
-- Vor: Keine
-- Erg: Eingegebener Preis in Knuts
geld2Knuts :: ZauberGeld -> Knuts
geld2Knuts (Preis k s g) = k+(s*29)+(g*493) -- Knutswert: k bleibt, s wird mit 29 multipliziert (29 Knuts für 1 Sickel), g mit 17*29 (493)

-- HILFSFUNKTION
-- Vor: Keine
-- Erg: Eingegebener Preis in Knuts wird wieder als ZauberGeld ausgegeben
knuts2Geld :: Knuts -> ZauberGeld
knuts2Geld n = (Preis (n `mod` 29) ((n `div` 29) `mod` (17)) (n `div` (493)))
-- Hier wird der Preis wieder ausgegeben. n mod 29 ist der Knutswert
-- Bei Sickel müssen wir (n `div` 29) `mod` 17 rechnen, damit wir bei alle Galleonen den Wert wieder auf 0 setzen.
-- Als letztes ist der Galleonenwert n `div` 493 (17*29) - hier brauchen wir keine weitere Modulorechnung, da es die höchstwertige Ziffer ist

-- Diese beiden Hilfsfunktionen sind sinnvoll, weil es am einfachsten ist, Rechenoperationen auf die reine Knutsmenge auszuführen.
-- Wenn wir die Geldmenge erstmal über geld2Knuts im Dezimalsystem haben, können wir also unsere Rechenoperation ausführen, und im Anschluss die ausgegebene
-- Zahl mit knuts2Geld zurück in das richtige Format konvertieren.

-- Vor: Keine
-- Erg: Differenz 
differenz :: ZauberGeld -> ZauberGeld -> ZauberGeld
differenz (Preis k s g) (Preis kk ss gg) = knuts2Geld(geld2Knuts (Preis k s g) - geld2Knuts (Preis kk ss gg))
-- Alternativ hätten die beiden Hilfsfunktionen auch lokal innerhalb von differenz definiert werden können. Da in diesem Programm die
-- Funktionen jedoch mehrmals benötigt werden, habe ich sie global deklariert.




-- TEILAUFGABE c)
-- Vor: Keine
-- Erg: Prüft, ob der Kunde den gewünschten Geldbetrag vom Konto abbuchen kann (True), oder der Kontenstand nicht ausreicht (False)
-- 1. Argument: Aktueller Kontenstand
-- 2. Argument: Gewünschte Abbuchung
kannAbheben :: ZauberGeld -> ZauberGeld -> Bool
kannAbheben (Preis k s g) (Preis kk ss gg) = (geld2Knuts(differenz (Preis k s g) (Preis kk ss gg))) >= 0
-- Wenn die Differenz der Gesamtmenge der Knuts größer/gleich 0 ist, dann kann die Buchung erfolgen



-- TEILAUFGABE d)
-- Vor: Keine
-- Erg: Geldbetrag mit der minimal nötigen Anzahl an Münzen
tausche :: ZauberGeld -> ZauberGeld
tausche (Preis k s g) = knuts2Geld((geld2Knuts (Preis k s g)))
-- Wir benutzen einfach die vorhin definierten Funktionen - knuts2Geld findet automatisch die optimale Sortierung der Münzen