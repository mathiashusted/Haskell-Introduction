-- Mathias Husted, Nourolhoda Bonyani
-- 30.11.2022
-- Aufgabe 1

module Aufgabe1 where

import Data.Char

-- TEILAUFGABE a)

ausdruck :: String

ausdruck = reverse(show(2+2 + (if True==True then 13 else 2) + 6))
{- Der Ausdruck ausdruck beinhaltet erst zwei verschachtelte Funktionsaufrufe (reverse auf show)
Im inneren wird ein Int berechntet, nämlich erst 2+2, dann wird 13 addiert, falls der Boolwert True==True ist (was richtig ist)
Anschließend wird die Konstante 6 addiert. Insgesamt ergibt das also 23. Durch die show-Funktion wird das in einen String umgewandelt,
der dann mit reverse umgedreht wird - also wird das Ergebnis zu 32
Konstante = 6
Arithmetischer Ausdruck = Innerhalb der Klammer der show-Funktion
Boolescher Ausdruck = True==True
Bedingter Ausdruck = das If-Statement in der inneren Klammer
Funktionsaufrufe = reverse, show
Ausgabetyp am Ende = String
-}

-- TEILAUFGABE b)
calc :: Integer -> String -> Integer -> Integer -- Definition der Funktion mit Argumenten Integer, String, Integer und Ausgabetyp Integer
calc a op b --Pattern matcing für die Fälle im String
  | op == "plus" = a + b -- +
  | op == "minus" = a - b -- -
  | op == "mal" = a * b -- *
  | op == "hoch" && b >= 0 = a^b -- Exponent
  | op == "hoch" && b < 0 = error"Exponent negativ" -- Ungültiger Exponent
  | otherwise = error"Ungültige Eingabe" -- Alle anderen Eingaben als die vorgegebenen gültigen


-- TEILAUFGABE c)

spanne :: Integer -> Integer -> Integer -> Integer -> Integer -- Vier Eingabewerte des Typs Integer sowie ein Integer als Ausgabewert
spanne a b c d = minimum[a,b,c,d] -- Minimumfunktion aus der Standardbibliothekk

-- TEILAUFGABE d)

alphabet = [ord 'A'..ord 'Z'] -- Erstelle eine Liste mit den Ordinalzahlen aller Großbuchstaben
alphabetReversed = reverse alphabet -- Erstelle dieselbe Liste aller Großbuchstaben mit umgekehrten Indizes.

-- Wir wissen, 'A' hat die Ordinalzahl 65, Z die Ordinalzahl 90
-- alphabet indiziert die Ordinalzahlen und matcht 65 -> 0.. 90 -> 25

spiegeln :: Char -> Char
spiegeln n
  | ord n >= ord 'A' && ord n <= ord 'Z' = chr(alphabetReversed!!(ord n-ord 'A'))
  -- Hier wird erst verglichen, ob sich die Ordinalzahl von dem eingegebenen Char in der Liste befindet (also ein Großbuchstabe ist)
  -- Anschließend wird der von ord(n) die Ordinalzahl von A abgezogen, sodass wir auf 0 landen und in unserer indizierten Listen suchen können
  -- Nun haben wir einen Index, den wir in die Liste alphabetReversed einsetzen (die Umkehrung von 'A' bis 'Z')
  -- Davon suchen wir den passenden Character dazu, und das ist unser Ausgabewert.
  | otherwise = error"Ungültige Eingabe! Es können nur Großbuchstaben eingegeben werden" -- Error für alle anderen Fälle
