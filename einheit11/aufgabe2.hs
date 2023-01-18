-- Mathias Husted
-- 18.01.2023
-- Aufgabe 2

module Aufgabe2 where

import Data.List

data BinBaum a = Nil | Knoten a (BinBaum a) (BinBaum a) deriving (Eq, Show, Ord) -- (unechte) Binärbäume

-- TEILAUFGABE a)


-- Die Funktion "einfuegen" wurde kopiert aus Quelltexte/binäresuchbäume.hs
-- Einfügen ----------------------------------------------------------
-- Vor.: Der eingegebene Baum erfüllt die BSB-Eigenschaft.
-- Erg.: Eingegebener Baum, in den das Element x
--       eingefügt ist, ist geliefert. Die Ausgabe erfüllt die
--       BSB-Eigenschaft.
einfuegen :: Ord a => a -> BinBaum a -> BinBaum a
einfuegen x Nil = Knoten x Nil Nil
einfuegen x (Knoten y lub rub)
    | x <= y = Knoten y (einfuegen x lub) rub
    | otherwise = Knoten y lub (einfuegen x rub)



-- Vor: Baum muss aus Strings bestehen
-- Erg: Gibt an, ob ein Char Bestandteil irgendeines Strings im Baum ist
contains :: BinBaum String -> Char -> Bool
contains Nil _ = False
contains (Knoten x rub lub) a = (elem a x) || contains lub a || contains rub a


b = foldr einfuegen Nil ["t", "te", "test", "tes"] -- Testbaum

-- Vor: Baum muss aus Strings bestehen
-- Erg: Gibt den längsten Strings des Baums an
longest :: BinBaum String -> String
longest Nil = ""
longest (Knoten x _ Nil) = x                -- Wenn es kein Element gibt, das weiter rechts ist, muss das aktuelle x das Maximum sein
longest (Knoten x lub rub) = longest (rub)  -- Wenn es rechts weitere Elemente gibt, dann suche dort weiter






-- TEILAUFGABE b)
type Eintrag = (Char,Int)
type Tabelle = [Eintrag]

-- Funktion "histogrammhelp" wurde kopiert aus 08/Quelltexte/kodierung.hs
-- Vor.: keine
-- Erg.: Ein Histogramm des Eingabestrings ist geliefert.
histogrammhelp :: String -> Tabelle
histogrammhelp [] = []
-- sortiere den Char c in das rekursiv berechnete Histogramm vom Rest ein
histogrammhelp (c:cs) = einsortieren c (histogrammhelp cs) where
    einsortieren c [] = [(c,1)] -- c ist noch nicht vorhanden, neu erstellen!
    einsortieren c ((d,i):dis)
      | c == d = (d,i+1) : dis -- c wurde gefunden, Counter erhöhen
      | otherwise = (d,i) : einsortieren c dis -- c wurde noch nicht gefunden

-- Hilfsfunktion: Wir wandeln den Baum in eine normale Liste um, um besser damit arbeiten zu können
-- Vor: Keine
-- Erg: Der Baum als Liste mittels Preorder
baum2List :: Ord a => BinBaum a -> [a]
baum2List Nil = []
baum2List (Knoten x lub rub) = [x] ++ baum2List lub ++ baum2List rub

-- Vor: Keine
-- Erg: Histogramm für alle Blätter eines Baums
histogramm :: BinBaum String -> [(Char, Int)]
histogramm Nil = []
histogramm (Knoten x lub rub) = histogrammhelp (intercalate "" (baum2List (Knoten x lub rub)))

-- chiffre :: Int -> BinBaum String -> BinBaum String






-- TEILAUFGABE c)
foldt :: (a -> b -> b -> b) -> b -> BinBaum a -> b
foldt f e Nil = e
foldt f e (Knoten x lub rub) = f x (foldt f e lub) (foldt f e rub)

{-
Aufgabe: Benenne Gemeinsamkeiten und Unterschiede zur Rechtsfaltung von Listen. Beziehe
hierbei sowohl die Signatur als auch die Funktionsdefinition mit ein.


Signatur von foldt: foldt :: (a -> b -> b -> b) -> b -> BinBaum a -> b
Signatur von foldr: foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

Gemeinsamkeiten:
    - Die beiden Funktionen haben beide dieselbe grundlegende Funktion - sie nehmen die Elemente aus einer Liste (bzw. Baum), ordnen sie "gefaltet" an,
        und wenden zwischen jeder Faltung die Funktion f an.
    - Es wird bei beiden Funktionen ein "Anfangselement" (=e - oft auch neutrales Element benannt) verwendet
    - 

Unterschiede:
    - Bei foldr wird das neutrale Element einmalig an den Anfangspunkt gesetzt (im innersten Term bei foldr), bei foldt wird dieser allerdings bei
        jeder Rekursion erneut eingefügt
    - Bei foldt wird bei jedem Durchlauf die Funktion zwei Mal rekursiv aufgerufen, während foldr nur ein rekursiver Aufruf benötigt (da er nicht zwei separate
        Bäume, sondern nur eine Liste verarbeitet)
    -
-}