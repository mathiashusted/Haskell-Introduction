-- Mathias Husted
-- 18.01.2023
-- Aufgabe 2

module Aufgabe2 where

import Data.Char
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
histogramm (Knoten x lub rub) = histogrammhelp (intercalate "" (baum2List (Knoten x lub rub))) -- Intercalate hilft hierbei, alle Elemente zu einem String zusammenzufassen.


-- Funktion aus Vorlesung
-- Vor: Keine
-- Erg: Die Funktion f wird auf jedes Element des Baums angewendet
mapT :: (a -> b) -> BinBaum a -> BinBaum b
mapT f Nil = Nil
mapT f (Knoten x l r) = Knoten (f x) (mapT f l) (mapT f r)

-- Funktion wurde kopiert aus 08/Quelltexte/kodierung.hs
-- Vor.: Eingabe-Char ist ein Kleinbuchstabe
-- Erg.: n-te Cäsarkodierung des Eingabechars ist geliefert.
kodiereChar :: Int -> Char -> Char
kodiereChar n c
   | isLower c = chr (ord 'a' + (ord c - ord 'a' + n)`mod`26)
   | otherwise = c

-- Funktion wurde kopiert aus 08/Quelltexte/kodierung.hs
-- Vor.: keine
-- Erg.: String ist geliefert, in dem alle Kleinbuchstaben durch ihre
--       n-te Cäsarkodierung ersetzt sind.
kodiere :: Int -> String -> String
kodiere n cs = map (kodiereChar n) cs

-- Vor: Der Baum besteht aus Strings
-- Erg: Jeder Buchstabe wird um n Stellen im Alphabet verschoben
chiffre :: Int -> BinBaum String -> BinBaum String
chiffre _ Nil = Nil
chiffre n (Knoten x lub rub) = mapT (kodiere n) (Knoten x lub rub)



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
    - Bei beiden Funktionen gibt es die gleiche Folge von Argumenten - Funktion, neutrales Element, Baum(/Liste)

Unterschiede:
    - Bei foldr wird das neutrale Element einmalig an den Anfangspunkt gesetzt (im innersten Term bei foldr), bei foldt wird dieser allerdings bei
        jeder Rekursion erneut eingefügt
    - Bei foldt wird bei jedem Durchlauf die Funktion zwei Mal rekursiv aufgerufen, während foldr nur ein rekursiver Aufruf benötigt (da er nicht zwei separate
        Bäume, sondern nur eine Liste verarbeitet) - dies kann die Laufzeit erheblich verlängern
-}




-- TEILAUFGABE d)

-- Vor: Keine
-- Erg: Bildet die Summe sämtlicher Elemente des Baums
sumTree :: Num a => BinBaum a -> a
sumTree Nil = 0
sumTree (Knoten x lub rub) = (foldt sumHelp 0 lub) + (foldt sumHelp 0 rub) + x where
    sumHelp x l r = x + l + r


-- TEILAUFGABE e)

{-
preOrder funktioniert genauso wie baum2List

Folgendes gilt bei den Traversierungen:
PreOrder:  [x] ++ L_L ++ L_R
InOrder:   L_L ++ [x] ++ L_R
PostOrder: L_L ++ L_R ++ [x]
-}

-- Vor: Keine
-- Erg: Baum sortiert nach preOrder als Liste
preOrder :: Ord a => BinBaum a -> [a]
preOrder Nil = []
preOrder (Knoten x lub rub) = [x] ++ preOrder lub ++ preOrder rub


-- Vor: Keine
-- Erg: Baum sortiert nach inOrder als Liste
inOrder :: Ord a => BinBaum a -> [a]
inOrder Nil = []
inOrder (Knoten x lub rub) = inOrder lub ++ [x] ++ inOrder rub



-- Vor: Keine
-- Erg: Baum sortiert nach postOrder als Liste
postOrder :: Ord a => BinBaum a -> [a]
postOrder Nil = []
postOrder (Knoten x lub rub) = postOrder lub ++ postOrder rub ++ [x]