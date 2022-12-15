-- Mathias Husted, Nourolhoda Bonyani
-- 15.12.2022
-- Einheit 8


-- TEILAUFGABE a)

-- Vor: keine
-- Erg: Die Liste ohne das erste Vorkommen des Elementes a
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove a (x:xs)
  | a == x = xs
  | otherwise = x:(remove a xs) -- Nur rekursiv, falls a noch nicht vorgekommen ist







-- TEILAUFGABE b)
{-
Liste, die zur Grundlage liegt: [1,6,2,7,9,0,4,2,9]
Die Funktionen wurden mit den Funktionen aus sortieren.hs ausgewertet.

Mit Insertsort => [0,1,2,2,4,6,7,9,9]

Funktionsweise: Insertsort nimmt eine Liste als Argument. Für jedes Element wird anschließend
rekursiv das *erste* Element der Liste ausgewertet. Es wird insert aufgerufen, wo der rekursive
Auruf von insertSort angegeben ist. Insert setzt die erste Stelle von der Liste an die richtige Stelle ein.
Dies muss so oft wiederholt werden, bis wir am Ende der Liste angekommen sind.

insert (1 insertSort ([6,2,6,9,0,4,2,9])) usw...
1 wird über Insert an die richtige Stelle eingesetzt (es wird wieder innerhalb von insert rekursiv mit dem Rest der Liste verglichen,
bis die richtige Stelle gefunden wurde) und im Anschluss in die Liste eingesetzt.

Dann wird wieder insertSort für das nächste Element den Prozess wiederholen, usw.

-}

-- Code aus sortieren.hs (weiter kommentiert):
-- Vor.: keine
-- Erg.: Eine Liste, welche die Eingabeliste aufsteigend sortiert hat,
--       ist geliefert.
insertSort :: Ord a => [a] -> [a]
insertSort [] = [] -- Leere Liste braucht nicht sortiert zu werden
insertSort (x:xs) = insert x (insertSort xs) -- Rekursiv insertSort auf xs anwenden, bis die leere Liste erreicht wurde

-- Vor.: Die Eingabeliste ist aufsteigend sortiert.
-- Erg.: Eine Liste, welche das einzelne Element y an der richtigen
--       Stelle der Eingabeliste enthält, ist geliefert.
insert :: Ord a => a -> [a] -> [a] -- Funktion wird von insertSort aufgerufen mit dem ERSTEN Element der Liste sowie die den rekursiven Aufruf für die Restliste
insert y [] = [y] -- Rekursionsanker - wenn Liste leer, muss logischerweise y in eine Liste, wo nur y steht
insert y (x:xs)
     | x < y = x : insert y xs -- 1. Fall: Stelle noch nicht gefunden
     | otherwise = y : x : xs -- 2. Fall: Stelle gefunden - y wird vor x eingesetzt und Rekursion beendet


{-
Zu Mergesort:
Gleiche Liste von oben mit Mergesort sortiert: [0,1,2,2,4,6,7,9,9]

Mergesort spaltet die Liste in der Mitte auf, bis die Elemente alleine stehen. Es entsteht also zuerst:

[0,1,2,2,4],[6,7,9,9] => [0,1,2], [2,4], [6,7], [9,9] usw.....
Diese Liste wird wieder halbiert, bis die Elemente alleine stehen.
Daraufhin werden die Elemente einzeln miteinander verglichen, um dann in eine immer größer werdene Liste einsortiert zu werden.
Das Element aus der linken Liste wird immer mit dem aus der rechten verglichen, wobei immer die längere Liste betrachtet wird.
[0] < [1] => [0,1] (Liste wird wieder größer)
[0,1] & [2,2] => [0,1,2,2], da [2,2] beide (einzeln betrachtet) größer sind als 0 und 1.
Die Liste wird so immer größer, bis eine sortierte Liste rauskommt.
-}

-- Code aus sortieren.hs (weiter kommentiert):
-- Vor.: keine
-- Erg.: Eine Liste, welche die Eingabeliste aufsteigend sortiert hat,
--       ist geliefert.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x] -- Ein Element => ein-elementige Liste
mergeSort xs = merge (mergeSort links) (mergeSort rechts) -- Rekursive Aufruf! Die beiden linken und rechte Listen werden mit merge zusammengefügt
         where (links,rechts) = halbiere xs -- Hilfsfunktion, womit die Liste halbiert wird

-- Vor.: keine
-- Erg.: Zwei Listen ls und rs sind geliefert, für die gilt ls++rs==xs
--       und die Länge beider Listen unterscheidet sich um höchstens 1
halbiere :: Ord a => [a] -> ([a], [a]) -- Halbiere Funktion, es kommt ein Tupel mit zwei Listen raus (links und rechts)
halbiere xs = help [] xs where -- Hilfsfunktion wird auf die leere Liste sowie xs angewendet
    help ys xs
     | length xs - length ys <= 1 = (ys,xs) -- wenn Listen gleich aufgeteilt, gebe Ergebnis aus
     | otherwise = help (ys ++ [head xs]) (tail xs) -- Wieder rekursiv head und tail der Liste

-- Vor.: Die beiden Eingabelisten sind aufsteigend sortiert.
-- Erg.: Eine aufsteigend sortierte Liste ist geliefert, welche genau die
--       Elemente der beiden Eingabelisten enthält.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs -- Bei einer leeren Liste ist das Ergebnis xs
merge [] ys = ys -- -||-
merge (x:xs) (y:ys)
     | x < y = x : merge xs (y:ys) -- Wenn erstes Element der ersten Liste kleiner ist als das erste Element der zweiten (rechten) Liste, führe merge auf den Rest von der ersten Liste sowie die ganze zweite Liste aus
     | otherwise = y : merge (x:xs) ys -- andernfalls wird y an die Rekursion geconst.





{-

-- TEILAUFGABE c)

-- Vor.: keine
-- Erg.: Eine Liste, welche die Eingabeliste aufsteigend sortiert hat,
--       ist geliefert.
triSort :: Ord a => [a] -> [a]
triSort [] = []
triSort [x] = [x]
triSort xs = mergetri (triSort eins) (triSort zwei) (triSort drei)  
         where (eins, zwei, drei) = durchdrei xs

-- Vor.: keine
-- Erg.: Zwei Listen ls und rs sind geliefert, für die gilt ls++rs==xs
--       und die Länge beider Listen unterscheidet sich um höchstens 1
durchdrei :: Ord a => [a] -> ([a], [a], [a])
durchdrei xs = help [] [] xs where
    help ys zs xs
      | otherwise = ([ys], [zs], [xs]) -- KORRIGIEREN
--     | length xs - length ys - length  <= 1 = (ys,xs)
--     | otherwise = help (ys ++ [head xs]) (tail xs)

-- Vor.: Die beiden Eingabelisten sind aufsteigend sortiert.
-- Erg.: Eine aufsteigend sortierte Liste ist geliefert, welche genau die
--       Elemente der beiden Eingabelisten enthält.
mergetri :: Ord a => [a] -> [a] -> [a] -> [a]
mergetri xs [] = xs
mergetri [] ys = ys
mergetri (x:xs) (y:ys)
     | x < y = x : mergetri xs (y:ys)
     | otherwise = y : mergetri (x:xs) ys


-}






-- TEILAUFGABE d)
type Eintrag = (Char, Int)
type Tabelle = [Eintrag]

maxZahl :: Tabelle -> Int
maxZahl = foldr1 max . map snd

maxEintrag :: Tabelle -> Eintrag
maxEintrag [] = (' ', 0)
maxEintrag (x:xs)
  | snd x == maxZahl (x:xs) = x
  | otherwise = maxEintrag xs

selectSort :: Tabelle -> Tabelle
selectSort [] = []
selectSort (x:xs) = maxEintrag (x:xs):selectSort (remove (maxEintrag (x:xs)) (x:xs))
