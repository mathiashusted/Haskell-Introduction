-- Mathias Husted, Nourolhoda Bonyani
-- 14.12.2022
-- Einheit 8

-- TEILAUFGABE a)

{-
foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
Die Signatur nimmt eine Operation als erstes Argument - dies muss eine Funktion mit zwei
Eingaben sein (z.B. max, mod, +, -) sowie einen Wert, der am "Ende" der Kette stehen muss.
In der innersten Klammer wird dieser Wert noch einmal auf die Liste angewendet, bevor die
Klammern alle geschlossen werden. Als letztes Argument nimmt die Funktion die Liste selbst,
mit den jeweiligen Werten, die zu berechnen sind.
Die foldr Funktion "faltet" eine Liste nach Rechts auf und wendet darauf eine jedes Mal die Operation an.
[1,2,3,4] würde bspw. als 1:(2:(3:(4:[]))) ausgewertet werden. Daraufhin werden die Cons mit
der Operation "ersetzt" und anstelle der leeren Liste (das letzte Element) kommt das
zweite Argument der foldr Funktion.
Beispiel:
-}
-- Vor: keine
-- Erg: Oben beschrieben
exfoldr = foldr (+) 11 [1,2,3,4] -- 1+(2+(3+(4+11))) = 21 (11 ist das "letzte Element")


{-
foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl hat exakt die gleiche Funktionsweise wie foldr, nur von rechts nach links.
anstelle von 1:(2:(3:...)) wird also ((([]:1):2):3):4 ausgewertet.
Beispiel:
-}
-- Vor: keine
-- Erg: Oben beschrieben
exfoldl = foldl (-) 11 [1,2,3,4] -- (((11-1)-2)-3)-4 = 1, da Minus kommutativ


{-
foldr1
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
foldr1 hat dieselbe Funktionsweise wie foldr, nur mit der Ausnahme, dass diese ein
Argument weniger nimmt. Das zweite Argument (mit dem "Wert ans Ende") fällt weg, und
wird durch das *letzte Element der Liste* ersetzt. Die obige Funktion bei foldr
könnte also mit foldr1 ausgedrückt werden als:
-}
-- Vor: keine
-- Erg: Oben beschrieben
exfoldr1 = foldr1 (+) [1,2,3,4,11] -- 1+(2+(3+(4+11))) = 21 (11 wird aus der Liste entnommen und ist nun hier "letztes Element")

{-
foldl1
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
foldl1 funktioniert analog zu foldr1, nur mit den Eigenschaften von foldl
-}
-- Vor: keine
-- Erg: Oben beschrieben
exfoldl1 = foldl1 (-) [1,2,3,4] -- (((11-1)-2)-3)-4 == 1 <- wie bei exfoldl




-- TEILAUFGABE b)

-- Vor: n ist eine natürliche Zahl
-- Erg: Binärdarstellung von n als String ([Char])
dez2bin :: Int -> String
dez2bin 0 = []
dez2bin n
  | n `mod` 2 == 0 = dez2bin (n `div` 2) ++ "0"
  | n `mod` 2 == 1 = dez2bin (n `div` 2) ++ "1"
  | otherwise = error"Fehler"

-- HILFSFUNKTION
-- Vor: Im String befindet sich eine natürliche Zahl
-- Erg: Die Zahl als Liste z.B. 23 = [2,3]
str2list :: String -> [Int]
str2list "" = []
str2list (c:cs) = (read [c] :: Int):(str2list cs)

-- Vor: n ist eine Binärzahl
-- Erg: n in Dezimaldarstellung
bin2dez :: String -> Int
bin2dez [] = 0
bin2dez xs = foldl1 ((+).(2*)) n where
                n = str2list xs



-- TEILAUFGABE c)

-- Z.B. Tabelle n = [('X',2), ('A',3)]
type Eintrag = (Char, Int)
type Tabelle = [Eintrag]

maxEintrag :: Tabelle -> Eintrag
maxEintrag (x:xs) = ('A',foldl (maximum) x xs) {-where
                    sucheEintraege :: Tabelle -> Eintrag
                    sucheEintraege [a] = a
                      | length xs > 0 = max (snd x, )
                    sucheEintraege (x:xs)-}