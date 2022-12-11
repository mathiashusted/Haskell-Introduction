-- Mathias Husted, Nourolhoda Bonyani
-- 07.12.2022
-- Aufgabe 2


module Aufgabe2 where
-- TEILAUFGABE a)

-- Vor: keine
-- Erg: Liefert ein Tupel mit der Anzahl positiver Werte in x:xs sowie die Anzahl negativer Werte in x:xs
lowhigh :: [Int] -> (Int, Int)
lowhigh xs = (length (filter (<0) xs), length (filter (>0) xs))


-- TEILAUFGABE b)

-- Vor: keine
-- Erg: Liefert ein Tupel, die die Anzahl der positiven Auswertungen der Funktion f unter einer beliebigen Liste ausgibt, und für die negativen Auswertungen analog
lowhighFunc :: (Int -> Int) -> [Int] -> (Int, Int)
lowhighFunc _ [] = (0,0)
lowhighFunc f xs = (length(filter (<0) (map f xs)),length(filter (>0) (map f xs)))
-- Gibt ein Tupel aus mit der Liste der Anzahl Male, wo xs jeweils positiv bzw. negativ ausgewertet wurde

-- TEILAUFGABE c)

-- Vor: keine
-- Erg: Die Zahl, dessen Betrag den kleinsten Abstand zur 0 hat.
absmin :: [Float] -> Float
absmin [] = 0
absmin (x:xs)
  | True `elem` (map (<(abs x)) (map abs xs)) = absmin xs -- In diesem Fall: Es gibt ein Element in der Liste, das noch kleiner ist als x - such weiter!
  | not(True `elem` (map (<(abs x)) (map abs xs))) = abs x -- Es gibt kein Element, das noch kleiner als x ist. x muss also das Ergebnis sein!
  | otherwise = error"Fehler"



-- TEILAUFGABE d)

-- Vor: keine
-- Erg: Eine Liste, die alle Elemente der Liste a enthalten, wobei darauf das Prädikat p angewendet wurde und ausschließlich aus booleschen Werten besteht
toBools :: (a -> Bool) -> [a] -> [Bool]
toBools _ [] = []
toBools p xs = map p xs -- Gebe eine Liste aus, die die Funktion p über map auf jedes Element ausgeführt hat