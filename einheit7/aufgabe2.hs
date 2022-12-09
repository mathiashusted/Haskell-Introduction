-- Mathias Husted, Nourolhoda Bonyani
-- 07.12.2022
-- Aufgabe 2


module Aufgabe2 where
-- TEILAUFGABE a)

-- (Hilfsfunktion)
-- Vor: keine
-- Erg: Liefert Anzahl negativer Werte in x:xs
low :: [Int] -> Int
low [] = 0
low (x:xs)
  | x < 0 = 1 + low xs -- x ist negativ, addiere 1
  | x >= 0 = low xs
  | otherwise = error"Fehler"


-- Hilfsfunktion
-- Vor: keine
-- Erg: Liefert Anzahl positiver Werte in x:xs
high :: [Int] -> Int
high [] = 0
high (x:xs)
  | x > 0 = 1 + high xs -- x ist positiv, addiere 1
  | x <= 0 = high xs
  | otherwise = error"Fehler"

-- Vor: keine
-- Erg: Liefert ein Tupel mit der Anzahl positiver Werte in x:xs sowie die Anzahl negativer Werte in x:xs
lowhigh :: [Int] -> (Int, Int)
lowhigh [] = (0,0)
lowhigh (x:xs) = (low (x:xs), high (x:xs)) -- Tupel mit lows und highs von x:xs




-- TEILAUFGABE b)

-- HILFSFUNKTION
-- Vor: keine
-- Erg: Liefert Anzahl der Zahlen aus der Liste x:xs, bei denen die Funktion f mit x positiv (> 0) ausgewertet wird
evaluatesPositive :: (Int -> Int) -> [Int] -> Int
evaluatesPositive _ [] = 0
evaluatesPositive f (x:xs)
  | f x > 0 = 1 + evaluatesPositive f xs -- x ist positiv, addiere 1
  | f x <= 0 = evaluatesPositive f xs
  | otherwise = error"Fehler"


-- HILFSFUNKTION
-- Vor: keine
-- Erg: Liefert Anzahl der Zahlen aus der Liste x:xs, bei denen die Funktion f mit x negativ (< 0) ausgewertet wird
evaluatesNegative :: (Int -> Int) -> [Int] -> Int
evaluatesNegative _ [] = 0
evaluatesNegative f (x:xs)
  | f x < 0 = 1 + evaluatesNegative f xs -- x ist negativ, addiere 1
  | f x >= 0 = evaluatesNegative f xs
  | otherwise = error"Fehler"

-- Vor: keine
-- Erg: Liefert ein Tupel, die die Anzahl der positiven Auswertungen der Funktion f unter einer beliebigen Liste ausgibt, und für die negativen Auswertungen analog
lowhighFunc :: (Int -> Int) -> [Int] -> (Int, Int)
lowhighFunc _ [] = (0,0)
lowhighFunc f (x:xs) = (evaluatesNegative f (x:xs), evaluatesPositive f (x:xs))


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
toBools p xs = map p xs