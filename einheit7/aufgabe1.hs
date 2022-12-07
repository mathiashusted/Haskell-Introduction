-- Mathias Husted, Nourolhoda Bonyani
-- 07.12.2022
-- Aufgabe 1

module Aufgabe1 where

-- TEILAUFGABE a)


-- Vor: Input: Liste von ganzen Zahlen
-- Erg: Alle Elemente der Liste werden aufaddiert, die durch 3 oder 4, aber nicht durch 6 teilbar sind
sumdv :: [Int] -> Int
sumdv [] = 0
sumdv (x:xs)
  | (x `mod` 3 == 0 || x `mod` 4 == 0) && x `mod` 6 /= 0 = x + sumdv (xs)
  | otherwise = sumdv(xs)


-- TEILAUFGABE b)


-- Vor: keine
-- Erg: Den eingegebenen String x:xs, wobei innerhalb von x:xs alle Vorkommen von n durch '*' ersetzt wurden 
zensiert :: Char -> String -> String
zensiert _ "" = ""
zensiert n (x:xs)
  | x == n = '*':(zensiert n xs)
  | otherwise = x:(zensiert n (xs))


-- TEILAUFGABE c)


-- Vor: Der eingegebene Int n muss >= 0 (eine natürliche Zahl) sein
-- Erg: Entfernt die n ersten sowie n letzten Elemente einer Liste, falls die Liste maximal 2*n Elemente enthält. Andernfalls
--      gibt sie die ursprüngliche Liste aus
dropTake :: Int -> [a] -> [a]
dropTake _ [] = []
dropTake n (x:xs)
  | n >= 0 && length (x:xs) >= (2*n) = take n(x:xs) ++ drop (length (x:xs) - n) (x:xs)
  -- Überprüft die Voraussetzung, dann wird die Liste mit den ersten n Elementen sowie die letzten n Elemente ausgegeben
  | n >= 0 && length (x:xs) < (2*n) = (x:xs)
  -- Bedingung ist nicht erfüllt
  | otherwise = error"Fehler"