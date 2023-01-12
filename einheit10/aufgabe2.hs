-- Mathias Husted, Nourolhoda Bonyani
-- Aufgabe 2
-- 09.01.2023


module Aufgabe2 where

import Data.List

data Menge a = Menge [a]

-- TEILAUFGABE a)
instance Show a => Show (Menge a) where -- Instanz Show des Datentyps Menge
  show (Menge []) = "∅"
  show (Menge (x:xs)) = "{" ++ (intercalate ", " (map show (x:xs)) ) ++ "}"



-- TEILAUFGABE b)
-- Hilfsfunktion
-- Vor: Keine
-- Erg: Wandelt Eingegebene Menge in eine interne Liste um
vonMenge :: Eq a => Menge a -> [a]
vonMenge (Menge []) = []
vonMenge (Menge (x:xs)) = [x] ++ vonMenge (Menge xs)

-- Vor: Keine
-- Erg: Wandelt interne Liste in eine Menge um
vonListe :: Eq a => [a] -> Menge a
vonListe [] = Menge []
vonListe [x] = Menge [x]
vonListe (x:xs)
  | x `elem` xs = vonListe xs
  | not(x `elem` xs) = Menge ([x]++(vonMenge(vonListe (xs))))



-- TEILAUFGABE c)
instance Eq a => Eq (Menge a) where -- Instanz Eq des Datentyps Menge
  (==) (Menge []) (Menge []) = True
  (==) (Menge (x:xs)) (Menge (y:ys)) = x == y && xs == ys
  (/=) (Menge (x:xs)) (Menge (y:ys)) = x /= y || xs /= ys


-- TEILAUFGABE d)
-- Funktion wurde vereinigung statt union genannt, um Konflikte mit Data.List zu vermeiden
-- Vor: Keine
-- Erg: Liefert Menge a vereinigt mit Menge b
vereinigung :: Eq a => Menge a -> Menge a -> Menge a
vereinigung (Menge []) (Menge []) = (Menge [])
vereinigung (Menge (x:xs)) (Menge (y:ys)) = (vonListe(x:xs ++ y:ys))

-- Diese Funktion wurde ebenfalls umbenannt
-- Vor: Keine
-- Erg: Liefert den Schnitt zwischen Menge a und Menge b
schnitt :: Eq a => Menge a -> Menge a -> Menge a
schnitt (Menge []) (Menge []) = (Menge [])
schnitt (Menge xs) (Menge ys) = vonListe(filter (`elem` ys) xs)
