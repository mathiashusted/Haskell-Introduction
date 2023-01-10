-- Mathias Husted, Nourolhoda Bonyani
-- Aufgabe 2
-- 09.01.2023


module Aufgabe2 where

data Menge a = Menge [a]

-- TEILAUFGABE a)
instance Show a => Show (Menge a) where
  show (Menge []) = "∅"
  show (Menge (x:xs)) = "{" ++ show x ++ ", " ++ (show (Menge xs)) ++ "}"

vonMenge :: Eq a => Menge a -> [a]
vonMenge (Menge []) = []
vonMenge (Menge (x:xs)) = [x] ++ vonMenge (Menge xs)

vonListe :: Eq a => [a] -> Menge a
vonListe [] = Menge []
vonListe [x] = Menge [x]
vonListe (x:xs)
  | x `elem` xs = vonListe xs
  | not(x `elem` xs) = Menge ([x]++(vonMenge(vonListe (xs))))

