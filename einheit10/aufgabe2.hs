-- Mathias Husted, Nourolhoda Bonyani
-- Aufgabe 2
-- 09.01.2023


module Aufgabe2 where

import Data.List

data Menge a = Menge [a]

-- TEILAUFGABE a)
instance Show a => Show (Menge a) where
  show (Menge []) = "∅"
  show (Menge (x:xs)) = "{" ++ (intercalate ", " (map show (x:xs)) ) ++ "}"



-- TEILAUFGABE b)
menge2Liste :: Eq a => Menge a -> [a]
menge2Liste (Menge []) = []
menge2Liste (Menge (x:xs)) = [x] ++ menge2Liste (Menge xs)

liste2Menge :: Eq a => [a] -> Menge a
liste2Menge [] = Menge []
liste2Menge [x] = Menge [x]
liste2Menge (x:xs)
  | x `elem` xs = liste2Menge xs
  | not(x `elem` xs) = Menge ([x]++(menge2Liste(liste2Menge (xs))))



-- TEILAUFGABE c)
instance Eq a => Eq (Menge a) where
  (==) (Menge []) (Menge []) = True
  (==) (Menge (x:xs)) (Menge (y:ys)) = x == y && xs == ys
  (/=) (Menge (x:xs)) (Menge (y:ys)) = x /= y || xs /= ys


-- TEILAUFGABE d)
-- Funktion wurde vereinigung statt union genannt, um Konflikte mit Data.List zu vermeiden
vereinigung :: Eq a => Menge a -> Menge a -> Menge a
vereinigung (Menge []) (Menge []) = (Menge [])
vereinigung (Menge (x:xs)) (Menge (y:ys)) = (liste2Menge(x:xs ++ y:ys))

schnitt :: Eq a => Menge a -> Menge a -> Menge a
schnitt (Menge []) (Menge []) = (Menge [])
schnitt (Menge []) _ = (Menge [])
schnitt _ (Menge []) = (Menge [])
schnitt (Menge (x:xs)) (Menge (y:ys))
  | x `elem` (y:ys) = vereinigung (Menge [x]) (schnitt (Menge (xs)) (Menge (y:ys)))
  | not(x `elem` (y:ys)) = schnitt (Menge xs) (Menge (y:ys))
   -- schnitt (Menge (x:xs)) (Menge (y:ys)) = (Menge (filter (elem (y:ys)) (x:xs)))
