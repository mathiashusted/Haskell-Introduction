-- Mathias Husted, Nourolhoda Bonyani
-- Aufgabe 2
-- 09.01.2023


module Aufgabe2 where

data Menge a = Menge [a]

-- TEILAUFGABE a)
instance Show a => Show (Menge a) where
  show (x:xs) = "{" ++ (show x)