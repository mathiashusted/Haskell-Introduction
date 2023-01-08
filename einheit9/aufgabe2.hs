-- Mathias Husted, Nourolhoda Bonyani
-- 04.01.2023
-- Aufgabe 2
-- Anmerkung: Restliche Aufgaben wurden in der PDF bearbeitet

module Aufgabe2 where

import Data.Char

-- TEILAUFGABE a)
-- Werte zu finden in Testausgaben.txt

a = [x*x | x <- [1..100], even x]
-- Typ: [Integer]
-- Gibt x² aus für 1 bis 100, wobei x gerade sein muss


b = [chr (ord x) | x <- ['a'..'z'], ord x `mod` 2 == 1]
-- Typ: [Char] (aka String)
-- Gibt jeden zweiten Buchstaben des Alphabets aus (beginnend bei a)

c = [x * pi * 2 | x <- [1.0..100.0]]
-- Typ: [Double]
-- Berechnet die Fläche aller Kreis mit einem Radius von 1 bis 100

-- TEILAUFGABE d)
-- Vor: Keine
-- Erg: Gibt an (True/False), ob Element n in der Liste enthalten ist.
zfElem :: Eq a => a -> [a] -> Bool
zfElem n xs = foldl f False xs where
              f a1 z = z == n || a1