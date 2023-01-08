-- Mathias Husted, Nourolhoda Bonyani
-- 03.01.2023
-- Aufgabe 1
-- Anmerkung: Restliche Teilaufgaben wurden in der PDF bearbeitet.

module Aufgabe1 where

import Data.Char

-- TEILAUFGABE a)
-- Vor: n ist natürliche Zahl
-- Erg: Liefert die Anzahlen der Nullen der Binärdarstellung von n
anzahlNullen :: Int -> Int
anzahlNullen n = help n 0 where                               -- Wir legen eine Hilfsfunktion an und geben den Startwert der Anzahl an Nullen (0) mit durch
                  help 0 m = m                                -- Rekursionsstart
                  help n m
                    | n `mod` 2 == 0 = help (n `div` 2) (m+1) -- Falls 0 gefunden, addiere 1 zu m (Akkumulator)
                    | n `mod` 2 == 1 = help (n `div` 2) (m)   -- Sonst mache weiter!
                    | otherwise = error"Fehler"               -- Pattern exhaustion


-- TEILAUFGABE b) Funktion vom ersten Übungsblatt
-- Vor: n ist eine natürliche Zahl
-- Erg: f(0) = 0
--      f(1) = 8
--      f(n) = 3*f(n-1)-2*f(n-2)
ersteU :: Int -> Int
ersteU n = help 0 8 n where                                   -- Anfangswerte f(0) und f(1) werden als Argumente durchgegeben
            help a1 a2 0 = a1                                 -- a1 und a2 als Akkumulatoren
            help a1 a2 n = help a2 ((3*a2)-(2*a1)) (n-1)      -- a2 wird als a1 weitergegeben und a2 als den ausgerechneten Funktionswert - n wird um 1 reduziert (geht also bis 0)


-- TEILAUFGABE c)

-- Vor: n ist eine nicht-leere Binärsequenz
-- Erg: Dezimalzahl, welche durch die Binärsequenz repräsentiert ist
bin2dez :: String -> Int
bin2dez xs = help ((length xs)-1) 0 (map (0-48+) (map (ord) xs)) where -- An die Hilfsfunktion geben wir weiter: p als Länge der Liste - 1 (um die Potenz zu ermitteln), n = 0 für die Dezimaldarstellung, sowie die Liste mit den Zahlen als jeweils ein Element
              help _ n [] = n                                          -- Rekursionsanker
              help p n xs
                | xs!!0 == 1 = help (p-1) (n+(2^p)) (drop 1 xs)        -- Erstes Element der Liste ist 1 => Wir erhöhen die Dezimalzahl um 2^p, verringern für das nächste Element die Potenz um 1
                | xs!!0 == 0 = help (p-1) (n) (drop 1 xs)              -- Erstes Element ist 0           => Wir lassen die Dezimalzahl, wie sie ist
                | otherwise = error"Fehler! Keine Binärdarstellung"    -- Pattern exhaustion

