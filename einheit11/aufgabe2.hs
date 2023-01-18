-- Mathias Husted
-- 18.01.2023
-- Aufgabe 2

module Aufgabe2 where

data BinBaum a = Nil | Knoten a (BinBaum a) (BinBaum a) deriving (Eq, Show, Ord) -- (unechte) Binärbäume

-- TEILAUFGABE a)


-- Diese Funktion "einfuegen" wurde kopiert aus binäresuchbäume.hs
-- Einfügen ----------------------------------------------------------
-- Vor.: Der eingegebene Baum erfüllt die BSB-Eigenschaft.
-- Erg.: Eingegebener Baum, in den das Element x
--       eingefügt ist, ist geliefert. Die Ausgabe erfüllt die
--       BSB-Eigenschaft.
einfuegen :: Ord a => a -> BinBaum a -> BinBaum a
einfuegen x Nil = Knoten x Nil Nil
einfuegen x (Knoten y lub rub)
    | x <= y = Knoten y (einfuegen x lub) rub
    | otherwise = Knoten y lub (einfuegen x rub)


bTest = foldr einfuegen Nil [1,2,3,4,1] -- Testbaum

-- Vor: Baum muss aus Strings bestehen
-- Erg: Gibt an, ob ein Char Bestandteil irgendeines Strings im Baum ist
contains :: BinBaum String -> Char -> Bool
contains Nil _ = False
contains (Knoten x rub lub) a = (elem a x) || contains lub a || contains rub a


b = foldr einfuegen Nil ["t", "te", "test", "tes"]
-- Vor: Baum muss aus Strings bestehen
-- Erg: Gibt den längsten Strings des Baums an
longest :: BinBaum String -> String
longest Nil = ""
longest (Knoten x _ Nil) = x                -- Wenn es kein Element gibt, das weiter rechts ist, muss das aktuelle x das Maximum sein
longest (Knoten x lub rub) = longest (rub)  -- Wenn es rechts weitere Elemente gibt, dann suche dort weiter



-- TEILAUFGABE b)
-- histogramm :: BinBaum String -> (Char, Int)
