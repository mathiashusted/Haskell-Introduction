-- Mathias Husted, Nourolhoda Bonyani
-- 30.11.2022
-- Aufgabe 2
-- Basierend auf seltsam.hs

module Aufgabe2 where

-- TEILAUFGABE a)
-- seltsam.hs:
a :: Int                -- Datentyp: Int
a = 2^63-1              
b = 2^63-1              -- Datentyp: Integer (wird automatisch zugewiesen, da kein Datentyp spezifiziert)
c = 2.0^63-1            -- Datentyp: Double
f :: Double -> Double   -- Datentyp: Double -> Double
f x = x*x               
g x = x+x               -- Datentyp:: Num a => a -> a

{-
AUSGABEN:
>>> a = 9223372036854775807      Grösstmöglicher Int, a+1 ist negativ
>>> b = 9223372036854775807      Nicht grösstmöglicher Integer - es können noch viel größere Zahlen dargestellt werden. b+1 wäre nicht negativ
>>> c = 9.223372036854776e18     Es gibt hier einen Überlauf, da Doubles nicht so groß sein können. Deswegen wird die Zahl hier mit der
        Exponentialschreibweise approximiert
>>> f a = Error: Couldn't match expected type ‘Double’ with actual type ‘Int’ --- a ist vom Typ Int, aber die Funktion f ist spezifiziert als eine Funktion,
          die ein Double als Input nimmt und ein Double ausgibt
>>> f b = Error: Couldn't match expected type ‘Double’ with actual type ‘Integer’ --- genauso wie a ist b kein Double, sondern in diesem Fall ein Integer
>>> f (b-c) = Error: Couldn't match expected type ‘Integer’ with actual type ‘Double’ --- Obwohl c ein Double ist, kann dieser nicht mit b (Integer) zusammen berechnet
              werden, ausschließlich ein Double als Eingabe erwartet wird. Hierfür müsste b erst in ein Double umgewandelt werden mit der entsprechenden Funktion
>>> g (a-b) = Error: Couldn't match expected type ‘Int’ with actual type ‘Integer’ --- Int und Integer sind für den Rechner zwei völlig verschiedene Datentypen,
              die nicht ohne weiteres für arithmetische Operationen kombiniert werden können. Wie oben, müsste einer der beiden Variablen in den anderen Datentyp
              umgewandelt werden. Welche in was, wäre allerdings egal, da hier eine Typeclass erwartet wird (ist dementsprechend flexibel).
>>> g b = 18446744073709551614 --- b ist vom Typ Integer. Dieser passt in die Typeclass, da ein einziger Datentyp als Argument erwartet wird. g gibt 2*b als Output
>>> g c = 1.8446744073709552e19 --- c ist vom Typ Float. Da es sich um (zu) große Zahl für diesen Datentyp handelt, ist der Ausgabewert ein Double.
          das können wir über den Befehl :t g c herausfinden. Die Typeclass findet also automatisch den passenden Datentyp für die Ausgabe. Doubles stehen 64
          anstatt 32 Bit zur Verfügung, und haben daher eine höhere Maschinengenauigkeit bei Zahlen dieser Größenordnung.
-}

-- TEILAUFGABE b)
-- Input: Natürliche Zahl n
-- Output: Anzahl der Nullen in der Binärdarstellung von n
-- Bsp: 18 -> 3

countZeros :: Int -> Int
countZeros 0 = 0 -- Rekursionsstart
countZeros n
  | n `mod` 2 == 0 = countZeros(n `div` 2) + 1 -- Falls 0 vorkommt, Ausgabewert +1
  | n `mod` 2 == 1 = countZeros(n `div` 2)
  | otherwise = error"Fehler!"
-- Hier wird n schrittweise in Binärdarstellung umgerechnet.
-- Falls eine 0 vorkommt, wird zu dem Ausgabewert + gerechnet, sonst einfach weitergemacht, bis 0 erreicht ist


-- TEILAUFGABE c)
-- Input: Zwei natürliche Zahlen n, k
-- Output: Ziffer an der 2^k-ten Stelle der Binär darstellung von n

-- Zunächst legen wir eine Hilfsfunktion toBinary fest, um uns die
-- Umwandelung in Binär später zu erleichtern
-- Hier wird eine Liste mit den Binärziffern ausgegeben, um jede Ziffer einzeln analysieren zu können
toBinary :: Int -> [Int]
toBinary 0 = [] -- Rekursionsanker: Leere Liste
toBinary n = toBinary(n `div` 2) ++ [n `mod` 2] -- Modulo-Ergebnis zur Liste hinzufügen

kteStelle :: Int -> Int -> Int -- 2 Int als Eingabe, Int als Ausgabe
kteStelle n k = (reverse(toBinary n))!!(k)
-- Der Binärwert von n wird umgedreht, und die k-te Stelle wird abgelesen
-- Da die toBinary Funktion einfach die Ziffern in der Reihenfolge 2^0, ..., 2^k anordnet,
-- können wir hier ganz einfach die k-te Stelle der Liste ablesen 

-- TEILAUFGABE d)
-- Input: Zwei natürliche Zahlen n, m
-- Output: Anzahl verschiedener Stellen von n und m

-- Auch hier legen wir eine Hilfsfunktion fest, um mit m zu rechnen
-- m wird in eine Liste umgewandelt, damit jede einzelne Ziffer
-- abgeglichen werden kann.
mAlsListe :: Int -> [Int] -- Int als Eingabe, Liste von Int als Ausgabe
mAlsListe 0 = []
mAlsListe n = mAlsListe(n `div` 10) ++ [n `mod` 10] -- Jede Stelle von 10^0, ... , 10^n wird zur Liste hinzugefügt

-- Jetzt können wir die verschiedenen Stellen berechnen

verschStellen :: Int -> Int -> Int -- 2 Int als Eingabe, Int als Ausgabe
verschStellen 0 _ = 0 -- Rekursionsstart (n = 0 ist erreicht)
verschStellen n m
  | not((n `mod` 10) `elem` mAlsListe m) = 1 + verschStellen ((n `div` 10)) m -- Wenn 10^n-te Stelle im Modulo vorhanden, addiere 1 zur Ausgabe
  | (n `mod` 10) `elem` mAlsListe m = verschStellen (n `div` 10) m
  | otherwise = error"Ungültige Eingabe"
{-
In dieser Funktion wird zuerst überprüft, ob sich 10^n NICHT
in der Liste mit den Ziffern von m befindet. Ist dies der Fall, sind die Ziffern
unterschiedlich. Darum wird eine 1 zum Ausgabewert addiert und die Funktion nochmal
mit einer Zehnerpotenz weniger ausgeführt.

Befindet sich 10^n in der Liste, wird die Funktion einfach
nochmal ausgeführt mit einer Zehnerpotenz weniger, bis n=0 erreicht ist
-}
