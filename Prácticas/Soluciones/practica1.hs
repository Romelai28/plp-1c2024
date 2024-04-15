-- EJ 1:


-- EJ 2:

    -- ITEM I --
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = (\x y -> f (x, y))

    -- ITEM II --
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = (\(x, y) -> f x y)

    -- ITEM III --
-- Pendiente, consultar


-- EJ 3:

    -- ITEM I --
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr (\x rec -> e == x || rec) False

(++.) :: [a] -> [a] -> [a]
(++.) s1 s2 = foldr (\x rec -> x : rec) s2 s1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x rec -> if p x then x : rec else rec) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x rec -> (f x) : rec) []

    -- ITEM II --
mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún comp = foldr1 (\x rec -> if comp x rec then x else rec)

    -- ITEM III --
sumasParciales :: Num a => [a] -> [a]
sumasParciales l = tail (reverse (foldl (\ac x -> (x + head(ac)) : ac) [0] l))

    -- ITEM IV --
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

    -- ITEM V --
sumaAltSentidoInverso :: Num a => [a] -> a
sumaAltSentidoInverso = foldl (\x y -> y - x) 0


-- EJ 4:

    -- ITEM I --
--permutaciones :: [a] -> [[a]]

    -- ITEM II --
--partes :: [a] -> [[a]]

    -- ITEM III --
--prefijos :: [a] -> [[a]]
--prefijos = foldr (\x rec -> rec : (prefijos (init rec)) ) [[]]
--prefijos = foldl (\ac x -> ((take ((length ac) -1) ac) : ac)) []

-- esto funciona lo de abajo
--prefijos [] = [[]]
--prefijos l = l : (prefijos (init l))


    -- ITEM IV --
--sublistas :: [a] -> [[a]]


-- EJ 5:


-- "Tina gracias"

prueba :: [[a]] -> [[a]]

--prueba l =  (map reverse . reverse) l
prueba = reverse . map reverse

-- reverse (map reverse ["anit","saicarg"])

-- prueba l = reverse (map reverse l)

-- (reverse . map reverse) ["saicarg","anit"]


-- EJ 8:

-- borrar:
sumaLoca :: Num a => a -> a -> a
sumaLoca a b= a+b 

    -- ITEM I --
mapPares :: (a->b->c)->[(a, b)]->[c]
--mapPares f = map (\(x, y) ->  f x y)
--mapPares = map (uncurry' f)
mapPares = map . uncurry'

    -- ITEM II --
--armarPares :: [a] -> [b] -> [(a, b)]


espejar (Left x) = Right x
espejar (Right x) = Left x


append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys


ponerAlFinal :: a -> [a] -> [a]
ponerAlFinal x = foldr (:) (x:[])
