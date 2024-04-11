-- EJ 1:

-- pendiente

-- EJ 2:

    -- ITEM A --
valorAbsoluto :: Float -> Float
valorAbsoluto x | x < 0 = -x
                | otherwise = x

    -- ITEM B --
bisiesto :: Int -> Bool
bisiesto n = n `mod` 4 == 0 && (not (n `mod` 100 == 0) || (n `mod` 400 == 0))

    -- ITEM C --
factorial :: Int -> Int
-- Requiere n>= 0
factorial n | n == 0 = 1
            | otherwise = n*factorial(n-1)

    -- ITEM D --
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = foldl (\ac x -> if (esPrimo x) then (ac + 1) else ac) 0 (listarDivisores x)

esPrimo :: Int -> Bool  -- Muy feo hacerlo así pero bueno
esPrimo x = length (listarDivisores x) < 2

listarDivisores :: Int -> [Int]
listarDivisores x | x < 1 = []
                  | otherwise = auxiliarDivisores x x []

auxiliarDivisores :: Int -> Int -> [Int] -> [Int]  -- No considera al 1.
auxiliarDivisores _ 1 ac = ac
auxiliarDivisores x y ac = if (mod x y == 0) then auxiliarDivisores x (y-1) (y:ac) else auxiliarDivisores x (y-1) ac


-- EJ 3:

    -- ITEM A --
inverso :: Float -> Maybe Float
inverso x | x == 0 = Nothing
          | otherwise = Just (1/x)

    -- ITEM B --
aEntero :: Either Int Bool -> Int  -- consultar si esta bien que le tenga que pasar los inputs con los left and right
aEntero (Left a) = a
aEntero (Right b) | b = 1
                  | otherwise = 0


-- EJ 4:

    -- ITEM A --
limpiar :: String -> String -> String
-- Me quedo con s2 y elimino todos los caracteres que aparezcan en s1
limpiar s1 s2 = filter (\x -> not (pertenece x s1)) s2

pertenece :: (Eq a) => a -> [a] -> Bool
-- o usar elem definida en el preludio
pertenece e = foldr (\x rec -> x == e || rec) False
--pertenece _ [] = False
-- pertenece e (x:xs) = (x == e) || pertenece e xs

    -- ITEM B --
difPromedio :: [Float] -> [Float]
difPromedio l = map (\x -> x - promedio l) l

promedio :: [Float] -> Float
promedio l = sumarLista l / fromIntegral (length l)

sumarLista :: [Float] -> Float
sumarLista = foldr (+) 0

    -- ITEM C --
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:ys) = (x == y) && todosIguales(y:ys)


-- EJ 5:

data AB a = Nil | Bin (AB a) a (AB a)

    -- ITEM A --
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

    -- ITEM B --
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)

    -- ITEM C --
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = (productoAB i) * r * (productoAB d)
