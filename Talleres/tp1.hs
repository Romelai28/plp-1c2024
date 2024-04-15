import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> a) -> (a -> Dirección -> a) -> (a -> a) -> Personaje -> a
foldPersonaje cPersonaje cMueve cMuere fulano = case fulano of
    Personaje pos nom -> cPersonaje pos nom
    Mueve per dir -> cMueve (rec per) dir
    Muere per -> cMuere (rec per)
  where rec = foldPersonaje cPersonaje cMueve cMuere

foldObjeto :: (Posición -> String -> a) -> (a -> Personaje -> a) -> (a -> a) -> Objeto -> a
foldObjeto cObjeto cTomado cEsDestruido cosa = case cosa of
    Objeto pos nom -> cObjeto pos nom
    Tomado obj per -> cTomado (rec obj) per
    EsDestruido obj -> cEsDestruido (rec obj)
  where rec = foldObjeto cObjeto cTomado cEsDestruido

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje const siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (flip const) const id
-- nombre_objeto = foldObjeto (flip(const)) const id ---- BORRAR VIEJO

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
--objetos_en u = [ x | Right x <- u ]
objetos_en = map objeto_de . filter es_un_objeto
-- objetos_en = foldr (\x rec -> if es_un_objeto x then objeto_de x : rec else rec) []  -- verisión Facu, capaz es mejor para la demostración

personajes_en :: Universo -> [Personaje]
-- personajes_en u = [ x | Left x <- u ]
personajes_en = map personaje_de . filter es_un_personaje
-- personajes_en = foldr (\x rec -> if es_un_personaje x then personaje_de x : rec else rec) []  -- verisión Facu, capaz es mejor para la demostración

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]  -- Recibe el nombre de un personaje y un universo, devuelve la lista de objetos que le pertenecen
objetos_en_posesión_de nom_per u = filter (en_posesión_de nom_per) (objetos_en u)
-- objetos_en_posesión_de nom_per u =  filter (\x -> en_posesión_de nom_per x) (objetos_en u)  ---- BORRAR VIEJO

{-Ejercicio 5-}

-- Asume que hay al menos un objeto
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano p u = foldr1 (\x rec -> if objeto_libre x && distancia (Right x) (Left p) < distancia (Right rec) (Left p) then x else rec) (objetos_en u)

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = cant_elem_que_cumplen es_una_gema (objetos_en_posesión_de "Thanos" u) == 6 -- ¿ó >=6?

-- Dada una condición y una lista, devuelve la cantidad de elementos de la lista que cumplen esa condición.
cant_elem_que_cumplen :: (a->Bool) -> [a] -> Int 
cant_elem_que_cumplen cond = length . filter cond

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool  --Falta testear!!!
podemos_ganarle_a_thanos u = not (tiene_thanos_todas_las_gemas u) && (thor_win || wanda_vision_win)
  where
    thor_win = está_el_personaje "Thor" u && está_el_objeto "Stormbreaker" u
    wanda_vision_win = está_el_personaje "Wanda" u && está_el_personaje "Visión" u && en_posesión_de "Visión" (objeto_de_nombre "Gema de la mente" u)


{- Tests -}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ 
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
thor = Personaje (4,6) "Thor"
thanos = Personaje (6,8) "Thanos"
wanda = Personaje (23,7) "Wanda"
vision = Personaje (23,8) "Visión"
gemaPoder = Objeto (23, 8) "Gema del poder"
gemaMente = Objeto (27, 8) "Gema de la mente"
gemaRealidad = Objeto (22, 45) "Gema de la realidad"
gemaTiempo = Objeto (25, 78) "Gema del tiempo"
gemaEspacio = Objeto (50, 86) "Gema del espacio"
gemaAlma = Objeto (2, 81) "Gema del alma"
gemaPoder2 = Tomado (Objeto (3, 18) "Gema del poder") thanos
gemaMente2 = Tomado (Objeto (33, 85) "Gema de la mente") thanos
gemaRealidad2 = Tomado (Objeto (14, 48) "Gema de la realidad") thanos
gemaTiempo2 = Tomado (Objeto (34, 78) "Gema del tiempo") thanos
gemaEspacio2 = Tomado (Objeto (75, 24) "Gema del espacio") thanos
gemaAlma2 = Tomado (Objeto (3, 81) "Gema del alma") thanos
stormbreaker = Objeto (56, 45) "Stormbreaker"
stormbreaker2 = Tomado (Objeto (56, 45) "Stormbreaker") thor
gemaMente3 = Tomado (Objeto (33, 85) "Gema de la mente") vision
personajeQueSeMueve = Mueve (Mueve (Mueve (Personaje (2,3) "Me Muevo") Norte) Sur) Norte

universo_con_thanos_sin_gemas = [Left thanos, Left wanda, Right gemaPoder2, Right gemaMente2, Right gemaRealidad2, Right gemaTiempo2, Right gemaEspacio2, Right gemaAlma, Left thor, Right mjölnir]

universo_thanos_todas_las_gemas = [Left thanos, Left wanda, Right gemaPoder2, Right gemaMente2, Right gemaRealidad2, Right gemaTiempo2, Right gemaEspacio2, Right gemaAlma2, Left thor, Right mjölnir]

universo_ganamos_a_thanos_con_vision = [Left thanos, Left wanda, Left vision, Right gemaPoder2, Right gemaMente3, Right gemaRealidad2, Right gemaTiempo2, Right gemaEspacio2, Right gemaAlma2, Left thor, Right mjölnir]

universo_ganamos_a_thanos_con_thor = [Left thanos, Left wanda, Right gemaPoder, Right gemaMente2, Right gemaRealidad2, Right gemaTiempo2, Right gemaEspacio2, Right gemaAlma2, Left thor, Right mjölnir, Right stormbreaker]

universo_ganamos_a_thanos_con_thor_2 = [Left thanos, Left wanda, Right gemaPoder, Right gemaMente2, Right gemaRealidad2, Right gemaTiempo2, Right gemaEspacio2, Right gemaAlma2, Left thor, Right mjölnir, Right stormbreaker2]

universo_sin_thanos = universo_con [phil] [mjölnir]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere(Mueve phil Norte))    -- Caso de test 3 - expresion a testear
    ~=? 2                                                                    -- Caso de test 3 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ,
  posición_personaje thanos       -- Caso de test 1 - expresión a testear
    ~=? (6,8)                   -- Caso de test 1 - resultado esperado
  ,
  posición_personaje vision      -- Caso de test 1 - expresión a testear
    ~=? (23,8)                   -- Caso de test 1 - resultado esperado
  ,
  posición_personaje thor       -- Caso de test 1 - expresión a testear
    ~=? (4,6)                   -- Caso de test 1 - resultado esperado
  ,
  posición_personaje personajeQueSeMueve       -- Caso de test 1 - expresión a testear
    ~=? (2,4)                                   -- Caso de test 1 - resultado esperado
  ,
  nombre_objeto stormbreaker            -- Caso de test 1 - expresión a testear
    ~=? "Stormbreaker"                  -- Caso de test 1 - resultado esperado
  ,
  nombre_objeto gemaMente3           -- Caso de test 1 - expresión a testear
    ~=? "Gema de la mente"                  -- Caso de test 1 - resultado esperado
  ,
  nombre_objeto gemaMente2           -- Caso de test 1 - expresión a testear
    ~=? "Gema de la mente"                  -- Caso de test 1 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ,
  objetos_en universo_con_thanos_sin_gemas       -- Caso de test 1 - expresión a testear
    ~=? [gemaPoder2, gemaMente2, gemaRealidad2, gemaTiempo2, gemaEspacio2, gemaAlma, mjölnir]            -- Caso de test 1 - resultado esperado
  ,
  objetos_en universo_sin_thanos                                  -- Caso de test 1 - expresión a testear
    ~=? [mjölnir]                                                 -- Caso de test 1 - resultado esperado
  ,
  personajes_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ,
  personajes_en universo_con_thanos_sin_gemas       -- Caso de test 1 - expresión a testear
    ~=? [thanos, wanda, thor]            -- Caso de test 1 - resultado esperado
  ,
  personajes_en universo_sin_thanos                                  -- Caso de test 1 - expresión a testear
    ~=? [phil]                                                 -- Caso de test 1 - resultado esperado
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? []                             -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Thanos" universo_con_thanos_sin_gemas       -- Caso de test 1 - expresión a testear
    ~=? [gemaPoder2, gemaMente2, gemaRealidad2, gemaTiempo2, gemaEspacio2]       -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Visión" universo_ganamos_a_thanos_con_vision       -- Caso de test 1 - expresión a testear
    ~=? [gemaMente3]       -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Thor" universo_ganamos_a_thanos_con_thor       -- Caso de test 1 - expresión a testear
    ~=? []       -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Thor" universo_ganamos_a_thanos_con_thor_2       -- Caso de test 1 - expresión a testear
    ~=? [stormbreaker2]       -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Thanos" universo_thanos_todas_las_gemas       -- Caso de test 1 - expresión a testear
    ~=? [gemaPoder2, gemaMente2, gemaRealidad2, gemaTiempo2, gemaEspacio2, gemaAlma2]       -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ,
  objeto_libre_mas_cercano vision universo_ganamos_a_thanos_con_vision        -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ,
  objeto_libre_mas_cercano wanda [Right gemaPoder, Right gemaMente]       -- Caso de test 1 - expresión a testear
    ~=? gemaPoder                                     -- Caso de test 1 - resultado esperado
  ,
  objeto_libre_mas_cercano thor universo_ganamos_a_thanos_con_thor       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                      -- Caso de test 1 - resultado esperado
  ,
  objeto_libre_mas_cercano thanos universo_con_thanos_sin_gemas       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                      -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ,
  tiene_thanos_todas_las_gemas universo_ganamos_a_thanos_con_vision       -- Caso de test 1 - expresión a testear
    ~=? False   
  ,
  tiene_thanos_todas_las_gemas universo_ganamos_a_thanos_con_thor       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ,
  tiene_thanos_todas_las_gemas universo_thanos_todas_las_gemas       -- Caso de test 1 - expresión a testear
    ~=? True                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ,
  podemos_ganarle_a_thanos universo_ganamos_a_thanos_con_thor         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ,
  podemos_ganarle_a_thanos universo_ganamos_a_thanos_con_vision         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ,
  podemos_ganarle_a_thanos universo_ganamos_a_thanos_con_thor_2         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ,
  podemos_ganarle_a_thanos universo_thanos_todas_las_gemas         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ,
  podemos_ganarle_a_thanos  universo_con_thanos_sin_gemas         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ]