module MisFunciones where

-- EJERCICIO 1 (1ERA FORMA)

{-
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas relaciones
  | not (relacionesIguales relaciones) && compara (head relaciones) (tail relaciones) && not (relacionesRepetidas relaciones) = False
  | otherwise = True
-}-- 2da forma
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas relaciones = not (relacionesIguales relaciones) && compara (head relaciones) (tail relaciones) && not (relacionesRepetidas relaciones)

----------
relacionesIguales :: [(String, String)] -> Bool -- De una dupla [("p1","p1")] -> verifica si una dupla son iguales!!
relacionesIguales [] = False -- osea no hay una dupla igual
relacionesIguales rel
  | fst (head rel) /= snd (head rel) = relacionesIguales (tail rel)
  | otherwise = True -- hay una dupla igua igual

------------------------
relacionesInversas :: (String, String) -> [(String, String)] -> Bool -- De las dos primeras duplas [("p1","p2"),("p2","p1")] -> True , verifica si una relacion es Inversa de la otra! -- cambio!!!
relacionesInversas elem red = fst (elem) == snd (head red) && snd (elem) == fst (head red)

compara :: (String, String) -> [(String, String)] -> Bool
compara _ [] = True -- compara el elemento con el resto de la lista si no hay nadie devuelve True.
compara elemento lista
  | relacionesInversas elemento lista == False = compara elemento (tail lista)
  | relacionesInversas elemento lista = True
  | otherwise = compara (head lista) (tail lista)

-------------------------
relacionesRepetidas :: [(String, String)] -> Bool -- Verifica si en una lista hay relaciones repetidas
relacionesRepetidas [] = False
relacionesRepetidas rep
  | (pertenece (head rep) (tail rep) == False) = relacionesRepetidas (tail rep)
  | otherwise = True

pertenece :: (Eq t) => t -> [t] -> Bool -- Verifica si un elemento pertenece a la lista
pertenece _ [] = False
pertenece x (y : xs)
  | x == y = True
  | otherwise = pertenece x xs

{-
relacionesValidas2 [("p1","p2"),("p3","p4"),("p5","p6"),("p7","p8"),("p2","p1")] -> False
relacionesValidas2 [("p1","p2"),("p3","p4"),("p2","p1"),("p8","p9")] -> False
relacionesValidas2 [("p1","p2"),("p3","p4"),("p5","p6"),("p7","p8"),("p1","p2")] -> False -- ("p1","p2") Bien -----
relacionesValidas2 [("p1","p2"),("p3","p4"),("p5","p5"),("p7","p8"),("p2","p9")] -> False -- ("p5","p5") Bien
relacionesValidas2 [("p1","p2"),("p3","p4"),("p4","p5"),("p4","p3"),("p2","p9")] -> False -- ("p4","p3") bien -----
---
relacionesValidas2 [("p1","p2"),("p3","p4"),("p2","p5"),("p9","p3"),("p2","p9")] -> True --bien
relacionesValidas2 [("p1","p2"),("p3","p4"),("p4","p5"),("p1","p38"),("p2","p9")] -> True --bien
relacionesValidas2 [("p1","p2"),("p3","p4")] -> True --bien
relacionesValidas2 [("p1","p2"),("p3","p4"),("p4","p5"),("p4","p9"),("p2","p9"),("p3","p42"),("p4","p51"),("p4","p95"),("p2","p93")] -> True --bien
-}

-- EJERCICIO 1 (2DA FORMA) --Con Pattern Matching
-- usando: relaciones = ((x,y):rs)

-- relacionesValidas2 :: [(String, String)] -> Bool
-- relacionesValidas2 rel = not (tuplasRepetidas rel) && not (componentesIguales rel)

relacionesValidas2 :: [(String, String)] -> Bool
relacionesValidas2 rel
  | tuplasRepetidas rel || componentesIguales rel = False
  | otherwise = True

tuplasRepetidas :: [(String, String)] -> Bool
tuplasRepetidas [] = False
tuplasRepetidas [(_, _)] = False
tuplasRepetidas ((x, y) : xs) = (pertenece (x, y) xs || pertenece (y, x) xs) || tuplasRepetidas xs

{-
tuplasRepetidas :: [(String, String)] -> Bool -- me dice si una tupla esta repetida
tuplasRepetidas [] = False -- ninguna tupla esta repetida
tuplasRepetidas [(_, _)] = False
tuplasRepetidas ((x, y) : (z, k) : res)
  | x == k && y == z = True
  | (x /= k && y /= z) || (x /= k && y == z) || (x == k && y /= z) = tuplasRepetidas ((x, y) : res)
  | otherwise = tuplasRepetidas ((z, k) : res)
-}

componentesIguales :: [(String, String)] -> Bool -- Me dice si los dos componentes de una tupla son iguales
componentesIguales [] = False
componentesIguales ((x, y) : res)
  | x == y = True
  | otherwise = componentesIguales res

---------------------------------------------------------------------------------------------------------------------
-- PROBLEMA 2

personas2 :: [(String, String)] -> [String]
personas2 [] = []
personas2 ((x, y) : res) = x : y : personas2 res

quitarRepetidos2 :: [String] -> [String]
quitarRepetidos2 [] = []
quitarRepetidos2 (x : xs) = x : quitarRepetidos2 (quitarTodos2 x xs)

quitarTodos2 :: (Eq t) => t -> [t] -> [t]
quitarTodos2 _ [] = []
quitarTodos2 x (y : xs)
  | x == y = quitarTodos2 x xs
  | otherwise = y : quitarTodos2 x xs

{-
personas :: [(String, String)] -> [String]
personas [] = []
-- personas ((x, y) : res) = quitarRepetidos ((x, y) : res) ++ personas res
quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos [x] = [x]
quitarRepetidos (x : y : xs)
  | x /= y = x : quitarRepetidos (quitarTodos x xs)
  | x == y = quitarRepetidos xs
  | otherwise = quitarRepetidos (y : xs)
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y : xs)
  | x == y = quitarTodos x xs
  | otherwise = y : quitarTodos x xs
-}
----------------------------------------------------------------------------------------------------------------
-- PROBLEMA 3

amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe x ((y, z) : rs)
  | x == y = z : amigosDe x rs
  | x == z = y : amigosDe x rs
  | otherwise = amigosDe x rs

----------------------------------------------------------------------------------------------------------------
-- PROBLEMA 4
{-
personaConMasAmigos :: [(String, String)] -> String -- Nos dice la persona que tiene mas amigos
personaConMasAmigos [] = []
personaConMasAmigos rel = maximo (head relaciones2) (contarAmigosDe head (relaciones2 rel) rel : contarAmigosDe (head (tail (relaciones2 rel))) rel)
-}

-- Forma de pensarlo:
--  personaConMasAmigos rel = fst (maximo (cantidadDeAmigos (personas rel) rel))

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos rel = fst (maximo2 (cantidadDeAmigos2 (personas2 rel) rel))

maximo2 :: [(String, Integer)] -> (String, Integer)
maximo2 [(x, x1)] = (x, x1)
maximo2 ((x, x1) : (y, y1) : rs)
  | x1 >= y1 = maximo2 ((x, x1) : rs)
  | otherwise = maximo2 ((y, y1) : rs)

cantidadDeAmigos2 :: [String] -> [(String, String)] -> [(String, Integer)]
cantidadDeAmigos2 [] _ = []
cantidadDeAmigos2 [x] rel = [(x, longitud (amigosDe x rel))]
cantidadDeAmigos2 (x : y : xs) rel = (x, longitud (amigosDe x rel)) : cantidadDeAmigos2 (y : xs) rel

maximo :: (String, Integer) -> [(String, Integer)] -> (String, Integer) -- Nos da el que tiene mas amigos : (p1,3) > (p2,1)
maximo a [] = a
maximo (x, x1) ((y, y1) : rs)
  | x1 >= y1 = maximo (x, x1) rs
  | otherwise = maximo (y, y1) rs

longitud :: (Eq t) => [t] -> Integer -- Nos da el numero de elementos de una lista
longitud [] = 0
longitud xs = 1 + longitud (tail xs)

contarAmigosDe :: String -> [(String, String)] -> (String, Integer) -- Nos da cuantos amigos tiene una persona (p1,#)
contarAmigosDe x rel = (x, longitud (amigosDe x rel))

-- lista :: [(String, String)] -> [(String, Integer)] -- Me da una lista de las personas con su cantidad de amigos cada uno
-- lista [] = []
-- lista ((x, y) : personas2) = contarAmigosDe x ((x, y) : rel) : (lista contarAmigosDe y ((x, y) : rel))

----------------------------------------------------------------
{-
-- Problema personaConMasAmigos
personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos rs = maximo listaPersonas cantAmigosPersonas
                where
                    listaPersonas      = personas rs
                    cantAmigosPersonas = cantidadDeAmigos listaPersonas rs
cantidadDeAmigos :: [String] -> [(String,String)] -> [Int]
cantidadDeAmigos [] _ = []
cantidadDeAmigos (p:ps) rs = (cantidadDeAmigosDe p rs) : (cantidadDeAmigos ps rs)
maximo :: [String] -> [Int] -> String
maximo [p] _ = p
maximo (p0:p1:ps) (c0:c1:cs)  | c0 > c1   = maximo (p0:ps) (c0:cs)
                              | otherwise = maximo (p1:ps) (c1:cs)
cantidadDeAmigosDe :: String -> [(String,String)] -> Int
cantidadDeAmigosDe p rs = length (amigosDe p rs)
-}
