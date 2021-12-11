-- Adrián Turiel Charro  -- Programación Declarativa -- 2021 --

data Rel a = R [ (a,a) ] deriving (Read, Show)

-- Relacion del enunciado
r1:: Rel Char
r1 =  R [('D','8'),('E','B'),('C','B'),('E','C'),('8','D')]

-- Relacion del enunciado
r2:: (Num a) => Rel [a]
r2 = (R [([1, 2], []),([2, 2], [3, 3, 1]),([1, 3], [0]),([4], [4])])

-- Relacion de equivalencia
r3:: Rel Char
r3 =  R [('E','B'),('B','E'), ('E','E'), ('B','B')]

-- Relacion de equivalencia de otro conjunto distinto
r4:: (Num a) => Rel [a]
r4 = (R [([1, 2], []),([], [1, 2]),([1, 2], [1, 2]),([], [])])

-- Relacion que no es de equivalencia
r5:: (Num a) => Rel [a]
r5 = (R [([1, 2], []),([1, 3], [0]),([2, -2], [3, 3, 1]),([4], [5])])

-- Relacion similar a r1
r6:: Rel Char
r6 =  R [('C','B'),('E','C'),('D','8'),('E','B'),('8','B')]


--EJERCICIO 1	-> True si r es una relacion, False en caso contrario.
esRelacion :: Eq b => Rel b -> Bool
esRelacion (R vlist) = not (hay_repetidos vlist)

-- aux -> Devuelve True si hay elementos repetidos en el conjunto de pares
hay_repetidos :: (Eq a, Eq b) => [(a, b)] -> Bool
hay_repetidos vlist = any (\(v1,v2) -> length (filter (==(v1,v2)) vlist) > 1) vlist

--EJERCICIO 2
instance Eq a => Eq (Rel a) where
 (R v1) == (R v2) = (length v1 == length v1) && (and $ map (\x -> elem x v1) v2)
 (R v1) /= (R v2) = not ((R v1) == (R v2))

--EJERCICIO 3
--fun 1	-> conjunto dominio de la relacion r.
dominio :: Eq a => Rel a -> [a]
dominio (R vlist) = eliminaRepetidos [v1| (v1,v2) <- vlist]

-- aux -> Devuelve una lista con los pares del conjunto, pero sin repetidos
eliminaRepetidos::Eq a => [a] -> [a]
eliminaRepetidos [] = [] 
eliminaRepetidos (x:xs) 
 | x `elem` xs = eliminaRepetidos(xs)
 | otherwise = x:eliminaRepetidos(xs)

--fun 2 -> conjunto sobre el que esta definida la relacion r
soporte :: Eq a => Rel [a] -> [a]
soporte (R vlist) =  eliminaRepetidos [ v4| (v1,v2) <- vlist, v3<- v1 : v2: [], v4 <- v3]

--fun 3 -> True si r es relacion de equivalencia, sino False
relEquivalencia:: Eq a => Rel a -> Bool
relEquivalencia r = reflexiva r && simetrica r && transitiva r

-- aux -> Dado un "a" existe aRa
reflexiva :: Eq a => Rel a -> Bool
reflexiva (R vlist) = and [elem (x,x) vlist | x <- (dominio (R vlist))]
-- aux -> Si existe aRb, existe bRa
simetrica :: Eq a => Rel a -> Bool
simetrica (R vlist) = and [(y,x) `elem` vlist | (x,y) <- vlist]
-- aux -> Si existe aRb y bRc, existe aRc
transitiva :: Eq b => Rel b -> Bool
transitiva (R vlist) = and [elem (x1,y2) vlist |(x1,y1) <- vlist, (x2,y2) <- vlist, y1 == x2]
 
--fun 4 ->
--conjCociente r::
--conjCociente r 
-- | (relEquivalencia r == True) = "Es relacion de equivalencia!"
-- | otherwise = "No es relacion de equivalencia! Deberia serlo"

--fun 5 -> r es la relacion {(x, y) | n ≤ x, y ≤ m, x es divisor de y}
generaDiv :: Integral a => a -> a -> Rel a
generaDiv n m = R [(x,y) | x <- [n..m], y <- [n..m], mod y x == 0]

--fun 6 -> r es la relacion ≥ sobre el conjunto de elementos de la lista xs
generaGE :: Ord a => [a] -> Rel a
generaGE xs = R [ (x1,x2)| x1 <- xs, x2 <- xs, x1 >= x2]

--fun 7 -> Composicion r1 ◦ r2. Dando por hecho que estan definidas sobre el mismo conjunto
composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (R vl1) (R vl2) = R [(v1,v4) | (v1,v2) <- vl1, (v3,v4) <- vl2, v2 == v3]

--EJERCICIO 4
--fun 1 -> lee una relacion introducida por el usuario
introRel :: IO (Rel String)
introRel = do 
 putStr "Introduzca el número de pares de elementos:\n"
 linea <- getLine
 plista <- leerPares (read linea::Integer)
 if esRelacion (R plista) then return (R plista)
 else do 
  putStr "Lo que ha introducido no es valido\n\n"
  introRel

-- aux -> Lee los elementos de cada relacion
leerPares :: (Eq t, Num t) => t -> IO [(String, String)]
leerPares 0 = return []
leerPares n = do 
 putStr "\nIntroduzca el primer elemento: \n"
 e1 <- getLine
 putStr "Introduzca el segundo elemento:\n"
 e2 <- getLine
 parAnt <- leerPares (n-1)
 return ((e1,e2) : parAnt)

--fun 2 -> Muestra la relacion introducida por el usuario en forma de matriz y algo datos adicionales
muestraRel :: IO ()
muestraRel = do 
 rr <- introRel
 putStr "\nInfo:\n" 
 putStr ((show rr)  ++ "\n")
 putStr ("Dominio (A -> ordenadas)=" ++ (show (dominio rr)) ++ "\n")
 putStr ("Rango (B -> abscisas)=" ++ (show (rango rr)) ++ ": \n\n")
 putStr "Tabla AxB:\n"
 muestraAux (mat_rel rr)

-- aux -> Le da forma de matriz a la lista de adyacencia calculada debajo
muestraAux :: Show a => [a] -> IO ()
muestraAux [] = return ()
muestraAux (x:xs) = do 
 print x
 muestraAux xs 

-- aux -> Representa la relacion en forma de lista de adyacencia, donde estara a 1 si a esta relacionado con b,
-- talque aϵDomino y bϵRango
mat_rel :: (Eq b, Num a) => Rel b -> [[a]]
mat_rel (R vlist) = [[if elem ((v1,v2)) vlist then 1 else 0 |  v2 <-(rango (R vlist))] | v1 <-(dominio (R vlist))]

-- aux -> Dada una relacion devuelve los elementos de su rango en una lista
rango :: Eq a => Rel a -> [a]
rango (R vlist) = eliminaRepetidos [v2| (v1,v2) <- vlist]

