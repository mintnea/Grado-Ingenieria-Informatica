{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module StockControl where

import Data.List (partition, sort, isPrefixOf)
import Debug.Trace (trace,traceStack)

-------------------------
-- IMPLEMENTACION TRIE --
-------------------------
data Stock = ROOTNODE [Stock]
            | INNERNODE Char [Stock]
            | INFONODE Int
            deriving (Show,Read,Eq)

-------------------------
-- FUNCIÓN CREATESTOCK --
-------------------------
-- FUNCIÓN QUE DEVUELVE UN STOCK VACÍO --
createStock :: Stock
createStock = ROOTNODE []


---------------------------
-- FUNCIÓN RETRIEVESTOCK --
---------------------------
-- FUNCIÓN QUE DEVUELVE EL NÚMERO DE UNIDADES DE UN PRODUCTO EN EL STOCK
-- SI NO ESTÁ, DEBERÁ DEVOLVER -1    
-- @params Stock, String (producto a buscar)   
-- @return Int (unidades encontradas (-1 si no encuentra ninguna))                               
retrieveStock :: Stock         -> String -> Int
retrieveStock stock producto
    | null producto = -1
    | otherwise = retrieveStock' stock producto -- sino devolvemos el resultado de la otra funcion (receiveStock')
    where
        -- Subfuncion que procesa los datos y la posicion de Stock en donde nos encontramos
        -- @params Stock, String (producto a buscar)   
        -- @return Int (unidades encontradas (-1 si no encuentra ninguna))   
        retrieveStock' :: Stock -> String -> Int
        retrieveStock' (ROOTNODE restoStock) producto = retrieveNodes restoStock producto -- ROOT -> llamar retrieveNodes
        retrieveStock' (INNERNODE letra restoStock) (primerNodo:restoNodos) -- INNER -> bucle
            | letra == primerNodo = retrieveNodes restoStock restoNodos -- matchea -> llamar retrieveNodes
            | otherwise = -1
        retrieveStock' (INFONODE valor) [] = valor -- INFONODE (se ha encontrado) -> return valor
        retrieveStock' _ _ = -1

        -- Subfuncion que llama recursivamente a retrieveStock'
        -- @params Stock, String (producto a buscar)    
        -- @return Int (unidades encontradas (-1 si no encuentra ninguna)) 
        retrieveNodes :: [Stock] -> String -> Int
        retrieveNodes [] _ = -1
        retrieveNodes (nodo:restoStock) producto =
            let result = retrieveStock' nodo producto -- llamada recursiva hasta que encuentre algo
            in if result == -1 then retrieveNodes restoStock producto else result
            -- si el resultado es -1 sigue buscando en el resto del stock


-------------------------
-- FUNCIÓN UPDATESTOCK --
-------------------------
-- FUNCIÓN QUE MODIFICA EL VALOR ASOCIADO A UN PRODUCTO EN EL STOCK --
-- SÓLO PUEDE ALMACENAR NÚMEROS MAYORES O IGUALES A 0               --
-- @params Stock (actual stock), String (nombre), Int (cantidad)
-- @return Stock (nuevo stock)
updateStock :: Stock -> String -> Int -> Stock
updateStock stock nombre cantidad =
  let actualiza = tryToUpdate stock nombre cantidad
  in if actualiza == stock
     then sortStocks (searchNodes nombre cantidad stock) -- si no se ha realizado ningun cambio crea el nodo (no existe el valor) y ordena stock
     else sortStocks actualiza -- si se ha encontrado y se ha actualizado, ordena los valores

-- Funcion inicial que intenta actualizar el Stock obteniendo como base el ROOTNODE
-- por cada nodo de Stock llama a la siguiente funcion (tryToUpdate')
-- se pasan backups del stock actual
-- @params Stock (actual stock), String (nombre), Int (cantidad)
-- @return Stock (nuevo stock)
tryToUpdate :: Stock -> String -> Int -> Stock
tryToUpdate (ROOTNODE xs) nombre cantidad = ROOTNODE (map (tryToUpdate' (ROOTNODE xs) nombre nombre cantidad) xs)
tryToUpdate x _ _ = x
-- Subfuncion que intenta actualizar el Stock desde el INNERNODE o INFONODE (si se encuentra)
-- @params Stock(original Stock),String(original nombre), String (nombre), Int (cantidad),Stock (actual stock)
-- @return Stock (nuevo stock)
tryToUpdate' :: Stock -> String -> String -> Int -> Stock -> Stock
tryToUpdate' originalStock originalNombre nombre cantidad (INNERNODE c xs)
  | null nombre  = INNERNODE c xs -- si ya se recorrio la palabra y todavia quedan INNERNODES solo existe de forma parcial = no se ha encontrado
  | c == head nombre = INNERNODE c (map (tryToUpdate' originalStock originalNombre (tail nombre) cantidad) xs) -- itera sobre la rama encontrada
  | otherwise = INNERNODE c xs
tryToUpdate' originalStock originalNombre nombre cantidad (INFONODE x)
  | null nombre = INFONODE cantidad -- Si ya se ha leido todo el nombre se actualiza la cantidad!!
  | otherwise = INFONODE x
tryToUpdate' _ _ _ _ x = x

-- Funcion inicial de busqueda de nodos repetidos
-- Se llega a ella si se han leido todas las ramas de Stock y no se ha actualizado nada
-- Iniciado en ROOTNODE
-- @params String(nombre), Int (cantidad),Stock (actual stock)
-- @return Stock (nuevo stock)
searchNodes :: String -> Int -> Stock -> Stock
searchNodes nombre cantidad (ROOTNODE restoNodos) = ROOTNODE (searchNodes' nombre cantidad restoNodos)
-- Subfuncion de creacion de nodos
-- Busca la posicion en la que empezar a escribir
-- @params String(nombre), Int (cantidad),Stock (actual stock)
-- @return Stock (nuevo stock)
searchNodes' :: String -> Int -> [Stock] -> [Stock]
searchNodes' nombre cantidad [] = [createNode nombre cantidad] -- si no queda stock que revisar se crea la hoja/rama
searchNodes' nombre cantidad (primerNodo@(INNERNODE letra rama):restoNodos) -- Docu http://aprendehaskell.es/content/Funciones.html
    | null nombre = createNode nombre cantidad:primerNodo:restoNodos -- si no hay mas letras en el nombre se crea el INFONODE en mitad de esa rama
    -- Si la letra de la rama coincide con el primer caracter del nombre se busca si hay mas nodos asi de forma recursiva
    | letra == head nombre = INNERNODE letra (searchNodes' (tail nombre) cantidad rama):restoNodos
    | otherwise = createNode nombre cantidad:primerNodo:restoNodos

-- Inserta una cantidad en un nodo hoja (INFONODE)
-- o crea una nueva rama con la primera letra del nombre y crea una rama entera llamandose a si misma
-- @params String(nombre), Int (cantidad)
-- @return Stock (nuevo stock)
createNode :: String -> Int -> Stock
createNode [] cantidad = INFONODE cantidad -- crea hoja
createNode (primeraLetra:restoPalabra) cantidad = INNERNODE primeraLetra [createNode restoPalabra cantidad] -- crea rama

-- ORDENAMIENTO
-- Se crea una instancia con las reglas a seguir para ordenar con sort
instance Ord Stock where
  INFONODE x <= INFONODE y = x <= y
  INFONODE _ <= INNERNODE _ _ = False
  INNERNODE x _ <= INNERNODE y _ = x <= y
  INNERNODE _ _ <= INFONODE _ = True
  INNERNODE _ _ <= ROOTNODE _ = False
  ROOTNODE _ <= INNERNODE _ _ = True
-- Funcion de ordenamiento que ordena ramas por orden alfabetico
-- @params Stock
-- @return Stock
sortStocks :: Stock -> Stock
sortStocks (ROOTNODE restoStock) = ROOTNODE (sort (map sortStocks restoStock))
sortStocks (INNERNODE nodo restoStock) = INNERNODE nodo (sort (map sortStocks restoStock))
sortStocks x = x


-----------------------
-- FUNCIÓN LISTSTOCK --
-----------------------
-- FUNCIÓN QUE DEVUELVE UNA LISTA PARES PRODUCTO-EXISTENCIA --
-- DEL CATÁLOGO QUE COMIENZAN POR LA CADENA PREFIJO p       --
-- @params Stock(stock), String(letra)
-- @return [(String,Int)] (listado de nodos coincidentes)
listStock :: Stock -> String -> [(String,Int)]
listStock (ROOTNODE resto) letra = 
  let coincidencias = (getPalabra resto letra) in
  if length coincidencias > 0 then sort coincidencias else [] -- si se han encontrado palabras las ordena y sino se devuelve vacio

-- Obtiene las palabras de las ramas parametrizadas con sus valores
-- @params [Stock]
-- @return [(String,Int)] (listado de nodos coincidentes)
getPalabra :: [Stock] -> String -> [(String, Int)]
getPalabra [] _ = [] -- No hay palabras coincidentes devuelve lista vacia
getPalabra (ROOTNODE x : xs) prefijo = getPalabra x prefijo-- caso no posible?
-- concatena todas las palabras obtenidas por bt y muestra su infovalue en forma [()]
getPalabra (primerNodo@(INNERNODE nodo rama):restoNodos) prefijo = concatMap (\x -> getPalabraAux x [nodo] prefijo) [rama] ++ getPalabra restoNodos prefijo

-- Recorre todas las ramas y va construyendo la palabra
-- @params [Stock], String (palabra)
-- @return [(String,Int)] (listado de nodos coincidentes)
getPalabraAux :: [Stock] -> String -> String -> [(String, Int)]
getPalabraAux (INFONODE valor : xs) palabra prefijo = if isPrefixOf prefijo palabra then [(palabra, valor)] else [] -- nodo info, se obtiene un nuevo palabra/valor
getPalabraAux (INNERNODE c hijos : xs) palabra prefijo = -- mientras que no se llegue al final de la palabra
  let totalHijos = concatMap (\hijo -> getPalabraAux [hijo] (palabra ++ [c]) prefijo) hijos -- llamada recursiva al metodo construyendo la palabra
  in totalHijos ++ concatMap (\hijo -> getPalabraAux [hijo] palabra prefijo) xs -- concatenacion final 

-- FUNCIÓN GENÉRICA DE BACKTRACKING --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    eS             c             n -- eS (es solucion, Boolean), c (hijos), n(nodo)
  | eS n      = [n] -- si eS (a->Bool) == n => solucion
  | otherwise = concat (map (bt eS c) (c n) ) -- sino se busca en sucesores de n