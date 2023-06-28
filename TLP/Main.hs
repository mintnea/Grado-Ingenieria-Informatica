module Main where

import System.IO
import Data.Char
import Text.Read
import StockControl

------------------
-- PERSISTENCIA --
------------------

-- NOMBRE DEL FICHERO DONDE SE GUARDA EL STOCK --
fileName :: FilePath
fileName = "stock.txt"

loadStock :: IO ()
loadStock = do { fcontent <- readFile' fileName;
                 let flines = (lines fcontent) in
                   case flines of
                     [] -> mainLoop createStock
                     s  -> mainLoop (read (head s)::Stock)
               }

saveStock :: Stock -> IO()
saveStock s = writeFile fileName (show s)

--------------------------
-- FUNCIONES AUXILIARES --
--------------------------

-- ERROR DE CADENA VACÍA --
errorCadenaVacia s = do { putStr "Debe introducir una cadena no vacía\n";
                          mainLoop s;
                        }

-- MENSAJE DE PRODUCTO DESCONOCIDO --
productoDesconocido p = "El producto \""++p++"\" nunca ha estado en nuestro catálogo.\n";

-- LEE DEL TECLADO UNA CADENA CON EL NOMBRE DEL PRODUCTO --
inputString :: IO String
inputString = do { hFlush stdout;
                   p <- getLine;
                   return (map toLower p);
                 }

inputInt :: IO (Maybe Int)
inputInt = do { hFlush stdout;
                n <- getLine;
                return (readMaybe n::Maybe Int)
              }

-- LEE DEL TECLADO EL NOMBRE DEL PRODUCTO Y EL NÚMERO DE UNIDADES --
inputData :: (Stock -> String -> Int -> IO ()) -> Stock -> IO ()
inputData op s = do { putStr "\nIntroduzca el nombre del producto: ";
                      p <- inputString;
                      if (p /= "")
                        then do { putStr "Introduzca el número de unidades: ";
                                  u <- inputInt;
                                  case u of
                                    Nothing -> do { putStr "Debe introducir un número entero de unidades.\n";
                                                    mainLoop s;
                                                  }
                                    Just uint -> if ( uint <= 0 )
                                                   then do { putStr "Debe introducir un número de unidades mayor que cero.\n";
                                                             mainLoop s;
                                                           }
                                                   else op s p uint
                                }
                        else errorCadenaVacia s;
                    }

-- MODIFICA EL STOCK CON UNA OPERACIÓN DE COMPRA --
compra :: Stock -> String -> Int -> IO ()
compra s p u = let us = retrieveStock s p in do {
                 if ( us == -1 )
                   then mainLoop (updateStock s p u);
                   else mainLoop (updateStock s p (us+u));
               }
               

-- MODIFICA EL STOCK CON UNA OPERACIÓN DE VENTA --
venta :: Stock -> String -> Int -> IO ()
venta s p u = let us = retrieveStock s p in do {
                if ( us == -1 )
                  then do { putStr (productoDesconocido p);
                            mainLoop s;
                          }
                  else if ( us >= u )
                         then mainLoop (updateStock s p (us-u));
                         else do { putStr "No hay tantas unidades para vender.\n";
                                   mainLoop s;
                                 }
              }

-- REALIZA UNA CONSULTA AL STOCK --
consulta :: Stock -> IO ()
consulta s = do { putStr "\nIntroduzca el nombre del producto: ";
                  p <- inputString;
                  if ( p /= "" )
                    then let msg = consultaMsg p (retrieveStock s p) in do {
                           putStr msg;
                           mainLoop s;
                         }
                    else errorCadenaVacia s;
                }
  where -- CONSTRUYE EL MENSAJE A DEVOLVER --
        consultaMsg :: String -> Int -> String
        consultaMsg p u
          | u > -1    = "El producto \""++p++"\" está en nuestro catálogo "++(if (u/=0) then "y" else "pero")++" tenemos "++(show u)++" unidad"++(if (u/=1) then "es" else "")++".\n"
          | otherwise = productoDesconocido p


listado :: Stock -> IO()
listado s = do { putStr "\nIntroduzca el prefijo de la búsqueda: ";
                 p <- inputString;
                 putStr ("\nListado del catálogo comenzando por \""++p++"\":\n\n");
                 putStr (unlines (map muestra (listStock s p)));
                 mainLoop s;
               }
  where muestra :: (String,Int) -> String
        muestra    (p     ,n     )  = p++": "++(show n)

-- "BUCLE" PRINCIPAL --
mainLoop :: Stock -> IO ()
mainLoop s = do { putStr "\nMenú\n----\n1-Comprar productos\n2-Vender productos\n3-Consultar stock\n4-Listado del stock\n0-Fin del programa\n\nSeleccione opción: ";
                  hFlush stdout;
                  option <- getLine;
                  putStr "\n";
                  case option of
                    "1" -> do { putStr "Compra de producto\n-----------------\n";
                                inputData compra s
                              }
                    "2" -> do { putStr "Venta de producto\n-----------------\n";
                                inputData venta s
                              }
                    "3" -> do { putStr "Consulta de stock\n-----------------\n";
                                consulta s
                              }
                    "4" -> do { putStr "Listado de stock\n----------------\n";
                                listado s
                              }
                    "0" -> do { putStr "Fin del programa.\n";
                                saveStock s;
                              }
                    _   -> do { putStr "Opción no reconocida.\n";
                                mainLoop s;
                              }
                 }

-- FUNCIÓN PRINCIPAL --
main :: IO ()
main = do { hSetBuffering stdin LineBuffering; -- Permite que la entrada pueda ser editada
            loadStock;                         -- Cargamos el stock de un fichero
          }
