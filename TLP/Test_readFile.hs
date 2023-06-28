module Main where

import System.IO

main :: IO ()
main = do { fcontent <- readFile' "Test_readFile.hs";
            let flines = (lines fcontent) in
              case flines of
                [] -> putStr "El fichero está vacío\n"
                s  -> putStr "El fichero tiene, al menos, una línea\n"
          }

