module ViewStock where

import System.IO

data Stock = ROOTNODE [Stock] | INNERNODE Char [Stock] | INFONODE Int
  deriving (Show,Read,Eq)

-- NOMBRE DEL FICHERO DONDE SE GUARDA EL STOCK --
stockFile :: FilePath
stockFile = "stock.txt"

latexFile :: FilePath
latexFile = "stock.tex"

loadStock :: IO (Maybe Stock)
loadStock = do { fcontent <- readFile stockFile;
                 let flines = (lines fcontent) in
                   case flines of
                     [] -> return Nothing
                     s  -> return (Just (read (head s)::Stock))
               }

toLatex :: Stock -> String
toLatex (ROOTNODE l)     = "\\Tree [.{ROOT} " ++ (concat (map toLatex l)) ++ " ]\n"
toLatex (INFONODE n)            = show n
toLatex (INNERNODE c l) = " [." ++ [tr c] ++ " " ++ (concat (map toLatex l)) ++ " ]"
  where tr :: Char -> Char
        tr    ' '   = '~'
        tr    c     = c

createLatex :: Stock -> String
createLatex s = "\\documentclass{standalone}\n\\usepackage{tikz, tikz-qtree}\n\\begin{document}\n" ++ (toLatex s) ++ "\\end{document}\n"

main :: IO()
main = do { s <- loadStock;
            case s of
              Nothing -> putStr "Error, no hay ningún stock en el fichero leído.\n";
              Just st -> writeFile latexFile (createLatex st);
          }
