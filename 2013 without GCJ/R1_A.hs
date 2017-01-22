import Data.List
import Data.Function
import Data.Maybe
import Data.Ratio
import Control.Monad
import System.Environment
import Control.Parallel.Strategies
import qualified Data.Map.Lazy as Map

-- input either from first args or inputFile
  
main =  getArgs >>= (readFile . i) >>= (putStr . haupt . drop 1 . lines)
  where i [] = "inputFile"
        i (b:_) = b

haupt x = concat [ "Case #"++ (show n) ++": "++ (solveStr y) ++"\n" |
                   (n,y) <- zip [1..] x ] 

solveStr :: String -> String
solveStr = show . solve



type D = Double

solve x = count . map (read :: String -> D) $ words x
  where
    count :: [D] -> Integer
    count [r',t'] = floor . snd . head . filter fi $ z
      where
        r = r' *1.0
        t = t' *1.0
        fi (a,b) = abs(b-a) < 0.01
        z = zip i $ tail i
        i = iterate next t
        next xn = xn - ((f xn) / (f' xn))
        f n = 2*n+(2+2*r-3) - t/n
        f' n = 4 + (2+2*r-3)/n
