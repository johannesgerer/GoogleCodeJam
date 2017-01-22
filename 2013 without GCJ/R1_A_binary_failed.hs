import Data.List
import Data.Function
import Data.Maybe
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

type D = Integer

solve x = count . map (read :: String -> D ) $ words x
  where
    count :: [D] -> D
    count [r,t] = binarySearch 1 t^2
      where 
        binarySearch :: D -> D -> D
        binarySearch low high
          | high == low  = low
          | otherwise = 
            let n   = (low + high) `div` 2
                ben = 2*n*n+n*(2+2*r-3)
            in case compare ben t of
              GT -> binarySearch low (n-1)
              LT -> binarySearch n high
              EQ -> n
