import Data.List
import Data.Function
import Data.Maybe
import Control.Monad
import System.Environment
import Control.Applicative
import Control.Parallel.Strategies
import qualified Data.Map.Lazy as Map

-- input either from first args or inputFile
  
main =   getArgs >>= (readFile . i) >>= ( mapM putStrLn <$> haupt . twolines . map words . drop 1 . lines )
  where i [] = "inputFile"
        i (b:_) = b

haupt x =  [ --show y ++ "\n"++
              "Case #"++ (show n) ++": "++ (solveStr y)  |
              (n,y) <- zip [1..] x ] 

solveStr :: ([Integer],[Integer]) -> String
solveStr = show . solve

solve ([e,r,n],v) =  x (reverse v) r'
                     -- all: [ (r,x (reverse y) r) | y <- inits v, r <- [r'..e] ]
  where
    r' = minimum [e,r]
    x (v:vs) j = maximum [ x vs k + (pos $ k-j+r' ) | k <- [r''..e] ]
      where
        pos x | x > 0 = x * v
              | True = 0
        r'' = maximum [j - r,  r' ]
    x [] j = 0

twolines :: [[String]] -> [([Integer],[Integer])]
twolines (a:(b:c)) = ((map read a,map read b): twolines c)
twolines [] = []
