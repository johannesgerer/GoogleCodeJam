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

haupt x = concat [ "Case #"++ (show n) ++": "++ (show $ count y)  ++"\n" |
                   (n,y) <- zip [1..] x ] 

-- worked up to 999999 in 49 seconds in ghc,      9999999 in 580s 
-- using 5.3GB on two processors  yielding numbers up to 10^28 
numbers = drop 1 $ map (strip '0') $ replicateM 4 ['0'..'9']
sqpalindromes = ( map (^2) [0..9] :
                  map makepal ("":(map show [0..9] ))
                )
                where makepal y = [(read ((reverse x)++y++x) :: Integer)^2 | x <- numbers ]
                      
ispalindrome a = let b = show a
                 in all (\(x,y)-> x==y) $ zip b $ reverse b

fairAndSquare = concat $ parMap rdeepseq (filter ispalindrome) sqpalindromes

sfas = Map.fromList ( zip (sort fairAndSquare) [0..] )
count c = let [a,b] = map read . words $ c
          in (-1) + (fromJust $ liftM2 ((-) `on` snd) (Map.lookupGT b sfas) (Map.lookupLT a sfas))

strip a = reverse . dropWhile (a==) . reverse
