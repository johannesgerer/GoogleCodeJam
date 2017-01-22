import Data.List
import Data.Function
import Data.Maybe
import Data.Ratio
import Control.Monad
import System.Environment
import qualified Data.Map.Lazy as Map

-- input either from first args or inputFile
  
main =  getArgs >>= (readFile . i) >>= (putStr . haupt . drop 1 . lines)
  where i [] = "inputFile"
        i (b:_) = b

haupt x = concat [ "Case #"++ (show n) ++": "++ (solveStr y) ++"\n" |
                   (n,y) <- zip [1..] x ] 

solveStr :: String -> String
solveStr = show . solve . words



-- solve:: [String] -> [String]
solve [w,n'] = sum $ filteredSubs w
  where n :: Int
        n = read n'
        f x = length x >= n
        filteredSubs x = map (asd . inits) ft
          where ft = filterTails (length $ head t) [] t
                t = tails x
                filterTails m r (x:xs) = if m >= n then filterTails (m-1) (x:r) xs
                                         else r
                filterTails _ _ [] = []
                filterSubs m x = if m == n then x
                                        else filterSubs (m+1) (tail x)
                asd x = count n $ filterSubs 0 x
        count m (x:xs) = if isN m x then 1+ (length xs)
                       else count (m+1) xs
        count _ [] = 0
        isN m x
          | m<n = False
          | isVowel (take n x) = isN (m-1) (tail x)
          | True = True
        isVowel x = any (flip elem "aeiou") x
                  
-- s2 cons w wr = (next w + next wr)
--   where next (h:t) = if cons2 < n then 0 else s2 cons2 t (reverse t)
--           where cons2 = if h `elem` "aeiou" then cons else cons-1
