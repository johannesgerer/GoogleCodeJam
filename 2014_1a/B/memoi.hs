


import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.Trans.State.Lazy
import GCJ
import System.IO
import Data.List
import Data.Graph
import Data.Tuple
import Data.Array
import Text.Printf
import Data.Function.Memoize
import Data.Function


  
main = mainId $ do
  let r = read::String->Int
      p (a:(b:[])) = (a,b)
  (n,nodes) <- cBlock 1 (map $ p. map r . words)
  let g = buildG (1,n) $ nodes ++ (swap <$> nodes)
      ll f cp j = if length r < 2 then 1
                  else (1+) $ sum $ take 2 $ sortBy (flip compare)
                       $ f (Just j) <$> r
        where r = maybe id (filter.(==)) cp  $ g!j
      ll2 :: Maybe Int -> Int -> Int
      ll2 = fix ll
  return $ show $ (n-) $ maximum $ ll2 Nothing <$> [1..n]

-- almost linear speedup!!!!!!!!!! .37 sec on i5 with 4 core 2,3 GHz
-- fix is faster than memoFix2 (no memoization needed)
