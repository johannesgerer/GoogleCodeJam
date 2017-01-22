-- https://code.google.com/codejam/contest/2984486/dashboard#s=p1

-- correct at first try

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


  
main = mainId $ do
  let r = read::String->Int
      p (a:(b:[])) = (a,b)
  (n,nodes) <- cBlock 1 (map $ p. map r . words)
  let g = buildG (1,n) $ nodes ++ (swap <$> nodes)
      ll cp j = if length r < 2 then 1
                else (1+) $ sum $ take 2 $ sortBy (flip compare) $ ll (j/=) <$> r
        where r = filter cp (g!j)
  return $ show $ (n-) $ maximum $ ll (const True) <$> [1..n]

-- almost linear speedup!!!!!!!!!! .38 sec on i5 with 4 core 2,3 GHz

