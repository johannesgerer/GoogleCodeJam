{-# LANGUAGE ScopedTypeVariables #-}

-- correct

-- too slow for large input. better: for every point sort all other
-- points by angle and then find the points that is starting point of
-- the circle section with angle not less than pi. O(n^2 log(n))

module C where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.State.Lazy
import           Data.Array
import           Data.Bits
import           Data.Either
import           Data.Function
import           Data.Function.Memoize
import           Data.Graph
import           Data.Int
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as S
import           Data.Tuple
import           Data.Word
import           GCJ
import           System.IO
import           Text.Printf

re = read :: String -> Int
  
type P = (Int,Int)

c (a1,a2) (b1,b2) =  (signum a1,a2*b1) >= (signum b1,a1*b2)
             
main2 = mainId $ do
  (_,cs) :: (Int,[(Int,Int)]) <- cBlock 0 $ fmap $ (\[a,b] -> (a,b)). fmap read . words
  let minNumber s os = if length os < 3 then 0  :: Int
                       else undefined
  return $ "\n" ++ (unlines $ show <$> go minNumber cs)

pairWith s mi o = stop 0 0 . fmap (on dp (minus s) o)
  where stop pos neg (h:t) =
          case compare h 0 of
           GT -> let a = pos + 1 in cp a neg $ stop a neg t
           LT -> let a = neg + 1 in cp a pos $ stop pos a t
           EQ -> stop pos neg t
        stop pos neg [] = min pos neg
        cp a b c = if a >= mi && b >= mi then mi else c

dp :: (Int, Int) -> (Int, Int) -> Int
dp (a1,a2) (b1,b2) = a1*b2 - a2*b1
minus (a1,a2) (b1,b2) = (a1-b1,a2-b2)

go action = go' []
  where go' done (h:t) = [action h (done ++ t)] ++ go'(h:done) t
        go' _ [] = []
