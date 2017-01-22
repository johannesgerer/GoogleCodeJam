{-# LANGUAGE ScopedTypeVariables #-}

-- Correct (small and large)

module B where

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
  
main2 = mainId $ do
  [b,n] <- fmap re . words <$> cLine
  ms <- fmap re . words <$> cLine
  let finished m = b + (sum $ div (m-1) <$> ms)
      myturn = binarySearch ( (< n) . finished) (0,n*maximum ms)
  return $ show $ fst $ head $ drop (n - finished myturn -1) $
    filter ((==0) . mod myturn . snd) $ zip [1..] ms

binarySearch :: Integral a => (a -> Bool) -> (a, a) -> a
binarySearch p (low,high) =
  if high - low == 1 then low 
  else binarySearch p $ if p mid then (mid,high) else (low,mid)
  where mid = (low + high) `div` 2
