{-# LANGUAGE ScopedTypeVariables #-}

module A where

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
  show . res . fmap re . words <$> cLine

res [r,c,1] = r * c
res [r,c,w] = if c==w then w
              else (q + w) -
                   if w==2 && re ==0 then 1 else 0 
  where (q,re) = quotRem c w
