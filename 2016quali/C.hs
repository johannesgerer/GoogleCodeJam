{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

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
import qualified Data.Vector as V
import           Data.Word
import           GCJ
import           Numeric
import           System.IO
import           Text.Printf

                  

main2 = mainM id $ do
  [n,j::Int] <-  fmap read . words <$> cLine
  -- [1..]
  return $ show [n,j]

pow :: V.Vector Integer
pow = V.fromList [ power b n | b <- [1..10], n <- [1..32]]

power a 1 = a
power a n = a*(power a $ n-1)

p b n = pow V.! ((b-1)*32+n-1)

dez [] _     _   = 0
dez (x:xs) b n = dez xs b (n-1) + case x of
  '1' -> p b n
  '0' -> 0

  
d w b n = (dez g b $ length g,g)
  where g = (take (n-2) $ printf "%b" w) ++ "1"
  
