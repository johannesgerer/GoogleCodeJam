{-# LANGUAGE ScopedTypeVariables #-}

-- correct


module B where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Array
import Data.Function
import Data.Ord
import Data.Function.Memoize
import Data.Graph
import Data.Set as S
import Data.Map as M
import Data.List
import Data.Tuple
import GCJ
import System.IO
import Text.Printf


  
main2 = mainId $ do
  ps <- fmap f . group . sortBy (flip compare) .
        fmap (read :: String -> Int) . words <$> (cLine >> cLine )
  let res cap = (cap+) . sum . fmap g . takeWhile ((> cap) . fst) $ ps
        where g (h,l) = (*) l $ (h-1) `div` cap
  return $ show $ minimum $ res <$> [1..(fst $ head ps)]
  where f l = (head l,length l)
