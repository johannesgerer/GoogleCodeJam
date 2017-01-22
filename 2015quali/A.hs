-- correct

module A where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Array
import Data.Function
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
  ns <- fmap (read . (:[])) . head . tail . words <$> cLine
  return $ show $ req 0 0 ns 0

req _        _     []    add = add
req standing level (h:r) add = req (standing + new + (h :: Int)) (level+1) r $ add + new
  where new = max 0 $ level - standing
