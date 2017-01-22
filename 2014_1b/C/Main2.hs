-- shit: still too slow
-- slow down is due to bad algorithm (not naivity of functional/immutable approch)

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

  -- NEU
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.Ord
import Data.Maybe
import Data.Graph
  -- NEU
import Text.Printf
import Data.Function.Memoize
import Data.Function

instance Functor First where
  fmap f = First . fmap f . getFirst
  
main = mainId $ do
  nr:(m:[]) <- (fmap read . words) <$> cLine
  z <- fmap (read :: String -> Int) <$> cLines nr
  f <- (fmap $ (\x -> (x!!0,x!!1)) . fmap (read :: String -> Int) . words) <$> cLines m
  let b = (0,nr)
      cA x = listArray b [x|i<-[0..nr]]
      gr = M.fromList . fmap (\x -> (z !! pred x,x)) <$> 
            buildG b (f ++ (swap <$> f) ++ -- if nr==5 then [(0,5)] else []) --
                      (zip (repeat 0) [1..nr]))
-- build graph from bidirectional flights and the starting steps, add
-- destinations zip codes and create a (sorted map)
      solve this ibA ibL vis nVis nexts =
        if nVis == nr then aFirst $ Just []
        else mconcat $ g <$> M.toAscList nexts' 
        where
          nexts' = M.filterWithKey f $ M.union nexts $  M.mapKeysMonotonic (flip (,) this) $ gr ! this
          f (toZ,fromI) toI = not (vis ! toI) && (case ibA' ! fromI of
            -1 -> False
            0 -> this == 0
            _ -> True)
          ibA' = ibA//[(this,length ibL)]
          as = concat $ replicate (nVis) "   "
          g ((toZ,fromI),toI)=
            fmap (show toZ++) $ 
                    -- fmap ((as -- ++ " ("++show ibA'++")\n"++as
                    --              ++"("++show nexts'++")\n"++as
                    --          ++"from "++
                    --          show fromI++" to " -- ++ show toZ++"/"
                    --          ++ show toI++"\n"++as)++) $
                               solve toI (ibA'//[(j,-1)|j<-del]) ibL2 (vis//[(toI,True)]) (nVis+1) nexts'
            where (del,ibL2) = splitAt (length ibL - (ibA' ! fromI)) $ this:ibL
  return $ getaFirst $ solve 0 (cA $ -1) [] (cA False) 0 M.empty
  -- return $ show $ cA $ -1
   

-- aFirst = return . fromMaybe "Nothing"
-- getaFirst = intercalate "\n\n"

aFirst = First
getaFirst = fromJust . getFirst
