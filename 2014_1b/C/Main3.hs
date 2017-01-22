


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
  let gr = buildG (0,nr) $ f ++ (zip (repeat 0) [1..nr]) ++ (swap <$> f)
      h = fmap (\x -> (x, z !! pred x)) <$> gr
      solve s node visited routes =
        if S.size visited == nr then First $ Just []
        else mconcat $ fmap g
             $ sortBy (comparing (snd.snd)) $ concat $
             zipWith (\x y-> ((,) x) <$> y) [0..] routes'
        where routes' = filter (flip S.notMember visited . fst) (h!node)
                        : fmap (filter ((/= node).fst)) routes
              g (i,(next,z)) = fmap (show z++) -- ((next,i):)
                               $ solve False next (S.insert next visited) $
                               if s then [] else drop i routes'
  return $ fromJust $ getFirst $ solve True 0 S.empty []

