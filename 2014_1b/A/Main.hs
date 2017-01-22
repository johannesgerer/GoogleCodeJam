


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
  (n,w) <- cBlock 0 id
  let r = ((fmap $ (head *** length) . join (,)) . group) <$> w
      k a = fmap (a <$>) r 
  return $ if length (head <$> (group $ k fst )) > 1
           then "Fegla Won" 
           -- else show $ transpose $ k snd
           else show $ sum $ fmap median $ transpose $ k snd


median x  = dev (sort x !! (length x `div` 2)) x


dev x = sum . fmap (abs.(x-))
