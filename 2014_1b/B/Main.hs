
import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.Trans.State.Lazy
import GCJ
import System.IO
import Data.List
import Data.Int
import Data.Graph
import qualified Data.Set as S
import Data.Tuple
import Data.Array
import Text.Printf
import Data.Function.Memoize
import Data.Function


  
main = mainId $ do
  w <-  words <$> (cLine >> cLine)
  let w2 = nub $ concat w
      sol s (c:r) = if length match == 1 then
                      if length (filter ((c==).head) $ group $ head match) == 1
                      then sol s r
                      else 0::Int64
                    else if l ls > 1 || l rs  > 1
                         then 0
                         else fac2 (l $ x voll) * sol (concat (ls++rs):nomatch) r
        where l = genericLength
              x f = filter f match
              match = filter (c `elem`) s
              nomatch = filter (not . (c `elem`)) s
              links w = head w == (c::Char) && voll (dropWhile (c/=) w)
              ls = x links
              rs = x (links.reverse)
              voll = and . fmap (c==) 
  return $ show $ w2

fac2 :: Int64 -> Int64
fac2 = (map fac [0 ..] !!) . fromIntegral
   where fac 0 = 1::Int64
         fac n = n * fac2 (n-1) `mod` 1000000007
