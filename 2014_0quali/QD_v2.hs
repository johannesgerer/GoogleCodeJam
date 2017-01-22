import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Exception
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import GCJ
import System.IO
import Data.List
import Data.Maybe
import Text.Printf


  
main = mainId $ do
  cLine
  naomi:(kent:_) <- ((reverse.sort.((read::String->Double) <$>).words)<$>) <$> cLines 2
  let s n k r l
        | length n == r = l
        | otherwise = if head k < head n then s (tail n) (tail k) r (l+1)
                      else s n (tail k) (r+1) l
      r = s naomi kent 0 0
      s2 n k r l
        | length k == r = l
        | otherwise = if head k < head n then s2 (tail n) k (r+1) (l+1)
                      else s2 (tail n) (tail k) r l
      r2 = s2 naomi kent 0 0
  return $ show r ++ " " ++ show r2 -- ++ "\n" ++ show [naomi,kent]
            
