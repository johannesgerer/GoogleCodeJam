-- Gesamt: 38min

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
  let s3 n k r l c
        | length n == r = l
        | otherwise = if op c (head n) (head k) then s3 (drop (1-c) n) (tail k) (r+c) (l+1) c
                      else s3 (drop c n) (tail k) (r+1-c) l c
        where op 1 = (<)
              op 0 = (>)
      re = show <$> [ s3 naomi kent 0 0 0,s3 kent naomi 0 0 1]
  return $ intercalate " " re
            
