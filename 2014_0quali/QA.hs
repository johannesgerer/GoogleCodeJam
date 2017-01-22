import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.Trans.State.Lazy
import GCJ
import System.IO
import Data.Set
import Text.Printf


  
main = mainId $ do
  r <- foldl1 intersection <$> sequence [l,l] 
  return $ case size r of
    0 -> "Volunteer cheated!"
    1 -> elemAt 0 r
    _ -> "Bad magician!"
  where l = cLine >>= \x -> fromList . words . (!!(read x-1)) <$> cLines 4
       
