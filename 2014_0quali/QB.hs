import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.Trans.State.Lazy
import GCJ
import System.IO
import Data.List
import Text.Printf


  
main = mainId $ do
  (c:(f:(x:[]))) <- ((read::String->Double) <$>). words <$> cLine
  let step g t = if (x-c)*f >= c*g then step (g+f) (t+c/g)
                 else t+x/g
  return $ show $ step 2.0 0.0
