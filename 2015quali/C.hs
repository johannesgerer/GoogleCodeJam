{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

-- correct


module C where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.State.Lazy
import           Data.Array
import           Data.Bits
import           Data.Either
import           Data.Function
import           Data.Function.Memoize
import           Data.Graph
import           Data.Int
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Set as S
import           Data.Tuple
import           Data.Word
import           GCJ
import           System.IO
import           Text.Printf

newtype F = F !Word8 deriving (Eq,Ord)

q@[o,i,j,k] = F  . (8*) <$> [0..3]
read2 'i' = i
read2 'j' = j
read2 'k' = k

instance Monoid F where
  mempty = F 0o0
  mappend (F a) (F b) = F $ a `xor` b `xor` (on v abs2 a b)
    where v c 0 = 0
          v 0 c = 0
          v 1 2 = 0
          v 2 3 = 0
          v 3 1 = 0
          v _ _ = 1

abs2 = flip shift (-3)

instance Show F where
  show (F a) = (if 0 == a .&. 0o1  then ""
                else "-") ++ f (abs2 a)
    where f 1 = "i"
          f 2 = "j"
          f 3 = "k"
          f 0 = "1"

matrix = [mappend x y | x <- q, y <- q]
ijk = mconcat $ read2 <$> "ijk"
                  

main2 = mainM id $ do
  x <-  read . (!!1) . words <$> cLine
  s <- fmap read2 <$> cLine
  let l = mconcat s
      x' = x `mod` 4
      r = if x' == 2 then l /= F 0 && l /= F 1
          else if x' == 0 then False
               else l == F 1
  if not r then return "NO" else do
    let help round = do y <- fromLeft s i $ mconcat $ replicate round l
                        return (min 4 $ x - 1 - round,y)
        ni = listToMaybe $ mapMaybe help [0..(min 3 $ x - 1)]
        nj = do (rounds,rest) <- ni
                fromLeft (rest ++ concat (replicate rounds s)) j mempty
    return $ maybe "NO" (const "YES") nj
    -- maybe (return "NO")
    --   (\m -> do when (k /= mconcat m)
    --               $ liftIO $ printf "Fehler (x=%d, mod=%d)\n%v\n%v\n%v\n%v\n\n" x x'
    --               (show2 s) (show ni)
    --               (show2 $ (snd $ fromJust ni) <> concat (replicate (fst $ fromJust ni) s)) (show2 m)
    --             return "YES") nj

show2 = concat . fmap show

  
fromLeft (h:rest) test cur =
  if new == test then Just rest
  else fromLeft rest test new
  where new = cur <> h
fromLeft []  _ _ = Nothing
