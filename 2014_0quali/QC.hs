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


  
main = mainM id $ ('\n':) <$> do
  r:(c:(m:[])) <- ((read::String->Int) <$>). words <$> cLine
  let debug = intercalate " " $ show <$> [r,c,m]
      tr = maybe "Impossible" (unlines.check.transpose)
      check x = assert (True  || length x /= r || all ((==c).length) x) x
  liftIO $ putStrLn debug
  return $ debug ++ "\n" ++ (tr $ solve r c m)
  where solve a b m
          | c==1 = Just $ f a b '*'
          | a>b = transpose <$> solve b a m
          | a==1 = transpose <$> Just [f2 m ".c"]
          | a==2 = ifm (c>3) $ ifm (m2==0) $ Just $ f a d2 '*' ++ f a (b-d2) '.'
          | a==3 = ifm (c>3) $ ifm (m2==0) $ Just $ f a d2 '*' ++ f a (b-d2) '.'
          | otherwise = Just debug
          -- | nn >= ma   = Nothing
          -- | m < min a b    = if m==2 && min a b == 3 then Nothing
          --                    else if a>b then transpose <$> solve b a m
          --                         else Just $ [f2 w ".c",f2 (m-w) ".."]++ f (a-2) '.'
          -- | otherwise       = (f d '*' ++).transpose <$> solve b (a-d) (m-d*b)
          where 
            debug = [intercalate "," $ show <$> [a,b,m,d]]
            d = min (max 0 $ a-nn) $ div m b
            f a b c = [[c| i <- [1..a]]| j <- [1..b]]
            f2 n n2 = [ '*' | i <- [1..n]] ++ [ '.' | i <- [1..b-n-2]] ++ n2
            c = a*b-m
            ma = max a b
            nn = 3
            (d2,m2)=divMod m 2
            ifm c r = if c then r else Nothing
            
            
