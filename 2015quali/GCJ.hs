{-# LANGUAGE
ScopedTypeVariables
,Rank2Types
,GeneralizedNewtypeDeriving
 #-}


module GCJ (
  GCJ()
  ,module Control.Applicative
  ,module Control.Monad
  ,cLine
  ,cLines
  ,cBlock
  ,mainM
  ,mainId
  ,liftIO
  )
       where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Trans.State.Lazy
import Control.Parallel.Strategies
import Data.List
import Prelude
import Safe
import System.Environment
import System.IO
import Text.Printf

-- data Type for Google Code Jam Solution Program
newtype GCJ m a = GCJ { fromGCJ :: (StateT [String] m a) }
                deriving (Functor,Applicative,Monad,MonadIO)

-- has to be Monad (e.g. to implement cBlock)
-- next action has to be able to depend on state

type Res m = GCJ m String
-- produce main from pure function
mainId :: Res Identity -> [String] -> IO ()
mainId = mainM $ return.runIdentity
                         
-- produce main from Monad
-- reads testInput or the file given on the command line
-- writes to standard out and to the given file with '.out' appended
mainM :: Monad m => (forall a. (m a -> IO a))
         -> Res m -> [String] -> IO ()
mainM exec (GCJ gen) args = do
  let cc []     = return ("test", return (), putStr)
      cc (a:[]) = do h <- openFile (a++".out") WriteMode 
                     return (a, hClose h, \x -> putStr x >> hPutStr h x) 
  (inF,close,myP) <- cc args
  printf "Reading file: %s\n" inF
  (head:tail) <- lines <$> readFile inF
  let ncase = read head
  printf "Found %d cases\n" ncase
  (work,rest) <- (exec $ runStateT (replicateM ncase gen) tail)
  zipWithM_ ((myP.).printf "Case #%u: %s") [(1::Int)..] $
    parMap rdeepseq (\y -> y ++ if "\n" `isSuffixOf` y then "" else "\n") work
  close >> when (length rest > 0) 
    (error $ "GCJ: not all input lines have been consumed:\n" ++ unlines rest)
  
-- WOW: althoug the parsing of the input cannot happen in parallel
-- (lines are consumed one by one, and input of the nexta example
-- depends on how much the current has consumed), the work can be
-- automatically parallized, if the number of lines to consume does
-- not depend on the result of the main workload of each case. THis is
-- demonstrated by the following example:


-- parallel test
-- mainP = mainId $ do
--        r <- return . length . show . product . enumFromTo (1::Integer) . read =<< cLine
--        if not parallel && r == 1 then return =<< cLine else return $ show r  
--        where parallel = False
-- input file:
-- 2
-- 200000
-- 200000
-- 200000
-- 200000
         
-- consumes and returns one line
cLine :: Monad m => GCJ m String
cLine = GCJ $ do x <- get
                 put $ tail x
                 return $ headNote "GCJ: No input lines left to consume" x

-- consumes and returns n lines
cLines n = sequence $ take n $ repeat cLine

-- consumes, transforms and returns next block:
-- a block consists of first line: n, then (n-m) lines
cBlock :: Monad m => Int -> ([String] -> a) -> GCJ m (Int,a)
cBlock m f = do n <-  liftM read cLine
                return . (,) n . f =<< cLines (n-m) 

