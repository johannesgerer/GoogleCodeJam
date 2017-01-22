{-# LANGUAGE
ScopedTypeVariables
,Rank2Types
,GeneralizedNewtypeDeriving
 #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This defines a Google Code Jam monad with a handful of combinators for simple parsing of input files.
--
-- The test cases are solved in parallel.
--
-- Although the parsing of the input cannot happen in parallel
-- (lines are consumed one by one, and input of the nexta example
-- depends on how much the current has consumed), the actual work can be
-- automatically parallelized, if the number of lines to consume does
-- not depend on the result of the main workload of each case.
--
-- This is demonstrated by the following example:
--
-- @
-- parallel test
-- mainP = mainId $ do
--        r <- return . length . show . product . enumFromTo (1::Integer) . read =<< cLine
--        if not parallel && r == 1 then return =<< cLine else return $ show r  
--        where parallel = False
-- @
--
-- input file:
--
-- @
-- 2
-- 200000
-- 200000
-- 200000
-- 200000
-- @

module GCJ (
  GCJ
  ,module Control.Applicative
  ,module Control.Monad
  )
       where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Parallel.Strategies
import Data.List
import Prelude
import Safe
import System.Environment
import System.IO
import Text.Printf

-- | data type for Google Code Jam Solution Program
--
-- It has to be a Monad (e.g. to implement cBlock).
--
-- Next action has to be able to depend on state.
newtype GCJ m a = GCJ { fromGCJ :: (StateT [String] m a) }
                deriving (Functor,Applicative,Monad,MonadIO,MonadTrans)

-- * Combinators       
  
-- | consumes and returns one line
cLine :: Monad m => GCJ m String
cLine = GCJ $ do x <- get
                 put $ tail x
                 return $ headNote "GCJ: No input lines left to consume" x

-- | consumes and returns n lines
cLines :: Monad m => Int -> GCJ m [String]
cLines n = sequence $ take n $ repeat cLine

-- | consumes, transforms and returns next block:
--
-- a block consists of first line: n, then (n-m) lines
cBlock :: Monad m => Int -> ([String] -> a) -> GCJ m (Int,a)
cBlock m f = do n <-  liftM read cLine
                return . (,) n . f =<< cLines (n-m) 


-- * Runners
type Runner m = GCJ m String -> [String] -> IO ()
             
-- | produce main from pure function
mainId :: Runner Identity
mainId = mainM $ return.runIdentity

-- | produce main from with transformed IO monad
mainIO :: Runner IO
mainIO = mainM $ id
                         
-- | produce main from Monad
--
-- reads testInput or the file given on the command line
--
-- writes to standard out and to the given file with '.out' appended
mainM :: Monad m => (forall a. (m a -> IO a))
         -> Runner m
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
  
