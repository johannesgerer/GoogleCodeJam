{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

module D where

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
import Data.Set (toList, fromList)

re = read :: String -> Int

main2 = mainId $ do
  [x,r,c] <- fmap re . words <$> cLine
  return $ show $ l 4
  
asd x = mapM_ putStrLn $ map textRepresentaton $ rank x
l x = let a = rank x in
      (length a, length $ unique a)

type Coord = Int
type Point = (Coord, Coord)
type Polyomino = [Point]
 
-- Finds the min x and y coordiate of a Polyomino.
minima :: Polyomino -> Point
minima (p:ps) = foldr (\(x, y) (mx, my) -> (min x mx, min y my)) p ps
 
translateToOrigin :: Polyomino -> Polyomino
translateToOrigin p =
    let (minx, miny) = minima p in
        map (\(x, y) -> (x - minx, y - miny)) p
 
rotate90, rotate180, rotate270, reflect :: Point -> Point
rotate90  (x, y) = ( y, -x)
rotate180 (x, y) = (-x, -y)
rotate270 (x, y) = (-y,  x)
reflect   (x, y) = (-x,  y)
 
-- All the plane symmetries of a rectangular region.
rotationsAndReflections :: Polyomino -> [Polyomino]
rotationsAndReflections p =
    [p,
     map rotate90 p,
     map rotate180 p,
     map rotate270 p,
     map reflect p,
     map (rotate90 . reflect) p,
     map (rotate180 . reflect) p,
     map (rotate270 . reflect) p]
 
canonical :: Polyomino -> Polyomino
canonical = minimum . map (sort . translateToOrigin) . rotationsAndReflections
 
unique :: (Ord a) => [a] -> [a]
unique = toList . fromList
 
-- All four points in Von Neumann neighborhood.
contiguous :: Point -> [Point]
contiguous (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
 
-- Finds all distinct points that can be added to a Polyomino.
newPoints :: Polyomino -> [Point]
newPoints p =
    let notInP = filter (not . flip elem p) in
        unique . notInP . concatMap contiguous $ p
 
newPolys :: Polyomino -> [Polyomino]
newPolys p = unique . map (canonical . flip (:) p) $ newPoints p
 
monomino = [(0, 0)]
monominoes = [monomino]
 
-- Generates polyominoes of rank n recursively.
rank :: Int -> [Polyomino]
rank 0 = []
rank 1 = monominoes
rank n = concatMap newPolys $ rank (n - 1)
 
-- Generates a textual representation of a Polyomino.
textRepresentaton :: Polyomino -> String
textRepresentaton p =
    unlines [[if elem (x,y) p then '#' else ' ' | x <- [0 .. maxx - minx]]
             | y <- [0 .. maxy - miny]]
    where
        maxima :: Polyomino -> Point
        maxima (p:ps) = foldr (\(x, y) (mx, my) -> (max x mx, max y my)) p ps
        (minx, miny) = minima p
        (maxx, maxy) = maxima p
 
