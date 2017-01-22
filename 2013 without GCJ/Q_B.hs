import Data.List
  
main = fmap (haupt 1 . drop 1 . lines) getContents >>= putStr
  
haupt _ [] = []
haupt counter input =
  let [ n,m ] = words $  input !! 0
      r = drop 1 input
      ni = read n :: Int
  in  "Case #"++ (show counter) ++": "++ (antwort $ take ni r )  ++"\n"
      ++ (haupt (counter+1) (drop ni r) )
        
antwort ra = let r = map (map (\x -> read x ::Int ) . words) ra
                 inner y = map ((\x -> minimum [x,y]).maximum) $ transpose r 
                 mins = concatMap (inner . maximum) r
  in if any (\(x,y) -> x > y) $  zip mins $ concat r 
  then "NO" else "YES"
