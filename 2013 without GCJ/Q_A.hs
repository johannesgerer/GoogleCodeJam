import Data.List
import Data.Int
import System.Environment

inputFile = "inputFile"

-- input either from first args or inputFile
main =  getArgs >>= (readFile . i) >>= (putStr . concat . formatter . haupt . drop 1 . lines)
  where i [] = "inputFile"
        i (b:_) = b
        formatter x = [ "Case #"++ (show n) ++": "++ y  ++"\n" | (n,y) <- zip [1..] x ]
  
haupt [] = []
haupt r = ( antwort ( take 4 r ) : haupt (drop 5 r) )

wins = [(c,1) | c <- [0,4..15]] ++ [(c,4) | c <- [0..3]] ++ [(0,5),(3,3)]
counts l = [ fst l  ] ++ ( take 3 $ repeat $ snd l -1 ) ++ [ 0 ]
inb l = take (fromInteger l) $ repeat False
pos =  map (intercalate [True] . (map inb) . counts ) wins

check player s = all (\(x,y) -> not y || x == player || x == 'T' ) s
won player s = any (check player . zip s) pos 

antwort sa = let s = concat sa in
  if won 'O' s then "O won"
  else if won 'X' s then "X won"
       else if all ('.'/=) s then "Draw"
            else "Game has not completed"
            
                   
