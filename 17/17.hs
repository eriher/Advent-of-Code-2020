import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d


updatePos [] m = m
updatePos (v:rest) m = updatePos rest (Map.insertWith (+) v 1 m)

part1 [] m = m
part1 ((x,y,z):rest) m = part1 rest (updatePos pos m)
            where pos = ([(x+x',y+y',z+z')| x'<-[-1..1], y'<-[-1..1], z'<-[-1..1]] \\ [(x,y,z)])

part2 [] m = m
part2 ((x,y,z,w):rest) m = part2 rest (updatePos pos m)
            where pos = ([(x+x',y+y',z+z',w+w')| x'<-[-1..1], y'<-[-1..1], z'<-[-1..1], w' <- [-1..1]] \\ [(x,y,z,w)])

main = do
      contents <- readFile "input.txt"
      let lines' = lines contents
      let activeIndxs = [(x,y) | x <- [0..((length lines')-1)], y <- [0..((length (head lines'))-1)], (lines' !! x !! y) == '#']

      let p1 = length $ foldl (\acc m -> Map.keys $ Map.filterWithKey (\key val -> (elem key acc && (val == 2 || val == 3)) || val == 3) $ part1 acc m) (map (\(x,y) -> (x,y,0)) activeIndxs) (replicate 6 Map.empty)
      let p2 = length $ foldl (\acc m -> Map.keys $ Map.filterWithKey (\key val -> (elem key acc && (val == 2 || val == 3)) || val == 3) $ part2 acc m) (map (\(x,y) -> (x,y,0,0)) activeIndxs) (replicate 6 Map.empty)
      print p1 
      print p2
      return ()
      
      
      