import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

stringToInt :: [Char] -> Int
stringToInt s = read s::Int

followPath :: [Char] -> (Int,Int) -> (Int,Int)
followPath [] pos = pos
followPath ('e':cs) (x,y)     = followPath cs (x+1,y-1)
followPath ('s':'e':cs) (x,y) = followPath cs (x,y-1)
followPath ('s':'w':cs) (x,y) = followPath cs (x-1,y)
followPath ('w':cs) (x,y)     = followPath cs (x-1,y+1)
followPath ('n':'w':cs) (x,y) = followPath cs (x,y+1)
followPath ('n':'e':cs) (x,y) = followPath cs (x+1,y)

updateTiles :: (Int,Int) -> Map (Int,Int) Int -> Map (Int,Int) Int
updateTiles tile tileMap = tileMap'
            where tileMap' = foldl (\acc x -> Map.insertWith (+) x 1 acc) tileMap (getNeighours tile)

getNeighours :: (Int,Int) -> [(Int,Int)]
getNeighours tile = neighbours
      where neighbours = map (\x -> followPath x tile) ["e","se","sw","w","nw","ne"]

nextDay :: Map (Int,Int) Int -> Map (Int,Int) Int
nextDay currentDay = nextDay
            where tiles = Map.keys currentDay
                  blackTiles = filter (\(tile,count)-> if Map.member tile currentDay then count == 1 || count == 2 else count == 2) $ Map.toList $ foldl (\acc x -> updateTiles x acc) currentDay tiles
                  nextDay = Map.fromList (map (\(x,_) -> (x,0)) blackTiles)

calcDays :: Map (Int,Int) Int -> Int -> Map (Int,Int) Int
calcDays day 0 = day
calcDays day numDays = calcDays (nextDay day) (numDays-1)

main = do
      contents <- readFile "input.txt"
      let lines' = lines contents
      let blackTiles = nub $ concat $ filter (\x -> odd $ length x) $ group $ sort $ map (\x -> followPath x (0,0)) lines'
      let day0 = Map.fromList (map (\x -> (x,0)) blackTiles)
      let p1 = length blackTiles
      let p2 = length $ calcDays day0 100
      print p1
      print p2
      return ()

      

      