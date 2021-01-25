
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Seat = Empty | Taken | Floor
      deriving (Eq, Ord, Show)

type Cord = (Int,Int)
type Seats = Map Cord Seat

charToSeat :: Char -> Seat
charToSeat 'L' = Empty
charToSeat '.' = Floor
charToSeat '#' = Taken

neighbours :: Cord -> Seats -> [Seat]
neighbours pos seats = foldl (\acc v -> if Map.member v seats then ((seats Map.! v):acc) else acc) [] pos'
            where pos' = [(\(a,b) -> (a+x,b+y)) pos | (x,y) <- directions]

directions = delete (0,0) [(x,y) | x <- [-1..1], y <- [-1..1]]

occupiedNeighbours :: Cord -> Seats -> Int
occupiedNeighbours pos seats = sum $ map (\x -> fromEnum $ x == Taken) $ neighbours pos seats

updateSeat :: Seat -> Int -> Seat
updateSeat Empty 0  = Taken
updateSeat Taken n | n > 3 = Empty
updateSeat state _ = state

runUntilStable :: Seats -> Seats
runUntilStable seatMap | seatMap == seatMap' = seatMap
                       | otherwise = runUntilStable seatMap' 
                       where seatMap' = Map.mapWithKey (\key val ->  updateSeat val $ occupiedNeighbours key seatMap) seatMap

losSeat :: Cord -> Seats -> Cord -> Int
losSeat (r,c) seats (incR,incC) | (not $ Map.member (newR,newC) seats) || seat == Empty = 0
                                | seat == Taken = 1
                                | otherwise = losSeat (newR,newC) seats (incR,incC)
                              where newR = r+incR
                                    newC = c+incC
                                    seat = seats Map.! (newR,newC)
                                    seatFound = seat /= Floor

occupiedLos :: Cord -> Seats -> Int
occupiedLos pos seats = sum $ map (\x -> losSeat pos seats x) directions

updateSeat2 :: Seat -> Int -> Seat
updateSeat2 Empty 0  = Taken
updateSeat2 Taken n | n > 4 = Empty
updateSeat2 state _ = state

runUntilStable2 :: Seats -> Seats
runUntilStable2 seatMap | seatMap == seatMap' = seatMap
                       | otherwise = runUntilStable2 seatMap' 
                       where seatMap' = Map.mapWithKey (\key val ->  updateSeat2 val $ occupiedLos key seatMap) seatMap

main = do
      contents <- readFile "input.txt"
      let seats = lines contents
      let seatMap = foldl (\acc (x,row) -> foldl (\acc2 (y,v) -> Map.insert (x,y) (charToSeat v) acc2) acc (zip [0..] row)) Map.empty (zip [0..] seats)

      putStrLn $ show $ length $ Map.filter (==Taken) $ runUntilStable seatMap
      putStrLn $ show $ length $ Map.filter (==Taken) $ runUntilStable2 seatMap
      return ()
