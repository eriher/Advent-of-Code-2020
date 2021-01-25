import Data.Char
import Data.Bool
import Data.List

getCoords :: (String, String) -> (Int,Int)
getCoords (r,c) = (row r, col c)
      where row x = foldl (\x y -> if y == 'B' then 1 + 2 * x else 2*x) 0 x
            col x = foldl (\x y -> if y == 'R' then 1 + 2 * x else 2*x) 0 x           

getId :: (Int, Int) -> Int
getId (x,y) = x * 8 + y

findMissing :: [Int] -> Int
findMissing (x:xs) | not (x + 1 == head xs) = x+1
                   | otherwise = findMissing xs

main = do
      contents <- readFile "input.txt"
      let seats = lines contents
      let rows = [take 7 s | s <- seats]
      let cols = [drop 7 s | s <- seats]
      putStr "1 or 2?\n"
      q <- getLine
      if q == "1"
      then return $ foldl (\x y -> if y > x then y else x) 0 [getId coords | x <- zip rows cols, let coords = getCoords x ]
      else return $ findMissing $ sort [getId coords | x <- zip rows cols, let coords = getCoords x ]