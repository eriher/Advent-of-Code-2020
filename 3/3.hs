import Data.Char
import Data.Bool

checkTree :: Int -> [Char] -> Int
checkTree idx line = (fromEnum $ line!!(mod idx (length line)) == '#')

main = do
      contents <- readFile "input.txt"
      let slope = lines contents
      putStr "1 or 2?\n"
      q <- getLine
      if q == "1"
      then return $ foldl (*) 1 [foldl (\acc (idx,line) -> acc + checkTree idx line) 0 sl | sl <- [zip [i*r|i<-[1..]] [slope !! (i*d)|i<-[1..(div (length slope-1) d)]]  | (r,d) <- [(3,1)]]]
      else return $ foldl (*) 1 [foldl (\acc (idx,line) -> acc + checkTree idx line) 0 sl | sl <- [zip [i*r|i<-[1..]] [slope !! (i*d)|i<-[1..(div (length slope-1) d)]]  | (r,d) <- [(1,1),(3,1),(5,1),(7,1),(1,2)]]]
      
      --return $ [ foldl (\acc (idx,line) -> acc + checkTree idx line) 0 sl | sl <- [zip [i*r|i<-[1..]] [slope !! (i*d)|i<-[1..(div (length slope) d)]]  | (r,d) <- [(1,1),(3,1),(5,1),(5,1),(1,2)]]]
      --return $ [ foldl (\acc (idx,line) -> acc + checkTree idx line) 0 sl | sl <- [zip [i*r|i<-[1..]] [slope !! i*d|i<-[1..(length slope)]]  | (r,d) <- [(1,1),(3,1),(5,1),(5,1),(1,2)]]]
      --return $ [zip [r*i|i <- [1..]] [slope !! (d*i)|i <- [1..(length slope)]] | (r,d) <- [(1,1),(3,1),(5,1),(5,1),(1,2)]] 
      --return $ [foldl (\acc (idx, line) -> acc + checkTree idx line) 0 (zip [r*i|i <- [1..]] [slope !! (d*i)|i <- [1..(length slope)]]) | (r,d) <- [(1,1),(3,1),(5,1),(5,1),(1,2)]] 
      --return $ [foldl (\acc (idx, line) -> acc + (fromEnum $ line!!(mod idx (length line)) == '#')) 0 (zip [r*i|i <- [1..]] [l | contents !! i <- [1..(length contents)]]) | (r,d) <- [(1,1),(3,1),(5,1),(5,1),(1,2)]] 
    --return $ doIt (tail (lines contents)) 1
--     putStr "1 or 2?\n"
--     q <- getLine
--     if q == "1"
--     then return $ foldl (\acc line -> acc + checkOccurence line) 0 (lines contents)
--     else return $ foldl (\acc line -> acc + checkPositions line) 0 (lines contents)