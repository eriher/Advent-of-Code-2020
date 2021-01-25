import Data.Char
import Data.Bool
import Data.List
checkTree :: Int -> [Char] -> Int
checkTree idx line = (fromEnum $ line!!(mod idx (length line)) == '#')

checkPath = product . map (\sl -> sum $ map (\(idx,line) -> checkTree idx line) sl)
createPath slope = map (\(r,d) -> zip [i*r | i <- [1..]] [slope !! (i*d) | i <- [1..(div (length slope-1) d)]])


main = do
      contents <- readFile "input.txt"
      let slope = lines contents
      let p1 = checkPath $ createPath slope [(3,1)]
      let p2 = checkPath $ createPath slope [(1,1),(3,1),(5,1),(7,1),(1,2)]
      print p1
      print p2
      return ()
      