import Data.Char
import Data.Bool

         
extractRange :: [Char] -> [Int]
extractRange c = addDigit 0 c
   where 
         addDigit :: Int -> [Char] -> [Int]
         addDigit num [] = [num]
         addDigit num ('-':cs) = [num] ++ (addDigit 0 cs)
         addDigit num (c:cs) = addDigit (10*num + digitToInt c) cs



checkOccurence :: [Char] -> Int
checkOccurence l = fromEnum $ occurence >= (range !! 0) && occurence <= (range !! 1)
         where block = words l
               range = extractRange $ block !! 0
               token = (block !! 1) !! 0
               pass = block !! 2
               occurence = foldl (\count char -> if char == token then (count + 1) else count) 0 pass

checkPositions :: [Char] -> Int
checkPositions l = fromEnum $ pos1 /= pos2
         where block = words l
               range = extractRange $ block !! 0
               token = (block !! 1) !! 0
               pass = block !! 2
               pos1 = pass !! ((range !! 0) - 1) == token
               pos2 = pass !! ((range !! 1) - 1) == token


main = do
    contents <- readFile "input.txt"
    let p1 = foldl (\acc line -> acc + checkOccurence line) 0 (lines contents)
    print $ p1
    let p2 = foldl (\acc line -> acc + checkPositions line) 0 (lines contents)
    print $ p2
    return ()