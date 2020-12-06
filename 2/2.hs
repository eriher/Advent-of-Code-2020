import Data.Char
import Data.Bool
-- stringToInt :: [Char] -> [Int]
-- stringToInt = foldl addDigit 0
--    where addDigit num '-' = [num]
--          addDigit num d = 10*num + digitToInt d
         
extractRange :: [Char] -> [Int]
extractRange c = addDigit 0 c
   where 
         addDigit :: Int -> [Char] -> [Int]
         addDigit num [] = [num]
         addDigit num ('-':cs) = [num] ++ (addDigit 0 cs)
         addDigit num (c:cs) = addDigit (10*num + digitToInt c) cs

--get number of occurence: stringToInt $ (words l) !! 0
--get char: words l !! 1 !! 0
--the string: words l !! 2

checkOccurence :: [Char] -> Int
checkOccurence l = fromEnum $ occurence >= (range !! 0) && occurence <= (range !! 1)
--checkLine l = fromEnum $ range !! occurence
         where block = words l
               range = extractRange $ block !! 0
               token = (block !! 1) !! 0
               pass = block !! 2
               occurence = foldl (\count char -> if char == token then (count + 1) else count) 0 pass

checkPositions :: [Char] -> Int
checkPositions l = fromEnum $ pos1 /= pos2
--checkLine l = fromEnum $ range !! occurence
         where block = words l
               range = extractRange $ block !! 0
               token = (block !! 1) !! 0
               pass = block !! 2
               pos1 = pass !! ((range !! 0) - 1) == token
               pos2 = pass !! ((range !! 1) - 1) == token


main = do
    contents <- readFile "input.txt"
    putStr "1 or 2?\n"
    q <- getLine
    if q == "1"
    then return $ foldl (\acc line -> acc + checkOccurence line) 0 (lines contents)
    else return $ foldl (\acc line -> acc + checkPositions line) 0 (lines contents)