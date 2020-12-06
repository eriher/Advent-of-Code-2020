import Data.Set (Set)  
import qualified Data.Set as Set
import Data.Char

stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d

--check if 2020 - x exits in set, if exists return x * (2020 - x), otherwise insert x
checkAndInsert :: Int -> [Int] -> Set Int -> Int
checkAndInsert val [] set = 0
checkAndInsert val (i:is) set | Set.member (val-i) set = i * (val - i)
checkAndInsert val (i:is) set = checkAndInsert val is (Set.insert i set)

checkPairs  :: [Int] -> Int
checkPairs ints = checkAndInsert 2020 ints Set.empty

checkTriples :: [Int] -> Int
checkTriples [] = 0
checkTriples (i:ints) | val > 0 =  i * val
                      | otherwise = checkTriples ints
            where val = checkAndInsert (2020 - i) ints Set.empty

main = do
    contents <- readFile "input.txt"
    putStr "1: Pair or 2: triple?\n"
    q <- getLine
    if q == "1"
    then return $ checkPairs  [ stringToInt x | x <- lines contents]
    else return $ checkTriples  [ stringToInt x | x <- lines contents]