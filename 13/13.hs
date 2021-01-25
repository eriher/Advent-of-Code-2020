import Data.Char
import Data.List.Split
import Data.List

stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d


part2 :: [(Integer,Integer)] -> Integer -> Integer -> Integer
part2 (l:ls) start incr | ls == [] = nStart
                        | otherwise = part2 ls nStart nIncr
                  where nums = take 2 [y |x <- [0..],let y = (start+incr*x), mod ((fst l)+y) (snd l) == 0]
                        nStart = head nums
                        nIncr = (last nums) - nStart
                        
main = do
      contents <- readFile "input.txt"
      let lines' = lines contents
      let timeStamp = stringToInt $ head lines'
      let bussIds = [stringToInt x | x <- (splitOn "," (head $tail lines')), x /= "x" ]
      let p1 = (\x -> (fst x)*(snd x)) $ head $ sort (map (\x -> (x - (mod timeStamp x), x)) bussIds)
      
      let bussIds2 = (zip [0..] (splitOn "," (head $ tail lines')))
      let bussIds3 = [(x,toInteger $ stringToInt y) | (x, y) <- bussIds2, y /= "x" ]
      let p2 = part2 bussIds3 1 1
      return (p1,p2)
      

      
      