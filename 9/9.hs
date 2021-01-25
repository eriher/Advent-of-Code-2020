import Data.Char
import Data.List

stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d

isSumOf :: Int -> [Int] -> Bool
isSumOf _ []          = False
isSumOf val (t:terms) = (val - t) `elem` terms || isSumOf val terms

findFirstNonSum :: [Int] -> [Int] -> Int
findFirstNonSum prev (curr:rest) | isSumOf curr prev = findFirstNonSum ((tail prev) ++ [curr]) rest
                                 | otherwise =  curr

findContinousSum :: Int -> [Int] -> Int
findContinousSum num terms | csIdx > 0 = smallest + largest
                         | otherwise = findContinousSum num (tail terms)
                where csIdx    = isContinousSumOf num terms
                      csSorted = sort $ take csIdx terms
                      smallest = head csSorted
                      largest  = last csSorted

isContinousSumOf :: Int -> [Int] -> Int
isContinousSumOf val (t:terms) | val == t = 0
                             | (val - t > 0) = 1 + isContinousSumOf (val-t) terms
                             | otherwise = (-val)

main = do
      contents <- readFile "input.txt"
      let numbers = map stringToInt $ lines contents
      let preamble = take 25 numbers
      let xmas = drop 25 numbers
      let noSumNum = findFirstNonSum preamble xmas

      return (noSumNum, findContinousSum noSumNum numbers)

      