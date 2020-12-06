import Data.Char
import Data.Bool
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List


stringToInt :: String -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d

checkLengthRange :: String -> Int -> Int -> Int -> Bool
checkLengthRange s l lb ub = length s == l && n >= lb && n <= ub
                    where n = stringToInt s

checkEyeColor :: String -> Bool
checkEyeColor s | elem s ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] = True      
checkEyeColor _ = False

checkHeight :: String -> Bool
checkHeight s | unit == "cm" = l >= 150 && l <= 193
              | unit == "in" = l >= 59 && l <= 76
              | otherwise = False
                  where d    = (length s - 2)
                        unit = drop d s;
                        l    = stringToInt $ take d s

checkPassport :: String -> Int
checkPassport line = fromEnum $ byr && iyr && eyr && hgt && hcl && ecl && pid
          where fieldMap = Map.fromList $ map (\arr -> (arr!!0,tail $ arr!!1)) $ map (\word -> groupBy (\a b -> b /= ':' ) word) $ words $ line
                byr = Map.member "byr" fieldMap && checkLengthRange (fieldMap Map.! "byr") 4 1920 2002
                iyr = Map.member "iyr" fieldMap && checkLengthRange (fieldMap Map.! "iyr") 4 2010 2020
                eyr = Map.member "eyr" fieldMap && checkLengthRange (fieldMap Map.! "eyr") 4 2020 2030
                hgt = Map.member "hgt" fieldMap && checkHeight (fieldMap Map.! "hgt")
                hcl = Map.member "hcl" fieldMap && (\x -> x !! 0 == '#' && (length $ tail x) == 6 && foldl(\acc x -> acc && elem x (['0'..'9']++['a'..'f'])) True (tail x) ) (fieldMap Map.! "hcl")
                ecl = Map.member "ecl" fieldMap && checkEyeColor (fieldMap Map.! "ecl")
                pid = Map.member "pid" fieldMap && checkLengthRange (fieldMap Map.! "pid") 9 0 999999999

parseLines :: String -> String
parseLines (x:[]) = [x]
parseLines ('\n':'\n':xs) = "\n" ++ parseLines xs
parseLines ('\n':xs) = " "++parseLines xs
parseLines (x:xs) = [x] ++ parseLines xs

main = do
      contents <- readFile "input.txt"
      return $ foldl (\acc line -> acc + checkPassport line) 0 (lines $ parseLines contents)
