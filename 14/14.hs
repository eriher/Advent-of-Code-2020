import Data.Char
import Data.List.Split
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d

decToBin :: Int -> String -> String
decToBin inp str | inp == 0 = str
                 | otherwise = decToBin npt nstr
                 where setBit = truncate (logBase 2 (fromIntegral inp))
                       npt    = inp - (2^setBit)
                       len = ((length str)-setBit)
                       nstr = (take (len-1) str) ++ "1" ++ (replicate (setBit) '0')

binToDec :: String -> Int
binToDec str = sum [2^y | (x,y) <- zip str (reverse [0..((length str) - 1)]), x == '1']

applyMask :: String -> String -> String
applyMask mask bin = [z | (x,y) <- zip mask bin, let z = if x == 'X' then y else x ]

part1 :: [String] -> String -> Map Int String -> Map Int String
part1 [] _ mem = mem
part1 (x:xs) mask mem | isPrefixOf "mask" x = part1 xs (snd (splitAt 7 x)) mem
                      | otherwise = part1 xs mask newMem
                      where   value = (splitOn "] = " x)
                              memId = stringToInt $ last $ splitOn "mem[" (head value)
                              memBin = decToBin (stringToInt (last value)) (replicate 36 '0')
                              maskedBin = applyMask mask memBin
                              newMem  = (Map.insert memId maskedBin mem)

test [] res = map binToDec res
test str res | curr == 'X' = test (init str) [(y:x) | x <- res, y <- ['0','1']]
             | otherwise = test (init str) (map (\x -> (curr:x)) res)
             where curr = last str

applyAdressMask :: String -> String -> [Int]
applyAdressMask mask bin = test masked [""]
                  where masked = [z | (x,y) <- zip mask bin, let z = if x == '0' then y else x ]

part2 :: [String] -> String -> Map Int String -> Map Int String
part2 [] _ mem = mem
part2 (x:xs) mask mem | isPrefixOf "mask" x = part2 xs (snd (splitAt 7 x)) mem
                      | otherwise = part2 xs mask newMem
                      where value = (splitOn "] = " x)
                            memId = stringToInt $ last $ splitOn "mem[" (head value)
                            memIds = applyAdressMask mask (decToBin memId (replicate 36 '0'))
                            memBin = decToBin (stringToInt (last value)) (replicate 36 '0')
                            newMem  = foldl (\mem' key -> Map.insert key memBin mem') mem memIds

main = do
      contents <- readFile "input.txt"
      let lines' = lines contents
      let p1 = Map.foldl (\acc v -> acc + binToDec v) 0 $ part1 lines' "" Map.empty
      let p2 = Map.foldl (\acc v -> acc + binToDec v) 0 $ part2 lines' "" Map.empty
      print p1
      print p2
      return ()
      


      
      