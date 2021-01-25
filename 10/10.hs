import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d

adapterChain :: [Int] -> Int -> Int -> (Int,Int)
adapterChain (a:[]) j1 j3           = (j1,j3+1)
adapterChain (a:adapters) j1 j3 = case ((head adapters) - a) of
                                            (1) -> adapterChain adapters (j1+1) j3
                                            (3) -> adapterChain adapters j1 (j3+1)
                                            (_) -> adapterChain adapters j1 j3

-- | iterate  throgh list in reverse order, at each increment calculate possible arrangements using previous calculations
adapterArrangements :: Map Int Int -> [Int] -> Int
adapterArrangements m a | a == [] = get 0 m --end
                        | Map.null m = adapterArrangements (Map.insert key 1 m) (init a) -- init map
                        | otherwise = adapterArrangements (Map.insert key val m) (init a) -- incremental step
               where 
                     get :: Int -> Map Int Int -> Int
                     get v m' = if Map.member v m' then m' Map.! v else 0
                     key = last a
                     val = (get (key+1) m) + (get (key+2) m) + (get (key+3) m)

main = do
      contents <- readFile "input.txt"
      let adapters = (0:(sort $ map stringToInt $ lines contents))
      return ((\x -> (fst x) * (snd x)) $ adapterChain adapters 0 0, adapterArrangements Map.empty adapters)
      