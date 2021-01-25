import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

stringToInt :: [Char] -> Integer
stringToInt s = read s::Integer


findLoops number val loops | res == number = loops
                           | otherwise = findLoops number res (loops+1)
                            where res = mod (val * 7) 20201227

getSecret subj loops = mod (subj^loops) 20201227

main = do
      contents <- readFile "input.txt"
      let lines' = map stringToInt $ lines contents
      print $ getSecret (last lines') (findLoops (head lines') 1 1)
      return ()