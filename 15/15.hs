import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.ST
import Data.STRef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d

--need current index and current number, check in map when it last occured, take diff and update map with new idx,
--p1 :: Integer -> Integer -> IntMap Integer Integer -> Integer -> Integer
game val idx m num | idx == num = val
                   | otherwise = game nVal (idx+1) nM num
                  where --first check if val has been spoken before
                        mVal = IntMap.lookup val m
                        nVal = if isJust mVal then (idx - fromJust mVal)  else 0
                        --update map with the index that 
                        nM   = IntMap.insert val idx m

main = do
      contents <- readFile "input.txt"
      let lines' = map (stringToInt) $ splitOn "," $ head $ lines contents
      let startMap = IntMap.fromList $ zip (lines') [1..]
      --return startMap
      let p1 =  game 0 (((length lines')+1)) startMap 2020
      let p2 =  game 0 (((length lines')+1)) startMap 30000000
      print p1
      print p2
      --print foo
      return ()
      


      
      