import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

stringToInt :: [Char] -> Int
stringToInt s = read s::Int

splitInput :: [String] -> [String] -> ([String], [String])
splitInput p1 (p:p2) | p == "" = (p1,p2)
                     | otherwise = splitInput (p1++[p]) p2

deriveRules::[Int] -> Map Int [[Int]] -> Map Int [String] -> Map Int [String]
deriveRules [] m1 m2 = m2
deriveRules (key:keys) m1 m2 | not check = deriveRules (keys++[key]) m1 m2
                             | otherwise = deriveRules keys nM1 nM2                    
                where v = (m1 Map.! key)
                      vv = map (\x -> [deriveRules y m1 m2 |y<-x])
                      check = and $ concatMap (\x -> map (\y -> Map.member y m2) x) v
                      nM1 = (Map.delete key m1)
                      nM2 = Map.insert key (concatMap (foldl1 (\acc x -> [a++y | a <- acc, y <- x])) (map (\z -> map (m2 Map.!) z) v)) m2

repeatingRule :: Int -> Int -> [String] -> Set String -> [[String]]
repeatingRule numChars depth strings rules | next == [] = [this]
                                            | otherwise = [this] ++ repeatingRule numChars (depth+1) next rules                  
                              where this = [s | s <- strings, let x = take numChars $ drop (numChars*depth) s, Set.member x rules]
                                    next = filter (\x -> length x > numChars + (numChars*(depth+1))) this

sandwichedRule :: (Int, [String]) -> Int -> Set String -> Set String -> [String]
sandwichedRule (i,strings) depth rl rr | next == [] = this
                                       | otherwise = this ++ (sandwichedRule (i,next) (depth+1) rl rr)
                           where filtered = filter (\x -> length x == (i*8)+(16*depth)) strings
                                 this     = [s | s <- filtered, let x = (\(b,e) -> (chunker b 8, chunker e 8)) (splitAt (8*depth) $ drop (i*8) s),
                                                 (and $ map (\y->Set.member y rl) (fst x)) && (and $ map (\y->Set.member y rr) (snd x))]
                                 next = filter (\x -> length x > (i*8)+(16*depth)) strings

chunker :: String -> Int -> [String]
chunker str num | length str == num = [str]
chunker str num = [take num str] ++ chunker (drop num str) num

main = do
      contents <- readFile "input.txt"
      let lines' = splitInput [] (lines contents)
      let rules = map (\x -> (\y->(stringToInt $ head y, last y)) $ splitOn ": " x) (fst lines')
      let inputs = (snd lines')
      let baseRules = Map.fromList $ map (\(x,y) -> (x,[[c | c <- y, elem c ['a'..'z']]])) $ filter (\x -> not $ isDigit $ head $ snd x) rules
      let derivRules = Map.fromList $ map (\(x,y) -> (x,map (\z ->map stringToInt $ splitOn " " z) $ splitOn " | " y)) $ filter (\x -> isDigit $ head $ snd x) rules
      let ruleMap = (deriveRules (Map.keys derivRules) derivRules baseRules)
      let r1 = Set.fromList $ ruleMap Map.! 8
      let s1 = (length $ Set.elemAt 0 r1)
      let filteredByR1 = repeatingRule s1 0 inputs r1
      let filteredIndexed = (zip [1..] filteredByR1)      

      let r2 = Set.fromList $ ruleMap Map.! 11
      let r3 = Set.fromList $ ruleMap Map.! 42
      let r4 = Set.fromList $ ruleMap Map.! 31
      let p1 = length $ head $ map (\(i,x) -> filter (\y -> Set.member (drop (i*s1) y) r2) x) $ take 1 filteredIndexed
      let p2 = length $ Set.fromList $ concatMap (\x ->sandwichedRule x 1 r3 r4) $ filteredIndexed
      print p1
      print p2
      return ()
