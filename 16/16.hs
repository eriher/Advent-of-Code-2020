import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d

checkRanges :: [Int] -> [((Int,Int),(Int,Int))] -> [Int] -> [Int]
checkRanges [] ranges acc = acc
checkRanges (field:ticket) ranges acc | fieldCheck = checkRanges ticket ranges acc
                                      | otherwise = checkRanges ticket ranges (field:acc)
                                      where fieldCheck = or [v | ((l1,h1), (l2,h2)) <- ranges, let v = (field >= l1 && field <= h1) || (field >= l2 && field <= h2)]

checkRanges2 :: [(Int,Int)] -> [((Int,Int),(Int,Int))] -> Map Int [Int] -> Map Int [Int]
checkRanges2 [] _ rangeMap = rangeMap
checkRanges2 ((idx,field):tickets) ranges rangeMap  = checkRanges2 tickets ranges nRangeMap
                                      where fieldCheck = [i | (i, ((l1,h1), (l2,h2))) <- zip [0..] ranges, (field < l1 || field > h1) && (field < l2 || field > h2)]
                                            nRangeMap =  Map.insert idx ((rangeMap Map.! idx) \\ fieldCheck) rangeMap

main = do
      contents <- readFile "input.txt"
      let lines' = lines contents
      let wut = splitOn "\nyour ticket:\n" contents
      let ranges = map (\x -> (head x, last x)) . map (map (\x -> (stringToInt $ head x, stringToInt $ last x)) . map (splitOn "-") . (splitOn " or ") .last . splitOn ": ") $ lines $ head $ wut
      let myTicket = map stringToInt $ splitOn "," $ head $ splitOn "\n\nnearby tickets:\n" (last wut)
      let nearbyTickets = map (\x -> map stringToInt ((splitOn ",") x)) $ lines $ last $ splitOn "\n\nnearby tickets:\n" (last wut)
      
      --P1, accumulate the invalid fields and sum them
      let res1 = sum $ concat [ checkRanges x ranges [] | x <- nearbyTickets]

      --P2, using a map of field->potential ranges, where we start with each field potentially being valid with every range. As each ticket is processed we can elimate potential ranges.
      let validTickets = [ x | x <- nearbyTickets, (length (checkRanges x ranges [])) == 0]
      let fieldToRangeMap = (Map.fromList [(x,[0..((length ranges)-1)]) | x <- [0..((length ranges)-1)]])
      --Reduce the fieldToRangeMap using the valid tickets
      let reducedfieldMap = sortBy (\x y -> compare (length (snd x)) (length (snd y))) $ Map.toList $ foldl (\acc ticket -> checkRanges2 (zip [0..] ticket) ranges acc) fieldToRangeMap (myTicket:validTickets)
      --Eliminate duplicate ranges between fields
      let reduce2 = sort . fst $ foldl (\(acc1,acc2) (x,y) -> (((x,y\\acc2):acc1),((y\\acc2)++acc2))) ([],[]) reducedfieldMap
      --Determine valid departure fields and extract their values, finally multiplying them
      let departureIndxs = map (\x -> fst x) . filter (\x -> "departure" `isPrefixOf` (snd x)) $ zip [0..] (lines $ head wut)
      let departureFields = sort . map (\x -> fst x) $ filter (\x -> elem (head $ snd x) departureIndxs) reduce2
      let res2 = foldl1 (*) [v | (idx,v) <- zip [0..] myTicket, elem idx departureFields]
      print res1
      print res2
      return ()
      
      