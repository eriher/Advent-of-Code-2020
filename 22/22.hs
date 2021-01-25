import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Deck = [Int]
type Decks = (Deck,Deck)

stringToInt :: [Char] -> Int
stringToInt s = read s::Int

playUntilEnd :: Decks -> Deck
playUntilEnd ([],cards) = cards
playUntilEnd (cards,[]) = cards
playUntilEnd cards = playUntilEnd $ playRound cards

playRound :: Decks -> Decks
playRound ((c1:cs1),(c2:cs2)) | c1 > c2 = (cs1++(c1:[c2]),cs2)
                              | c1 < c2 = (cs1,cs2++(c2:[c1]))

calculateScore :: Deck -> Int
calculateScore [] = 0
calculateScore cards = score + (calculateScore $ tail cards)
            where score = (head cards) * (length cards)

playRecursiveCombat :: Decks -> Set Decks -> Decks
playRecursiveCombat ([],cards) _ = ([],cards)
playRecursiveCombat (cards,[]) _ = (cards,[])
playRecursiveCombat ((c1:cs1),(c2:cs2)) hist | Set.member ((c1:cs1),(c2:cs2)) hist = (cs1++(c1:[c2]),[])
                                             | enterSubGame && (snd subGame) == [] = playRecursiveCombat (cs1++(c1:[c2]),cs2) newHist
                                             | enterSubGame && (fst subGame) == [] = playRecursiveCombat (cs1,cs2++(c2:[c1])) newHist
                                             | otherwise = playRecursiveCombat (playRound ((c1:cs1),(c2:cs2))) newHist
                                             where enterSubGame = (length cs1) >= c1  && (length cs2) >= c2
                                                   subGame = playRecursiveCombat (take c1 cs1, take c2 cs2) Set.empty
                                                   newHist = (Set.insert ((c1:cs1),(c2:cs2)) hist)

main = do
      contents <- readFile "input.txt"
      let inputs = (\x -> (head x, last x)) $ map (map stringToInt . tail . lines) $ splitOn "\n\n" contents
      let p1 = calculateScore $ playUntilEnd inputs
      let p2 = calculateScore $ (\(x,y)->if length x > 0 then x else y) $ playRecursiveCombat inputs Set.empty
      print p1
      print p2
      return ()