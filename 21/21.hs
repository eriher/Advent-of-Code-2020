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

splitInput :: [String] -> [([String],[String])]
splitInput input = zip ingredients allergens
      where allergens = map (splitOn ", " . init . last . splitOn "(contains ") input
            ingredients = map (init . splitOn " " . head . splitOn "(contains ") input


findAllergenCands :: String -> [String] -> Map String [String] -> Map String [String]
findAllergenCands ingredient allergens m = nm
                  where nm = if Map.member ingredient m && length (m Map.! ingredient) > 0
                        then Map.insert ingredient (intersect (m Map.! ingredient) allergens) m
                        else Map.insert ingredient allergens m

determineAllergens  :: [(String,[String])] -> [(String,[String])]
determineAllergens (x:[]) = [x]
determineAllergens (x:xs) = [x] ++ determineAllergens upd
            where allogen = snd x
                  upd = sortBy (\a b -> compare (length $ snd a) (length $ snd b)) $ map (\(y,z)->(y,z \\ allogen)) xs

main = do
      contents <- readFile "input.txt"
      let lines' = lines contents
      let inputs = splitInput lines'

      let ingredients = foldl (\x y -> x ++ (fst y)) [] inputs
      let allergenCandidates = sortBy (\a b -> compare (length $ snd a) (length $ snd b)) $ Map.toList $
                              foldl (\acc (ingr, alle) -> foldl (\acc2 y -> findAllergenCands y ingr acc2) acc alle) Map.empty inputs
      let allergenIngredients = determineAllergens allergenCandidates
      let allergens = foldl (\x y -> x ++ snd y) [] allergenIngredients
      let p1 = length $ filter (\x -> not $ elem x allergens) ingredients
      let p2 = foldl1 (\x y -> x ++ (',':y)) $ concatMap (\x -> snd x) $ sortBy (\a b -> compare (fst a) (fst b)) allergenIngredients
      print p1
      print p2
      return ()

      

      