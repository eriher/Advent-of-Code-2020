import Data.List
import Data.Maybe (fromJust)
import Data.List.Split
import Data.Char
import Data.Function

-- | Node, made of String label and list of (Integer,Node) pairs
data Node = Node
    { label    :: String
    , adjacent :: [(Int, Node)]
    }
    deriving (Show,Eq)

-- | Graph, made of list of nodes
data Graph = Graph [Node]
    deriving (Show)

-- | mkGraph creates a graph representaion of provided string
mkGraph :: [(String, [String])] -> Graph
mkGraph links = Graph $ map snd nodeLookupList where
    
    mkNode (lbl, adjacent) = (lbl, Node lbl $ map (\(x:y) -> (digitToInt x, lookupNode y)) adjacent)
    
    nodeLookupList = map mkNode links
    
    lookupNode lbl = fromJust $ lookup lbl nodeLookupList

-- | findNode, searches Graph for a Node with label matching String
findNode :: Graph -> String -> Maybe Node
findNode (Graph []) _ = Nothing 
findNode (Graph (Node n s:ns)) l |    n == l = Just (Node n s)
                                 | otherwise = findNode (Graph ns) l

-- | bagCount, given a bags adjecency list, counts the number of bags that can be contained in that bag
bagCount :: [(Int, Node)] -> Int
bagCount [] = 1
bagCount ((i,n):xs) = i * (bagCount $ adjacent n) + (bagCount xs)

-- | fitsIn, given a bags adjecency list, returns all unique bags that bag fits in
fitsIn :: [(Int, Node)] -> [String]
fitsIn [] = []
fitsIn ((i,x):xs) = nub ([label x] ++ (fitsIn $ adjacent x) ++ (fitsIn xs))


main = do
      contents <- readFile "input.txt"
      --read contents, split into lines
      let bags = lines $ contents
      --parsing to get a workable format. [parent, children..]
      let l = [init $ splitOn "," $ concat [if isInfixOf "bag" z then  "," else z | z <- words bag, z /= "contain"] | bag <- bags]
      --parent -> child map. each child prepended with related Int. [parent, [children]]
      let cMap = [(head q, if isDigit (head $ head $ tail q) then map (\x -> x) (tail q) else [])| q <- l]
      --child -> parent map. each parent prepended with dummy Int(since not used). [child, [parent]]
      let pMap = map (\x -> (fst $ head x, foldl (\acc y -> acc ++ snd y) [] x)) $ groupBy ((==) `on` fst) $ sort $ concatMap (\(x,y) -> (x,[]) : map (\z -> (tail z,["0"++x])) y) cMap
      --graphs for each task
      let g1 = mkGraph pMap
      let g2 = mkGraph cMap
      --first task, how many bags does the shiny gold bag fit into? Solved by using child to parent graph and dfs
      --second task, how many bags fit into the shiny gold bag?  Solved by using parent to child graph and dfs
      return (length $ fitsIn $ adjacent $ fromJust $ findNode g1 "shinygold", bagCount (adjacent $ fromJust $ findNode g2 "shinygold") - 1)
      