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

lineMatch :: String -> String -> Bool
lineMatch l1 l2 = check1 || check2
            where check1 = l1 == l2
                  check2 = l1 == (reverse l2)

getEdgeTiles :: Int -> Map Int [[Int]] -> [Int]
getEdgeTiles id tileMap | nextId == [] = (id:(filter (\x->length (concat $ tileMap Map.! x) == 2) $ ((concat $ tileMap Map.! id))))
                        | otherwise = (id:(getEdgeTiles (head nextId) newMap))
      where 
            nextId = filter (\x->(Map.member x tileMap) && length (concat $ tileMap Map.! x) == 3) $ ((concat $ tileMap Map.! id))
            newMap = Map.insert (head nextId) ((tileMap Map.! (head nextId)) \\ [[id]]) tileMap

getRow :: Int -> Map Int [[Int]] -> ([Int], Map Int [[Int]])
getRow id tileMap | filter2 == [] = ([id,lastId], (Map.delete id (Map.delete lastId tileMap)))
                  | otherwise   = (\(ids,m) -> ((id:ids),m)) $ getRow (head filter2) (Map.delete id tileMap)
            where 
                  filter1 = filter (\x -> Map.member x tileMap && length (concat $ tileMap Map.! x) == 4) (concat $ tileMap Map.! id)
                  filter2 = filter (\x ->(sum $ map (\y ->fromEnum $ Map.member y tileMap) (concat $ tileMap Map.! x)) == 3 ) filter1
                  lastId = last $ filter (\x->Map.member x tileMap && length (concat $ (tileMap Map.! x)) == 3) $ concat $ tileMap Map.! id

rotateBlock :: [String] -> String -> [String]
rotateBlock block "left"  = rotateLeft block []
rotateBlock block "right" = rotateRight block []

rotateRight :: [String] -> [String] -> [String]
rotateRight block [] = rotateRight (tail block) (transpose [head block])
rotateRight [] block = block
rotateRight block newBlock = rotateRight (tail block) buildingBlock
                        where buildingBlock = map (\x -> [fst x] ++ (snd x)) $ zip (head block) newBlock

rotateLeft :: [String] -> [String] -> [String]
rotateLeft block [] = rotateLeft (tail block) (transpose [reverse $ head block])
rotateLeft [] block = block
rotateLeft block newBlock = rotateLeft (tail block) buildingBlock
                        where buildingBlock = map (\x -> (snd x) ++ [fst x]) $ zip (reverse $ head block) newBlock

flipBlock :: [String] -> [String]
flipBlock = map (\x-> reverse x)

printBlock block = mapM_ print $ block

getEdges :: [String] -> [String]
getEdges block = [head block,last block, head (reverse $ transpose block), last (reverse $ transpose block)]

rotateUntilValid :: [[Int]] -> (Int,Int) -> Map Int [String] -> Map Int [String]
rotateUntilValid layout (r,c) blockMap | top && bot && left && right = blockMap
                                       | top && bot = rotateUntilValid layout (r,c) $ Map.insert blockId (flipBlock block) blockMap
                                       | otherwise = rotateUntilValid layout (r,c) blockMap'
                  where blockId = layout !! r !! c
                        block = blockMap Map.! blockId
                        top = r == 0 || (or $ map (lineMatch (head block)) (getEdges $ blockMap Map.! (layout !! (r-1) !! c)))
                        bot = r == (length layout - 1) || (or $ map (lineMatch (last block)) (getEdges $ blockMap Map.! (layout !! (r+1) !! c)))
                        left = c == 0 || (or $ map (lineMatch (head $ transpose block)) (getEdges $ blockMap Map.! (layout !! (r) !! (c-1))))
                        right = c == ((length $ head layout) - 1) || (or $ map (lineMatch (last $ transpose block)) (getEdges $ blockMap Map.! (layout !! (r) !! (c+1))))
                        blockMap' = Map.insert blockId (rotateBlock block "right") blockMap

removeEdges :: [String] -> [String]
removeEdges block = transpose $ init $ tail $ transpose $ init $ tail $ block


checkAlignments image | numMonsters > 0 = numMonsters * monsterHash
                      | numMonsters2 > 0 = numMonsters2 * monsterHash
                      | otherwise = checkAlignments (rotateBlock image "right")
                  where patterns = [[18],[0,5,6,11,12,17,18,19],[1,4,7,10,13,16]]
                        numMonsters = sum [checkSeaMonster (take 3 $ drop idx image) patterns 0 |idx <- [0..(length image -3)]]
                        flippedImage = flipBlock image
                        numMonsters2 = sum [checkSeaMonster (take 3 $ drop idx flippedImage) patterns 0 |idx <- [0..(length flippedImage -3)]]
                        monsterHash = foldl (\acc x -> acc+(length x)) 0 patterns


checkSeaMonster :: [String] -> [[Int]] -> Int -> Int
checkSeaMonster [l1,l2,l3] [p1,p2,p3] counter | length l1 < 20 = counter
                                              | check1 && check2 && check3 = checkSeaMonster [tail l1,tail l2,tail l3] [p1,p2,p3] (counter+1)
                                              | otherwise = checkSeaMonster [tail l1,tail l2,tail l3] [p1,p2,p3] counter
                                    where check1 = foldl (\acc idx -> acc && l1 !! idx == '#') True p1
                                          check2 = foldl (\acc idx -> acc && l2 !! idx == '#') True p2
                                          check3 = foldl (\acc idx -> acc && l3 !! idx == '#') True p3

main = do
      contents <- readFile "input.txt"
      let lines' = map lines (splitOn "\n\n" contents)
      let tiles = map (\x -> ((stringToInt $ init $ last $ splitOn "Tile " (head x)),tail x)) $ lines'
      let tileMap = Map.fromList tiles

      let edges = map (\(x,y) -> (x,getEdges y)) tiles 
      let edgeMap = Map.fromList edges
      let matchings = map (\current -> (fst current, 
                              map (\x -> [oId | (oId,ol) <- (edges \\ [current]), or $ map (lineMatch x) ol]) $ snd current)) edges  
      let matchinMap = (Map.fromList matchings)
      let corners = filter (\(id,matches)-> (sum $ map (\x -> fromEnum $ length x > 0) matches) == 2) matchings
      -- p1 answer only required corners.
      let p1 = foldl1 (*) $ map (\(id,_) -> id) corners

      -- p2 requires actually constructing the image, matchings has info about how everything is connected but not proper alignment
      -- choose 1 corner as starting point and align everything from it row by row
      let startCorner = head corners
      let startId = fst startCorner
      let startNeighbours = concat $ snd startCorner
      let startRow = head startNeighbours
      let startCol = last startNeighbours

      --Treating first and last row special cases
      let firstRow = startId:(getEdgeTiles startRow (Map.insert startRow ((matchinMap Map.! startRow) \\ [[startId]]) matchinMap))
      let lastRow = 1543:(getEdgeTiles 2707 matchinMap)

      let col = getEdgeTiles startCol (Map.insert startCol ((matchinMap Map.! startCol) \\ [[startId]]) matchinMap)
      -- removed the visited tiles from map, i.e. first row
      let midMap = foldl (\acc x -> Map.delete x acc) matchinMap (firstRow)
      let midRows = fst $ foldl (\(list,m) curr -> (\(result,nm) -> ([result]++list,nm)) $ getRow curr m) ([],midMap) $ init col

      let rows = [lastRow]++midRows++[firstRow]

      let properlyAligned = foldl (\acc x -> rotateUntilValid rows x acc) tileMap [(x,y)|x<-[0..11],y<-[0..11]]
      
      let actualImage = concatMap (\row -> foldl1 (\x y -> map (\(a,b)->a++b) (zip x y)) row) $ map (\x -> map (\y -> removeEdges $ properlyAligned Map.! y) x) rows
      
      let allHashes = foldl (\acc x -> acc + (sum $ map (\y -> fromEnum $ y =='#') x)) 0 actualImage
      let monsterHashes = checkAlignments actualImage
      let p2 = allHashes - monsterHashes
      print p1
      print p2
      return () 
      
      
      

      