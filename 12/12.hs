import Data.Char

stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d


rotateFacing :: Char -> Int -> Char
rotateFacing facing degrees = degToFacing $ mod (facingToDeg facing + degrees) 360
                  where facingToDeg f = case f of 'E' -> 0
                                                  'N' -> 90
                                                  'W' -> 180
                                                  'S' -> 270
                        degToFacing d = case d of 0 -> 'E'
                                                  90 -> 'N'
                                                  180 -> 'W'
                                                  270 -> 'S'

performAction :: Char -> Int -> Char -> (Int,Int) -> (Char, (Int,Int))
performAction action units facing (x,y) = case action of 'N' -> (facing,(x,y+units))
                                                         'S' -> (facing,(x,y-units))
                                                         'E' -> (facing,(x+units,y))
                                                         'W' -> (facing,(x-units,y))
                                                         'R' -> (rotateFacing facing (-units),(x,y))
                                                         'L' -> (rotateFacing facing (units),(x,y))
                                                         'F' -> performAction facing units facing (x,y)

followInstructions :: [String] -> Char -> (Int,Int) -> (Int,Int)
followInstructions (inst:instructions) facing (x,y) | instructions == [] = snd res
                                                    | otherwise = followInstructions instructions (fst res) (snd res)
                                                where action = head inst
                                                      units = stringToInt $ tail inst
                                                      res = performAction action units facing (x,y)   

rotateWaypoint :: Char -> (Int,Int) -> Int -> (Int,Int)
rotateWaypoint dir (x,y) 270 | dir == 'R' = rotateWaypoint 'L' (x,y) 90
rotateWaypoint dir (x,y) 270 | dir == 'L' = rotateWaypoint 'R' (x,y) 90
rotateWaypoint dir (x,y) 180 = ((-x),(-y))
rotateWaypoint 'R' (x,y) 90 = ((y),(-x))
rotateWaypoint 'L' (x,y) 90 = ((-y),(x))
rotateWaypoint dir (x,y) _ = ((x),(y))

moveWaypoint :: Char -> Int -> (Int,Int) -> (Int,Int)
moveWaypoint action units (x,y) = case action of 'N' -> (x,y+units)
                                                 'S' -> (x,y-units)
                                                 'E' -> (x+units,y)
                                                 'W' -> (x-units,y)
                                                 _   -> rotateWaypoint action (x,y) units
                                                 

waypointAction :: (Int,Int) -> (Int,Int) -> Char -> Int -> ((Int,Int),(Int,Int))
waypointAction (x,y) (wx,wy) action units | action == 'F' = ((x+wx*units,y+wy*units),(wx,wy))
                                          | otherwise     = ((x,y),moveWaypoint action units (wx,wy))

followWaypoint :: [String] -> (Int,Int) -> (Int,Int) -> (Int,Int)
followWaypoint instructions (x,y) (wx,wy) | instructions == [] = (x,y)
                                          | otherwise = followWaypoint (tail instructions) (fst res) (snd res)
                                          where inst = head instructions
                                                action = head inst
                                                units = stringToInt $ tail inst
                                                res = waypointAction (x,y) (wx,wy) action units
main = do
      contents <- readFile "input.txt"
      let instructions = lines contents
      return $ ((\(x,y) -> abs x + abs y) $ followInstructions instructions 'E' (0,0), (\(x,y) -> abs x + abs y) $ followWaypoint instructions (0,0) (10,1))