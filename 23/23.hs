import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad

stringToInt :: [Char] -> Int
stringToInt s = read s::Int

getLower 0 res removed = getLower res res removed
getLower val res removed | not $ elem val removed = val
                         | otherwise = getLower (val-1) res removed

pairNext (y:[]) = []
pairNext (x:y:ls) = ((x,y):(pairNext (y:ls)))


moveCups cups = do
                  current <- MVec.read cups 0
                  picked1 <- MVec.read cups current
                  picked2 <- MVec.read cups picked1
                  picked3 <- MVec.read cups picked2
                  current' <- MVec.read cups picked3
                  let target = (getLower (current-1) ((MVec.length cups) - 1) [picked1,picked2,picked3])
                  nextTo <- MVec.read cups target
                  MVec.write cups current current'
                  MVec.write cups picked3 nextTo
                  MVec.write cups target picked1
                  MVec.write cups 0 current'
                  return ()


part2 :: [(Int,Int)] -> Int -> IO ()
part2 cups turns = do 
                        cups' <- MVec.replicate (length cups) (0 :: Int)
                        forM_ cups $ \(i, j) -> MVec.write cups' i j
                        forM_ [1..turns] (\_ -> moveCups cups')
                        cup2 <- MVec.read (cups') 1
                        cup3 <- MVec.read (cups') cup2
                        print (cup2*cup3)


part1 :: [(Int,Int)] -> Int -> IO ()
part1 cups turns = do 
                        cups' <- MVec.replicate (length cups) (0 :: Int)
                        forM_ cups $ \(i, j) -> MVec.write cups' i j
                        forM_ [1..turns] (\_ -> moveCups cups')
                        f <- Vec.freeze cups'
                        let idx = Vec.toList f
                        let result = reverse $ init $ foldl (\acc v -> ((idx !! (head acc)):acc)) [1] [0..(length idx-3)]
                        putStrLn $ map (intToDigit) result


main = do
      contents <- readFile "input.txt"
      let input = map (digitToInt) contents

      let pairedInput = (0,head input):pairNext input
      
      let inputP1 = map (snd) $ sort ((last input, head input):pairedInput)

      let input2 = map (snd) $ sort ((last input, length input+1):pairedInput)
      let inputP2 = input2 ++ [(length input2+1)..1000000] ++ [head input]

      part1 (zip [0..] inputP1) 100
      part2 (zip [0..] inputP2) 10000000

      return ()