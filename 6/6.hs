import Data.List

parseLines :: String -> String
parseLines (x:[]) = [x]
parseLines ('\n':'\n':xs) = "\n" ++ parseLines xs
parseLines ('\n':xs) = " "++parseLines xs
parseLines (x:xs) = [x] ++ parseLines xs

main = do
      contents <- readFile "input.txt"
      let groups = lines $ parseLines contents
      let summer = foldl (\acc val -> acc + val) 0
      putStr "1 or 2?\n"
      q <- getLine
      if q == "1"
      then return $ summer [length $ foldl (\acc word -> union acc word) (head (words x)) (tail (words x)) | x <- groups]
      else return $ summer [length $ foldl (\acc word -> intersect acc word) (head (words x)) (tail (words x)) | x <- groups]