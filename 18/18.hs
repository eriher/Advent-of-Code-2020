import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

type ParserFun a = String -> Maybe (a,String)

num :: ParserFun Integer
num s = case span isDigit s of
          ("",_)    -> Nothing
          (ds,rest) -> Just(read ds, rest)

addition :: ParserFun Integer
addition s = case num s of
            Just(n,'+':rest) -> case num rest of
                                    Just (m, rest') -> Just (n + m, rest')
                                    Nothing         -> addition (((show n)++"+") ++ parse rest)
            _                -> Nothing

multiplication :: ParserFun Integer                                          
multiplication s = case num s of
            Just(n,'*':rest) -> case num rest of
                              Just (m, rest') -> Just (n * m, rest')
                              Nothing         -> multiplication (((show n)++"*") ++ parse rest)
            _                -> Nothing

split' :: String -> String -> Int -> (String,String)
split' str1 (curr:str2) cnt | curr == '(' = split' (str1++[curr]) str2 (nCnt+1)
                            | nCnt == 0   = (str1,str2)
                            | match = split' (str1++[curr]) str2 nCnt
                            | otherwise   = split' (str1++[curr]) str2 nCnt
                            where match = curr == ')'
                                  nCnt  = cnt - fromEnum match
paran :: String -> String                                                      
paran s = case num s of
         Just(n,')':rest) -> ((show n) ++ rest)
         _                -> s

parse :: String -> String
parse str | isAdd = parse (newStr $ fromJust add)
          | isMul = parse (newStr $ fromJust mul)          
          | head str == '(' = ((\(x,y)-> (show $ topParse x)++y) (split' "" (tail str) 1))
          | otherwise = paran str
            where add = addition str
                  isAdd = isJust add
                  mul = multiplication str
                  isMul = isJust mul
                  newStr y = (show $ fst y) ++ (snd y)

topParse str | isNum = fst $ fromJust $ num'
             | otherwise = topParse $ parse str
             where num' = num str
                   isNum = isJust num' && (length (snd (fromJust num'))) < 2 


addition2 :: ParserFun Integer
addition2 s = case num s of
            Just(n,'+':rest) -> case num rest of
                                    Just (m, rest') -> Just (n + m, rest')
                                    Nothing         -> addition2 (((show n)++"+") ++ parse2 rest)
            _                -> Nothing                   


multiplication2 :: ParserFun Integer                                          
multiplication2 s = case num s of
            Just(n,'*':rest) -> case addition2 rest of
                              Just(res,rest') -> multiplication2 ((show n)++"*"++(show res)++rest')
                              Nothing -> case num rest of
                                    Just (m, rest') -> Just (n * m, rest')
                                    Nothing         -> multiplication2 (((show n)++"*") ++ parse2 rest)
            _                -> Nothing

parse2 :: String -> String
parse2 str | isAdd = parse2 (newStr $ fromJust add)
           | isMul = parse2 (newStr $ fromJust mul)          
           | head str == '(' = ((\(x,y)-> (show $ topParse2 x)++y) (split' "" (tail str) 1))
           | otherwise = paran str
            where add = addition2 str
                  isAdd = isJust add
                  mul = multiplication2 str
                  isMul = isJust mul
                  newStr y = (show $ fst y) ++ (snd y)

topParse2 str | isNum = fst $ fromJust $ num'
              | otherwise = topParse2 $ parse2 str
              where num' = num str
                    isNum = isJust num' && (length (snd (fromJust num'))) < 2                     

main = do
      contents <- readFile "input.txt"
      let lines' = lines contents
      let p1 = sum $ map topParse [filter (\xs -> xs /= ' ') x | x <- lines']
      let p2 = sum $ map topParse2 [filter (\xs -> xs /= ' ') x | x <- lines']
      print p1
      print p2
      return ()
      
      