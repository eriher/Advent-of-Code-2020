import Data.Char

-- | stringToInt, converts string to integer
stringToInt :: [Char] -> Int
stringToInt = foldl addDigit 0
   where addDigit num d = 10*num + digitToInt d
-- | signedStringToInt, converts string prepended with sign to integer
signedStringToInt :: [Char] -> Int
signedStringToInt (c:cs) | c == '-'  = -1 * (stringToInt cs)
                         | otherwise = stringToInt cs

-- | noDuplicate, checks if instructions will lead to duplicate pointer location
noDuplicate :: [(String,Int)] -> Int -> [Int] -> Bool
noDuplicate instrs ptr prev | ptr >= (length instrs) = True
                            | ptr `elem` prev        = False
                            | instr == "nop"         = noDuplicate instrs (ptr+1) (ptr:prev)
                            | instr == "acc"         = noDuplicate instrs (ptr+1) (ptr:prev)
                            | instr == "jmp"         = noDuplicate instrs (ptr+num) (ptr:prev)
                            where (instr,num) = instrs !! ptr

-- | insertInstruction, inserts instruction v, at index i, in list l
insertInstruction :: [(String,Int)] -> Int -> (String,Int) -> [(String,Int)]
insertInstruction l i v = s ++ [v] ++ (tail e)
                where (s,e) = splitAt i l

-- | checkInstr2, follows instructions and corrects for the error, returning accumulated value when end is reached
checkInstrs2 :: [(String,Int)] -> Int -> Int -> [Int] -> Bool -> Int
checkInstrs2 instrs ptr acc prev alt | ptr >= (length instrs) = acc
                                     | instr == "nop" && alt && altdups = checkInstrs2 altInstrs (ptr+num) acc (ptr:prev) False
                                     | instr == "nop"  = (checkInstrs2 instrs (ptr+1) acc (ptr:prev) alt)
                                     | instr == "jmp" && alt && altdups = checkInstrs2 altInstrs (ptr+1) acc (ptr:prev) False
                                     | instr == "jmp"  = (checkInstrs2 instrs (ptr+num) acc (ptr:prev) alt)
                                     | instr == "acc"  = checkInstrs2 instrs (ptr+1) (acc+num) (ptr:prev) alt                                     
                                     where (instr,num)  = instrs !! ptr
                                           altInstrs = if instr == "nop" then insertInstruction instrs ptr ("jmp",num) else insertInstruction instrs ptr ("nop",num)
                                           altdups   = if instr == "nop" then (noDuplicate altInstrs (ptr+num) (ptr:prev)) else (noDuplicate altInstrs (ptr+1) (ptr:prev))

-- | checkInstr, follows instructions and returns accumulated when a duplicate pointer position is reached
checkInstrs :: [(String,Int)] -> Int -> Int -> [Int] -> Int
checkInstrs instrs ptr acc prev | ptr >= (length instrs) || ptr `elem` prev = acc
                                | instr == "nop"                           = checkInstrs instrs (ptr+1) acc (ptr:prev)
                                | instr == "acc"                           = checkInstrs instrs (ptr+1) (acc+num) (ptr:prev)
                                | instr == "jmp"                           = checkInstrs instrs (ptr+num) acc (ptr:prev)
                                where (instr,num) = instrs !! ptr

main = do
      contents <- readFile "input.txt"
      --read contents, split into lines
      let instructions = [(ins,num) |line <- lines $ contents, let ins = (words line) !! 0, let num = signedStringToInt $ (words line) !! 1]
      return (checkInstrs instructions 0 0 [], checkInstrs2 instructions 0 0 [] True)

      