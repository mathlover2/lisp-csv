module Parser (parseToGrades)
       where

import Data.Char

-- | Parses grades into tuples that can be quickly converted to Grade
-- objects.

parseToGrades :: String -> [(String, String, String, [Int],Int)]
parseToGrades =  parseToEntries . expandReplacements

-- Expands various replacements made in Lisp file

expandReplacements :: String -> String
expandReplacements string =
  replace (countReplacements string) string

countReplacements = length . (filter isReplacement) . words . filter (`notElem`"()")
  where isReplacement str =
          head str == '#'
          && tail str == '#'
          && all isDigit (init $ tail str)


                 

-- Parses string into grades

parseToEntries = convert . (map removeOuterParens) . (partition 0 "") . takeParens . removeOuterParens
  where removeOuterParens = tail . init
        partition n _ []
          | n == 0 = []
          | otherwise = error $ "Parentheses in lisp database are unbalanced!"
        partition n l (c:xs) =
          case (c,n)
          of (' ',0) -> (reverse l) : partition n [] xs
             ('(',_) -> partition (n+1) (c:l) xs
             (')',_) -> partition (n-1) (c:l) xs
             _ -> partition n (c:l) xs
        convert = 
        
-- | Create the file contents with the grades appended.
