module Parser
       where

import Text.Parsec

-- | Parses grades into tuples that can be quickly converted to Grade
-- objects.


parseToGrades :: String -> IO [(String,[Int],Int)]
parseToGrades str
  = do x <- getGradePortionOfString str
       let y = makeReplacements x
       z <- breakUp y
       return $ toGrades z

getGradePortionOfString :: String -> IO String
getGradePortionOfString str = printErrorOrReturn (parse allButEnd "" str)

allButEnd =
  do spaces
     openParen
     result <- readParen'd
     many parenCompound
     closeParen
     return result

makeReplacements :: String -> String
makeReplacements x =
  case findSharpIndex x
  of   Just (n,s) -> return (replace sexpt s x)
                     >>= replace sexp1 s
         where sexpt = "#" ++ n ++ "#"
               sexp1 = "#" ++ n ++ "=" ++ s
       Nothing -> x

breakUp str = printErrorOrReturn $ breakUp' str
breakUp' = parse (do openParen
                     l <- many $
                          do x <- readParen'd
                             spaces
                             return x
                     closeParen
                     spaces
                     eof
                     return l) ""

toGrades = map toGrade
  where toGrade str = case toGrade' str
                      of   Left a -> error "Bad parse"
                           Right x -> x
        toGrade'
          = parse
            ( do openParen
                 string ":|NAME|"
                 spaces
                 name <- quotedString 
                 spaces
                 string ":|SCORES|"
                 spaces
                 openParen
                 scores <- (sepBy1 pern spaces)
                 closeParen
                 spaces
                 string ":|GRADE|"
                 spaces
                 grade <- number
                 period
                 closeParen
                 eof
                 return (name,map read scores,read grade)
            ) ""
        pern = do x <- number
                  period
                  return x
                 
findSharpIndex str = let x = dropWhile (/='#') str
                     in  case (parse sharpExpression "" x)
                         of Left _ -> Nothing
                            Right a -> Just a

sharpExpression = do sharp
                     n <- number
                     equalsSign
                     x <- number <|> quotedString <|> readParen'd
                     return (n, x)

openParen = char '('
sharp = char '#'
equalsSign = char '='
quote = char '\"'
closeParen = char ')'
period = string "."

number = many1 digit

quotedString = do quote
                  x <- stringContents
                  quote
                  return x

stringContents = fmap concat $ many (many1 (satisfy (/='\"')) <|> string "\\\"")

replace = replace' []
replace' acc _ _ [] = reverse acc
replace' acc old new target@(t0:ts) =
  if take len target == old
  then replace' acc old new (new++drop len target)
  else replace' (t0:acc) old new ts
       where len = length old

readParen'd =
  do openParen
     result <- fmap concat $ many parenCompound
     closeParen
     return $ "("++result++")"

parenCompound = many1 (noneOf "()") <|> readParen'd

printErrorOrReturn :: Either ParseError a -> IO a
printErrorOrReturn result =
     case result of
       Left err -> error $ "Parse failed: Exception was ++ \""
                   ++ show err ++ "\""
       Right xs -> return xs
