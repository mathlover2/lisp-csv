module Parser (parseToGrades)
       where

import Text.Parsec

-- | Parses grades into tuples that can be quickly converted to Grade
-- objects.


parseToGrades :: String -> IO [(String, String, String, [Int],Int)]
parseToGrades str
  = do x <- printErrorOrReturn (parse allButEnd "" str)
       let y = makeReplacements x
       z <- breakUp y
       return $ toGrades z

getGradePortionOfFile :: String -> IO String
getGradePortionOfFile str = printErrorOrReturn (parse allButEnd "" str)

allButEnd =
  do spaces
     openParen
     result <- readParen'd
     many parenCompound
     closeParen
     return result

openParen = char '('

closeParen = char ')'

readParen'd =
  do openParen
     result <- fmap concat $ many parenCompound
     closeParen
     return $ "("++result++")"

parenCompound = many1 (noneOf "()") <|> readParen'd

printErrorOrReturn :: Either ParseError String -> IO String
printErrorOrReturn result =
     case result of
       Left err -> error $ "Parse failed: Exception was ++ \"" ++ show err ++ "\""
       Right xs -> return xs
