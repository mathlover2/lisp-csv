import Sorter
import Parser (parseToGrades)
import Data.List (sortBy)
import System.Environment (getArgs)

main = do [lispFile,csvFile] <- fmap (take 2) getArgs
          med <- convertFromLisp lispFile
       	  sortBy sorter med
       	  let xcl = convertToCSV med
	  putInCSVFile xcl csvFile

-- | Convert a Lisp string into a list of grades.

convertFromLisp :: FilePath -> IO Grades
convertFromLisp file =
  do rawLisp <- readFile file
     parsed <- parseToGrades rawLisp
     return $ map toGrades parsed
       where toGrades (x,y,z,w,v) = Grades x y z w v

convertToCSV :: Grades -> String
convertToCSV = concat . map converter
  where converter (lastname,firstname,email,_,score)
          = email ++ ", " ++ lastname ++ ", "
            ++ firstname ++ ", " ++ (show score) ++ " \n"

putInCSVFile :: String -> FilePath -> IO ()
putInCSVFile string filepath = writeFile filepath string

type Grades = [Grade]

data Grade = Grade
     	     { getLastName :: String
             , getFirstName :: String
             , getEmail :: String
     	     , getPoints :: [Int]
	     , getScore :: Int
	     }
