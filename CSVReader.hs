module CSVReader where
import Data.List.Split

csvReader :: FilePath -> IO [[String]]
csvReader file = do
    content <- readFile file
    let lineSep = lines content
    let lists = [splitOn "," x | x <- lineSep]
    return lists