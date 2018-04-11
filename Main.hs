module Main where
import Drammar
import Dokens
import CSVReader
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec
import Data.List



-- Searches a list of tuples and returns the value corresponding to a key.
search :: Eq a => a -> [(a, t)] -> t
search key []  = error " ~ Unrecognised variable. ~ "
search key ((x,y):xys)
    | key == x  = y
    | otherwise  = search key xys

search' :: Eq a => a -> [(a, [t])] -> [t]
search' key []  = []
search' key ((x,y):xys)
    | key == x  = y
    | otherwise  = search' key xys

searchFile :: (Eq a1, Monad m) => a1 -> [(a1, a)] -> m a
searchFile key list = do
    return (search key list)

searchVar :: Eq a1 => a1 -> [[(a1, [a])]] -> [a]
searchVar key list
    | null l = []
    | otherwise = head l
    where l = filter (\x -> length x > 0) [ search' key r | r <- list ] 
    

transposer :: [[b]] -> [[b]]
transposer ([]:_) = []
transposer x = (map head x) : transposer (map tail x)

-- Cartesian product of the rows in the taken relations.
populateList :: [[(a, [a1])]] -> [[a1]]
populateList lists = [ concat x | x <- (rowCartProd [ transposer [(snd n) | n <- list] | list <- lists ]) ]


rowCartProd :: [[a]] -> [[a]]
rowCartProd = foldr
    (\xs as ->
        [ x : a
        | x <- xs
        , a <- as ])
    [[]]


(+++) :: Monad m => m [a] -> m [a] -> m [a]
ms1 +++ ms2 = do
    s1 <- ms1
    s2 <- ms2
    return $ s1 ++ s2

-- Checks whether values in a list represent a list of twin pairs.
checker :: Eq a => [a] -> Bool
checker [] = False
checker (x:y:[]) = (x==y)
checker (x:y:xys) = (x==y) && checker xys

-- Deletes xth element in a list.
deleteElem :: Int -> [a] -> [a]
deleteElem _ [] = []
deleteElem x zs | x > 0 = take (x-1) zs ++ drop x zs
                | otherwise = zs

-- Deletes the listed elements from a list.
deleteElems :: Foldable t => t Int -> [a] -> [a]
deleteElems xs zs = foldr deleteElem zs xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

countVars :: Eq a => Integral b => a -> [a] -> b
countVars e [] = 0
countVars e (a:xs) = (countVars e xs +) $ if a == e then 1 else 0

-- Takes a list of relations and returns all vars in it.
getVars :: [[(t, b)]] -> [t]
getVars relationsList = [ fst r | rel <- relationsList, r <- rel ]

-- Takes a list of relations and returns all dupicate var names (aka ["x1","x3"]).
getDupVars :: Eq a => [[(a, b)]] -> [a]
getDupVars relationsList = removeDuplicates [ a | a <- (getVars relationsList), countVars a (getVars relationsList) > 1 ]


-- Creates a list of relations from an empty relations list l (only var names!) and contents c.
helper :: [(t, b)] -> [[t1]] -> [[(t, [t1])]]
helper l c = [ [ ( fst (l!!m), [ (c!!n)!!m | n <- [0..length c - 1] ] ) | m <- [0..length l - 1] ] ]



-- Returns an empty relations list (contains only var names!).
evalList :: List -> [(String, [String])]
evalList (Put el) = do
    return (el, [])

evalList (Add el list) = do
    [(el, [])] ++ evalList list



-- Creates a list of bindings between relation names and csv files.
evalAssign :: Assignment -> [(String, String)]
evalAssign (Assign var file) = do
    return (var, file)

evalAssign (AndA ass1 ass2) = do
    evalAssign ass1 ++ evalAssign ass2



-- Fills the relations list.
evalQuery :: Query -> [(String, FilePath)] -> IO [[(String, [String])]]
evalQuery (Relate var list) filesList = do
    file  <- searchFile var filesList
    content <- csvReader file
    if (null content) then return [[]] -- error " ~ Empty csv file. ~ "
    else if (length (evalList list)) /= (length (head content)) then error " ~ Wrong arity. ~ "
    else
      return (helper (evalList list) content)

evalQuery (And q1 q2) filesList = do
    evalQuery q1 filesList +++ evalQuery q2 filesList



-- Working equivalent of the function `elem` - checks whether a var is found in a given list.
isInList :: Eq a => a -> [a] -> Bool
isInList var [] = False
isInList var (x:xs)
    | var == x = True
    | otherwise = isInList var xs

-- Goes through a list of lists and checks whether a var is contained in any of them - if it is,
-- retuns the index of the list; if not, returns (-1).
searchEq :: Eq a => a -> [[a]] -> Int
searchEq var list 
    | null list || null index = -1
    | otherwise = head index 
    where index = [ n | n <- [0..length list - 1], isInList var (list!!n) ]


-- Returns all groups of equal vars in a list.
evalCondition :: Condition -> [[String]] -> [[String]]
evalCondition (Compare var1 var2) equalityList
    | var1 == var2 = equalityList
    | (searchEq var1 equalityList == -1 && searchEq var2 equalityList == -1) = equalityList ++ [[var1, var2]]
    | searchEq var1 equalityList == -1 = (take index2 equalityList ++ [(equalityList!!index2 ++ [var1])] ++ drop (index2 + 1) equalityList)
    | searchEq var2 equalityList == -1 = (take index1 equalityList ++ [(equalityList!!index1 ++ [var2])] ++ drop (index1 + 1) equalityList)
    where 
        index1 = searchEq var1 equalityList
        index2 = searchEq var2 equalityList

evalCondition (AndC c1 c2) equalityList = evalCondition c2 (evalCondition c1 equalityList)



-- Creates all cartesian product of all rows from the taken relations and binds them accordingly.
getCrossoverColumns :: [[(a, [t])]] -> [[(a, [t])]]
getCrossoverColumns relationsList = [ [ ( fst ((concat relationsList)!!m), [ ((populateList relationsList)!!n)!!m | n <- [0..length (populateList relationsList) - 1] ] ) | m <- [0..length (concat relationsList) - 1] ] ]

-- Returns the contents of matching by name columns.
getMatchColumns :: Eq a => [[(a, [t])]] -> [[t]]
getMatchColumns relationsList = [ snd rel | dup <- (getDupVars relationsList), rel <- head (getCrossoverColumns relationsList), fst rel == dup]

-- Returns the indeces of the mismatched rows - to be removed from the result getCrossoverColumns.
getDiffIndeces :: (Eq a, Eq a1) => [[(a, [a1])]] -> [Int]
getDiffIndeces relationsList = map (+1) [ a | let rows = (transposer (getMatchColumns relationsList)), a <- [0..length rows - 1], checker (rows!!a) == False ]

-- Removes mismatched rows from the cartesian product.
getCrossoverColumnsClear relationsList = [[ ( fst r, (deleteElems (getDiffIndeces relationsList) (snd r)) )| let rels = head (getCrossoverColumns relationsList), r <- rels ]]
getCrossoverColumnsClear :: (Eq a, Eq a1) => [[(a1, [a])]] -> [[(a1, [a])]]

-- If no duplicating var names are found, do not modify the cartesian product; otherwisee clear it.
getNewColumns relationsList
    | null (getDupVars relationsList) = getCrossoverColumns relationsList
    | otherwise = getCrossoverColumnsClear relationsList

-- Removes the redundant relations from the relations list.
removeRedundant :: (Eq a, Eq t) => [a] -> [[(a, [t])]] -> [[(a, [t])]]
removeRedundant list relationsList = removeDuplicates [ rel | rel <- relationsList, key <- removeDuplicates list, (not.null) (searchVar key [rel]) ]

clearCrossoverList :: (Eq a1, Eq a) => [(a1, b)] -> [[(a1, [a])]] -> [[a]]
clearCrossoverList list relationsList =  [ searchVar (fst key) (getNewColumns (filter (not.null) relationsList)) | key <- list ]

-- Checks whether all vars used in Format are binded to a column.
oneMoreHelper :: (Eq t, Eq a) => [(a, b)] -> [[(a, [t])]] -> [[t]]
oneMoreHelper list relationsList
    | (length (varsFromRelations \\ (removeDuplicates varsFromFormat)) /= ((length varsFromRelations) - (length (removeDuplicates varsFromFormat)))) = [[]] 
    | otherwise = l
    where
        varsFromFormat = (getVars [list])
        varsFromRelations = (getVars relationsList)
        neededList = removeRedundant varsFromFormat relationsList
        l = clearCrossoverList list neededList
 


renameVar :: Eq a => a -> a -> [(a, t)] -> [(a, t)]
renameVar var1 var2 xs = [ (if fst x == var2 then var1 else fst x, snd x) | x <- xs]

renameVars :: Eq a => a -> [a] -> [(a, t)] -> [(a, t)]
renameVars new [] rel = rel
renameVars new (x:xs) rel = renameVars new xs (renameVar new x rel)

renameInRel :: Eq a => [a] -> [(a, t)] -> [(a, t)]
renameInRel equality rel = renameVars (head equality) (tail equality) rel

rename :: Eq a => [[a]] -> [(a, t)] -> [(a, t)]
rename [] rel = rel
rename (x:xs) rel = rename xs (renameInRel x rel)

renameAll :: Eq a => [[a]] -> [[(a, t)]] -> [[(a, t)]]
renameAll equalityList relationsList = map (rename equalityList) relationsList


-- Return a list of the rows to be printed.
evalOutput :: Eq b => Output -> [[String]] -> [[(String, [b])]] -> [[b]]
evalOutput (Format list) equalityList relationsList = transposer (oneMoreHelper (rename equalityList (evalList list)) relationsList)



-- Prints the result of the expression.
evalExp :: Exp -> IO ()
evalExp (Given q a o) = do
    query <- evalQuery q (evalAssign a)
    mapM_ putStrLn [ intercalate ","  list | list <- (sort (evalOutput o [] query))]

evalExp (GivenC q c a o) = do
    query <- evalQuery q (evalAssign a)
    mapM_ putStrLn [ intercalate ","  list | list <- (sort (evalOutput o (evalCondition c []) (renameAll (evalCondition c []) query)))]



-- Reads the program, parses it and evaluates the result. 
main = do
    args <- getArgs
    case args of
      [file] -> do
        s <- readFile file
        let ast = runCalc s
        evalExp ast