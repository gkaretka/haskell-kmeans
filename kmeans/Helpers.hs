module Helpers (
    parseCsvLine,
    getIndexedValues,
    listListToCsvString,
    listToCsvString,
    listToNewLine
) where

-- Separate CSV string to [String]
parseCsvLine :: String -> [String]
parseCsvLine = words . foldr (\x acc -> if x == ',' then ' ' : acc else x : acc) []

-- Csv/new line output helpers
listListToCsvString :: (Show a) => [[a]] -> String
listListToCsvString []      = ""
listListToCsvString (x:xs)  = take (length concatedX - 1) concatedX ++ ['\n'] ++ listToCsvString xs
    where concatedX = concatMap (\x -> rmq $ show x ++ ",") x

listToNewLine :: (Show a) => [[a]] -> String
listToNewLine []      = ""
listToNewLine (x:xs)  = take (length concatedX - 1) concatedX ++ ['\n'] ++ listToNewLine xs
    where concatedX = concatMap (\x -> rmq $ show x ++ ",") x

listToCsvString :: (Show a) => [a] -> String
listToCsvString x = take (length concatedX - 1) concatedX
    where concatedX = concatMap (\x -> rmq $ show x ++ ",") x

rmq :: String -> String
rmq = filter (/='"')

-- Get only desired indexes of array
-- ["a", "b", "c"] [0, 2] -> ["a", "c"]
getIndexedValues :: [a] -> [Int] -> [a]
getIndexedValues = getIedVa 0

-- Helper for getIndexedValues
getIedVa :: Int -> [a] -> [Int] -> [a]
getIedVa _ [] _     = []
getIedVa n (z:zs) ks
    | n `elem` ks   = z : getIedVa (n+1) zs ks
    | otherwise     = getIedVa (n+1) zs  ks