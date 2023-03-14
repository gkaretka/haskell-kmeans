-- Project: Kmeans
-- Login: xkaret00
-- Name: Gregor Karetka
-- Year: 2023

module Helpers (
    parseCsvLine,
    getIndexedValues,
    listListToCsvString,
    listToCsvString,
    listToNewLine,
    fstMinElemIdx
) where

-- Separate CSV string to [String]
parseCsvLine :: String -> [String]
parseCsvLine = words . foldr (\x acc -> if x == ',' then ' ' : acc else x : acc) []

-- Csv/new line output helpers
listListToCsvString :: (Show a) => [[a]] -> String
listListToCsvString []      = ""
listListToCsvString (x:xs)  = take (length concatedX - 1) concatedX ++ ['\n'] ++ listToCsvString xs
    where concatedX = concatMap (\p -> rmq $ show p ++ ",") x

listToNewLine :: (Show a) => [[a]] -> String
listToNewLine []      = ""
listToNewLine (x:xs)  = take (length concatedX - 1) concatedX ++ ['\n'] ++ listToNewLine xs
    where concatedX = concatMap (\p -> rmq $ show p ++ ",") x

listToCsvString :: (Show a) => [a] -> String
listToCsvString x = take (length concatedX - 1) concatedX
    where concatedX = concatMap (\p -> rmq $ show p ++ ",") x

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


fstMinElemIdx :: (Ord a, Eq a) => [a] -> Int
fstMinElemIdx = someElemIdx (>=)

-- Takes last element occurance
someElemIdx :: (Ord a, Eq a) => (a -> a -> Bool) -> [a] -> Int
someElemIdx _ []   = error "Empty list"
someElemIdx f as   = snd (minElemIdx' as 0)
    where
        minElemIdx' [] _        = error "Empty list"
        minElemIdx' [a] cid     = (a, cid)
        minElemIdx' (x:xs) cid  = if prev_val `f` x then (x, cid) else (prev_val, prev_idx)
            where
                prev_min_tup    = minElemIdx' xs (cid+1)
                prev_val        = fst prev_min_tup
                prev_idx        = snd prev_min_tup
