module Helpers (
    parseCsvLine,
    getIndexedValues
) where

-- Separate CSV string to [String]
parseCsvLine :: String -> [String]
parseCsvLine = words . foldr (\x acc -> if x == ',' then ' ' : acc else x : acc) []

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