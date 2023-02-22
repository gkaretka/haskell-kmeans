{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
module Main where

import System.IO
import Data.List
import qualified Helpers as H
import qualified DataP as DP
import qualified Kmeans as K

seed::Int
seed = 40

main :: IO ()
main = do
    -- Get data file path
    putStrLn "File path: "
    filePath <- getLine

    -- Open and read file
    rHandle <- openFile filePath ReadMode
    rContent <- hGetContents rHandle

    -- Display features
    let features = H.parseCsvLine . unlines . take 1 $ lines rContent
    putStrLn ("Features\n" ++ intercalate ", " features)

    -- Select features (separate with comma)
    putStrLn ("Select features 0 - " ++ show ((+) (-1) $ length features) ++ " (eg. 1,3,5)")
    sel_features <- getLine
    let selected_features = map (\x -> read x :: Int) (H.parseCsvLine sel_features)
    putStrLn ("Selected features: " ++ intercalate ", " (H.getIndexedValues features selected_features))

    -- Input number of clusters
    putStrLn "Number of clusters (K): "
    k <- getLine
    putStrLn ("Number of clusters (K): " ++ k)

    -- Create data vector and sample centroids from data
    let clustersCount = read k :: Int
    let selectedData = map (`DP.csvToVect` selected_features) (drop 1 $ lines rContent)
    let featureCount = length (head selectedData)
    let dataCount = length selectedData

    -- Show dataset dims and random gen. seed
    putStrLn ("Dataset length: " ++ show dataCount ++ "x" ++ show featureCount)
    putStrLn ("Seed: " ++ show seed)

    let res = map show $ K.kmeans selectedData clustersCount seed 100
    writeFile "../out/cents.csv" (unlines res)

    -- close file
    hClose rHandle

