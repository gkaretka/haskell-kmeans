{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
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
    let selectedFeatures = map (\x -> read x :: Int) (H.parseCsvLine sel_features)
    let selectedFeaturesString = intercalate ", " (H.getIndexedValues features selectedFeatures)
    putStrLn ("Selected features: " ++ selectedFeaturesString)

    -- Input number of clusters
    putStrLn "Number of clusters (K): "
    k <- getLine
    putStrLn ("Number of clusters (K): " ++ k)

    -- Create data vector and sample centroids from data
    let clustersCount = read k :: Int
    let selectedData = map (`DP.csvToVect` selectedFeatures) (drop 1 $ lines rContent)
    let featureCount = length (head selectedData)
    let dataCount = length selectedData

    -- Show dataset dims and random gen. seed
    putStrLn ("Dataset length: " ++ show dataCount ++ "x" ++ show featureCount)
    putStrLn ("Seed: " ++ show seed)

    -- ([Cluster, [Vect]])
    let result = K.kmeans selectedData clustersCount seed 100

    writeFile "out/cluster_info.csv" $ filter (/= ' ') selectedFeaturesString ++ ",ClusterID\n" 
        ++ (H.listToNewLine $ zipWith (\x y -> [H.listToCsvString x, show y]) selectedData (fst result))

    writeFile "out/cluster_position.csv" $ ((intercalate "," $ take featureCount ["x", "y", "z", "w", "i", "j", "k"]) ++ "\n")
        ++ (H.listToNewLine $ snd result)

    -- close file
    hClose rHandle

