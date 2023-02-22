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
    handle <- openFile filePath ReadMode
    content <- hGetContents handle

    -- Display features
    let features = H.parseCsvLine . unlines . take 1 $ lines content
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
    let clusters = read k :: Int
    let selected_data = map (`DP.csvToVect` selected_features) (drop 1 $ lines content)
    let centroids = K.giveRandomCentroids selected_data (read k) seed
    let featureCount = length (head selected_data)
    let dataCount = length selected_data

    -- Show dataset dims and random gen. seed
    putStrLn ("Dataset length: " ++ show dataCount ++ "x" ++ show featureCount)
    putStrLn ("Seed: " ++ show seed)

    -- Show samples centroids
    putStrLn "Centroids: "
    centroidsString <- DP.vectListToIO centroids

    -- Calc dist from centroids
    putStrLn "Distance from centroids:"
    let distFromCentroid = K.distanceFromCentroids centroids selected_data
    putStrLn ("Size: " ++ show (length distFromCentroid) ++ "x" ++ show (length (head distFromCentroid)))

    -- Calc cluster where data belong
    let dataBelongTo = K.assignCluster distFromCentroid
    let clusterizedData = K.clusterizeClusters selected_data dataBelongTo
    print $ K.calculateNewCentroids clusterizedData clusters featureCount

    -- close file
    hClose handle

