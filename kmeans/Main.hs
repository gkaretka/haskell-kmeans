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

    putStrLn "Number of clusters (K): "
    k <- getLine
    putStrLn ("Number of clusters (K): " ++ k)
    let clusters = read k :: Int
    let selected_data = map (`DP.csvToVect` selected_features) (drop 1 $ lines content)
    putStrLn ("Dataset length: " ++ show (length selected_data) ++ "x" ++ show (length (selected_data !! 1)))

    -- close file
    hClose handle

