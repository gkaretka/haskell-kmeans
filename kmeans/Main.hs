{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Main where

import System.IO
import Data.List
import qualified Helpers as H
import qualified DataP as V

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
    putStrLn ("Selected features\n" ++ intercalate ", " (H.getIndexedValues features selected_features))

    let selected_data = foldr (\x acc -> (V.csvToVect x selected_features) : acc) [] (drop 1 $ lines content)
    V.vectListToCsv selected_data
    
    -- close file
    hClose handle

