{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Kmeans (
    distanceFromCentroids,
    giveRandomCentroids,
    assignCluster
) where

import System.Random
import qualified DataP as DP

-- Assuming data looks like this
-- Data x Features (eg. 200 x 2, 200 samples with 2 features)

-- [vect] - centroids
-- [vect] - features
-- [vect] - dist from all centroids
distanceFromCentroids :: [DP.Vect] -> [DP.Vect] -> [DP.Vect]
distanceFromCentroids fts = map (DP.euclideanDistanceList fts)

assignCluster :: [DP.Vect] -> [DP.Cluster]
assignCluster = map minElemIdx

-- Get index of max element in list
maxElemIdx :: (Ord a, Eq a) => [a] -> Int
maxElemIdx [] = -1
maxElemIdx xs = maxElemIdx' xs 0
    where
        maxElemIdx' (z:zs) n
            | z == maxElem  = n
            | otherwise     = maxElemIdx' zs (n+1)
        maxElem = maxInList xs

minElemIdx :: (Ord a, Eq a) => [a] -> Int
minElemIdx [] = -1
minElemIdx xs = minElemIdx' xs 0
    where
        minElemIdx' (z:zs) n
            | z == minElem  = n
            | otherwise     = minElemIdx' zs (n+1)
        minElem = minInList xs

-- Get max element in list
maxInList :: (Ord a, Eq a) => [a] -> a
maxInList [] = error "Empty list"
maxInList [a] = a
maxInList (x:xs) = max x (maxInList xs)

-- Get min element in list
minInList :: (Ord a, Eq a) => [a] -> a
minInList [] = error "Empty list"
minInList [a] = a
minInList (x:xs) = min x (minInList xs)

-- Give k random centroid
-- Works funky - might shuffle your data
-- xs, k, seed
giveRandomCentroids :: (Eq a) => [a] -> Int -> Int -> [a]
giveRandomCentroids [] _ _ = []
giveRandomCentroids xs k seed = giveRandomCentroids' xs k seed []
    where
        giveRandomCentroids' xs' k' seed' cents'
            | null xs'              = cents'
            | length cents' >= k'   = cents'
            | otherwise             = giveRandomCentroids' (filter (\x -> x `notElem` (cent:cents')) xs') k' (seed'+1) (cent:cents')
            where cent = giveRandomElement xs' seed'

-- Randomly select centroids
giveRandomElement :: [a] -> Int -> a
giveRandomElement xs seed = (!!) xs rand
    where
        n = length xs
        (rand, _) = randomR (0, n-1) (mkStdGen seed)