{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Kmeans (
    distanceFromCentroids,
    giveRandomCentroids,
    assignCluster,
    clusterizeClusters,
    calculateNewCentroids
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

-- [(Vect, Cluster)] clusterized data
-- numer of clusters
-- dimension of vectors
calculateNewCentroids :: [(DP.Vect, DP.Cluster)] -> Int -> Int -> [DP.Vect]
calculateNewCentroids [] k dim = replicate k (replicate dim 0)  -- create k zero vectors with dimension of dim
calculateNewCentroids (x:xs) k dim = prev_values_pre ++ [cur_value `DP.vp` val] ++ prev_values_last
    where
        val = fst x
        idx = snd x
        prev_values = calculateNewCentroids xs k dim
        prev_values_pre = take idx prev_values
        prev_values_last = drop (idx+1) prev_values
        cur_value = prev_values !! idx

clusterizeClusters :: [DP.Vect] -> [DP.Cluster] -> [(DP.Vect, DP.Cluster)]
clusterizeClusters = zip


minElemIdx :: (Ord a, Eq a) => [a] -> Int
minElemIdx [] = -1
minElemIdx xs = minElemIdx' xs 0
    where
        minElemIdx' (z:zs) n
            | z == minElem  = n
            | otherwise     = minElemIdx' zs (n+1)
        minElem = minInList xs

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