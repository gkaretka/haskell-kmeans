{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Kmeans (
    distanceFromCentroids,
    giveRandomCentroids,
    assignCluster,
    clusterizeClusters,
    calculateNewCentroids,
    kmeans
) where

import System.Random
import qualified DataP as DP

-- Pipeline (how to perform first iteration)
    -- calculateNewCentroids clusterizedData clusters featureCount
        -- clusterizeClusters selected_data dataBelongTo
            -- assignCluster
                -- distanceFromCentroids centroids selected_data
                    -- giveRandomCentroids selected_data k seed

-- [Vect] selected_data -- containing only desired features
-- Int                  -- number of clusters
-- Int                  -- seed for random number generator
-- Int                  -- number of iterations
kmeans :: [DP.Vect] -> Int -> Int -> Int -> ([DP.Cluster],[DP.Vect])
kmeans _sData _k _seed _reqIters = kmeans' _sData _k _seed _reqIters 0 []
    where
        kmeans' sData k seed reqIters curIter centroids
            | reqIters >= curIter   = (asignedClusters, newCentroids)
            | otherwise             = kmeans' sData k seed reqIters (curIter+1) newCentroids
            where
                dim                 = length (head sData)
                centroids           = if curIter == 0 then giveRandomCentroids sData k seed else centroids
                asignedClusters     = assignCluster $ distanceFromCentroids centroids sData
                clusterizedClusters = clusterizeClusters sData asignedClusters
                newCentroids        = calculateNewCentroids clusterizedClusters k dim

-- Assuming data looks like this
-- Data x Features (eg. 200 x 2, 200 samples with 2 features)

-- [vect] - centroids
-- [vect] - features
-- [vect] - dist from all centroids
distanceFromCentroids :: [DP.Vect] -> [DP.Vect] -> [DP.Vect]
distanceFromCentroids fts = map (DP.euclideanDistanceList fts)

assignCluster :: [DP.Vect] -> [DP.Cluster]
assignCluster = map minElemIdx

-- [(vect, int)] points, count
-- [vect] -- new centroids
calculateNewCentroids :: [(DP.Vect, DP.Cluster)] -> Int -> Int -> [DP.Vect]
calculateNewCentroids xs k dim = map (\x -> fst x `DP.vdiv` (fromIntegral $ snd x)) $ calculateNewCentroids' xs k dim

-- [(Vect, Cluster)] clusterized data
-- numer of clusters
-- [(Vect, Int)] -- sum point as vector + count of points
calculateNewCentroids' :: [(DP.Vect, DP.Cluster)] -> Int -> Int -> [(DP.Vect, Int)]
calculateNewCentroids' [] k dim     = replicate k (replicate dim 0,0)  -- create k zero vectors with dimension of dim
calculateNewCentroids' (x:xs) k dim = prev_values_pre ++ [(cur_value_val `DP.vp` val, cur_value_cnt+1)] ++ prev_values_last
    where
        val = fst x
        idx = snd x
        prev_values = calculateNewCentroids' xs k dim
        prev_values_pre = take idx prev_values
        prev_values_last = drop (idx+1) prev_values
        cur_value = prev_values !! idx
        cur_value_val = fst cur_value
        cur_value_cnt = snd cur_value


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
minInList []    = error "Empty list"
minInList [a]   = a
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