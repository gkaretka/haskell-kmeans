module Kmeans (
    distanceFromCentroids,
    giveRandomCentroids,
    assignCluster,
    clusterizeClusters,
    calculateNewCentroids,
    kmeans,
    fstMinElemIdx,
    sumOfSquareDistances,
    sumOfSquareDistancesForRange
) where

import System.Random
-- import Control.Parallel.Strategies
import qualified DataP as DP

-- Pipeline (how to perform from first iteration)
    -- calculateNewCentroids clusterizedData clusters featureCount
        -- clusterizeClusters selected_data dataBelongTo
            -- assignCluster
                -- distanceFromCentroids centroids selected_data
                    -- giveRandomCentroids selected_data k seed

-- [Vect] selected_data         -- containing only desired features
-- Int                          -- number of clusters
-- Int                          -- seed for random number generator
-- Int                          -- number of iterations
-- ([DP.Cluster], [DP.Vect])    -- [1..d] clusterIds, [1..k] Cluster centroids
kmeans :: [DP.Vect] -> Int -> Int -> Int -> ([DP.DPoint], [DP.Vect])
kmeans _sData _k _seed _reqIters = kmeans' _sData _k _seed _reqIters 0 []
    where
        kmeans' sData k seed reqIters curIter centroids
            | reqIters >= curIter   = (zipWith DP.DPoint _sData reasignedClusters, newCentroids)
            | otherwise             = kmeans' sData k seed reqIters (curIter+1) newCentroids
            where
                dim                 = length (head sData)
                cents               = if curIter == 0 then giveRandomCentroids sData k seed else centroids
                asignedClusters     = assignCluster $ distanceFromCentroids cents sData
                clusterizedClusters = clusterizeClusters sData asignedClusters
                newCentroids        = calculateNewCentroids clusterizedClusters k dim
                reasignedClusters   = assignCluster $ distanceFromCentroids newCentroids sData

-- Assuming data looks like this
-- Data x Features (eg. 200 x 2, 200 samples with 2 features)

-- [vect] - centroids
-- [vect] - features
-- [vect] - dist from all centroids
distanceFromCentroids :: [DP.Vect] -> [DP.Vect] -> [DP.Vect]
distanceFromCentroids fts = map (DP.euclideanDistanceList fts)

-- [Vect] selected_data         -- containing only desired features
-- [Int]                        -- list of k values to try
-- Int                          -- seed for random number generator
-- Int                          -- number of iterations
-- ([Float])                    -- number of square distances for n-th k
sumOfSquareDistancesForRange :: [DP.Vect] -> [Int] -> Int -> Int -> [Float]
sumOfSquareDistancesForRange _ [] _ _ = []
sumOfSquareDistancesForRange sData k seed reqIters = sumOfSQr : sumOfSquareDistancesForRange sData (tail k) seed reqIters
    where
        sumOfSQr = sumOfSquareDistances (fst result) (snd result)
        result = kmeans sData (head k) seed reqIters

sumOfSquareDistances :: [DP.DPoint] -> [DP.Vect] -> Float
sumOfSquareDistances points centroids = foldr squareDist 0 points
    where
        squareDist pt acc = DP.vnorm (DP.point pt `DP.vm` cPoint)
            where cPoint = centroids !! DP.cluster pt

assignCluster :: [DP.Vect] -> [DP.Cluster]
assignCluster = map fstMinElemIdx

-- [(vect, int)] points, count
-- [vect] -- new centroids (calculating average from vectors and number of vectors)
calculateNewCentroids :: [(DP.Vect, DP.Cluster)] -> Int -> Int -> [DP.Vect]
calculateNewCentroids xs k dim = map (\x -> fst x `DP.vdiv` fromIntegral (snd x)) $ calculateNewCentroids' xs k dim

-- [(Vect, Cluster)] clusterized data
-- numer of clusters
-- [(Vect, Int)] -- sum point as vector + count of points
calculateNewCentroids' :: [(DP.Vect, DP.Cluster)] -> Int -> Int -> [(DP.Vect, Int)]
calculateNewCentroids' [] k dim     = replicate k (replicate dim 0,0)  -- create k zero vectors with dimension of dim
calculateNewCentroids' (x:xs) k dim = prev_values_pre ++ [(cur_value_val `DP.vp` val, cur_value_cnt+1)] ++ prev_values_last
    where
        val = fst x                                     -- vector [x_1,x_2, ..., x_m]
        idx = snd x                                     -- cluster it belongs to
        prev_values = calculateNewCentroids' xs k dim   -- previous [(cluster info)]
        prev_values_pre = take idx prev_values          -- take cluster info (1,2 ... without N)
        prev_values_last = drop (idx+1) prev_values     -- drop cluster info (without N ... N+1, N+2)
        cur_value = prev_values !! idx                  -- just cluster info of N
        cur_value_val = fst cur_value                   -- sum of vectors from cluster so far
        cur_value_cnt = snd cur_value                   -- count of vectors from cluster so far


clusterizeClusters :: [DP.Vect] -> [DP.Cluster] -> [(DP.Vect, DP.Cluster)]
clusterizeClusters = zip

fstMinElemIdx :: (Ord a, Eq a) => [a] -> Int
fstMinElemIdx = someElemIdx (>=)

-- Takes last element occurance
someElemIdx :: (Ord a, Eq a) => (a -> a -> Bool) -> [a] -> Int
someElemIdx _ []   = error "Empty list"
someElemIdx f xs   = snd (minElemIdx' xs 0)
    where
        minElemIdx' [] _        = error "Empty list"
        minElemIdx' [a] cid     = (a, cid)
        minElemIdx' (x:xs) cid  = if prev_val `f` x then (x, cid) else (prev_val, prev_idx)
            where
                prev_min_tup    = minElemIdx' xs (cid+1)
                prev_val        = fst prev_min_tup
                prev_idx        = snd prev_min_tup

-- Give k random centroid
-- Works funky - might shuffle your data
-- xs, k, seed
giveRandomCentroids :: (Eq a) => [a] -> Int -> Int -> [a]
giveRandomCentroids [] _ _ = []
giveRandomCentroids xs k seed = giveRandomCentroids' xs k (mkStdGen seed) []
    where
        giveRandomCentroids' xs' k' gen cents'
            | null xs'              = cents'
            | length cents' >= k'   = cents'
            | otherwise             = giveRandomCentroids' (filter (\x -> x `notElem` (cent:cents')) xs') k' newGen (cent:cents')
            where
                cent = xs' !! elemIdx
                n = length xs'
                (elemIdx, newGen) = randomR (0, n-1) gen

