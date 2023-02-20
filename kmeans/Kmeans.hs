module Kmeans (
    distanceFromCentroids
) where

import qualified DataP as DP
import System.Random

-- Assuming data looks like this
-- Data x Features (eg. 200 x 2, 200 samples with 2 features)

-- [vect] - features
-- [vect] - centroids
-- [vect] - dist from all centroids
distanceFromCentroids :: [DP.Vect] -> [DP.Vect] -> [DP.Vect]
distanceFromCentroids fts = map (DP.euclideanDistanceList fts)

-- Give k random centroid
-- giveRandomCentroids :: [a] -> Int -> Int -> [a]
-- giveRandomCentroids [] _ _ = []
-- giveRandomCentroids xs k seed = giveRandomCentroids xs k seed []
--     where
--         giveRandomCentroids' xs' k' seed' cents'
--             | length cents' >= k = cents'
--             | otherwise = randElem : cents'

-- Randomly select centroids
giveRandomElement :: [a] -> Int -> a
giveRandomElement xs seed = (!!) xs rand
    where
        n = length xs
        (rand, _) = randomR (0, n-1) (mkStdGen seed)