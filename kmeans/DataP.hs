{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module DataP (
    DPoint, Vect, Cluster,
    vp, vm, vdiv, vnorm,
    euclideanDistance,
    euclideanDistanceList,
    csvToVect,
    vectListToIO,
) where

import qualified Helpers as H
import Data.List

type Cluster = Int
type Vect = [Float]
data DPoint = DPoint Vect Cluster deriving (Eq, Ord, Show)

-- Euclidean distance between two vectors (points)
euclideanDistance :: Vect -> Vect -> Float
euclideanDistance a b = vnorm (a `vm` b)

-- Euclidean  distance between list of vectors and vector (point)
euclideanDistanceList :: [Vect] -> Vect -> [Float]
euclideanDistanceList [] _ = []
euclideanDistanceList dat pt = map (euclideanDistance pt) dat

-- Vector addition (5, 1, 3) + (4, 9, 3) = (9, 10, 6)
vp :: Vect -> Vect -> Vect
vp [] [] = []
vp [] _ = []
vp _ [] = []
vp (x:xs) (y:ys) = (x+y) : vp xs ys

-- Vector subtraction (5, 1, 3) - (4, 9, 3) = (-1, -8, 0)
vm :: Vect -> Vect -> Vect
vm [] [] = []
vm [] _ = []
vm _ [] = []
vm (x:xs) (y:ys) = (x-y) : vm xs ys

vdiv :: Vect -> Float -> Vect
vdiv [] 0 = error "Division by zero"
vdiv [] _ = []
vdiv xs a = map (\x -> x / a) xs

-- Vector norm (1, 5, 6, ...) = 1^2 + 5^2 + 6^2 ...
vnorm :: Vect -> Float
vnorm [] = 0
vnorm xs = sum $ map (^2) xs

-- String csv to vector (select only desired features)
-- "1,5,6,7" [0,1,3] -> [1, 5, 7]
csvToVect :: String -> [Int] -> Vect
csvToVect str = map (\x -> read x::Float) . H.getIndexedValues (H.parseCsvLine str)

-- [Vect] to IO ()
-- Print [Vect] nicely
-- https://stackoverflow.com/questions/26546523/print-2d-list-of-lists-in-haskell
vectListToIO :: [Vect] -> IO ()
vectListToIO xxs
    | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
    | otherwise = mapM_ printRow xxs
        where printRow xs = mapM_ print_ xs >> putStrLn ""
              print_ x =  putStr $ show x ++ "\t"