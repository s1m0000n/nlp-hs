module StrSim where

import qualified Data.Text as T
import Data.Text.Metrics ( damerauLevenshteinNorm )
import Data.Ratio (Ratio)

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve xs = splitAt h xs where h = length xs `div` 2

insertToSortedK :: Ord k => (k, v) -> [(k, v)] -> [(k, v)]
insertToSortedK t [] = [t]
insertToSortedK (k, v) [(kl, vl)]
    | k > kl = [(kl, vl), (k, v)]
    | otherwise = [(k, v), (kl, vl)]
insertToSortedK (k, v) xs
    | even $ length xs = let (lowerHalf, higherHalf) = halve xs in insertToSortedK (k, v) lowerHalf ++ insertToSortedK (k, v) higherHalf
    | otherwise = let 
        lowerHalfLastIdx = length xs `div` 2
        lowerHalf = take lowerHalfLastIdx xs
        midIdx = lowerHalfLastIdx + 1
        (mk, mv) = xs !! midIdx
        higherHalf = drop midIdx xs
    in if k < mk then insertToSortedK (k, v) lowerHalf ++ (mk, mv) : higherHalf else lowerHalf ++ [(mk, mv)] ++ insertToSortedK (k, v) higherHalf

mapSortedK :: Ord k => (a -> (k, v)) -> [a] -> [(k, v)]
mapSortedK _ [] = []
mapSortedK f (x:xs) = insertToSortedK (f x) $ mapSortedK f xs

findSortEditDists :: T.Text -> [T.Text] -> [(Ratio Int, T.Text)]
findSortEditDists query = mapSortedK (\d -> (damerauLevenshteinNorm query d, d))
