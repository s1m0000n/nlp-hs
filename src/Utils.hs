module Utils where

import Data.Ord (comparing)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VAlgo

argSort :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
argSort xs = VU.map fst $ VU.create $ do
    xsi <- VU.thaw $ VU.indexed xs
    VAlgo.sortBy (comparing snd) xsi
    return xsi
