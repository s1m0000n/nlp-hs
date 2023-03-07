{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Ngram (Ngrams(..)) where

import qualified Data.Text as T
-- import Data.List (tails)
import Data.List (tails)

class Ngrams a where
    ngrams :: Int -> a -> [a]
    multiNgrams :: [Int] -> a -> [a]
    multiNgrams (x:xs) src = ngrams x src ++ multiNgrams xs src
    multiNgrams [] _ = []
    flatNgrams :: Foldable ((->) a) => Int -> [a]
    flatNgrams = concat . ngrams
    flatMultiNgrams :: Foldable ((->) a) => [Int] -> [a]
    flatMultiNgrams = concat . multiNgrams

instance Ngrams T.Text where
    ngrams n = map (T.take n) . T.tails

instance Ngrams [a] where
    ngrams n = map (take n) . tails
