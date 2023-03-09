module Ngram (ngrams, multiNgrams) where

import qualified Data.Text as T
import Data.List (tails)

ngrams :: Int -> [T.Text] -> [T.Text]
ngrams n doc
    | n <= 0 = error "N-gram size is always >= 1"
    | n == 1 = doc
    | otherwise = foldl1 (\acc t -> if length t < n then acc else T.unwords (take n t) : acc) $ tails doc

multiNgrams :: [Int] -> [T.Text] -> [T.Text]
multiNgrams (x:xs) doc = ngrams x doc ++ multiNgrams xs doc
multiNgrams [] _ = []
