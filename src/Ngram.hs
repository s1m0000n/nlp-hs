module Ngram (ngram, ngrams) where

import qualified Data.Text as T
import Data.List (tails)
import Doc (Doc)

ngram :: Int -> Doc -> Doc
ngram n doc
    | n <= 0 = error "N-gram size is always >= 1"
    | n == 1 = doc
    | otherwise = foldl1 (\acc t -> if length t < n then acc else T.unwords (take n t) : acc) $ tails doc

ngrams :: [Int] -> Doc -> Doc
ngrams (x:xs) doc = ngram x doc ++ ngrams xs doc
ngrams [] _ = []
