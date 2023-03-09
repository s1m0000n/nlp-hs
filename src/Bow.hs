module Bow (Vocab, buildVocab, bowMVec, bowIOVec, termFreqs, inverseDocFreqs, tfIdfs, bowVec) where

import qualified Data.Text as T
import qualified Data.Set.Ordered as OS
import Doc
-- import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad (forM_)
import Control.Monad.ST (ST, stToIO)
import qualified Data.Vector.Unboxed as V
import GHC.Float (logDouble)
import Control.Conditional ((<|))
import Data.Function.Flip (flip3)

type Vocab = OS.OSet T.Text
class BuildVocab a where
    buildVocab :: a -> Vocab
instance BuildVocab Doc where
    buildVocab = OS.fromList
instance BuildVocab [Doc] where
    buildVocab = buildVocab . foldr1 (++)

bowMVec :: Vocab -> Doc -> ST s (VM.MVector s Int)
bowMVec vocab tokens = do 
    vec <- VM.new $ length vocab
    forM_ tokens $ \token ->
        forM_ (OS.findIndex token vocab) $
            VM.unsafeModify vec (+ 1)
    return vec

bowIOVec :: Vocab -> Doc -> IO (V.Vector Int)
bowIOVec vocab tokens = stMVtoIO $ bowMVec vocab tokens
    where stMVtoIO v = stToIO $ v >>= V.unsafeFreeze

-- PERF: many times slower that mutable version
bowVec :: Vocab -> Doc -> V.Vector Int
bowVec vocab [] = V.replicate (OS.size vocab) 0
bowVec vocab (x:xs) = (id <| ((\i -> V.modify (flip3 VM.modify i (+1))) <$> OS.findIndex x vocab)) $ bowVec vocab xs

-- TODO: extract to separate module
termFreqs :: V.Vector Int -> V.Vector Double
termFreqs b = V.map (\c -> fromIntegral c / s) b
    where s = fromIntegral $ V.sum b

inverseDocFreqs :: [V.Vector Int] -> V.Vector Double
inverseDocFreqs bs = V.map (\c -> logDouble $ numDoc / fromIntegral c) $ foldl1 (V.zipWith (+)) bs
    where numDoc = fromIntegral $ length bs

tfIdfs :: [V.Vector Int] -> [V.Vector Double]
tfIdfs bs = map (V.zipWith (*) idfs . termFreqs) bs
    where idfs = inverseDocFreqs bs

