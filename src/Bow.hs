module Bow (Vocab, buildVocab, bowMVec, bowMVecIO, termFreqs, inverseDocFreqs, tfIdfs) where

import qualified Data.Text as T
import qualified Data.Set.Ordered as OS
import Doc
-- import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad (forM_)
import Control.Monad.ST (ST, stToIO)
import qualified Data.Vector.Unboxed as V
import GHC.Float (logDouble)

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
            VM.modify vec (+ 1)
    return vec

bowMVecIO :: Vocab -> Doc -> IO (V.Vector Int)
bowMVecIO vocab tokens = stMVtoIO $ bowMVec vocab tokens
    where stMVtoIO v = stToIO $ v >>= V.freeze

-- TODO: implement efficient immutable solution as well
-- bowVec :: Vocab -> Doc -> V.Vector Int
-- bowVec vocab tokens = V.replicate (length vocab) 0 where 
--     indices = mapMaybe (`OS.findIndex` vocab) tokens
-- bowVec vocab [] = V.replicate (length vocab) 0 
-- bowVec vocab (t:ts) = OS.findIndex t vocab <&> \i -> 
-- bowVec vocab (t:ts) = 

termFreqs :: V.Vector Int -> V.Vector Double
termFreqs b = V.map (\c -> fromIntegral c / s) b
    where s = fromIntegral $ V.sum b

inverseDocFreqs :: [V.Vector Int] -> V.Vector Double
inverseDocFreqs bs = V.map (\c -> logDouble $ numDoc / fromIntegral c) $ foldl1 (V.zipWith (+)) bs
    where numDoc = fromIntegral $ length bs

tfIdfs :: [V.Vector Int] -> [V.Vector Double]
tfIdfs bs = map (V.zipWith (*) idfs . termFreqs) bs
    where idfs = inverseDocFreqs bs

-- TODO: implement Naive Bayes Classifier
-- data NBModelType = Bernoulli | Multinomial
-- data NBModel = NBModel {
--     log_p_cls :: Vector Double,
--     §
-- }
-- trainNB :: NBModelType -> [(V.Vector Int, Int)] -> NBModel 

