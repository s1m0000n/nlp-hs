{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Tok
import Bow ( buildVocab, tfIdfs, bowIOVec )
import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import System.Environment (getArgs)
import qualified Data.Vector as V
import Utils (argSort)
import Data.Set.Ordered (elemAt)
import Control.Monad (forM, forM_)
import Pipeline (tokenLevelPipeline, defaultTokenLevelPipelineConfig)

-- for dynamic
-- import Utils (isJustTrue)
-- import Data.Dynamic (fromDynamic)
-- import Data.Functor ((<&>))


main :: IO ()
main = do
    -- NOTE: right now (before refactoring) this is used as a manual test sandbox (with reading file list from args)
    args <- getArgs

    -- 1. Just pretty print tokens of single file
    -- Tio.readFile (head args) >>= print . length . defaultTokenLevelPipeline 


    -- 2. ~Topic modelling with TF-IDF on multiple files
    docs <- forM args \fileName -> do
        text <- Tio.readFile fileName
        return 
            -- $ ngrams [1, 2, 3]
            $ map (T.toLower . fromToken) 
            $ filter (isT word . val) 
            $ tokenLevelPipeline defaultTokenLevelPipelineConfig text 
    let vocab = buildVocab $ foldr1 (++) docs
    bows <- mapM (bowIOVec vocab) docs
    let scores = tfIdfs bows
    forM_ (zip [0..] args) \(i, fileName) -> do
        let topWords = mapM (elemAt vocab) $ V.take 10 $ V.reverse $ V.convert $ argSort $ scores !! i
        putStrLn $ "10 most important words in \"" ++ fileName ++ "\" => " ++ show topWords
