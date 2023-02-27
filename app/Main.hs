{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Tok
-- import TokFast
import Preproc (filterSW, defaultStopwords, ngrams)
import Bow ( buildVocab, bowMVecIO, tfIdfs )
import Doc ( Doc )
import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import System.Environment (getArgs)
import qualified Data.Vector as V
import Utils (argSort)
import Data.Set.Ordered (elemAt)
import Control.Monad (forM, forM_)
import Data.Traversable (for)


main :: IO ()
main = do
    args <- getArgs
    Tio.readFile (head args) >>= Tio.putStr . prettyTextTokens . tokenize defaultTokenizerConfig {parseDateTime=True}
    -- docs <- forM args \fileName -> do
    --     text <- Tio.readFile fileName
    --     let typedTokens = tokenize (defaultTokenizerConfig {parseDateTime=True}) text
    --     let cleanTokens = filterSW defaultStopwords $ map T.toLower $ fromTokens $ filter (isT word . val) typedTokens :: Doc
    --     -- return $ ngrams [1, 2, 3] cleanTokens
    --     return cleanTokens
    -- let megaDoc = foldr1 (++) docs
    -- let vocab = buildVocab megaDoc
    -- bows <- mapM (bowMVecIO vocab) docs
    -- let scores = tfIdfs bows
    -- forM_ (zip [0..] args) \(i, fileName) -> do
    --     let topWords = mapM (elemAt vocab) $ V.take 10 $ V.reverse $ V.convert $ argSort $ scores !! i
    --     putStrLn $ "10 most important words in \"" ++ fileName ++ "\" => " ++ show topWords


    -- texts <- mapM Tio.readFile args
    -- let docs = for texts $ filterSW defaultStopwords . map T.toLower . fromTokens . filter (isT word . val) . tokenize defaultTokenizerConfig
