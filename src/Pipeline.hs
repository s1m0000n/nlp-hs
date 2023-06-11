-- |
-- Module      : Pipeline
-- Description : Popular ready NLP pipelines & simplified configuration tools
-- Stability   : experimental
{-# LANGUAGE ImportQualifiedPost #-}
module Pipeline (TokenLevelPipelineConfig (..), tokenizer, stemmer, tokenLevelPipeline, defaultTokenLevelPipelineConfig) where

import Control.Conditional ((<|))
import Data.Function ((&))
import Data.HashSet qualified as HS
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Language (Language (..), languageToSnowballAlgorithm)
import NLP.Snowball (stem)
import StopWords qualified as SW
import Tok (MaybeValueWithType (..), Token (val), TokenizerConfig (stopWords), Value (..), defaultTokenizerConfig, tokenize)
import Utils ((-?>))

data TokenLevelPipelineConfig = TokenLevelPipelineConfig
  { applyStemming :: Bool, -- WARN: produces incorrect words with pseudo normal form, useful for some cases
    languages :: [Language],
    autoIncludeStopWords :: Bool,
    tokenizerConfig :: Tok.TokenizerConfig
  }

defaultTokenLevelPipelineConfig :: TokenLevelPipelineConfig
defaultTokenLevelPipelineConfig =
  TokenLevelPipelineConfig
    { applyStemming = False,
      languages = [English],
      autoIncludeStopWords = True,
      tokenizerConfig = Tok.defaultTokenizerConfig
    }

data TokenLevelPipelineConfig' = TokenLevelPipelineConfig'
  { applyStemming' :: Bool,
    languages' :: [Language],
    tokenizerConfig' :: Tok.TokenizerConfig
  }

tokenizer :: TokenLevelPipelineConfig' -> T.Text -> [Tok.Token]
tokenizer config' = Tok.tokenize $ tokenizerConfig' config'

singleLanguageStemmer :: Language -> [Tok.Token] -> [Tok.Token]
singleLanguageStemmer lang = case stem <$> languageToSnowballAlgorithm lang of
  Just stem_ -> map (\token -> token <| ((\data_ -> token {val = Tok.Word data_}) . stem_ <$> Tok.word (val token)))
  Nothing -> id

multiLanguageStemmer :: NEL.NonEmpty Language -> [Tok.Token] -> [Tok.Token]
multiLanguageStemmer = foldr1 (.) . NEL.map singleLanguageStemmer

stemmer :: TokenLevelPipelineConfig' -> [Tok.Token] -> [Tok.Token]
stemmer config = applyStemming' config -?> multiLanguageStemmer (NEL.fromList $ languages' config)

tokenLevelPipeline :: TokenLevelPipelineConfig -> T.Text -> [Tok.Token]
tokenLevelPipeline config text = stemmer config' $ tokenizer config' text
  where
    lang = languages config
    config' = TokenLevelPipelineConfig'
        { applyStemming' = applyStemming config,
          languages' = lang,
          tokenizerConfig' =
            tokenizerConfig config & autoIncludeStopWords config
              -?> \cfg -> cfg {stopWords = HS.unions $ mapMaybe SW.stopWords lang}
        }
