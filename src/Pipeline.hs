{-|
Module      : Pipeline
Description : Popular ready NLP pipelines & simplified configuration tools
Stability   : experimental
-}

module Pipeline (TokenLevelPipelineConfig(..), tokenizer, stemmer, tokenLevelPipeline, defaultTokenLevelPipelineConfig, LanguagesConfig(..)) where

import qualified Data.Text as T
import Control.Conditional ((<|))
import Language (Language(..), languageToSnowballAlgorithm, detectLanguageGroup)
import NLP.Snowball (stem)
import qualified Data.List.NonEmpty as NEL
import Tok (Token (val), Value(..), MaybeValueWithType(..), defaultTokenizerConfig, tokenize, TokenizerConfig (stopWords))
import qualified StopWords as SW
import Data.Maybe (mapMaybe)
import qualified Data.HashSet as HS
import Utils ((-?>))
import Data.Function ((&))

data LanguagesConfig 
    = Auto -- WARN: experimental, unsafe
           -- PERF: current implementation takes ~ 5 * time (whole other tokenLevelPipeline) to lang detect
    | List [Language]

data TokenLevelPipelineConfig = TokenLevelPipelineConfig
    { applyStemming :: Bool -- WARN: produces incorrect words with pseudo normal form, useful for some cases
    , languages :: LanguagesConfig
    , autoIncludeStopWords :: Bool
    , tokenizerConfig :: Tok.TokenizerConfig
    }

defaultTokenLevelPipelineConfig :: TokenLevelPipelineConfig
defaultTokenLevelPipelineConfig = TokenLevelPipelineConfig 
    { applyStemming=False
    , languages=List [English]
    , autoIncludeStopWords=True
    , tokenizerConfig=Tok.defaultTokenizerConfig
    }

data TokenLevelPipelineConfig' = TokenLevelPipelineConfig'
    { applyStemming' :: Bool
    , languages' :: [Language]
    , tokenizerConfig' :: Tok.TokenizerConfig
    }

tokenizer :: TokenLevelPipelineConfig' -> T.Text -> [Tok.Token]
tokenizer config' = Tok.tokenize $ tokenizerConfig' config'

singleLanguageStemmer :: Language -> [Tok.Token] -> [Tok.Token] 
singleLanguageStemmer lang = case stem <$> languageToSnowballAlgorithm lang of
    Just stem_ -> map (\token -> token <| ((\data_ -> token {val=Tok.Word data_}) . stem_ <$> Tok.word (val token)))
    Nothing -> id

multiLanguageStemmer :: NEL.NonEmpty Language -> [Tok.Token] -> [Tok.Token]
multiLanguageStemmer = foldr1 (.) . NEL.map singleLanguageStemmer

stemmer :: TokenLevelPipelineConfig' -> [Tok.Token] -> [Tok.Token]
stemmer config = applyStemming' config -?> multiLanguageStemmer ( NEL.fromList $ languages' config)

tokenLevelPipeline :: TokenLevelPipelineConfig -> T.Text -> [Tok.Token]
tokenLevelPipeline config text = stemmer config' $ tokenizer config' text
  where
    lang = case languages config of
        Auto -> detectLanguageGroup text
        List list -> list
    config' = TokenLevelPipelineConfig' 
        { applyStemming'=applyStemming config
        , languages'=lang
        , tokenizerConfig'= tokenizerConfig config 
            & autoIncludeStopWords config -?> \cfg -> cfg { stopWords=HS.unions $ mapMaybe SW.stopWords lang }
        }
