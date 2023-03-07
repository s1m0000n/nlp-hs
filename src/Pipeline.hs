{-|
Module      : Pipeline
Description : Popular ready NLP pipelines & simplified configuration tools
Stability   : experimental
-}

{-# LANGUAGE LambdaCase #-}

module Pipeline (SimpleTokenizerConfig(..), TokenLevelPipelineConfig(..), tokenizer, stemmer, tokenLevelPipeline, defaultTokenLevelPipelineConfig, LanguagesConfig(..)) where

import qualified Tok
import qualified Data.Text as T
import Control.Conditional ((<|))
import Language (Language(..), languageToSnowballAlgorithm, detectLanguageGroup)
import NLP.Snowball (stem)
import qualified Data.List.NonEmpty as NEL
import Data.Functor ((<&>))
import Tok (Token (val), Value(..), MaybeValueWithType(..), defaultTokenizerConfig, tokenize, TokenizerConfig)
import qualified StopWords as SW
import Data.Maybe (mapMaybe)
import qualified Data.HashSet as HS
import Utils ((-?>))

data LanguagesConfig 
    = Auto -- WARN: experimental, unsafe
           -- PERF: current implementation takes ~ 5 * time (whole other tokenLevelPipeline) to lang detect
    | List [Language]

data TokenLevelPipelineConfig = TokenLevelPipelineConfig
    { applyStemming :: Bool -- WARN: produces incorrect words with pseudo normal form
    , languages :: LanguagesConfig
    , tokenizerConfig :: Maybe (Either SimpleTokenizerConfig TokenizerConfig)
    }

defaultTokenLevelPipelineConfig :: TokenLevelPipelineConfig
defaultTokenLevelPipelineConfig = TokenLevelPipelineConfig 
    { applyStemming=False
    , languages=List [English]
    , tokenizerConfig=Just $ Right defaultTokenizerConfig
    }

data TokenLevelPipelineConfig' = TokenLevelPipelineConfig'
    { applyStemming' :: Bool
    , languages' :: [Language]
    , tokenizerConfig' :: TokenizerConfig
    }

data SimpleTokenizerConfig = SimpleTokenizerConfig
    { lowerCase :: Bool
    , applySafeJoins :: Bool

    , parseNumbers :: Bool
    , parsePrefixMoney :: Bool
    , parseSuffixMoney :: Bool
    , parseDateTime :: Bool

    , removeStopWords :: Bool
    , removePunct :: Bool
    }


tokenizer :: TokenLevelPipelineConfig' -> T.Text -> [Token]
tokenizer config' = tokenize $ tokenizerConfig' config'

singleLanguageStemmer :: Language -> [Token] -> [Token] 
singleLanguageStemmer lang = case stem <$> languageToSnowballAlgorithm lang of
    Just stem_ -> map (\token -> token <| ((\data_ -> token {val=Word data_}) . stem_ <$> word (val token)))
    Nothing -> id

multiLanguageStemmer :: NEL.NonEmpty Language -> [Token] -> [Token]
multiLanguageStemmer = foldr1 (.) . NEL.map singleLanguageStemmer

stemmer :: TokenLevelPipelineConfig' -> [Token] -> [Token]
stemmer config = applyStemming' config -?> multiLanguageStemmer ( NEL.fromList $ languages' config)

tokenLevelPipeline :: TokenLevelPipelineConfig -> T.Text -> [Token]
tokenLevelPipeline config text = stemmer config' $ tokenizer config' text
  where
    lang = case languages config of
        Auto -> detectLanguageGroup text
        List list -> list
    config' = TokenLevelPipelineConfig' 
        { applyStemming'=applyStemming config
        , languages'=lang
        , tokenizerConfig'= defaultTokenizerConfig <| (
            tokenizerConfig config <&> \case
                Left simpleConfig -> defaultTokenizerConfig 
                    { Tok.lowerCase=lowerCase simpleConfig
                    , Tok.parseINum=parseNumbers simpleConfig
                    , Tok.parseFNum=parseNumbers simpleConfig
                    , Tok.sameSpacesResolutionStrategy=if applySafeJoins simpleConfig then Tok.Join else Tok.None
                    , Tok.eliminateHyphens=applySafeJoins simpleConfig
                    , Tok.parseDateTime=parseDateTime simpleConfig
                    , Tok.removePunct=removePunct simpleConfig
                    , Tok.stopWords=if removeStopWords simpleConfig then HS.unions $ mapMaybe SW.stopWords lang else HS.empty
                    }
                Right regularConfig -> regularConfig 
            )
        }
