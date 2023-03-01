{-|
Module      : Pipeline
Description : Popular ready NLP pipelines & simplified configuration tools
Stability   : experimental
-}

{-# LANGUAGE LambdaCase #-}

module Pipeline (SimpleTokenizerConfig(..), TokenLevelPipelineConfig(..), tokenizer, stemmer, tokenLevelPipeline, defaultTokenLevelPipelineConfig, defaultTokenLevelPipeline) where

import qualified Tok
import qualified Data.Text as T
import Control.Conditional ((<|))
import Language (Language(..), languageToSnowballAlgorithm)
import NLP.Snowball (stem)
import qualified Data.List.NonEmpty as NEL
import Data.Functor ((<&>))
import Tok (Token (val), Value(..), MaybeValueWithType(..), defaultTokenizerConfig, tokenize, TokenizerConfig)
import qualified StopWords as SW
import Data.Maybe (mapMaybe)
import qualified Data.HashSet as HS
import Utils ((-?>))


data TokenLevelPipelineConfig = TokenLevelPipelineConfig
    { applyStemming :: Bool
    , languages :: [Language]
    , tokenizerConfig :: Maybe (Either SimpleTokenizerConfig TokenizerConfig)
    }

defaultTokenLevelPipelineConfig :: TokenLevelPipelineConfig
defaultTokenLevelPipelineConfig = TokenLevelPipelineConfig 
    { applyStemming=True
    , languages=[English]
    , tokenizerConfig=Just $ Right defaultTokenizerConfig
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


tokenizer :: TokenLevelPipelineConfig -> T.Text -> [Token]
tokenizer config = tokenize $ defaultTokenizerConfig <| (
        tokenizerConfig config <&> \case
            Left simpleConfig -> defaultTokenizerConfig 
                { Tok.lowerCase=lowerCase simpleConfig
                , Tok.parseINum=parseNumbers simpleConfig
                , Tok.parseFNum=parseNumbers simpleConfig
                , Tok.sameSpacesResolutionStrategy=if applySafeJoins simpleConfig then Tok.Join else Tok.None
                , Tok.eliminateHyphens=applySafeJoins simpleConfig
                , Tok.parseDateTime=parseDateTime simpleConfig
                , Tok.removePunct=removePunct simpleConfig
                , Tok.stopWords=if removeStopWords simpleConfig then HS.unions $ mapMaybe SW.stopWords $ languages config else HS.empty
                }
            Right regularConfig -> regularConfig 
    )

singleLanguageStemmer :: Language -> [Token] -> [Token] 
singleLanguageStemmer lang = map (\token -> token <| ((\stemmedData -> token {val=Word stemmedData}) <$> stem (languageToSnowballAlgorithm lang)) <$> word (val token))

multiLanguageStemmer :: NEL.NonEmpty Language -> [Token] -> [Token]
multiLanguageStemmer = foldr1 (.) . NEL.map singleLanguageStemmer

stemmer :: TokenLevelPipelineConfig -> [Token] -> [Token]
stemmer config = applyStemming config -?> multiLanguageStemmer ( NEL.fromList $ languages config)

tokenLevelPipeline :: TokenLevelPipelineConfig -> T.Text -> [Token]
tokenLevelPipeline config = stemmer config . tokenizer config

defaultTokenLevelPipeline :: T.Text -> [Token]
defaultTokenLevelPipeline = tokenLevelPipeline defaultTokenLevelPipelineConfig
