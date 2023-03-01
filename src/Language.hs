module Language (Language(..), languageToSnowballAlgorithm, detectLanguageGroup) where

import qualified NLP.Snowball as S
import Hunch (identifyLanguage', Writing (..))
import qualified Data.Text as T

-- NOTE: just added Snowball-stemmer languages for now
data Language
    = Danish
    | Dutch
    | English
    | Finnish
    | French
    | German
    | Hungarian
    | Italian
    | Norwegian
    | Portuguese
    | Romanian
    | Russian
    | Spanish
    | Swedish
    | Turkish

languageToSnowballAlgorithm :: Language -> Maybe S.Algorithm
languageToSnowballAlgorithm Danish = Just S.Danish 
languageToSnowballAlgorithm Dutch = Just S.Dutch
languageToSnowballAlgorithm English = Just S.English
languageToSnowballAlgorithm Finnish = Just S.Finnish
languageToSnowballAlgorithm French = Just S.French
languageToSnowballAlgorithm German = Just S.German
languageToSnowballAlgorithm Hungarian = Just S.Hungarian
languageToSnowballAlgorithm Italian = Just S.Italian
languageToSnowballAlgorithm Norwegian = Just S.Norwegian
languageToSnowballAlgorithm Portuguese = Just S.Portuguese
languageToSnowballAlgorithm Romanian = Just S.Romanian 
languageToSnowballAlgorithm Russian = Just S.Russian
languageToSnowballAlgorithm Spanish = Just S.Spanish
languageToSnowballAlgorithm Swedish = Just S.Swedish
languageToSnowballAlgorithm Turkish = Just S.Turkish

-- WARN: to be replaced soon (maybe with internal solution, based on Text)
-- PERF: VERY SLOW, takes ~ 5 * time (whole other tokenLevelPipeline) to lang detect
detectLanguageGroup :: T.Text -> [Language]
detectLanguageGroup text = case identifyLanguage' $ T.unpack text of
    WLatin -> [Danish, Dutch, English, Finnish, French, German, Hungarian, Italian, Norwegian, Portuguese, Romanian, Spanish, Swedish, Turkish]
    WCyrillic -> [Russian]
    _ -> [] -- hebrew is being detected by Hunch, but missing support in current system
