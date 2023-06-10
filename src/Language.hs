module Language (Language(..), languageToSnowballAlgorithm) where

import qualified NLP.Snowball as S

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
