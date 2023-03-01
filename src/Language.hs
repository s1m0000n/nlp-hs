module Language (Language(..), languageToSnowballAlgorithm) where
import qualified NLP.Snowball as S

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

languageToSnowballAlgorithm :: Language -> S.Algorithm
languageToSnowballAlgorithm Danish = S.Danish 
languageToSnowballAlgorithm Dutch = S.Dutch
languageToSnowballAlgorithm English = S.English
languageToSnowballAlgorithm Finnish = S.Finnish
languageToSnowballAlgorithm French = S.French
languageToSnowballAlgorithm German = S.German
languageToSnowballAlgorithm Hungarian = S.Hungarian
languageToSnowballAlgorithm Italian = S.Italian
languageToSnowballAlgorithm Norwegian = S.Norwegian
languageToSnowballAlgorithm Portuguese = S.Portuguese
languageToSnowballAlgorithm Romanian = S.Romanian 
languageToSnowballAlgorithm Russian = S.Russian
languageToSnowballAlgorithm Spanish = S.Spanish
languageToSnowballAlgorithm Swedish = S.Swedish
languageToSnowballAlgorithm Turkish = S.Turkish

