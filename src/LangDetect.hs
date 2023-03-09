module LangDetect (LangDetectNgramModelConfig, defaultLangDetectNgramModelConfig) where

import Tok (Token, MaybeValueWithType (word), val)
import qualified Data.Text as T
import Data.Trie.Text (Trie)
import Language (Language)
import Doc (Doc)

data LangDetectNgramModelConfig = LangDetectNgramModelConfig
    { ns :: [Int]
    }

defaultLangDetectNgramModelConfig :: LangDetectNgramModelConfig
defaultLangDetectNgramModelConfig = LangDetectNgramModelConfig {ns=[1, 2, 3]}

type LangDetectNgramModel = Trie Language 

-- trainLangDetectNgramModel :: LangDetectNgramModelConfig -> [(Language, [Token])] -> LangDetectNgramModel
-- trainLangDetectNgramModel config trainData = LangDetectNgramModel config $ 

-- predictProba :: LangDetectNgramModel -> Doc -> [(Language, Double)]
-- predict :: LangDetectNgramModel -> Doc -> Language

-- TODO: use bow model
