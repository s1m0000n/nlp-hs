{-# LANGUAGE OverloadedStrings #-}

module TokFast where

import qualified Data.Text as T
import qualified Data.HashSet as HS
import Control.Monad (guard)
import Data.Char (isAlpha)

data TokenizerConfig = TokenizerConfig {
    delims :: HS.HashSet Char
}

defaultTokenizerConfig :: TokenizerConfig
defaultTokenizerConfig = TokenizerConfig {
    delims=HS.fromList " \n\t\r.!,?:;(){}[]@#$%^&*-+_=/|\\~`"
}

updateHead :: (a -> a) -> [a] -> [a]
updateHead _ []       = []
updateHead f (a : as) = f a : as


tokenize :: TokenizerConfig -> T.Text -> [T.Text]
tokenize cfg = T.split (`HS.member` rmDel)
    where rmDel = delims cfg



