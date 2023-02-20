{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Tok where

import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Data.PartialSemigroup
import Data.Functor ((<&>))
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Function ((&))
import qualified Data.HashSet as HS
import Data.Trie.Text (Trie, fromList, match)

data Pos = Pos {
    start :: Int,
    end :: Int
}
data Value = Word T.Text | Punct T.Text | Space T.Text | INum Integer | FNum Double deriving Show
data Token = Token {
    val :: Value,
    pos :: Pos
}

class MaybeValueWithType a where
    word :: a -> Maybe T.Text
    punct :: a -> Maybe T.Text
    space :: a -> Maybe T.Text
    inum :: a -> Maybe Integer
    fnum :: a -> Maybe Double

    num :: a -> Maybe Double
    num x = (fromInteger <$> inum x) <|> fnum x
    isWord :: a -> Bool
    isWord = isJust . word
    isPunct :: a -> Bool
    isPunct = isJust . punct
    isSpace :: a -> Bool
    isSpace = isJust . space
    isINum :: a -> Bool
    isINum = isJust . inum
    isFNum :: a -> Bool
    isFNum = isJust . fnum
    isNum :: a -> Bool
    isNum x = isINum x || isFNum x
    wordP :: a -> (T.Text -> Bool) -> Bool
    wordP tok p = case p <$> word tok of
        Just True -> True
        _ -> False
    punctP :: a -> (T.Text -> Bool) -> Bool
    punctP tok p = case p <$> punct tok of
        Just True -> True
        _ -> False
    spaceP :: a -> (T.Text -> Bool) -> Bool
    spaceP tok p = case p <$> space tok of
        Just True -> True
        _ -> False
    inumP :: a -> (Integer -> Bool) -> Bool
    inumP tok p = case p <$> inum tok of
        Just True -> True
        _ -> False
    fNumEq :: a -> (Double -> Bool) -> Bool
    fNumEq tok p = case p <$> fnum tok of
        Just True -> True
        _ -> False
    numEq :: a -> (Double -> Bool) -> Bool
    numEq tok p = case p <$> num tok of
        Just True -> True
        _ -> False

class RawFromTokenVal a where
    raw :: Value -> a
    rawVal :: Token -> a
    rawVal tok = raw $ val tok

instance Show Pos where
    show (Pos s e) = "[" ++ show s ++ ":" ++ show e ++ "]"

instance Semigroup Pos where
    (<>) (Pos s _) (Pos _ e) = Pos s e

instance PartialSemigroup Value where
    (<>?) (Word s1) (Word s2) = Just $ Word $ s1 <> s2
    (<>?) (Punct s1) (Punct s2) = Just $ Punct $ s1 <> s2
    (<>?) (Space s1) (Space s2) = Just $ Space $ s1 <> s2
    (<>?) _ _ = Nothing

instance MaybeValueWithType Value where
    word (Word t) = Just t
    word _ = Nothing
    punct (Punct t) = Just t
    punct _ = Nothing
    space (Space t) = Just t
    space _ = Nothing
    inum (INum i) = Just i
    inum _ = Nothing
    fnum (FNum f) = Just f
    fnum _ = Nothing

instance MaybeValueWithType Token where
    word = word . val
    punct = punct . val
    space = space . val
    inum = inum . val
    fnum = fnum . val

instance Show Token where
    show (Token value pos) = show pos ++ " " ++ show value

instance PartialSemigroup Token where
    (<>?) (Token value1 pos1) (Token value2 pos2) = value1 <>? value2 <&> \v -> Token v $ pos1 <> pos2

instance RawFromTokenVal T.Text where
    raw (Word t) = t
    raw (Punct t) = t
    raw (Space t) = t
    raw (INum n) = T.pack $ show n
    raw (FNum n) = T.pack $ show n

instance RawFromTokenVal String where
    raw (Word t) = T.unpack t
    raw (Punct t) = T.unpack t
    raw (Space t) = T.unpack t
    raw (INum n) = show n
    raw (FNum n) = show n

data TokenizerConfig = TokenizerConfig {
    specials :: Trie (T.Text -> Value, Int),
    parseINum :: Bool,
    parseFNum :: Bool,
    fNumDels :: [T.Text],
    removeSpaces :: Bool,
    removeNumbers :: Bool,
    removeWords :: [T.Text],
    removePunct :: Bool,
    eliminateHyphens :: Bool,
    hyphens :: HS.HashSet T.Text 
}
defaultTokenizerConfig :: TokenizerConfig
defaultTokenizerConfig= TokenizerConfig {
    specials=fromList $ zip puncts (map ((Punct,) . T.length) puncts) ++ zip spaces (map ((Space,) . T.length) spaces),
    parseINum=True,
    parseFNum=True,
    fNumDels=["."],
    removeSpaces=False,
    removeNumbers=False,
    removeWords=[],
    removePunct=False,
    eliminateHyphens=True,
    hyphens=HS.singleton "-"
} where 
    puncts = ["...", ",", "!", "?", ".", ";", "(", ")", "[", "]", "{", "}", "--", "-"]
    spaces = ["\t", "\n", " "]

special :: TokenizerConfig -> Int -> T.Text -> Maybe (Token, T.Text)
special cfg posShift text = match (specials cfg) text >>= \(tokText, (type_, len), remainder) -> Just (tok tokText type_ len, remainder)
    where tok tokText type_ len = Token {val=type_ tokText, pos=Pos {start=posShift, end=posShift + len}}

-- PERF: very slow tokenization with too much ram used
-- to be investigated further, for now - just leaving for TokFast
specialOrChar :: TokenizerConfig -> Int -> T.Text -> Maybe (Token, T.Text)
specialOrChar cfg posShift text = T.uncons text >>= \(x, xs) -> 
    special cfg posShift text <|> Just (Token {val=Word $ T.singleton x, pos=Pos {start=posShift, end=posShift + 1}}, xs)

tokenizeSpecials :: TokenizerConfig -> Int -> T.Text -> [Token]
tokenizeSpecials _ _ "" = []
tokenizeSpecials cfg posShift text = case specialOrChar cfg posShift text of
    Nothing -> []
    Just (token, tailToks) -> token : tokenizeSpecials cfg (end $ pos token) tailToks

-- PERF: optimize as eliminateNLHyphens
parseWords :: [Token] -> [Token]
parseWords (x1:x2:xs)
    | isJust (word x1) && isJust (word x2) = parseWords $ merged ++ xs
    | otherwise = x1 : parseWords (x2:xs)
        where
            merged = case x1 <>? x2 of
                Just tok -> [tok]
                Nothing -> [x1, x2]
parseWords xs = xs

-- PERF: optimize as eliminateNLHyphens
parseINums :: [Token] -> [Token]
parseINums (x:xs) = case readMaybe (rawVal x) of
    Just num -> x {val=INum num} : tailToks
    _ -> x : tailToks
    where tailToks = parseINums xs
parseINums [] = []

-- PERF: optimize as eliminateNLHyphens
parseFNums :: TokenizerConfig -> [Token] -> [Token]
parseFNums cfg (i1 : dot : i2 : xs)
    | isJust (sequence [word i1, word i2, punct dot]) && elem (rawVal dot) (fNumDels cfg)
        = case readMaybe $ concatMap rawVal [i1, dot, i2] of 
            Nothing -> i1 : dot : parseFNums cfg (i2 : xs)
            Just f -> Token {val=FNum f, pos = pos i1 <> pos i2} : parseFNums cfg xs
    | otherwise = i1 : parseFNums cfg (dot : i2 : xs)
parseFNums _ xs = xs

eliminateNLHyphens :: TokenizerConfig -> [Token] -> [Token]
eliminateNLHyphens cfg (w1 : h : nl : w2 : ts)
    | punctP h (`HS.member` hyphens cfg) && spaceP nl (== "\n") = 
        let mbNewWord = word w1 >>= \w1t -> word w2 <&> \w2t -> Token {val=Word $ T.concat [w1t, T.singleton '-', w2t], pos=pos w1 <> pos w2}
        in case mbNewWord of 
            Just newWord -> newWord : eliminateNLHyphens cfg ts
            Nothing -> w1 : h : nl : eliminateNLHyphens cfg (w2 : ts)
    | otherwise = w1 : eliminateNLHyphens cfg (h : nl : w2 : ts)
eliminateNLHyphens _ xs = xs

(-?>) :: Bool -> (a -> a) -> (a -> a)
(-?>) True f = f
(-?>) False _ = id

applyPostprocessing :: TokenizerConfig -> [Token] -> [Token]
applyPostprocessing cfg tokens = tokens 
    & parseFNum cfg -?> parseFNums cfg
    & parseINum cfg -?> parseINums
    & removeSpaces cfg -?> filter (not . isSpace)
    & removeNumbers cfg -?> filter (not . isNum)
    & eliminateHyphens cfg -?> eliminateNLHyphens cfg
    & removePunct cfg -?> filter (not . isPunct)
    & not (null (removeWords cfg)) -?> 
        filter (\t -> case word t of
            Just w -> notElem w $ removeWords cfg
            Nothing -> True
        )

tokenize :: TokenizerConfig -> T.Text -> [Token]
tokenize cfg text = tokenizeSpecials cfg 0 text & parseWords & applyPostprocessing cfg 

fromTokens :: RawFromTokenVal a => [Token] -> [a]
fromTokens = map rawVal

-- Reassembles close to original text
foldFromTokens :: RawFromTokenVal a => Monoid a => [Token] -> a
foldFromTokens = foldr ((<>) . rawVal) mempty

-- TODO: use text builder
-- TODO: avoid string conversions
prettyTextTokens :: [Token] -> T.Text
prettyTextTokens = foldl (\acc tok -> acc <> T.cons '\n' (T.pack (show tok))) ""
