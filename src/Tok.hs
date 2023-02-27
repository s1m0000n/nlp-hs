{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Tok where

import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Data.PartialSemigroup
import Data.Functor ((<&>), ($>))
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Function ((&))
import qualified Data.HashSet as HS
import Data.Trie.Text (Trie, fromList, match)
import Data.List (union)
import Utils
import Data.Time (UTCTime (UTCTime))
import qualified Text.RE.TestBench.Parsers as TP

data Pos = Pos 
    { start :: Int
    , end :: Int
    }

data Value 
    = Word T.Text 
    | Punct T.Text 
    | Space T.Text 
    | INum Integer 
    | FNum Double 
    | DateTime UTCTime 
    | Special String T.Text -- key, value; for specific custom cases 
    deriving Show

data Prop 
    = SentStart | SentEnd
    | Joined
    | FromSpecial
    | Tag String
    deriving (Show, Eq)

data Token = Token 
    { val :: Value
    , pos :: Pos
    , props :: [Prop]
    }

class MaybeValueWithType a where
    word :: a -> Maybe T.Text
    punct :: a -> Maybe T.Text
    space :: a -> Maybe T.Text
    inum :: a -> Maybe Integer
    fnum :: a -> Maybe Double
    dateTime :: a -> Maybe UTCTime
    special :: a -> Maybe (String, T.Text)
    num :: a -> Maybe Double
    num x = (fromInteger <$> inum x) <|> fnum x

isT :: (a -> Maybe b) -> a -> Bool
isT t = isJust . t

predT :: (a -> Maybe b) -> (b -> Bool) -> a -> Bool
predT t p tok = isJustTrue $ p <$> t tok

-- TODO: improve spectrum of such functions
popT :: (Value -> Maybe b) -> [Token] -> Maybe ((Token, b), [Token])
popT t (x:xs) = t (val x) <&> \pureValue -> ((x, pureValue), xs)
popT _ [] = Nothing

popIfData :: (Value -> Maybe b) -> (b -> Bool) -> [Token] -> Maybe ((Token, b), [Token])
popIfData t p (x:xs) = t (val x) >>= \pureValue -> if p pureValue then Just ((x, pureValue), xs) else Nothing
popIfData _ _ [] = Nothing

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
    dateTime (DateTime dt) = Just dt
    dateTime _ = Nothing
    special (Special k v) = Just (k, v)
    special _ = Nothing

instance Show Token where
    show (Token value pos props)  = show pos ++ " " ++ show value ++ " (props: " ++ propsRepr ++ ")"
      where
        propsRepr = foldl (\acc n -> acc ++ show n ++ ", ") "" props

instance PartialSemigroup Token where
    (<>?) (Token value1 pos1 props1) (Token value2 pos2 props2) = value1 <>? value2 <&> \v -> Token v pos props
      where
        pos = pos1 <> pos2
        props = [Joined] `union` props1 `union` props2

instance RawFromTokenVal T.Text where
    raw (Word t) = t
    raw (Punct t) = t
    raw (Space t) = t
    raw (INum n) = T.pack $ show n
    raw (FNum n) = T.pack $ show n
    raw (DateTime dt) = T.pack $ show dt
    raw (Special _ t) = t

instance RawFromTokenVal String where
    raw (Word t) = T.unpack t
    raw (Punct t) = T.unpack t
    raw (Space t) = T.unpack t
    raw (INum n) = show n
    raw (FNum n) = show n
    raw (DateTime dt) = show dt
    raw (Special _ t) = T.unpack t

data TokenizerConfig = TokenizerConfig 
    { preWordMatchers :: Trie (T.Text -> Value, Int) -- trie of converter, length
    , parseINum :: Bool
    , parseFNum :: Bool
    , fNumDels :: HS.HashSet T.Text
    , removeSpaces :: Bool
    , removeNumbers :: Bool
    , removeWords :: [T.Text]
    , removePunct :: Bool
    , eliminateHyphens :: Bool
    , hyphens :: HS.HashSet T.Text
    , parseDateTime :: Bool
    , specials :: [[Token] -> [Token]] -- modifies tokens list
    }

defaultTokenizerConfig :: TokenizerConfig
defaultTokenizerConfig= TokenizerConfig 
    { preWordMatchers=fromList $ zip puncts (map ((Punct,) . T.length) puncts) ++ zip spaces (map ((Space,) . T.length) spaces)
    , parseINum=True
    , parseFNum=True
    , fNumDels=HS.singleton "."
    , removeSpaces=False
    , removeNumbers=False
    , removeWords=[]
    , removePunct=False
    , eliminateHyphens=True
    , hyphens=HS.singleton "-"
    , parseDateTime=False
    , specials=[]
    } 
  where 
    puncts = ["...", ",", "!", "?", ".", ";", "(", ")", "[", "]", "{", "}", "--", "-"]
    spaces = ["\t", "\n", " "]

matchPreWords :: TokenizerConfig -> Int -> T.Text -> Maybe (Token, T.Text)
matchPreWords cfg posShift text = liftFst tok . to2h <$> match (preWordMatchers cfg) text 
  where 
    tok (tokText, (type_, len)) = Token 
        { val=type_ tokText
        , pos=Pos 
            { start=posShift
            , end=posShift + len
            }
        , props=[]
        }

preWordsOrChar :: TokenizerConfig -> Int -> T.Text -> Maybe (Token, T.Text)
preWordsOrChar cfg posShift text = matchPreWords cfg posShift text 
    <|> liftFst (\x -> Token 
            { val=Word $ T.singleton x
            , pos=Pos 
                { start=posShift
                , end=posShift + 1
                }
            , props=[]
            }
        ) <$> T.uncons text

tokenizePreWords :: TokenizerConfig -> Int -> T.Text -> [Token]
tokenizePreWords _ _ "" = []
tokenizePreWords cfg posShift text = case preWordsOrChar cfg posShift text of
    Nothing -> []
    Just (token, tailToks) -> token : tokenizePreWords cfg (end $ pos token) tailToks

nextTok :: ([Token] -> [Token]) -> [Token] -> [Token]
nextTok f (x:xs) = x : f xs
nextTok _ [] = []

parseWords :: [Token] -> [Token]
parseWords toks = nextTok parseWords toks 
    <? popT word toks >>|^ popT word >>= \((w1, _), ((w2, _), t)) -> w1 <>? w2 <&> parseWords . (: t)

-- Post-processing

-- TODO: refactor (<? nextTok (parseFNums cfg) toks) parts to a single upper level func
-- Example: f = inumParser <|> fnumParser <|> ... ?> nextTok f
-- use: https://hackage.haskell.org/package/cond-0.4.1.1/docs/Control-Conditional.html

parseINums :: [Token] -> [Token]
parseINums toks = nextTok parseINums toks 
    <? popT word toks >>= \((w1, w1d), t) -> readMaybe (T.unpack w1d) <&> \i -> w1 {val=INum i} : parseINums t

parseFNums :: TokenizerConfig -> [Token] -> [Token]
parseFNums cfg toks = nextTok (parseFNums cfg) toks
    <? popT word toks 
    >>|^ popIfData punct (`HS.member` fNumDels cfg)
    >>|^.|^ popT word
    >>= \((w1, w1Data), ((d, dData), ((w2, _), t))) -> Token {val=Word $ w1Data <> dData, pos=pos w1 <> pos d, props=props w1 `union` props d} <>? w2 
    >>= \w -> word (val w) >>= readMaybe . T.unpack
    <&> \f -> w {val=FNum f} : parseFNums cfg t

eliminateNLHyphens :: TokenizerConfig -> [Token] -> [Token]
eliminateNLHyphens cfg (w1 : h : nl : w2 : ts)
    | predT punct (`HS.member` hyphens cfg) (val h) && predT space (== "\n") (val nl) = 
      let mbNewWord = word (val w1) >>= \w1t -> word (val w2) >>= \w2t 
            -> w1 <>? w2 <&> \tok -> tok { val=Word $ T.concat [w1t, T.singleton '-', w2t] }
      in case mbNewWord of 
            Just newWord -> newWord : eliminateNLHyphens cfg ts
            Nothing -> w1 : h : nl : eliminateNLHyphens cfg (w2 : ts)
    | otherwise = w1 : eliminateNLHyphens cfg (h : nl : w2 : ts)
eliminateNLHyphens _ xs = xs

-- WARN: does not work!
-- PERF: unoptimized (worse than naive); experimental
-- parseDateTimes :: [Token] -> [Token]
-- parseDateTimes toks = nextTok parseDateTimes toks
--     <? tryFindLongest toks
--     <&> \((dt, dtData), t) -> dt {val=DateTime dtData} : parseDateTimes t
--   where
--     tryFindLongest = (\((dtTok, _, mbDtData), acc) -> mbDtData <&> \dtData -> ((dtTok, dtData), acc)) . tryFindLongestHelper
--     tryFindLongestHelper = foldlUntil (\(accTok, accText, _) next -> 
--         (\v -> word v <|> punct v) (val next) >>= \nextText -> 
--         let newAccText = accText <> nextText
--             in TP.parseDateTime newAccText >>= (\dt -> (,newAccText, Just dt) <$> (accTok <>? next))
--         ) (Token{val=Word "", pos=Pos{start=0, end=0}, props=[]}, "", Nothing)

applyPostprocessing :: TokenizerConfig -> [Token] -> [Token]
applyPostprocessing cfg tokens = tokens 
    -- & parseDateTime cfg -?> parseDateTimes
    & parseFNum cfg -?> parseFNums cfg
    & parseINum cfg -?> parseINums
    & removeSpaces cfg -?> filter (not . isT space . val)
    & removeNumbers cfg -?> filter (not . isT num . val)
    & eliminateHyphens cfg -?> eliminateNLHyphens cfg
    & removePunct cfg -?> filter (not . isT punct . val)
    & not (null (removeWords cfg)) -?> 
        filter (\t -> case word $ val t of
            Just w -> notElem w $ removeWords cfg
            Nothing -> True
        )

tokenize :: TokenizerConfig -> T.Text -> [Token]
tokenize cfg text = foldr (.) (applyPostprocessing cfg) (specials cfg) $ parseWords $ tokenizePreWords cfg 0 text 

fromTokens :: RawFromTokenVal a => [Token] -> [a]
fromTokens = map rawVal

-- Reassembles close to original text
foldFromTokens :: RawFromTokenVal a => Monoid a => [Token] -> a
foldFromTokens = foldr ((<>) . rawVal) mempty

-- TODO: use text builder
-- TODO: avoid string conversions
prettyTextTokens :: [Token] -> T.Text
prettyTextTokens = foldl (\acc tok -> acc <> T.cons '\n' (T.pack (show tok))) ""
