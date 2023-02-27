{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Tok where

import Data.Maybe (isJust, fromMaybe)
import Control.Applicative ((<|>))
import Data.PartialSemigroup
import Data.Functor ((<&>), ($>))
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.HashSet as HS
import Data.Trie.Text (Trie, fromList, match)
import Data.List (union)
import Utils
import Data.Time (UTCTime (UTCTime))
import Control.Conditional (guard, (<|))

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
    , props :: [Prop] -- TODO: replace with a set
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
    , removeWords :: HS.HashSet T.Text
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
    , removeWords=HS.empty
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
    <| popT word toks 
    >>|^ popT word 
    >>= \((w1, _), ((w2, _), t)) -> w1 <>? w2 
    <&> parseWords . (: t)

inumParser :: [Token] -> Maybe [Token]
inumParser toks =
    popT word toks 
    >>= \((w1, w1d), t) -> readMaybe (T.unpack w1d) 
    <&> \i -> (w1 {val=INum i}) : t

fnumParser :: TokenizerConfig -> [Token] -> Maybe [Token]
fnumParser cfg toks = 
    popT word toks 
    >>|^ popIfData punct (`HS.member` fNumDels cfg)
    >>|^.|^ popT word
    >>= \((w1, w1Data), ((d, dData), ((w2, _), t))) -> Token {val=Word $ w1Data <> dData, pos=pos w1 <> pos d, props=props w1 `union` props d} <>? w2 
    >>= \w -> word (val w) >>= readMaybe . T.unpack
    <&> \f -> (w {val=FNum f}) : t


nlHyphensParser :: TokenizerConfig -> [Token] -> Maybe [Token]
nlHyphensParser cfg toks =
    popT word toks
    >>|^ popIfData punct (`HS.member` hyphens cfg)
    >>|^.|^ popIfData space (== "\n") -- TODO: replace with hash set of newlines
    >>|^.|^.|^ popT word
    >>= \((w1, _), (_, (_, ((w2, _), t)))) -> w1 <>? w2
    <&> (: t)

extrasParser :: TokenizerConfig -> [Token] -> [Token]
extrasParser cfg toks = nextTok (extrasParser cfg) $ toks <| 
    (guard (parseINum cfg) *> inumParser toks 
    <|> guard (parseFNum cfg) *> fnumParser cfg toks
    <|> guard (eliminateHyphens cfg) *> nlHyphensParser cfg toks)

filterWords :: TokenizerConfig -> [Token] -> [Token]
filterWords cfg toks = nextTok (filterWords cfg) toks <| popIfData word (`HS.member` removeWords cfg) toks <&> snd

applyFilters :: TokenizerConfig -> [Token] -> [Token]
applyFilters cfg = 
    filterWords cfg
    . (removePunct cfg -?> filter (not . isT punct . val))
    . (removeNumbers cfg -?> filter (not . isT num . val))
    . (removeSpaces cfg -?> filter (not . isT space . val))

tokenize :: TokenizerConfig -> T.Text -> [Token]
tokenize cfg = 
    foldr (.) (applyFilters cfg) (specials cfg) 
    . extrasParser cfg
    . parseWords 
    . tokenizePreWords cfg 0

fromTokens :: RawFromTokenVal a => [Token] -> [a]
fromTokens = map rawVal

-- Reassembles close to original text
foldFromTokens :: RawFromTokenVal a => Monoid a => [Token] -> a
foldFromTokens = foldr ((<>) . rawVal) mempty

-- TODO: use text builder
-- TODO: avoid string conversions
prettyTextTokens :: [Token] -> T.Text
prettyTextTokens = foldl (\acc tok -> acc <> T.cons '\n' (T.pack (show tok))) ""
