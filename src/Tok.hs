{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Tok where

import Data.Maybe (isJust)
import Control.Applicative ((<|>))
import Data.PartialSemigroup
import Data.Functor ((<&>))
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.HashSet as HS
import Data.Trie.Text (Trie, fromList, match)
import Utils
import Data.Time (UTCTime)
import Control.Conditional (guard, (<|), (|>))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.List (uncons)
import Data.Dynamic (Dynamic)

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
    | Money Double T.Text
    | DateTime UTCTime -- not implemented yet
    | Special String Dynamic -- key, value; for specific custom cases 
    deriving Show

data Prop 
    = SentStart | SentEnd -- TODO: implement detection
    | Joined
    | FromSpecial
    | NewLine
    | Tag String
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Hashable)

data Token = Token 
    { val :: Value
    , pos :: Pos
    , props :: HS.HashSet Prop
    }

class MaybeValueWithType a where
    word :: a -> Maybe T.Text
    punct :: a -> Maybe T.Text
    space :: a -> Maybe T.Text
    inum :: a -> Maybe Integer
    fnum :: a -> Maybe Double
    dateTime :: a -> Maybe UTCTime
    special :: a -> Maybe (String, Dynamic)
    num :: a -> Maybe Double
    num x = (fromInteger <$> inum x) <|> fnum x

isT :: (a -> Maybe b) -> a -> Bool
isT t = isJust . t

predT :: (a -> Maybe b) -> (b -> Bool) -> a -> Bool
predT t p tok = isJustTrue $ p <$> t tok

popT :: (Value -> Maybe b) -> [Token] -> Maybe ((Token, b), [Token])
popT t (x:xs) = t (val x) <&> \pureValue -> ((x, pureValue), xs)
popT _ [] = Nothing
skipT :: (Value -> Maybe b) -> [Token] -> Maybe [Token]
skipT type_ toks = snd <$> popT type_ toks
popTokenT :: (Value -> Maybe b) -> [Token] -> Maybe (Token, [Token])
popTokenT t list = liftFst fst <$> popT t list
popDataT :: (Value -> Maybe b) -> [Token] -> Maybe (b, [Token])
popDataT t list = liftFst snd <$> popT t list

popIfData :: (Value -> Maybe b) -> (b -> Bool) -> [Token] -> Maybe ((Token, b), [Token])
popIfData t p (x:xs) = t (val x) >>= \pureValue -> if p pureValue then Just ((x, pureValue), xs) else Nothing
popIfData _ _ [] = Nothing
skipIfData :: (Value -> Maybe b) -> (b -> Bool) -> [Token] -> Maybe [Token]
skipIfData type_ predicate toks = snd <$> popIfData type_ predicate toks
popTokenIfData :: (Value -> Maybe b) -> (b -> Bool) -> [Token] -> Maybe (Token, [Token])
popTokenIfData t p list = liftFst fst <$> popIfData t p list
popDataIfData :: (Value -> Maybe b) -> (b -> Bool) -> [Token] -> Maybe (b, [Token])
popDataIfData t p list = liftFst snd <$> popIfData t p list

skipMany1T :: (Value -> Maybe b) -> [Token] -> Maybe [Token]
skipMany1T type_ toks = skipT type_ toks >>= \t -> skipMany1T type_ t <|> Just t
skipManyT :: (Value -> Maybe b) -> [Token] -> Maybe [Token]
skipManyT type_ toks = skipMany1T type_ toks <|> Just toks
skipMany1IfData :: (Value -> Maybe b) -> (b -> Bool) -> [Token] -> Maybe [Token]
skipMany1IfData type_ predicate toks = skipIfData type_ predicate toks >>= \t -> skipMany1IfData type_ predicate t <|> Just t
skipManyIfData :: (Value -> Maybe b) -> (b -> Bool) -> [Token] -> Maybe [Token]
skipManyIfData type_ predicate toks = skipMany1IfData type_ predicate toks <|> Just toks

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
        props = HS.insert Joined $ HS.union props1 props2

instance RawFromTokenVal T.Text where
    raw (Word t) = t
    raw (Punct t) = t
    raw (Space t) = t
    raw (INum n) = T.pack $ show n
    raw (FNum n) = T.pack $ show n
    raw (Money i c) = T.pack (show i) <> T.pack (show c)
    raw (DateTime dt) = T.pack $ show dt
    raw (Special k v) = T.pack $ raw $ Special k v

instance RawFromTokenVal String where
    raw (Word t) = T.unpack t
    raw (Punct t) = T.unpack t
    raw (Space t) = T.unpack t
    raw (INum n) = show n
    raw (FNum n) = show n
    raw (Money i c) = show i ++ show c
    raw (DateTime dt) = show dt
    raw (Special k v) = k ++ " -> " ++ show v


data SameSpacesResolutionStrategy = None 
                                  | Skip 
                                  | Join
                                  deriving Eq

-- NOTE: PERF comments are for default config with this option changed only
data TokenizerConfig = TokenizerConfig 
    -- base parsing options
    { preWordMatchers :: Trie (T.Text -> Value, Int)                    -- trie of converter, length
    , lowerCase :: Bool
    -- extras parsing
    , parseINum :: Bool                                                 -- PERF: ~1/5 added time
    , parseFNum :: Bool                                                 -- PERF: would depend on type of text, for most texts - insignificant
    , sameSpacesResolutionStrategy :: SameSpacesResolutionStrategy      -- PERF: time | Skip < time | Join << time | None
    , parseMoney :: Bool                                                -- PERF: insignificant added time 
    , eliminateHyphens :: Bool                                          -- PERF: insignificant
    , parseDateTime :: Bool                                             -- WARN: not implemented yet

    -- filters
    , removeSpaces :: Bool
    , removeNumbers :: Bool
    , removeWords :: HS.HashSet T.Text
    , removePunct :: Bool

    -- misc
    , fNumDels :: HS.HashSet T.Text
    , hyphens :: HS.HashSet T.Text
    , specials :: [[Token] -> [Token]]
    , postParser :: [Token] -> [Token]
    , newLines :: HS.HashSet T.Text
    , currenciesParsers :: [[Token] -> Maybe [Token]]
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
    , specials = []
    , postParser=id
    , newLines=HS.singleton "\n"
    , sameSpacesResolutionStrategy=Skip
    , parseMoney=True
    , currenciesParsers = usDollarParser : plainCurrenciesParsers 
    , lowerCase=True
    } 
  where 
    puncts = ["...", ",", "!", "?", ".", ";", "(", ")", "[", "]", "{", "}", "--", "-"]
    spaces = ["\t", "\n", " "]
    plainCurrenciesParsers = map (\c -> fmap (uncurry (:)) . popTokenIfData word (=== c)) ["$", "€", "₽", "¥", "₣", "£", "USD", "EUR", "EURO", "RUB"]
    usDollarParser toks = 
        popTokenIfData word (=== "u") toks
        >>|^ (\tail_ ->
            skipIfData punct (=== ".") tail_
            >>= skipManyT space
            >>= skipIfData word (=== "s")
            >>= skipIfData punct (=== ".")
            >>= skipManyT space
        )
        >>|^ ((<|>) <$> popTokenIfData word (=== "dollar") <*> popTokenIfData word (=== "$"))
        >>= \(w1, (w2, tail_)) -> w1 <>? w2 
        <&> \w -> (w {val=Word "$"}) : tail_

matchPreWords :: TokenizerConfig -> Int -> T.Text -> Maybe (Token, T.Text)
matchPreWords cfg posShift text = liftFst tok . to2h <$> match (preWordMatchers cfg) text 
  where 
    tok (tokText, (type_, len)) = Token 
        { val=type_ tokText
        , pos=Pos 
            { start=posShift
            , end=posShift + len
            }
        , props=HS.empty
        }

preWordsOrChar :: TokenizerConfig -> Int -> T.Text -> Maybe (Token, T.Text)
preWordsOrChar cfg posShift text = matchPreWords cfg posShift text 
    <|> liftFst (\x -> Token 
            { val=Word $ T.singleton x
            , pos=Pos 
                { start=posShift
                , end=posShift + 1
                }
            , props=HS.empty
            }
        ) <$> T.uncons text

spacePunctLowLevelParser :: TokenizerConfig -> Int -> T.Text -> [Token]
spacePunctLowLevelParser _ _ "" = []
spacePunctLowLevelParser cfg posShift text = case preWordsOrChar cfg posShift text of
    Nothing -> []
    Just (token, tailToks) -> token : spacePunctLowLevelParser cfg (end $ pos token) tailToks

nextTok :: ([Token] -> [Token]) -> [Token] -> [Token]
nextTok f (x:xs) = x : f xs
nextTok _ [] = []

wordParser :: [Token] -> [Token]
wordParser toks = nextTok wordParser toks 
    <| popTokenT word toks 
    >>|^ popTokenT word 
    >>= \(w1, (w2, t)) -> w1 <>? w2 
    <&> wordParser . (: t)

inumParser :: [Token] -> Maybe [Token]
inumParser toks =
    popT word toks 
    >>= \((w1, w1d), t) -> readMaybe (T.unpack w1d) 
    <&> \i -> (w1 {val=INum i}) : t

fnumParser :: TokenizerConfig -> [Token] -> Maybe [Token]
fnumParser cfg toks = 
    popT word toks 
    >>|^ popIfData punct (`HS.member` fNumDels cfg)
    >>|^.|^ popTokenT word
    >>= \((w1, w1Data), ((d, dData), (w2, t))) -> Token {val=Word $ w1Data <> dData, pos=pos w1 <> pos d, props=props w1 `HS.union` props d} <>? w2 
    >>= \w -> word (val w) >>= readMaybe . T.unpack
    <&> \f -> (w {val=FNum f}) : t


nlHyphensParser :: TokenizerConfig -> [Token] -> Maybe [Token]
nlHyphensParser cfg toks =
    popTokenT word toks
    >>|^ popIfData punct (`HS.member` hyphens cfg)
    >>|^.|^ popIfData space (`HS.member` newLines cfg)
    >>|^.|^.|^ popTokenT word
    >>= \(w1, (_, (_, (w2, t)))) -> w1 <>? w2
    <&> (: t)

sameSpacesParser :: TokenizerConfig -> [Token] -> Maybe [Token]
sameSpacesParser cfg toks =
    popT space toks
    >>|^ popT space
    >>= \((s1, s1Data), ((s2, s2Data), tail_)) -> (s1Data /= s2Data |> s1 <>? s2)
    >>= (\s -> case sameSpacesResolutionStrategy cfg of
        Join -> s
        Skip -> s <&> \s_ -> s_ {val=Space s1Data}
        None -> error "Should not get here, as None is expected to be filtered out on parser combinator level"
    ) <&> (: tail_)

foldParsers :: [[Token] -> Maybe [Token]] -> [Token] -> Maybe [Token]
foldParsers = foldr1 (\acc next -> (<|>) <$> next <*> acc)

-- Independent single-pass parser combination for built-ins
independentExtrasLevel0Parser :: TokenizerConfig -> [Token] -> [Token]
independentExtrasLevel0Parser cfg toks =
    nextTok (independentExtrasLevel0Parser cfg) $ toks <| (
        guard (parseFNum cfg) *> fnumParser cfg toks
        <|> guard (parseINum cfg) *> inumParser toks 
        <|> guard (sameSpacesResolutionStrategy cfg /= None) *> sameSpacesParser cfg toks
        <|> guard (eliminateHyphens cfg) *> nlHyphensParser cfg toks
    )

moneyParser :: TokenizerConfig -> [Token] -> Maybe [Token]
moneyParser cfg toks =
    popT num toks 
    >>|^ skipManyT space
    >>|^ foldParsers (currenciesParsers cfg)
    >>|^ uncons
    >>= \((vTok, vData), (currencyTok, tail_)) -> word (val currencyTok)
    <&> \currencyData -> Token 
        { val=Money vData currencyData
        , pos=pos vTok <> pos currencyTok
        , props=props vTok `HS.union` props currencyTok
        }
        : tail_

independentExtrasLevel1Parser :: TokenizerConfig -> [Token] -> [Token]
independentExtrasLevel1Parser cfg toks =
    nextTok (independentExtrasLevel1Parser cfg) $ toks <|
        guard (parseMoney cfg) *> moneyParser cfg toks

-- Dependent parser combination for built-ins
recursiveExtrasParser :: TokenizerConfig -> [Token] -> [Token]
recursiveExtrasParser _ = id -- TODO: implement when needed

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
    foldr (.) (applyFilters cfg) (specials cfg) -- any specials (user-defined)
    . recursiveExtrasParser cfg                 -- dependent extras (built-in)
    . independentExtrasLevel1Parser cfg         -- independent extras (built-in), dependent on level 0
    . independentExtrasLevel0Parser cfg         -- independent extras (built-in)
    . wordParser
    . spacePunctLowLevelParser cfg 0
    . (lowerCase cfg -?> T.toLower)

fromTokens :: RawFromTokenVal a => [Token] -> [a]
fromTokens = map rawVal

-- Reassembles close to original text
foldFromTokens :: RawFromTokenVal a => Monoid a => [Token] -> a
foldFromTokens = foldr ((<>) . rawVal) mempty

-- TODO: use text builder
-- TODO: avoid string conversions
prettyTextTokens :: [Token] -> T.Text
prettyTextTokens = foldl (\acc tok -> acc <> T.cons '\n' (T.pack (show tok))) ""
