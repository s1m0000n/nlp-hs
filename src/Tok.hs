{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Tok where

import Data.Maybe (isJust, fromJust)
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
import TextShow.TH
import TextShow (TextShow(showb, showbList, showt), fromString)

data Pos = Pos 
    { start :: Int
    , end :: Int
    }
$(deriveTextShow ''Pos)

instance TextShow UTCTime where
    showb = fromString . show

data Value 
    = Word T.Text 
    | Punct T.Text 
    | Space T.Text 
    | INum Integer 
    | FNum Double 
    | Money Double T.Text
    | DateTime UTCTime -- not implemented yet
    | Special String Dynamic -- key, value; for specific custom cases 
    -- | Filepath String
    deriving Show
$(deriveTextShow ''Value)

data Prop 
    = SentStart | SentEnd -- TODO: implement detection
    | Joined
    | FromSpecial
    | NewLine
    | Tag String
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Hashable)
$(deriveTextShow ''Prop)

instance TextShow a => TextShow (HS.HashSet a) where
    showb = showbList . HS.toList

data Token = Token 
    { val :: Value
    , pos :: Pos
    , props :: HS.HashSet Prop
    }
$(deriveTextShow ''Token)

class FromToken a where
    fromToken :: Token -> a

instance FromToken T.Text where
    fromToken token = fromJust $
        showt <$> word (val token)
        <|> showt <$> punct (val token)
        <|> showt <$> space (val token)
        <|> showt <$> inum (val token)
        <|> showt <$> fnum (val token)
        <|> (\(a, b) -> a <> " " <> b) . liftFst showt <$> money (val token) -- PERF: use text builder instead
        <|> showt <$> dateTime (val token)
        <|> showt <$> special (val token)

class MaybeValueWithType a where
    word :: a -> Maybe T.Text
    punct :: a -> Maybe T.Text
    space :: a -> Maybe T.Text
    inum :: a -> Maybe Integer
    fnum :: a -> Maybe Double
    money :: a -> Maybe (Double, T.Text)
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
    money (Money d c) = Just (d, c)
    money _ = Nothing
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
    , parseSuffixMoney :: Bool                                          -- PERF: insignificant added time
    , parsePrefixMoney :: Bool                                          -- PERF: ~1/2 added time
    , eliminateHyphens :: Bool                                          -- PERF: might even improve performance
    , parseDateTime :: Bool                                             -- WARN: not implemented yet

    -- filters
    , removeSpaces :: Bool
    , removeNumbers :: Bool
    , removePunct :: Bool
    , removeWords :: Bool
    , stopWords :: HS.HashSet T.Text

    -- misc
    , fNumDels :: HS.HashSet T.Text
    , hyphens :: HS.HashSet T.Text
    , independentSpecialParsersLevels :: [[[Token] -> Maybe [Token]]]
    , newLines :: HS.HashSet T.Text
    , prefixCurrencyParser :: [[Token] -> Maybe [Token]]
    , suffixCurrencyParser :: [[Token] -> Maybe [Token]]
    }

defaultTokenizerConfig :: TokenizerConfig
defaultTokenizerConfig= TokenizerConfig 
    { preWordMatchers=fromList $ zip puncts (map ((Punct,) . T.length) puncts) ++ zip spaces (map ((Space,) . T.length) spaces)
    , parseINum=True
    , parseFNum=True
    , fNumDels=HS.singleton "."
    , removeSpaces=True
    , removeNumbers=False
    , stopWords=HS.empty
    , removePunct=False
    , eliminateHyphens=True
    , hyphens=HS.singleton "-"
    , parseDateTime=False
    , independentSpecialParsersLevels = []
    , newLines=HS.fromList ["\n", "\r\n"]
    , sameSpacesResolutionStrategy=Skip
    , parseSuffixMoney=True
    , parsePrefixMoney=False
    , prefixCurrencyParser = currencyParser
    , suffixCurrencyParser = currencyParser
    , lowerCase=True
    , removeWords=False
    } 
  where 
    puncts = ["...", ",", "!", "?", ".", ";", "(", ")", "[", "]", "{", "}", "--", "-", "\\", "`", "<", ">"]
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
    currencyParser = usDollarParser : plainCurrenciesParsers 

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
        None -> error "Should not get here, as None is expected to be filtered out on parser combinator level" ) 
    <&> (: tail_)

suffixMoneyParser :: TokenizerConfig -> [Token] -> Maybe [Token]
suffixMoneyParser cfg toks = 
    popT num toks 
        >>|^ skipManyT space
        >>|^ foldParsers (suffixCurrencyParser cfg)
        >>|^ uncons
        >>= \((vTok, vData), (currencyTok, tail_)) -> word (val currencyTok)
        <&> \currencyData -> Token 
            { val=Money vData currencyData
            , pos=pos vTok <> pos currencyTok
            , props=props vTok `HS.union` props currencyTok
            }
        : tail_

prefixMoneyParser :: TokenizerConfig -> [Token] -> Maybe [Token]
prefixMoneyParser cfg toks =
    (foldParsers (prefixCurrencyParser cfg) toks >>= uncons)
    >>|^ skipManyT space
    >>|^ popT num 
    >>= \(currencyTok, ((vTok, vData), tail_)) -> word (val currencyTok)
    <&> \currencyData -> Token 
        { val=Money vData currencyData
        , pos=pos vTok <> pos currencyTok
        , props=props vTok `HS.union` props currencyTok
        }
    : tail_

-- Independent single-pass parser combinations for additional built-in features {

independentLevel0Parser :: TokenizerConfig -> [Token] -> [Token]
independentLevel0Parser cfg toks =
    nextTok (independentLevel0Parser cfg) $ toks <| (
        skipIfData word (`HS.member` stopWords cfg) toks
        <|> guard (sameSpacesResolutionStrategy cfg /= None) *> sameSpacesParser cfg toks
        <|> guard (removeSpaces cfg) *> skipT space toks
        <|> guard (eliminateHyphens cfg) *> nlHyphensParser cfg toks
    )

independentLevel1Parser :: TokenizerConfig -> [Token] -> [Token]
independentLevel1Parser cfg toks =
    nextTok (independentLevel1Parser cfg) $ toks <| (
        guard (removePunct cfg) *> skipT punct toks
        <|> guard (removeNumbers cfg) *> skipT num toks 
        <|> guard (removeWords cfg) *> skipT word toks
        <|> guard (parseFNum cfg) *> fnumParser cfg toks
        <|> guard (parseINum cfg) *> inumParser toks 
    )

independentLevel2Parser :: TokenizerConfig -> [Token] -> [Token]
independentLevel2Parser cfg toks =
    nextTok (independentLevel2Parser cfg) $ toks <| (
        guard (parsePrefixMoney cfg) *> prefixMoneyParser cfg toks
        <|> guard (parseSuffixMoney cfg) *> suffixMoneyParser cfg toks
    )

-- }

-- dynamic parser combinators {

foldParsers :: [[Token] -> Maybe [Token]] -> [Token] -> Maybe [Token]
foldParsers = foldr1 (\acc next -> (<|>) <$> next <*> acc)

finalParserFromIndependentParsers :: ([Token] -> Maybe [Token]) -> [Token] -> [Token]
finalParserFromIndependentParsers independentParser toks = 
    nextTok (finalParserFromIndependentParsers independentParser) $ toks <| independentParser toks

-- }

tokenize :: TokenizerConfig -> T.Text -> [Token]
tokenize cfg = 
    (not ( null $ independentSpecialParsersLevels cfg) -?> foldr1 (.) (map (finalParserFromIndependentParsers . foldParsers) $ independentSpecialParsersLevels cfg))
    . independentLevel2Parser cfg
    . independentLevel1Parser cfg
    . independentLevel0Parser cfg
    . wordParser
    . spacePunctLowLevelParser cfg 0
    . (lowerCase cfg -?> T.toLower)
