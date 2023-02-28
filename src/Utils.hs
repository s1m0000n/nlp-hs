{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Utils where

import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VAlgo
import qualified Data.Text as T

argSort :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
argSort xs = VU.map fst $ VU.create $ do
    xsi <- VU.thaw $ VU.indexed xs
    VAlgo.sortBy (comparing snd) xsi
    return xsi

liftFst :: (a -> b) -> (a, c) -> (b, c)
liftFst f (a,c) = (f a, c)

liftSnd :: (a -> b) -> (c, a) -> (c, b)
liftSnd f (c,a) = (c, f a)

-- Coupled bituple & functor lifting
-- ^| - lift fst and functor
-- |^ - lift snd and functor
-- biLiftM - lift fst, snd and functor

(^|) :: Functor f => (t1 -> f a) -> (t1, t2) -> f (a, t2)
(^|) f (x, y) = (, y) <$> f x
(|^) :: Functor f => (t1 -> f a) -> (t2, t1) -> f (t2, a)
(|^) f (e0, e1) = (e0,) <$> f e1
biLiftM :: Monad m => (t1 -> m a) -> (t2 -> m b) -> (t1, t2) -> m (a, b)
biLiftM f0 f1 t = f0 ^| t >>= (f1 |^)

-- Kind of chaining them
-- NOTE: other variants to be added on demand
(|^.|^) :: Functor f => (t1 -> f a) -> (t3, (t2, t1)) -> f (t3, (t2, a))
(|^.|^) = (|^) . (|^)
(|^.|^.|^) :: Functor f => (t1 -> f a) -> (t4, (t3, (t2, t1))) -> f (t4, (t3, (t2, a)))
(|^.|^.|^) = (|^) . (|^.|^)
(|^.|^.|^.|^) :: Functor f => (t1 -> f a) -> (t5, (t4, (t3, (t2, t1)))) -> f (t5, (t4, (t3, (t2, a))))
(|^.|^.|^.|^) = (|^) . (|^.|^.|^)
(|^.|^.|^.|^.|^) :: Functor f => (t1 -> f a) -> (t6, (t5, (t4, (t3, (t2, t1))))) -> f (t6, (t5, (t4, (t3, (t2, a)))))
(|^.|^.|^.|^.|^) = (|^) . (|^.|^.|^.|^)

(|^.^|) :: Functor f => (t2 -> f r) -> (t1, (t2, t3)) -> f (t1, (r, t3))
(|^.^|) = (|^) . (^|)

-- "Contextual" lifting chained infix
(>>|^) :: Monad m => m (t1, t2) -> (t2 -> m r) -> m (t1, r)
(>>|^) i f = i >>= (f |^)
(>>|^.|^) :: Monad m => m (t1, (t2, t3)) -> (t3 -> m r) -> m (t1, (t2, r))
(>>|^.|^) i f = i >>= (f |^.|^)
(>>|^.^|) :: Monad m => m (t1, (t2, t3)) -> (t2 -> m r) -> m (t1, (r, t3))
(>>|^.^|) i f = i >>= (f |^.^|)
(>>|^.|^.|^) :: Monad m => m (t1, (t2, (t3, t4))) -> (t4 -> m r) -> m (t1, (t2, (t3, r)))
(>>|^.|^.|^) i f = i >>= (f |^.|^.|^)
(>>|^.|^.|^.|^) :: Monad m => m (t5, (t4, (t3, (t2, t1)))) -> (t1 -> m a) -> m (t5, (t4, (t3, (t2, a))))
(>>|^.|^.|^.|^) i f = i >>= (f |^.|^.|^.|^)
(>>|^.|^.|^.|^.|^) :: Monad m => m (t6, (t5, (t4, (t3, (t2, t1))))) -> (t1 -> m a) -> m (t6, (t5, (t4, (t3, (t2, a)))))
(>>|^.|^.|^.|^.|^) i f = i >>= (f |^.|^.|^.|^.|^)

to2h :: (a, b, c) -> ((a, b), c)
to2h (e0, e1, e2) = ((e0, e1), e2)

flattenSnd :: (a, (b, c)) -> (a, b, c)
flattenSnd (a, (b, c)) = (a, b, c)

isJustTrue :: Maybe Bool -> Bool
isJustTrue = \case
    Just True -> True
    _ -> False

(-?>) :: Bool -> (a -> a) -> (a -> a)
(-?>) True f = f
(-?>) False _ = id

-- TODO: extend definition ([t] to Foldable t?)
foldlUntil :: (a -> t -> Maybe a) -> a -> [t] -> (a, [t])
foldlUntil f acc (x:xs) = case f acc x of
    Just res -> foldlUntil f res xs
    Nothing -> (acc, x:xs)
foldlUntil _ acc [] = (acc, [])

-- TODO: extend definition ([t] to Foldable t?)
foldl1Until :: (t -> t -> Maybe t) -> [t] -> Maybe (t, [t])
foldl1Until f (x:xs) = Just $ foldlUntil f x xs
foldl1Until _ [] = Nothing

(===) :: T.Text -> T.Text -> Bool
(===) x y = T.toCaseFold x == T.toCaseFold y
