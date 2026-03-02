{-# LANGUAGE InstanceSigs #-}
module Probability where

import qualified Data.Map.Strict as M
import Data.List (maximumBy)
import Data.Function (on)

newtype Probability a = Probability { getProbabilities :: [(a,Double)] }
    deriving Show

instance Functor Probability where
    fmap :: (a -> b) -> Probability a -> Probability b
    fmap f (Probability ps) =
        Probability { getProbabilities = [(f a, p) | (a, p) <- ps] }

instance Applicative Probability where
    pure :: a -> Probability a
    pure a = Probability { getProbabilities = [(a, 1)] }

    (<*>) :: Probability (a -> b) -> Probability a -> Probability b
    (Probability fs) <*> (Probability ps) =
        Probability { getProbabilities = [(f a, p * q) | (a, p) <- ps, (f, q) <- fs] }

instance Monad Probability where
    (>>=) :: Probability a -> (a -> Probability b) -> Probability b
    (Probability ps) >>= f =
        Probability { getProbabilities = [(b, p * q) | (a, p) <- ps, (b, q) <- getProbabilities (f a)] }

normalize :: (Ord a) => Probability a -> Probability a
normalize (Probability ps) =
    Probability { getProbabilities = M.toAscList (M.fromListWith (+) ps) }

roll :: Probability Int
roll = Probability { getProbabilities = [(i, 1 / 6) | i <- [1 .. 6]] }

twoRolls :: Probability Int
twoRolls = do
    a <- roll
    b <- roll
    pure $ a + b

rollDice :: Int -> Probability Int
rollDice 0 = pure 0
rollDice n = normalize $ do
    prev <- rollDice (n - 1)
    r <- roll
    pure (prev + r)

mode :: Probability a -> (a, Double)
mode (Probability ps) = maximumBy (compare `on` snd) ps
