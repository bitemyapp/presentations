{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module Ranky where

-- Examples from the GHC user documentation.

-- Ordinary, rank-1 types with prenex form
f1 :: forall a b . a -> b -> a
f1 = undefined

g1 :: forall a b . (Ord a, Eq  b) => a -> b -> a
g1 = undefined

-- rank-2 type, needs rank2 or rankN pragma
f2 :: (forall a . a -> a) -> Int -> Int
f2 = undefined

g2 :: (forall a . Eq a => [a] -> a -> Bool) -> Int -> Int
g2 = undefined

-- rank-3 type, needs rankN pragma
f3 :: ((forall a . a -> a) -> Int) -> Bool -> Bool
f3 = undefined
