{-# LANGUAGE RankNTypes #-}

module T where

function :: (forall a . a -> a) -> (Int, String) -> (Int, String)
function f (i, s) = (f i, f s)

funct :: (Int, String) -> (Int, String)
funct (i, s) = (id i, id s)

addBoth :: (forall a . Num a => a -> a) -> (Int, Double) -> (Int, Double)
addBoth f (i, d) = (f i, f d)

-- explicit forall
-- rank 2 / rank n
-- type forall s t a b . Lens s t a b =
