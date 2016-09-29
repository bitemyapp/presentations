{-# LANGUAGE RankNTypes #-}

module Examples where

-- might remind you of an existential
b :: (forall a. Show a => a -> String) -> (String, String)
b f = (f 1, f 'v')
