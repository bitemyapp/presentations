{-# LANGUAGE StandaloneDeriving #-}

module Exist where

newtype S =
  S { t :: forall a . Show a => a }

deriving instance Show S
