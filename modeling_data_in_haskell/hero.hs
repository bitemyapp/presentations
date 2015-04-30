module RPG where

data Hero =
  Hero {
    class :: String
  , race :: String
  , statusEffects :: [String]
  , inventory :: Map String Int
  } deriving Show
