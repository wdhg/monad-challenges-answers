{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set2 where

import MCPrelude

-- ex 2 - 1

data Maybe a
  = Just a
  | Nothing

instance Show a => Show (Maybe a) where
  show (Just value)
    = "Just " ++ show value
  show Nothing
    = "Nothing"

instance Eq a => Eq (Maybe a) where
  (Just x) == (Just y)
    = x == y
  Nothing == Nothing
    = True
  _ == _
    = False

-- ex 2 - 2

headMay :: [a] -> Maybe a
headMay (x : xs)
  = Just x
headMay _
  = Nothing

tailMay :: [a] -> Maybe [a]
tailMay (_ : xs)
  = Just xs
tailMay _
  = Nothing

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay key table
  = case filter ((== key) . fst) table of
      ((_, v) : _) -> Just v
      _            -> Nothing

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0
  = Nothing
divMay x y
  = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay []
  = Nothing
maximumMay (x : xs)
  = case maximumMay xs of
      Just y  -> Just $ max x y
      Nothing -> Just x

minimumMay :: Ord a => [a] -> Maybe a
minimumMay []
  = Nothing
minimumMay (x : xs)
  = case minimumMay xs of
      Just y  -> Just $ min x y
      Nothing -> Just x

-- ex 2 - 3

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData key
  = case lookupMay key greekData of
      Nothing -> Nothing
      Just xs ->
        case headMay xs of
          Nothing -> Nothing
          Just h  ->
            case tailMay xs of
              Nothing -> Nothing
              Just t  ->
                case maximumMay t of
                  Nothing -> Nothing
                  Just m  ->
                    case divMay (fromIntegral m) (fromIntegral h) of
                      Nothing -> Nothing
                      Just v  -> Just v
