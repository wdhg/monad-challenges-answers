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

-- ex 2 - 4

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing
  = Nothing
chain func (Just value)
  = func value

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing _
  = Nothing
link (Just value) func
  = func value

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekData key
  = (lookupMay key greekData) `link` (\xs ->
      headMay xs `link` (\h ->
        tailMay xs `link` maximumMay `link` (\m ->
          divMay (fromIntegral m) (fromIntegral h))))

-- ex 2 - 5

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries name1 name2
  = case lookupMay name1 salaries of
      Nothing -> Nothing
      Just x  ->
        case lookupMay name2 salaries of
          Nothing -> Nothing
          Just y  -> mkMaybe $ x + y

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink func x y
  = x `link`
    (\x' -> mkMaybe $ func x') `link`
    (\func' -> y `link`
      (\y' -> mkMaybe $ func' y'))

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salaries name1 name2
  = yLink (+) (lookupMay name1 salaries) (lookupMay name2 salaries)

mkMaybe :: a -> Maybe a
mkMaybe
  = Just

-- ex 2 - 6

tailProd :: Num a => [a] -> Maybe a
tailProd []
  = Nothing
tailProd [x]
  = Just 1
tailProd xs
  = tailMay xs `link` (mkMaybe . product)

tailSum :: Num a => [a] -> Maybe a
tailSum []
  = Nothing
tailSum [x]
  = Just 0
tailSum xs
  = tailMay xs `link` (mkMaybe . sum)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe _ Nothing
  = Nothing
transMaybe func (Just x)
  = Just $ func x

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax []
  = Nothing
tailMax [x]
  = Just $ Just x
tailMax xs
  = transMaybe maximumMay $ tailMay xs

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin []
  = Nothing
tailMin [x]
  = Just $ Just x
tailMin xs
  = transMaybe minimumMay $ tailMay xs

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing
  = Nothing
combine (Just x)
  = x
