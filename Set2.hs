{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set2 where

import MCPrelude

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
