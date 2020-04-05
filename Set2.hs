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
