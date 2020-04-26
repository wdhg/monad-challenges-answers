{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set3 where

import MCPrelude

data Card
  = Card Int String

instance Show Card where
  show (Card value suit)
    = (show value) ++ suit

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _
  = []
allPairs (x:xs) ys
  =  zip (repeat x) ys ++ remaining
    where
      remaining
        = allPairs xs ys

allCards :: [Int] -> [String] -> [Card]
allCards [] _
  = []
allCards (x:xs) ys
  = map (Card x) ys ++ remaining
    where
      remaining
        = allCards xs ys

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _
  = []
allCombs func (x:xs) ys
  = map (func x) ys ++ allCombs func xs ys
