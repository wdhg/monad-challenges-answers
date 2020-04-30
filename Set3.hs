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

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 func xs ys zs
  = concatMap (\f -> map f zs) $ concatMap (\f -> map f ys) $ map func xs

combStep :: [a -> b] -> [a] -> [b]
combStep _ []
  = []
combStep funcs (x:xs)
  = map (\f -> f x) funcs ++ combStep funcs xs

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' func xs ys
  = combStep (map func xs) ys

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' func xs ys zs
  = combStep (combStep (map func xs) ys) zs
