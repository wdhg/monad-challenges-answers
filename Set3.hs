{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _
  = []
allPairs (x:xs) ys
  =  zip (repeat x) ys ++ remaining
    where
      remaining
        = allPairs xs ys
