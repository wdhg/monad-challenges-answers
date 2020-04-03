{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set1 where

import MCPrelude

-- naive approach
fiveRands :: [Integer]
fiveRands
  = [r1, r2, r3, r4, r5]
    where
      s0       = mkSeed 1
      (r1, s1) = rand s0
      (r2, s2) = rand s1
      (r3, s3) = rand s2
      (r4, s4) = rand s3
      (r5, _)  = rand s4

randLetter :: Seed -> (Char, Seed)
randLetter seed
  = (toLetter r, seed')
    where
      (r, seed') = rand seed
