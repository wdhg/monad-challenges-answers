{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set1 where

import MCPrelude

type Gen a
  = Seed -> (a, Seed)

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

randLetter :: Gen Char
randLetter seed
  = (toLetter r, seed')
    where
      (r, seed') = rand seed

randString3 :: String
randString3
  = [c1, c2, c3]
    where
      s0       = mkSeed 1
      (c1, s1) = randLetter s0
      (c2, s2) = randLetter s1
      (c3, _)  = randLetter s2

randEven :: Gen Integer
randEven seed
  = (r * 2, seed')
    where
      (r, seed') = rand seed

randOdd :: Gen Integer
randOdd seed
  = (r * 2 + 1, seed')
    where
      (r, seed') = rand seed

randTen :: Gen Integer
randTen seed
  = (r * 10, seed')
    where
      (r, seed') = rand seed
