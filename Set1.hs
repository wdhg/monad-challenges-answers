{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set1 where

import MCPrelude

type Gen a
  = Seed -> (a, Seed)

-- ex 1 - 1

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

-- ex 1 - 2

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

-- ex 1 - 3

randEven :: Gen Integer
randEven seed
  = (r * 2, seed')
    where
      (r, seed') = rand seed

randOdd :: Gen Integer
randOdd seed
  = (r + 1, seed')
    where
      (r, seed') = randEven seed

randTen :: Gen Integer
randTen seed
  = (r * 10, seed')
    where
      (r, seed') = rand seed

generalA :: (a -> b) -> Gen a -> Gen b
generalA func gen seed
  = (func r, seed')
    where
      (r, seed') = gen seed

randEven' :: Gen Integer
randEven'
  = generalA (*2) rand

randOdd' :: Gen Integer
randOdd'
  = generalA (+1) randEven'

randTen' :: Gen Integer
randTen'
  = generalA (*10) rand

-- ex 1 - 4

randPair :: Gen (Char, Integer)
randPair s0
  = ((c, r), s2)
    where
      (c, s1) = randLetter s0
      (r, s2) = rand s1

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair gen1 gen2 s0
  = ((r1, r2), s2)
    where
      (r1, s1) = gen1 s0
      (r2, s2) = gen2 s1

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB func gen1 gen2 s0
  = (func r1 r2, s2)
    where
      (r1, s1) = gen1 s0
      (r2, s2) = gen2 s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2
  = generalB (,)
