{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

module Set4 where

import MCPrelude
import Set2

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

newtype Gen a
  = Gen
  { runGen :: Seed -> (a, Seed)
  }

evalGen :: Gen a -> Seed -> a
evalGen (Gen runGen)
  = fst . runGen

instance Monad Gen where
  bind gen func
    = Gen (\seed ->
      let (x, s') = runGen gen seed in
          runGen (func x) s')
  return x
    = Gen $ \s -> (x, s)
