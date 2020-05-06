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
