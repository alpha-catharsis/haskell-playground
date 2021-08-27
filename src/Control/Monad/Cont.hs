-- ----------------------------------------------------------------------------
-- Language Extensions
-- ----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

-- ----------------------------------------------------------------------------
-- Module declaration
-- ----------------------------------------------------------------------------

module Control.Monad.Cont
  (
    Cont(..)
  , callCC
  ) where

-- ----------------------------------------------------------------------------
-- External imports
-- ----------------------------------------------------------------------------

import Prelude (($), (.), const)

-- ----------------------------------------------------------------------------
-- Internal imports
-- ----------------------------------------------------------------------------

import Control.Monad
import Functor.Applicative
import Functor.Covariant

-- ----------------------------------------------------------------------------
-- Cont data type definition
-- ----------------------------------------------------------------------------

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- ----------------------------------------------------------------------------
-- Cont instance for Covariant
-- ----------------------------------------------------------------------------

instance Covariant (Cont r) where
  fmap f (Cont g) = Cont $ \k -> g (k . f)

-- ----------------------------------------------------------------------------
-- Cont instance for Applicative
-- ----------------------------------------------------------------------------

instance Applicative (Cont r) where
  pure x = Cont ($ x)
  Cont f <*> Cont g = Cont $ \k -> f (\h -> g (k . h))

-- ----------------------------------------------------------------------------
-- Cont instance for Monad
-- ----------------------------------------------------------------------------

instance Monad (Cont s) where
  join (Cont f) = Cont $ \k -> f (\(Cont g) -> g k)

-- ----------------------------------------------------------------------------
-- Cont monad functions
-- ----------------------------------------------------------------------------

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \k -> runCont (f (Cont . const . k)) k
