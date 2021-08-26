-- ----------------------------------------------------------------------------
-- Language Extensions
-- ----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

-- ----------------------------------------------------------------------------
-- Module declaration
-- ----------------------------------------------------------------------------

module Control.Monad.Writer
  (
    Writer(..)
  , tell
  , listen
  , pass
  ) where

-- ----------------------------------------------------------------------------
-- Internal imports
-- ----------------------------------------------------------------------------

import Control.Monad
import Data.Monoid
import Data.Semigroup
import Functor.Applicative
import Functor.Covariant

-- ----------------------------------------------------------------------------
-- Writer data type definition
-- ----------------------------------------------------------------------------

newtype Writer w a = Writer { runWriter :: (w, a) }

-- ----------------------------------------------------------------------------
-- Writer instance for Covariant
-- ----------------------------------------------------------------------------

instance Monoid w => Covariant (Writer w) where
  fmap f (Writer (w, x)) = Writer (w, f x)

-- ----------------------------------------------------------------------------
-- Writer instance for Applicative
-- ----------------------------------------------------------------------------

instance Monoid w => Applicative (Writer w) where
  pure x = Writer (mempty, x)
  Writer (w, f) <*> Writer (w', x) = Writer (w <> w', f x)

-- ----------------------------------------------------------------------------
-- Writer instance for Monad
-- ----------------------------------------------------------------------------

instance Monoid w => Monad (Writer w) where
  join (Writer (w, Writer (w', x))) = Writer (w <> w', x)

-- ----------------------------------------------------------------------------
-- Writer monad functions
-- ----------------------------------------------------------------------------

tell :: Monoid w => w -> Writer w ()
tell w = Writer (w, ())

listen :: Monoid w => Writer w a -> Writer w (w, a)
listen m@(Writer (w, _)) = m >>= \x -> Writer (mempty, (w, x))

pass :: Monoid w => Writer w (w -> w, a) -> Writer w a
pass m@(Writer (w, _)) = m >>= \(f, x) -> Writer (f w, x)
