-- ----------------------------------------------------------------------------
-- Language Extensions
-- ----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

-- ----------------------------------------------------------------------------
-- Module declaration
-- ----------------------------------------------------------------------------

module Functor.Product
  (
    FProduct(..)
  ) where

-- ----------------------------------------------------------------------------
-- Internal imports
-- ----------------------------------------------------------------------------

import Functor.Applicative
import Functor.Covariant

-- ----------------------------------------------------------------------------
-- Covariant functor product data type
-- ----------------------------------------------------------------------------

data FProduct f g a = FProduct (f a) (g a)

-- ----------------------------------------------------------------------------
-- Functor product instance for Covariant
-- ----------------------------------------------------------------------------

instance (Covariant f, Covariant g) => Covariant (FProduct f g) where
  fmap f (FProduct l r) = FProduct (fmap f l) (fmap f r)

-- ----------------------------------------------------------------------------
-- Functor product instance for Applicative
-- ----------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (FProduct f g) where
  pure x = FProduct (pure x) (pure x)
  FProduct lf rf <*> FProduct lx rx = FProduct (lf <*> lx) (rf <*> rx)
