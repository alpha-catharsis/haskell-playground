-- ----------------------------------------------------------------------------
-- Language Extensions
-- ----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

-- ----------------------------------------------------------------------------
-- Module declaration
-- ----------------------------------------------------------------------------

module Control.Monad.Transformers.MaybeT
  (
    MaybeT(..)
  ) where

-- ----------------------------------------------------------------------------
-- External imports
-- ----------------------------------------------------------------------------

import Prelude (($))

-- ----------------------------------------------------------------------------
-- Internal imports
-- ----------------------------------------------------------------------------

import Control.Monad
import Data.Maybe
import Functor.Applicative
import Functor.Covariant

-- ----------------------------------------------------------------------------
-- MaybeT data type definition
-- ----------------------------------------------------------------------------

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- ----------------------------------------------------------------------------
-- MaybeT instance for Covariant
-- ----------------------------------------------------------------------------

instance Covariant m => Covariant (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ fmap (fmap f) m

-- ----------------------------------------------------------------------------
-- MaybeT instance for Applicative
-- ----------------------------------------------------------------------------

instance Monad m => Applicative (MaybeT m) where
  pure x = MaybeT $ pure (Just x)
  MaybeT mf <*> MaybeT mx = MaybeT $ do
    mbf <- mf
    case mbf of
      Nothing -> pure Nothing
      Just f -> do
        mbx <- mx
        case mbx of
          Nothing -> pure Nothing
          Just x -> pure (Just (f x))

-- ----------------------------------------------------------------------------
-- MaybeT instance for Monad
-- ----------------------------------------------------------------------------

instance Monad m => Monad (MaybeT m) where
  join (MaybeT mm) = MaybeT $ do
    mmb <- mm
    case mmb of
      Nothing -> pure Nothing
      Just (MaybeT m) -> do
        mb <- m
        case mb of
          Nothing -> pure Nothing
          Just x -> pure (Just x)
