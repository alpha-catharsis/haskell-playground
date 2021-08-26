-- ----------------------------------------------------------------------------
-- Language Extensions
-- ----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}

-- ----------------------------------------------------------------------------
-- Module declaration
-- ----------------------------------------------------------------------------

module Control.Monad.State
  (
    State(..)
  , get
  , gets
  , put
  , modify
  ) where

-- ----------------------------------------------------------------------------
-- External imports
-- ----------------------------------------------------------------------------

import Prelude (($), const)

-- ----------------------------------------------------------------------------
-- Internal imports
-- ----------------------------------------------------------------------------

import Control.Monad
import Functor.Applicative
import Functor.Covariant

-- ----------------------------------------------------------------------------
-- State data type definition
-- ----------------------------------------------------------------------------

newtype State s a = State { runState :: s -> (s, a) }

-- ----------------------------------------------------------------------------
-- State instance for Covariant
-- ----------------------------------------------------------------------------

instance Covariant (State s) where
  fmap f (State g) = State $ \s -> let (s', x) = g s in (s', f x)

-- ----------------------------------------------------------------------------
-- State instance for Applicative
-- ----------------------------------------------------------------------------

instance Applicative (State s) where
  pure x = State $ \s -> (s, x)
  State f <*> State g = State $ \s -> let (s', h) = f s
                                          (s'', x) = g s'
                                      in (s'', h x)

-- ----------------------------------------------------------------------------
-- State instance for Monad
-- ----------------------------------------------------------------------------

instance Monad (State s) where
  join (State f) = State $ \s -> let (s', State g) = f s in g s'

-- ----------------------------------------------------------------------------
-- State monad functions
-- ----------------------------------------------------------------------------

get :: State s s
get = State $ \s -> (s, s)

gets :: (s -> a) -> State s a
gets f = State (\s -> (s, f s))

put :: s -> State s ()
put s = State $ const (s, ())

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())