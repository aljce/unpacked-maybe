{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}

module Data.Maybe.Internal.Unsafe
  ( Maybe(..)
  , nothing
  , just
  , maybe
  , toMaybe
  ) where

import GHC.Base hiding (Maybe(..))
import GHC.Prim
import GHC.Types
import Unsafe.Coerce (unsafeCoerce)

import Prelude () -- only import instances

newtype Maybe a = Maybe Any

nothing :: Maybe a
nothing = Maybe thunk
{-# inline nothing #-}

just :: a -> Maybe a
just a = Maybe (unsafeCoerce a)
{-# inline just #-}

nothingSurrogate :: Any
nothingSurrogate = error "Data.Maybe.Unpacked.nothingSurrogate evaluated"
{-# noinline nothingSurrogate #-}

-- | Thunk stands in for the value Nothing; It is distinguished by its pointer
thunk :: Any
thunk = unsafeCoerce nothingSurrogate
{-# noinline thunk #-}

maybe :: b -> (a -> b) -> Maybe a -> b
maybe def f !(Maybe a) = case reallyUnsafePtrEquality# a nothingSurrogate of
  0# -> f (unsafeCoerce a)
  _  -> def
{-# inline maybe #-}

toMaybe :: a -> Maybe a
toMaybe x = case reallyUnsafePtrEquality# x (unsafeCoerce nothingSurrogate) of
  0# -> just x
  _  -> nothing
{-# inline toMaybe #-}
