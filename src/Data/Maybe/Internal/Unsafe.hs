{-# LANGUAGE UnboxedTuples, MagicHash, BangPatterns #-}
module Data.Maybe.Internal.Unsafe (nothingSurrogate
                                  ,nothingSurrogateSN
                                  ,thunk
                                  ,thunkSN
                                  ,Maybe(..)
                                  ,nothing
                                  ,just
                                  ,maybe) where

import Prelude hiding (Maybe(..),maybe)

import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Prim (makeStableName#,StableName#,Any,eqStableName#,orI#)
import GHC.Types (IO(..))

import GHC.Prim (stableNameToInt#)
import GHC.Base (Int(..))
import Debug.Trace

traceSN :: String -> StableName a -> StableName a
traceSN name sn = trace str sn
  where str = ("StableName: " ++ name ++ " has the hash: " ++
               show (I# (stableNameToInt# (getStableName sn))))

-- I had to roll my own StableName so I could access the 'StableName#'
-- inside. I had to do this for two reasons:
-- 1: You cant have top level expressions of kind #
-- 2: The 'StableName' module from Base doesnt export Constructors
data StableName a = StableName { getStableName :: StableName# a }

-- | 'nothingSurrogate' simulates a null pointer, it is required to be a closure
-- to trick ghc's runtime
nothingSurrogate :: Int -> Int
nothingSurrogate _ = error "Data.Maybe.Unsafe.nothingSurrogate: evaluated"
{-# NOINLINE nothingSurrogate #-}

-- | This is a toplevel identifier for the pointer to nothingSurrogate
--
nothingSurrogateSN :: StableName (Int -> Int)
nothingSurrogateSN = unsafeDupablePerformIO $ IO $ \s1 -> case makeStableName# nothingSurrogate s1 of
  (# s2 , name #) -> (# s2 , StableName name #)
{-# INLINE nothingSurrogateSN #-}

-- | Thunk stands in for the value Nothing; we distinguish it by pointer
thunk :: Any
thunk = unsafeCoerce nothingSurrogate
{-# NOINLINE thunk #-}

thunkSN :: StableName Any
thunkSN = unsafeDupablePerformIO $ IO $ \s1 -> case makeStableName# thunk s1 of
  (# s2 , name #) -> (# s2 , StableName name #)
{-# INLINE thunkSN #-}

data Maybe a = Maybe {-# UNPACK #-} !Any

nothing :: Maybe a
nothing = Maybe thunk
{-# INLINE nothing #-}

just :: a -> Maybe a
just a = Maybe (unsafeCoerce a)
{-# INLINE just #-}

maybe :: b -> (a -> b) -> Maybe a -> b
maybe def transform (Maybe !a) = unsafeDupablePerformIO $ IO $ \s1 -> case makeStableName# a s1 of
  (# s2, named #) -> case eqStableName# (getStableName thunkSN) named `orI#`
                          eqStableName# (getStableName nothingSurrogateSN) named of
    0# -> (# s2 , transform (unsafeCoerce a) #)
    _  -> (# s2 , def #)
{-# INLINE maybe #-}
