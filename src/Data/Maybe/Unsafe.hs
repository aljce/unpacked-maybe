{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module Data.Maybe.Unsafe (UnsafeMaybe
                         ,just
                         ,nothing
                         ,fromMaybe
                         ,maybe
                         ,toMaybe) where

import Data.Maybe (Maybe(..))

import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Prim (makeStableName#,StableName#,Any,eqStableName#,orI#)
import GHC.Types (IO(..))

import Data.Functor (Functor(..))
import Control.Applicative (Applicative(..),Alternative(..),liftA2)
import Control.Monad (Monad(..),MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.Functor.Classes (Eq1(..),Ord1(..),Show1(..),Read1(..))
import Text.Read
import Text.ParserCombinators.ReadPrec

import Prelude hiding (maybe)

data StableName a = StableName { getStableName :: StableName# a }

-- | Thunk requires the wildcard argument to trick runtime
thunk :: Int -> Int
thunk _ = error "Data.Maybe.Unsafe.nothingSurrogate: evaluated"
{-# NOINLINE thunk #-}

thunkStableName :: StableName (Int -> Int)
thunkStableName = unsafeDupablePerformIO $ IO $ \s1 -> case makeStableName# thunk s1 of
  (# s2 , name #) -> (# s2 , StableName name #)
{-# NOINLINE thunkStableName #-}

-- | nothingSurrogate stands in for the value Nothing; we distinguish it by pointer
nothingSurrogate :: Any
nothingSurrogate = unsafeCoerce thunk
{-# NOINLINE nothingSurrogate #-}

nothingStableName :: StableName Any
nothingStableName = unsafeDupablePerformIO $ IO $ \s1 -> case makeStableName# nothingSurrogate s1 of
  (# s2 , name #) -> (# s2 , StableName name #)
{-# NOINLINE nothingStableName #-}

newtype UnsafeMaybe a = UnsafeMaybe Any

maybe :: b -> (a -> b) -> UnsafeMaybe a -> b
maybe def transform (UnsafeMaybe !a) = unsafeDupablePerformIO $ IO $ \s1 -> case makeStableName# a s1 of
  (# s2, named #) -> case eqStableName# (getStableName nothingStableName) named `orI#` eqStableName# (getStableName thunkStableName) named of
    0# -> (# s2 , transform (unsafeCoerce a) #)
    _  -> (# s2 , def #)

instance Functor UnsafeMaybe where
  fmap f = maybe nothing (just . f)
  {-# INLINE fmap #-}

instance Applicative UnsafeMaybe where
  pure = just
  {-# INLINE pure #-}
  mf <*> mx = maybe nothing (\f -> maybe nothing (just . f) mx) mf
  {-# INLINE (<*>) #-}

instance Monad UnsafeMaybe where
  return = just
  {-# INLINE return #-}
  mx >>= f = maybe nothing f mx
  {-# INLINE (>>=) #-}

instance MonadFix UnsafeMaybe where
  mfix f = error "Data.Maybe.MonadFix: Is this defineable? I think it might be too strict"

instance MonadZip UnsafeMaybe where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

instance Alternative UnsafeMaybe where
  empty = nothing
  {-# INLINE empty #-}
  mx <|> my = maybe my just mx
  {-# INLINE (<|>) #-}

instance MonadPlus UnsafeMaybe where
  mzero = nothing
  {-# INLINE mzero #-}
  mplus mx my = maybe my just mx
  {-# INLINE mplus #-}

-- TODO: Impliment all the functions
instance Foldable UnsafeMaybe where
  foldMap f ma = maybe mempty f ma
  {-# INLINE foldMap #-}

instance Traversable UnsafeMaybe where
  sequenceA ma = maybe (pure nothing) (fmap just) ma
  {-# INLINE sequenceA #-}
  traverse f ma = maybe (pure nothing) (fmap just . f) ma
  {-# INLINE traverse #-}

instance Semigroup a => Semigroup (UnsafeMaybe a) where
  ma <> mb = maybe mb (\a -> maybe ma (\b -> just (a <> b)) mb) ma
  {-# INLINE (<>) #-}

instance Semigroup a => Monoid (UnsafeMaybe a) where
  mempty = nothing
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance Eq a => Eq (UnsafeMaybe a) where
  ma == mb = maybe (maybe True (const False) mb)
                   (\a -> maybe False (\b -> a == b) mb) ma
  {-# INLINE (==) #-}

instance Ord a => Ord (UnsafeMaybe a) where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma
  {-# INLINE compare #-}

appPrec :: Int
appPrec = 10
-- TODO: Invesitage why this derps out
instance Show a => Show (UnsafeMaybe a) where
  showsPrec p ma = maybe (showString "Nothing")
                         (\a -> ("Just "++) . showParen (p > 10) (showsPrec (appPrec + 1) a)) ma

instance Read a => Read (UnsafeMaybe a) where
  readPrec = parens $ nothingP +++ justP
    where nothingP = prec appPrec $ do
            Ident "Nothing" <- lexP
            pure nothing
          justP = prec appPrec $ do
            Ident "Just" <- lexP
            a <- step readPrec
            return (just a)

instance Eq1 UnsafeMaybe where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance Ord1 UnsafeMaybe where
  compare1 = compare
  {-# INLINE compare1 #-}

instance Show1 UnsafeMaybe where
  showsPrec1 = showsPrec

instance Read1 UnsafeMaybe where
  readsPrec1 = readPrec_to_S readPrec

just :: a -> UnsafeMaybe a
just a = UnsafeMaybe (unsafeCoerce a)
{-# INLINE just #-}

nothing :: UnsafeMaybe a
nothing = UnsafeMaybe nothingSurrogate
{-# INLINE nothing #-}

fromMaybe :: a -> UnsafeMaybe a -> a
fromMaybe def ma = maybe def id ma
{-# INLINE fromMaybe #-}

toMaybe :: UnsafeMaybe a -> Maybe a
toMaybe = maybe Nothing Just
{-# INLINE toMaybe #-}
