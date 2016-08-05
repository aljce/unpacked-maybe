{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Data.Maybe.Unpacked (Maybe
                           ,just
                           ,pattern Just
                           ,nothing
                           ,pattern Nothing
                           ,maybe
                           ,isJust
                           ,isNothing
                           ,fromJust
                           ,fromMaybe
                           ,listToMaybe
                           ,maybeToList
                           ,catMaybes
                           ,mapMaybe
                           ,toMaybe) where

import Prelude hiding (Maybe(..),maybe)
import qualified Data.Maybe as Old

import Control.Applicative (Alternative(..),liftA2)
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix (MonadFix(..),fix)
import Control.Monad.Zip (MonadZip(..))
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Text.Read (parens,Lexeme(..),lexP,(+++),readPrec)
import Text.ParserCombinators.ReadPrec (prec,step)

import Data.Maybe.Internal.Unsafe (nothing,just,maybe,Maybe)

instance Functor Maybe where
  fmap f = maybe nothing (just . f)
  {-# INLINE fmap #-}

instance Applicative Maybe where
  pure = just
  {-# INLINE pure #-}
  mf <*> mx = maybe nothing (\f -> maybe nothing (just . f) mx) mf
  {-# INLINE (<*>) #-}

instance Monad Maybe where
  return = just
  {-# INLINE return #-}
  mx >>= f = maybe nothing f mx
  {-# INLINE (>>=) #-}

-- This instance is too strict mfix (\x -> nothing) diverges
-- instance MonadFix Maybe where
--   mfix f = let a = f (fromJust a) in a

instance MonadZip Maybe where
  mzipWith = liftA2
  {-# INLINE mzipWith #-}

instance Alternative Maybe where
  empty = nothing
  {-# INLINE empty #-}
  mx <|> my = maybe my just mx
  {-# INLINE (<|>) #-}

instance MonadPlus Maybe where
  mzero = nothing
  {-# INLINE mzero #-}
  mplus mx my = maybe my just mx
  {-# INLINE mplus #-}

-- TODO: Impliment all the functions
instance Foldable Maybe where
  foldMap f ma = maybe mempty f ma
  {-# INLINE foldMap #-}

instance Traversable Maybe where
  sequenceA ma = maybe (pure nothing) (fmap just) ma
  {-# INLINE sequenceA #-}
  traverse f ma = maybe (pure nothing) (fmap just . f) ma
  {-# INLINE traverse #-}

instance Semigroup a => Semigroup (Maybe a) where
  ma <> mb = maybe mb (\a -> maybe ma (\b -> just (a <> b)) mb) ma
  {-# INLINE (<>) #-}

instance Semigroup a => Monoid (Maybe a) where
  mempty = nothing
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance Eq a => Eq (Maybe a) where
  ma == mb = maybe (maybe True (const False) mb)
                   (\a -> maybe False (\b -> a == b) mb) ma
  {-# INLINE (==) #-}

instance Ord a => Ord (Maybe a) where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma
  {-# INLINE compare #-}

appPrec :: Int
appPrec = 10

instance Show a => Show (Maybe a) where
  showsPrec p ma = maybe (showString "Nothing")
                         (\a -> showParen (p > appPrec) (showString "Just " . (showsPrec (appPrec + 1) a))) ma

instance Read a => Read (Maybe a) where
  readPrec = parens $ nothingP +++ justP
    where nothingP = prec appPrec $ do
            Ident "Nothing" <- lexP
            pure nothing
          justP = prec appPrec $ do
            Ident "Just" <- lexP
            a <- step readPrec
            return (just a)

-- When stackage gets transformers 5 or ghc 8
-- instance Eq1 Maybe where
--   {-# INLINE liftEq #-}

-- instance Ord1 Maybe where
--   {-# INLINE liftCompare #-}

-- instance Show1 Maybe where

-- instance Read1 Maybe where

isJust :: Maybe a -> Bool
isJust = maybe False (const True)
{-# INLINE isJust #-}

isNothing :: Maybe a -> Bool
isNothing = maybe True (const False)
{-# INLINE isNothing #-}

-- TODO: Investigate why prelude uses errorWithoutStackTrace
fromJust :: Maybe a -> a
fromJust = fromMaybe (error "Data.Maybe.Unpacked.fromJust: Nothing")
{-# INLINE fromJust #-}

fromMaybe :: a -> Maybe a -> a
fromMaybe def ma = maybe def id ma
{-# INLINE fromMaybe #-}

listToMaybe :: [a] -> Maybe a
listToMaybe []    = nothing
listToMaybe (x:_) = just x
{-# INLINE listToMaybe #-}

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] (:[])
{-# INLINE maybeToList #-}

catMaybes :: [Maybe a] -> [a]
catMaybes = mapMaybe id
{-# INLINE catMaybes #-}

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (a:as) = let bs = mapMaybe f as in maybe bs (:bs) (f a)
{-# INLINE mapMaybe #-}

toMaybe :: Maybe a -> Old.Maybe a
toMaybe = maybe Old.Nothing Old.Just
{-# INLINE toMaybe #-}

pattern Just :: a -> Maybe a
pattern Just x <- (toMaybe -> Old.Just x)

pattern Nothing :: Maybe a
pattern Nothing <- (toMaybe -> Old.Nothing)
