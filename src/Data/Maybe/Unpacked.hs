--------------------------------------------------------------------------------

-- Copyright © 2016 Kyle McKean
-- Copyright © 2018 Daniel Cartwright

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of Kyle McKean nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedSums        #-}
{-# LANGUAGE UnboxedTuples      #-}

--------------------------------------------------------------------------------

{-| This module is intended to be a drop-in replacement
    for 'Data.Maybe'. To shave off pointer chasing, it
    uses @'-XUnboxedSums'@ to represent the @'Maybe'@ type
    as two machine words that are contiguous in memory, without
    loss of expressiveness that 'Data.Maybe' provides.

    This library provides pattern synonyms @'Just'@ and @'Nothing'@
    that allow users to pattern match on an Unpacked Maybe
    in a familiar way.

    Functions are also provided for converting an Unpacked Maybe
    to the base library's Maybe.
-}

module Data.Maybe.Unpacked
  ( Maybe(Maybe, Just, Nothing)
  , maybe
  , isJust
  , isNothing
  , fromJust
  , fromMaybe
  , listToMaybe
  , maybeToList
  , catMaybes
  , mapMaybe
  , fromBaseMaybe
  , toBaseMaybe
  ) where

--------------------------------------------------------------------------------

import Prelude
  ()

import           Control.Applicative (Alternative(empty, (<|>)), Applicative(liftA2, pure, (<*>), (*>)))

import           Control.Monad       (Monad(return, (>>=)), MonadPlus(mzero, mplus))
import           Control.Monad.Fail  (MonadFail(fail))
import           Control.Monad.Fix   (MonadFix(mfix))
import           Control.Monad.Zip   (MonadZip(mzipWith))

import           Data.Data
  ( Constr
  , Data(dataTypeOf, gunfold, toConstr) 
  , DataType
  , Fixity(Prefix)
  , mkConstr
  , mkDataType
  )
import           Data.Eq             (Eq((==)))
import           Data.Foldable
  (Foldable(foldMap, foldr, foldl, length, null, product, sum))

import           Data.Function       (const, flip, id, (.), ($))
import           Data.Functor        (Functor(fmap))
import           Data.Functor.Classes
  ( Eq1(liftEq)
  , Ord1(liftCompare)
  , Read1(liftReadPrec, liftReadListPrec, liftReadList)
  , Show1(liftShowsPrec)
  , readData
  , readUnaryWith
  , liftReadListPrecDefault
  , liftReadListDefault
  , showsUnaryWith
  )

import qualified Data.Maybe as BaseMaybe
import           Data.Monoid         (Monoid(mempty,mappend))
import           Data.Ord            (Ord(compare, (>)), Ordering(EQ, GT, LT))
import           Data.Semigroup      (Semigroup((<>)))
import           Data.Traversable    (Traversable(sequenceA, traverse))

import           GHC.Base            (Bool(False,True), Int, build)
import           GHC.Err             (error, errorWithoutStackTrace)
import           GHC.Num             ( (+) )
import           GHC.Read            (Read(readPrec), expectP)
import           GHC.Show            (Show(showsPrec), showString, showParen)

import           Text.Read           (parens, Lexeme(Ident), lexP, (+++))
import           Text.ParserCombinators.ReadPrec
  (prec, step)

--------------------------------------------------------------------------------

data Maybe a = Maybe (# (# #) | a #)

-- | The 'Just' pattern synonym mimics the functionality of the 'Just' constructor
--   from 'Data.Maybe'. This is just provided to ensure 'Data.Maybe.Unpacked' is a drop in replacement for 'Data.Maybe'.
--
pattern Just :: a -> Maybe a
pattern Just a = Maybe (# | a #)

-- | The 'Nothing' pattern synonym mimics the functionality of the 'Nothing' constructor
--   from 'Data.Maybe'. This is just provided to ensure 'Data.Maybe.Unpacked' is a drop-in replacement for 'Data.Maybe'.
-- 
pattern Nothing :: Maybe a
pattern Nothing = Maybe (# (# #) | #)

{-# COMPLETE Just, Nothing #-}

nothing :: Maybe a
nothing = Maybe (# (# #) | #)
{-# INLINE nothing #-}

just :: a -> Maybe a
just x = Maybe (# | x #)
{-# INLINE just #-}

maybe :: b -> (a -> b) -> Maybe a -> b
maybe def f (Maybe x) = case x of
  (# (# #) |   #) -> def
  (#       | a #) -> f a
{-# INLINE maybe #-}

isNothing :: Maybe a -> Bool
isNothing = maybe True (const False)
{-# INLINE isNothing #-}

-- | The 'isJust' function returns 'True' if its argument is of the
-- form @Just _@.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isJust (just 3)
-- True
--
-- >>> isJust (just ())
-- True
--
-- >>> isJust nothing
-- False
--
-- Only the outer constructor is taken into consideration:
--
-- >>> isJust (just nothing)
-- True
--
isJust :: Maybe a -> Bool
isJust = maybe False (const True)
{-# INLINE isJust #-}

-- | The 'fromJust' function extracts the element out of a 'just' and
-- throws an error if its argument is 'nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromJust (just 1)
-- 1
--
-- >>> 2 * (fromJust (just 10))
-- 20
--
-- >>> 2 * (fromJust nothing)
-- *** Exception: Data.Maybe.Unpacked.fromJust: Nothing
--
fromJust :: Maybe a -> a
fromJust = fromMaybe (error "Data.Maybe.Unpacked.fromJust: Nothing")
{-# INLINE fromJust #-}

-- | The 'fromMaybe' function takes a default value and and 'Maybe'
-- value.  If the 'Maybe' is 'nothing', it returns the default values;
-- otherwise, it returns the value contained in the 'Maybe'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromMaybe "" (just "Hello, World!")
-- "Hello, World!"
--
-- >>> fromMaybe "" nothing
-- ""
--
-- Read an integer from a string using 'readMaybe'. If we fail to
-- parse an integer, we want to return @0@ by default:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromBaseMaybe . readMaybe :: String -> Maybe Int
-- >>> fromMaybe 0 (parse "5")
-- 5
-- >>> fromMaybe 0 (parse "")
-- 0
--
fromMaybe :: a -> Maybe a -> a
fromMaybe def = maybe def id
{-# INLINE fromMaybe #-}

-- | The 'maybeToList' function returns an empty list when given
-- 'nothing' or a singleton list when not given 'nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> maybeToList (just 7)
-- [7]
--
-- >>> maybeToList nothing
-- []
--
-- One can use 'maybeToList' to avoid pattern matching when combined
-- with a function that (safely) works on lists:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromBaseMaybe . readMaybe :: String -> Maybe Int
-- >>> sum $ maybeToList (parse "3")
-- 3
-- >>> sum $ maybeToList (parse "")
-- 0
--
-- This being said 'Maybe' is an instance of the 'Foldable' typeclass
-- so the example above could also be written as:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromBaseMaybe . readMaybe :: String -> Maybe Int
-- >>> sum $ parse "3"
-- 3
-- >>> sum $ parse ""
-- 0
--
maybeToList :: Maybe a -> [a]
maybeToList = maybe [] (: [])
{-# INLINE maybeToList #-}

-- | The 'listToMaybe' function returns 'Nothing' on an empty list
-- or @'Just' a@ where @a@ is the first element of the list.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> listToMaybe []
-- Nothing
--
-- >>> listToMaybe [9]
-- Just 9
--
-- >>> listToMaybe [1,2,3]
-- Just 1
--
-- Composing 'maybeToList' with 'listToMaybe' should be the identity
-- on singleton/empty lists:
--
-- >>> maybeToList $ listToMaybe [5]
-- [5]
-- >>> maybeToList $ listToMaybe []
-- []
--
-- But not on lists with more than one element:
--
-- >>> maybeToList $ listToMaybe [1,2,3]
-- [1]
--
listToMaybe :: [a] -> Maybe a
listToMaybe []    = nothing
listToMaybe (x:_) = just x

-- | The 'catMaybes' function takes a list of 'Maybe's and returns
-- a list of all the 'just' values.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> catMaybes [just 1, nothing, just 3]
-- [1,3]
--
-- When constructing a list of 'Maybe' values, 'catMaybes' can be used
-- to return all of the \"success\" results (if the list is the result
-- of a 'map', then 'mapMaybe' would be more appropriate):
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromBaseMaybe . readMaybe :: String -> Maybe Int
-- >>> [ parse x | x <- ["1", "Foo", "3"] ]
-- [Just 1,Nothing,Just 3]
-- >>> catMaybes $ [ parse x | x <- ["1", "Foo", "3"] ]
-- [1,3]
--
catMaybes :: [Maybe a] -> [a]
catMaybes = mapMaybe id
{-# INLINE catMaybes #-}

-- | The 'mapMaybe' function is a version of 'map' which can throw
-- out elements.  In particular, the functional argument returns
-- something of type @'Maybe' b@.  If this is 'Nothing', no element
-- is added on to the result list.  If it is @'Just' b@, then @b@ is
-- included in the result list.
--
-- ==== __Examples__
--
-- Using @'mapMaybe' f x@ is a shortcut for @'catMaybes' $ 'map' f x@
-- in most cases:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromBaseMaybe . readMaybe :: String -> Maybe Int
-- >>> mapMaybe parse ["1", "Foo", "3"]
-- [1,3]
-- >>> catMaybes $ map parse ["1", "Foo", "3"]
-- [1,3]
--
-- If we map the 'just' function, the entire list should be returned:
--
-- >>> mapMaybe just [1,2,3]
-- [1,2,3]
--
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f !(a:as) = let bs = mapMaybe f as in maybe bs (: bs) (f a)
{-# NOINLINE [1] mapMaybe #-}

{-# RULES
"mapMaybe"     [~1] forall f xs. mapMaybe f xs
                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
  #-}

{-# NOINLINE [0] mapMaybeFB #-}
mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

-- | The 'fromBaseMaybe' function converts 'Data.Maybe' maybes to
-- 'Data.Maybe.Unpacked' maybes. This function is good for using existing
-- functions that return 'Data.Maybe' maybes.
--
-- ====  __Examples__
--
-- Basic usage:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromBaseMaybe . readMaybe :: String -> Maybe Int
-- >>> parse "3"
-- Just 3
-- >>> parse ""
-- Nothing
--
fromBaseMaybe :: BaseMaybe.Maybe a -> Maybe a
fromBaseMaybe (BaseMaybe.Just x) = just x
fromBaseMaybe _                  = nothing
{-# INLINE fromBaseMaybe #-}

-- | The 'toBaseMaybe' function converts "Data.Maybe.Unpacked" maybes to
-- 'Data.Maybe' maybes. This function is provided for testing and convenience
-- but it is not an idiomatic use of this library. It ruins the speed and space gains from
-- unpacking the 'Maybe'. I implore you to destruct the 'Maybe' with 'maybe' instead.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> import Data.List (unfoldr)
-- >>> let ana n = if n == 5 then nothing else just (n+1,n+1)
-- >>> unfoldr (toBaseMaybe . ana) 0
-- [1,2,3,4,5]
--
toBaseMaybe :: Maybe a -> BaseMaybe.Maybe a
toBaseMaybe = maybe BaseMaybe.Nothing BaseMaybe.Just
{-# INLINE toBaseMaybe #-}

--------------------------------------------------------------------------------

-- Below here lie only instances, and helpers for instances

maybeDataType :: DataType
maybeDataType = mkDataType "Data.Maybe.Unpacked.Maybe" [nothingConstr, justConstr]

nothingConstr :: Constr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix

justConstr :: Constr
justConstr = mkConstr maybeDataType "Just" [] Prefix

instance Data a => Data (Maybe a) where
  toConstr = maybe nothingConstr (const justConstr)
  gunfold _ _ = errorWithoutStackTrace "Data.Data.gunfold(Data.Maybe.Unpacked.Maybe)"
  dataTypeOf _ = maybeDataType

instance Functor Maybe where
  fmap f = maybe nothing (just . f)
  {-# INLINE fmap #-}

instance Applicative Maybe where
  pure = just
  {-# INLINE pure #-}
  mf <*> mx = maybe nothing (\f -> fmap f mx) mf
  {-# INLINE (<*>) #-}

instance Monad Maybe where
  return = just
  {-# INLINE return #-}
  mx >>= f = maybe nothing f mx
  {-# INLINE (>>=) #-}

instance MonadFail Maybe where
  fail _ = nothing
  {-# INLINE fail #-}

instance MonadFix Maybe where
  mfix f = let a = f (fromJust a) in a
  {-# INLINE mfix #-}

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

instance Foldable Maybe where
  foldMap f ma = maybe mempty f ma
  {-# INLINE foldMap #-}
  foldr f z ma = maybe z ((flip f) z) ma 
  {-# INLINE foldr #-}
  foldl f z ma = maybe z (f z) ma
  {-# INLINE foldl #-} 
  length  = maybe 0 (const 1)
  {-# INLINE length #-}
  null    = isNothing
  {-# INLINE null #-}
  product = maybe 0 id
  {-# INLINE product #-}
  sum     = maybe 0 id
  {-# INLINE sum #-}

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
  ma == mb = maybe (isNothing mb)
                   (\a -> maybe False (\b -> a == b) mb) ma
  {-# INLINE (==) #-}

instance Ord a => Ord (Maybe a) where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma
  {-# INLINE compare #-}

appPrec :: Int
appPrec = 10
{-# INLINE appPrec #-}

instance Show a => Show (Maybe a) where
  showsPrec p ma = maybe (showString "Nothing")
                         (\a -> showParen (p > appPrec) (showString "Just " . (showsPrec (appPrec + 1) a))) ma

instance Read a => Read (Maybe a) where
  readPrec = parens $ nothingP +++ justP
    where nothingP = prec appPrec $ do
            Ident "Nothing" <- lexP
            return nothing
          justP = prec appPrec $ do
            Ident "Just" <- lexP
            a <- step readPrec
            return (just a)

instance Eq1 Maybe where
    liftEq _  Nothing  Nothing  = True
    liftEq _  Nothing  (Just _) = False
    liftEq _  (Just _) Nothing  = False
    liftEq eq (Just x) (Just y) = eq x y
    -- this is a redundant pattern match, but GHC can't see that
    --liftEq _  _        _        = False

instance Ord1 Maybe where
    liftCompare _    Nothing  Nothing  = EQ
    liftCompare _    Nothing  (Just _) = LT
    liftCompare _    (Just _) Nothing  = GT
    liftCompare comp (Just x) (Just y) = comp x y
    -- this is a redundant pattern match, but GHC can't see that 
    --liftCompare _    _        _        = EQ

instance Read1 Maybe where
    liftReadPrec rp _ =
        parens (expectP (Ident "Nothing") *> pure nothing)
        <|>
        readData (readUnaryWith rp "Just" just)

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

instance Show1 Maybe where
    liftShowsPrec _  _ _ Nothing  = showString "Nothing"
    liftShowsPrec sp _ d (Just x) = showsUnaryWith sp "Just" d x
    -- this is a redundant pattern match, but GHC can't see that
    --liftShowsPrec _  _ _ _        = showString "Nothing" 
