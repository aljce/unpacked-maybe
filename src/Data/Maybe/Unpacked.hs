{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Data.Maybe.Unpacked where

import Prelude hiding (Maybe(..),maybe)
import qualified Data.Maybe as Old
import GHC.Base (build)

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

instance Foldable Maybe where
  foldMap f ma = maybe mempty f ma
  {-# INLINE foldMap #-}
  foldr f z ma = maybe z ((flip f) z) ma 
  {-# INLINE foldr #-}
  foldl f z ma = maybe z (f z) ma
  {-# INLINE foldl #-} 
  length  = maybe 0 (const 1) 
  null    = isNothing 
  product = maybe 0 id 
  sum     = maybe 0 id

instance Traversable Maybe where
  sequenceA ma = maybe (pure nothing) (fmap just) ma
  {-# INLINE sequenceA #-}
  traverse f ma = maybe (pure nothing) (fmap just . f) ma
  {-# INLINE traverse #-}

instance Semigroup a => Semigroup (Maybe a) where
  ma <> mb = maybe mb (\a -> maybe ma (\b -> just (a <> b)) mb) ma
  {-# INLINE (<>) #-}

-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\" Unlike
-- in "Data.Maybe" this library can depend on 'semigroups'. So this
-- instance's superclass is less restrictive than the instance then the
-- 'Data.Maybe' instance.
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

-- | The 'isNothing' function returns 'True' if its argument is 'nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isNothing (just 3)
-- False
--
-- >>> isNothing (just ())
-- False
--
-- >>> isNothing nothing
-- True
--
-- Only the outer constructor is taken into consideration:
--
-- >>> isNothing (just nothing)
-- False
--

isNothing :: Maybe a -> Bool
isNothing = maybe True (const False)
{-# INLINE isNothing #-}

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
-- >>> let parse = fromOldMaybe . readMaybe :: String -> Maybe Int
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
-- >>> let parse = fromOldMaybe . readMaybe :: String -> Maybe Int
-- >>> sum $ maybeToList (parse "3")
-- 3
-- >>> sum $ maybeToList (parse "")
-- 0
--
-- This being said 'Maybe' is an instance of the 'Foldable' typeclass
-- so the example above could also be written as:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromOldMaybe . readMaybe :: String -> Maybe Int
-- >>> sum $ parse "3"
-- 3
-- >>> sum $ parse ""
-- 0
--

maybeToList :: Maybe a -> [a]
maybeToList = maybe [] (:[])
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
{-# INLINE listToMaybe #-}

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
-- >>> let parse = fromOldMaybe . readMaybe :: String -> Maybe Int
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
-- >>> let parse = fromOldMaybe . readMaybe :: String -> Maybe Int
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
mapMaybe f (a:as) = let bs = mapMaybe f as in maybe bs (:bs) (f a)
{-# NOINLINE [1] mapMaybe #-}

{-# RULES
"mapMaybe"     [~1] forall f xs. mapMaybe f xs
                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
  #-}

{-# NOINLINE [0] mapMaybeFB #-}
mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

-- | The 'fromOldMaybe' function converts 'Data.Maybe' maybes to
-- 'Data.Maybe.Unpacked' maybes. This function is good for using existing
-- functions that return 'Data.Maybe' maybes.
--
-- ====  __Examples__
--
-- Basic usage:
--
-- >>> import Text.Read ( readMaybe )
-- >>> let parse = fromOldMaybe . readMaybe :: String -> Maybe Int
-- >>> parse "3"
-- Just 3
-- >>> parse ""
-- Nothing
--

fromOldMaybe :: Old.Maybe a -> Maybe a
fromOldMaybe (Old.Just x)  = just x
fromOldMaybe (Old.Nothing) = nothing
{-# INLINE fromOldMaybe #-}

-- | The 'toOldMaybe' function converts "Data.Maybe.Unpacked" maybes to
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
-- >>> unfoldr (toOldMaybe . ana) 0
-- [1,2,3,4,5]
--

toOldMaybe :: Maybe a -> Old.Maybe a
toOldMaybe = maybe Old.Nothing Old.Just
{-# INLINE toOldMaybe #-}

-- | The 'Just' pattern synonym mimics the functionality of the 'Just' constructor
-- from 'Data.Maybe'. As with 'toOldMaybe' this is not an idiomatic use of this library,
-- and it is just provided to ensure 'Data.Maybe.Unpack' is a drop in replacement for 'Data.Maybe'.
-- The problem with 'Just' stems from its definition:
-- > pattern Just x <- (toOldMaybe -> Old.Just x)
-- Anytime you use the pattern synonym 'Just' you unpack the 'Maybe' maybe to a
-- 'Data.Maybe' maybe. Instead of pattern matching on the maybe I highly suggest destructing the
-- 'Maybe' with the functions 'maybe' or 'fromMaybe'.

pattern Just :: a -> Maybe a
pattern Just x <- (toOldMaybe -> Old.Just x)

-- | The 'Nothing' pattern synonym mimics the functionality of the 'Nothing' constructor
-- from 'Data.Maybe'. As with the function 'toOldMaybe' and the pattern synonym 'Just', 'Nothing'
-- is not an idiomatic use of this library. See the docs for 'Just' for an explaintion.

pattern Nothing :: Maybe a
pattern Nothing <- (toOldMaybe -> Old.Nothing)
