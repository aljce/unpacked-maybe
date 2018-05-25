{-# LANGUAGE KindSignatures #-}

module Main (main) where

import Prelude hiding (Maybe(..), maybe)

import Control.Applicative
import Control.Monad (liftM, MonadPlus)
import Control.Monad.Zip (MonadZip)
import Data.Functor.Classes
import Data.Maybe.Unpacked (Maybe(Just, Nothing), nothing, just)
import Data.Monoid (Monoid(mempty,mappend))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup((<>)))
import Test.QuickCheck.Classes
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

main :: IO ()
main = lawsCheckMany myClassTests

myClassTests :: [(String, [Laws])]
myClassTests =
  [ ("Ground types", myLaws maybeInt)
  , ("Higher-kinded types", myLaws1 maybeProxy)
  ]

myLaws
  :: (Arbitrary a, Monoid a, Eq a, Ord a, Semigroup a, Show a, Read a)
  => Proxy a -> [Laws]
myLaws p =
  [ commutativeMonoidLaws p
  , eqLaws p
  , ordLaws p
  , semigroupLaws p
  , showReadLaws p
  ]

myLaws1
  :: (Arbitrary1 a, Alternative a, Applicative a, Foldable a, Functor a, Monad a, MonadPlus a, MonadZip a, Traversable a, Eq1 a, Show1 a)  
  => Proxy a -> [Laws]
myLaws1 p =
  [ alternativeLaws p
  , applicativeLaws p
  , foldableLaws p
  , functorLaws p
  , monadLaws p
  , monadPlusLaws p
  , monadZipLaws p
  , traversableLaws p
  ]

maybeProxy :: Proxy Maybe
maybeProxy = Proxy

maybeInt :: Proxy (Maybe Int)
maybeInt = Proxy

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
  mappend = (+)

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = arbitrary1
  shrink    = shrink1

instance Arbitrary1 Maybe where
  liftArbitrary arb = frequency [(1, return nothing), (3, liftM just arb)]

  liftShrink shr (Just x) = nothing : [ just x' | x' <- shr x ]
  liftShrink _   Nothing  = []
