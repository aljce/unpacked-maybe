module Main (main) where

import Prelude (IO)
import Data.Maybe.Unpacked
import Test.DocTest

main :: IO ()
main = doctest [ "src" ]
