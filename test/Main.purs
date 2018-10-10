module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import JVM.FlagsTest as FlagsTest

main :: Effect Unit
main = do
  FlagsTest.spec
