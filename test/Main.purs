module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import JVM.FlagsTest as FlagsTest
import Data.Binary.DecoderTest as DecoderTest

main :: Effect Unit
main = runTest do
  suite "JVM" do
    test "FlagsTest" do
      FlagsTest.spec
  suite "Binary" do
    test "Decoder test" do
      DecoderTest.spec
