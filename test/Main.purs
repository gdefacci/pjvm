module Test.Main where

import Prelude
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

import JVM.FlagsTest as FlagsTest
import JVM.ConverterTest as ConverterTest
import Data.Binary.DecoderTest as DecoderTest

main :: Effect Unit
main = runTest do
  suite "JVM" do
    test "FlagsTest" do
      FlagsTest.spec
    suite "ConverterTest" do
      ConverterTest.spec
  suite "Binary" do
    test "Decoder test" do
      DecoderTest.spec
