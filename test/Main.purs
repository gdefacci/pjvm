module Test.Main where

import Prelude
import Effect (Effect)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)

import JVM.FlagsTest as FlagsTest
import JVM.ConverterTest as ConverterTest
import Data.Binary.DecoderTest as DecoderTest
import Data.Binary.PutTest as PutTest

main :: Effect Unit
main = runTest do
  suite "Binary" do
    test "Put test" do
      PutTest.spec
    test "Decoder test" do
      DecoderTest.spec
  suite "JVM" do
    test "FlagsTest" do
      FlagsTest.spec
    suite "ConverterTest" do
      ConverterTest.spec


