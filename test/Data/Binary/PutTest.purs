module Data.Binary.PutTest where

import Prelude

import Data.Binary.Binary (class Binary, put)
import Data.Binary.Put (runPut)
import Data.Int (binary, fromStringAs)
import Data.Maybe (fromJust)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
import Test.Unit.Assert as Assert
import TestHelper (bufferToArray)

type Description = String

testPutEquals :: forall a. Eq a => Binary a => Description -> a -> (Array Int) -> Aff Unit
testPutEquals desc inp expected = do
  buff <- runPut (put inp)
  Assert.assert desc $ expected == (bufferToArray buff)

bin :: String -> Int
bin = (unsafePartial fromJust) <<< (fromStringAs binary)

spec :: Aff Unit
spec = do
  testPutEquals "ù" "ù" [bin "0", bin "10", bin "11000011", bin "10111001"]
  testPutEquals "à" "à" [bin "0", bin "10", bin "11000011", bin "10100000"]
  testPutEquals "è" "è" [bin "0", bin "10", bin "11000011", bin "10101000"]
  testPutEquals "é" "é" [bin "0", bin "10", bin "11000011", bin "10101001"]
  testPutEquals "д" "д" [bin "0", bin "10", bin "11010000", bin "10110100"]
  testPutEquals "и" "и" [bin "0", bin "10", bin "11010000", bin "10111000"]
  testPutEquals "й" "й" [bin "0", bin "10", bin "11010000", bin "10111001"]
  testPutEquals "电" "电" [bin "0", bin "11", bin "11100111", bin "10010100", bin "10110101"]
  testPutEquals "脑" "脑" [bin "0", bin "11", bin "11101000", bin "10000100", bin "10010001"]



