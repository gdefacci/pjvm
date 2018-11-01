module TestHelper where

import Prelude

import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer)

bufferToArray :: ArrayBuffer -> Array Int
bufferToArray bf1 =
  let dv1 = DV.whole bf1
      toArray = asUint8Array >>> toIntArray
  in toArray dv1

bufferEquals :: ArrayBuffer -> ArrayBuffer -> Boolean
bufferEquals bf1 bf2 = (bufferToArray bf1) == (bufferToArray bf2)
