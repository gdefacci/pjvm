module Data.Binary.Encoder where

import Prelude

import Data.ArrayBuffer.DataView (Setter, setFloat32be, setFloat64be, setUint16be, setUint32be, setUint8)
import Data.ArrayBuffer.Types (ByteLength, ByteOffset, DataView)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.UInt (UInt, fromInt)
import Effect (Effect)
import Effect.Exception (throw)

type Encoder a = a -> ByteOffset -> DataView -> Effect Unit

fromSetter :: forall a. Setter a -> Encoder a
fromSetter setter a ofs dv = setter dv a ofs

putUInt8 :: Encoder UInt
putUInt8 = (fromSetter setUint8)

putUInt16 :: Encoder UInt
putUInt16 = (fromSetter setUint16be)

putUInt32 :: Encoder UInt
putUInt32 = (fromSetter setUint32be)

putFloat32 :: Encoder Number
putFloat32 = (fromSetter setFloat32be)

putFloat64 :: Encoder Number
putFloat64 = (fromSetter setFloat64be)

putChar8 :: Encoder Char
putChar8 = toCharCode >>> fromInt >>> putUInt8