module Data.Binary.Binary where

import Prelude

import Data.ArrayBuffer.DataView (setFloat32be, setFloat64be, setUint16be, setUint32be, setUint8)
import Data.ArrayBuffer.Types (ByteLength, ByteOffset)
import Data.Binary.Decoder (ByteLengthString(..), Decoder, getByteLengthString, getChar, getFloat32, getFloat64, getString, getUInt16, getUInt32, getUInt8)
import Data.Binary.Put (Put, charPut, fromSetter, putN, uint8Put)
import Data.Binary.Types (Float32(..), Float64(..), Word16(..), Word32(..), Word64(..), Word8(..))
import Data.Foldable (class Foldable, foldMap)

import Data.Newtype (unwrap)
import Data.String as STR
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UInt (fromInt)

class Binary a where
  put :: a -> Put
  get :: Decoder a

instance binaryWord8 :: Binary Word8 where
  put = unwrap >>> uint8Put
  get = Word8 <$> getUInt8

instance binaryWord16 :: Binary Word16 where
  put = unwrap >>> (fromSetter 2 setUint16be)
  get = Word16 <$> getUInt16

instance binaryWord32 :: Binary Word32 where
  put = unwrap >>> (fromSetter 4 setUint32be)
  get = Word32 <$> getUInt32

instance binaryWord64 :: Binary Word64 where
  put =
    (wfst >>> put) <> (wsnd >>> put)
    where
      wfst (Word64 a _) = Word32 a
      wsnd (Word64 _ b) = Word32 b

  get = do
    (Word32 w32a) <- get
    (Word32 w32b) <- get
    pure $ Word64 w32a w32b

instance binaryFloat32 :: Binary Float32 where
  put = unwrap >>> (fromSetter 4 setFloat32be)
  get = Float32 <$> getFloat32

instance binaryFloat64 :: Binary Float64 where
  put = unwrap >>> (fromSetter 8 setFloat64be)
  get = Float64 <$> getFloat64

instance binaryChar :: Binary Char where
  put = charPut
  get = getChar

instance binaryString :: Binary String where
  put =
    (STR.length >>> fromInt >>> Word16 >>> put) <>
    (toCharArray >>> putFoldable)

  get = getString

instance binaryByteLengthString :: Binary ByteLengthString where
  put =
    (getLen >>> fromInt >>> Word16 >>> put) <>
    (getStr >>> toCharArray >>> putFoldable)
    where
      getLen (ByteLengthString l _) = l
      getStr (ByteLengthString _ s) = s

  get = getByteLengthString


instance binaryTuple :: (Binary a, Binary b) => Binary (Tuple a b) where
  put = (fst >>> put) <> (snd >>> put)

  get = do
    a <- get
    b <- get
    pure $ Tuple a b

putFoldable :: forall f a. Foldable f => Binary a => f a -> Put
putFoldable = foldMap put

putPad :: forall a. Binary a => (ByteOffset -> ByteLength) -> a -> Put
putPad = putN put