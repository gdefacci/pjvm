module Data.Binary.Encoder where

import Prelude

import Data.ArrayBuffer.DataView (Setter, setFloat32be, setFloat64be, setUint16be, setUint32be, setUint8)
import Data.ArrayBuffer.Types (ByteLength, ByteOffset, DataView)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldl)
import Data.Int.Bits (shr, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.UInt (UInt, fromInt)
import Effect (Effect)
import Effect.Exception (throw)

type Encoder a = a -> DataView -> ByteOffset -> Effect Int

fromSetter :: forall a. ByteLength -> Setter a -> Encoder a
fromSetter len setter a dv ofs = do
  _ <- setter dv a ofs
  pure $ len

putFoldable :: forall f a. Foldable f => Encoder a -> Encoder (f a)
putFoldable enc bts dv ofs =
  let zero :: Effect Int
      zero = pure ofs
  in foldl (accum dv) zero bts
  where
    accum dv1 ofs1Eff b = do
      ofs1 <- ofs1Eff
      enc b dv1 ofs1

putBytes :: Encoder (Array UInt)
putBytes = putFoldable putUInt8

putUInt8 :: Encoder UInt
putUInt8 = (fromSetter 1 setUint8)

putUInt16 :: Encoder UInt
putUInt16 = (fromSetter 2 setUint16be)

putUInt32 :: Encoder UInt
putUInt32 = (fromSetter 4 setUint32be)

putFloat32 :: Encoder Number
putFloat32 = (fromSetter 4 setFloat32be)

putFloat64 :: Encoder Number
putFloat64 = (fromSetter 8 setFloat64be)

charBytes :: Char -> Maybe (Array Int)
charBytes ch =
  let x0080   = 128
      x07ff   = 2047
      x0800   = 2048
      x0f     = 15
      x1f     = 31
      x3f     = 63
      x7f     = 127
      x80     = 128
      xc0     = 192
      xe0     = 224
      xffff   = 65535
  in case toCharCode ch of
    0 ->
      Just [ 0, 0]
    c | c > 0 && c <= x7f ->
      Just [c]
    c | c >= x0080 && c <= x07ff ->
      Just [  xc0 .|. (x1f .&. (shr c 6)),
              x80 .|. (x3f .&. c) ]
    c | c >= x0800 && c <= xffff ->
      Just [  xe0 .|. (x0f .&. (shr c 12)),
              x80 .|. (x3f .&. (shr c 6)),
              x80 .|. (x3f .&. c) ]
    _ -> Nothing

putChar8 :: Encoder Char
putChar8 = toCharCode >>> fromInt >>> putUInt8

putChar :: Encoder Char
putChar ch =
  case charBytes ch of
    (Just chbs) -> putBytes (fromInt <$> chbs)
    Nothing -> \dv -> \ofs -> throw $ "Cant convert char " <> (show ch) <> " to modified UTF-8"

{- putStringLength :: Encoder String
putStringLength =
  (STR.length >>> fromInt >>> Word16) `cmap` putW16

putString :: Encoder String
putString =
  STR.toCharArray `cmap` (putFoldable putChar) -}