module Data.Binary.Encoder where

import Prelude

import Data.ArrayBuffer.DataView (Setter, setUint16be, setUint32be, setUint8)
import Data.ArrayBuffer.Types (ByteLength, ByteOffset, DataView)
import Data.Binary.Types (Word16(..), Word32(..), Word64(..), Word8(..))
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Int.Bits (shr, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as STR
import Data.UInt (UInt, fromInt)
import Effect (Effect)
import Effect.Exception (throw)

data Encoder a = Encoder (a -> DataView -> ByteOffset -> Effect Int)

runEncoder :: forall a. Encoder a -> a -> DataView -> ByteOffset -> Effect Int
runEncoder (Encoder enc) a dv ofs = enc a dv ofs

instance semigroupEncoder :: Semigroup (Encoder a) where
  append (Encoder s1) (Encoder s2) =
    Encoder $ \a -> \dv -> \ofs -> do
      ofs1 <- s1 a dv ofs
      s2 a dv ofs1

instance contravariantEncoder :: Contravariant Encoder where
  cmap f (Encoder s) = Encoder $ \a -> \dv -> \ofs -> s (f a) dv ofs

fromSetter :: forall a. ByteLength -> Setter a -> Encoder a
fromSetter len setter =
  Encoder $ \a -> \dv -> \ofs -> do
    _ <- setter dv a ofs
    pure $ len

putUInt8 :: Encoder UInt
putUInt8 = fromSetter 1 setUint8

putFoldable :: forall f a. Foldable f => Encoder a -> Encoder (f a)
putFoldable enc =
  Encoder $ \bts -> \dv -> \ofs ->
    let zero :: Effect Int
        zero = pure ofs
    in foldl (accum dv) zero bts
    where
      accum dv ofs1Eff b = do
        ofs1 <- ofs1Eff
        runEncoder enc b dv ofs1

putBytes :: Encoder (Array UInt)
putBytes = putFoldable putUInt8

putW8 :: Encoder Word8
putW8 = cmap (\(Word8 w8) -> w8) (fromSetter 1 setUint8)

putW16 :: Encoder Word16
putW16 = cmap (\(Word16 w16) -> w16) (fromSetter 2 setUint16be)

putW32 :: Encoder Word32
putW32 = cmap (\(Word32 w32) -> w32) (fromSetter 4 setUint32be)

putW64 :: Encoder Word64
putW64 =  (cmap wfst putW32) <> (cmap wsnd putW32)
  where
    wfst (Word64 a _) = Word32 a
    wsnd (Word64 _ b) = Word32 b

putChar :: Encoder Char
putChar = Encoder putCharBytes
  where
    x0080   = 128
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

    putCharBytes ch = case charBytes ch of
      (Just chbs) -> runEncoder putBytes (fromInt <$> chbs)
      Nothing -> \dv -> \ofs -> throw $ "Cant convert char " <> (show ch) <> " to modified UTF-8"

    charBytes ch = case toCharCode ch of
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

putString :: Encoder String
putString =
  ((STR.length >>> fromInt >>> Word16) `cmap` putW16) <>
  (STR.toCharArray `cmap` (putFoldable putChar))