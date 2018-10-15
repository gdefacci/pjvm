module Data.Binary.Binary where

import Prelude

import Data.ArrayBuffer.Types (DataView, ByteOffset)
import Data.Binary.Decoder (Decoder(..), getChar8, getFloat32, getFloat64, getString, getW16, getW32, getW64, getW8)
import Data.Binary.Encoder (Encoder, putUInt16, putUInt32, putUInt8, putFloat32, putFloat64)
import Data.Binary.Types (Float32(..), Float64(..), Word16(..), Word32(..), Word64(..), Word8(..))
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)

newtype Put = Put (DataView -> ByteOffset -> Effect Int)

instance semigroupPut :: Semigroup Put where
  append (Put f1) (Put f2) = Put $ \dv -> \ofs -> do
    ofs1 <- f1 dv ofs
    f2 dv ofs1

instance monoidPut :: Monoid Put where
  mempty = Put $ \_ -> \ofs -> pure ofs

putFail :: (ByteOffset -> String) -> Put
putFail msg = Put $ \_ -> \ofs -> throw $ msg ofs

class Binary a where
  put :: a -> Put
  get :: Decoder a

fromEncoder :: forall a. Encoder a -> a -> Put
fromEncoder f a = Put (f a)

instance binaryWord8 :: Binary Word8 where
  put = unwrap >>> (fromEncoder putUInt8)
  get = getW8

instance binaryWord16 :: Binary Word16 where
  put = unwrap >>> (fromEncoder putUInt16)
  get = getW16

instance binaryWord32 :: Binary Word32 where
  put = unwrap >>> (fromEncoder putUInt32)
  get = getW32

instance binaryWord64 :: Binary Word64 where
  put =
    (wfst >>> put) <> (wsnd >>> put)
    where
      wfst (Word64 a _) = Word32 a
      wsnd (Word64 _ b) = Word32 b
  get = getW64

instance binaryFloat32 :: Binary Float32 where
  put = unwrap >>> (fromEncoder putFloat32)
  get = getFloat32

instance binaryFloat64 :: Binary Float64 where
  put = unwrap >>> (fromEncoder putFloat64)
  get = getFloat64

{- instance binaryString :: Binary String where
  put = fromEncoder putString
  get = getString
 -}

foldablePut :: forall f a. Foldable f => Binary a => f a -> Put
foldablePut = foldMap put
