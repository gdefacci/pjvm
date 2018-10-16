module Data.Binary.Binary where

import Prelude

import Data.ArrayBuffer.Types (DataView, ByteOffset)
import Data.Binary.Decoder (Decoder(..), getChar, getChar8, getFloat32, getFloat64, getString, getUInt16, getUInt32, getUInt8)
import Data.Binary.Encoder (Encoder, putChar, putFloat32, putFloat64, putUInt16, putUInt32, putUInt8)
import Data.Binary.Types (Float32(..), Float64(..), Word16(..), Word32(..), Word64(..), Word8(..))
import Data.Foldable (class Foldable, foldMap)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String as STR
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt, fromInt)
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
  get = Word8 <$> getUInt8 

instance binaryWord16 :: Binary Word16 where
  put = unwrap >>> (fromEncoder putUInt16)
  get = Word16 <$> getUInt16

instance binaryWord32 :: Binary Word32 where
  put = unwrap >>> (fromEncoder putUInt32)
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
  put = unwrap >>> (fromEncoder putFloat32)
  get = Float32 <$> getFloat32

instance binaryFloat64 :: Binary Float64 where
  put = unwrap >>> (fromEncoder putFloat64)
  get = Float64 <$> getFloat64

instance binaryChar :: Binary Char where 
  put = (fromEncoder putChar)
  get = getChar

instance binaryString :: Binary String where
  put = 
    ((STR.length >>> fromInt >>> Word16) >>> put) <>
    (toCharArray >>> foldablePut)


  get = getString

foldablePut :: forall f a. Foldable f => Binary a => f a -> Put
foldablePut = foldMap put
