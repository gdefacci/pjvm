module Data.Binary.Binary where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.DataView (setFloat32be, setFloat64be, setUint16be, setUint32be)
import Data.ArrayBuffer.Types (ByteLength, ByteOffset)
import Data.Binary.Decoder (ByteLengthString(..), Decoder, getByteLengthString, getChar8, getFloat32, getFloat64, getString, getUInt16, getUInt32, getUInt8)
import Data.Binary.Put (Put, putWithOffset, byteLength, float32Put, float64Put, putArray, putChar, putChar8, putFail, uint16Put, uint32Put, uint8Put)
import Data.Binary.Types (Float32(..), Float64(..), Word16(..), Word32(..), Word64(..), Word8(..))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import Data.UInt (fromInt)

class Binary a where
  put :: a -> Put
  get :: forall m . Monad m => Decoder m a

instance binaryWord8 :: Binary Word8 where
  put = unwrap >>> uint8Put
  get = Word8 <$> getUInt8

instance binaryWord16 :: Binary Word16 where
  put = unwrap >>> uint16Put
  get = Word16 <$> getUInt16

instance binaryWord32 :: Binary Word32 where
  put = unwrap >>> uint32Put
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
  put = unwrap >>> float32Put
  get = Float32 <$> getFloat32

instance binaryFloat64 :: Binary Float64 where
  put = unwrap >>> float64Put
  get = Float64 <$> getFloat64

instance binaryChar :: Binary Char where
  put = putChar8
  get = getChar8

instance binaryString :: Binary String where
  put str =
    let putStr = putArray $ putChar <$> (toCharArray str)
    in case byteLength putStr of
      (Left err) -> putFail $ \_ -> err
      (Right len) -> put (Word16 $ fromInt len) <> putStr

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
  where
    putN fp fo a =
      putWithOffset putArr
      where
        putArr ofs =
          let repn = fo ofs
          in putArray $ A.replicate repn (fp a)

