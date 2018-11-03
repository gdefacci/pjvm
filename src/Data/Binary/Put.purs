module Data.Binary.Put (Put, byteLength, putArray, putChar, putWithOffset, putFail, runPut, uint8Put, uint16Put, uint32Put, float32Put, float64Put, putToString, putToDataView, putToInt8Array, putChar8) where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView (setFloat32be, setFloat64be, setInt8, setUint16be, setUint32be, setUint8)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asInt8Array, asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, ByteOffset, DataView)
import Data.Char (fromCharCode, toCharCode)
import Data.Char as CH
import Data.Either (Either(..))
import Data.Int.Bits (shr, (.&.), (.|.))
import Data.Maybe (Maybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.UInt (UInt, fromInt)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

data Put = PutInt8 Int
           | PutUInt8 UInt
           | PutUInt16 UInt
           | PutUInt32 UInt
           | PutFloat32 Number
           | PutFloat64 Number
           | PutChar Char
           | PutWithOffset (ByteOffset -> Put)
           | PutArray (Array Put)
           | PutFail (ByteOffset -> String)

instance semigroupPut :: Semigroup Put where
  append (PutArray arr1) (PutArray arr2) = PutArray $ arr1 <> arr2
  append (PutArray arr) i = PutArray $ A.snoc arr i
  append i (PutArray arr) = PutArray $ [i] <> arr
  append a b = PutArray $ [a,b]

instance monoidPut :: Monoid Put where
  mempty = PutArray []

uint8Put :: UInt -> Put
uint8Put = PutUInt8

uint16Put :: UInt -> Put
uint16Put = PutUInt16

uint32Put :: UInt -> Put
uint32Put = PutUInt32

float32Put :: Number -> Put
float32Put = PutFloat32

float64Put :: Number -> Put
float64Put = PutFloat64

int8Put :: Int -> Put
int8Put = PutInt8

putChar8 :: Char -> Put
putChar8 = uint8Put <<< fromInt <<< CH.toCharCode

putChar :: Char -> Put
putChar = PutChar

putWithOffset :: (Int -> Put) -> Put
putWithOffset = PutWithOffset

putFail :: (Int -> String) -> Put
putFail = PutFail

putArray :: Array Put -> Put
putArray = PutArray

byteLength :: Put -> Either String ByteOffset
byteLength put = byteLengthAt put 0

byteLengthAt :: Put -> ByteOffset -> Either String ByteOffset
byteLengthAt (PutInt8 _)       ofs = Right $ ofs + 1
byteLengthAt (PutUInt8 _)      ofs = Right $ ofs + 1
byteLengthAt (PutUInt16 _)     ofs = Right $ ofs + 2
byteLengthAt (PutUInt32 _)     ofs = Right $ ofs + 4
byteLengthAt (PutFloat32 _)    ofs = Right $ ofs + 4
byteLengthAt (PutFloat64 _)    ofs = Right $ ofs + 8
byteLengthAt (PutChar ch)      ofs = (\i -> ofs + i) <$> charSize ch
byteLengthAt (PutWithOffset f) ofs = byteLengthAt (f ofs) ofs
byteLengthAt (PutArray arr)    ofs =
  A.foldl accum (Right ofs) arr
  where
    accum (Left err) _ = Left err
    accum (Right ofst1) pt = byteLengthAt pt ofst1
byteLengthAt (PutFail ferr) ofs = Left $ ferr ofs

x0080 :: Int
x0080   = 128
x07ff :: Int
x07ff   = 2047
x0800 :: Int
x0800   = 2048
xffff :: Int
xffff   = 65535
x0f :: Int
x0f     = 15
x1f :: Int
x1f     = 31
x3f :: Int
x3f     = 63
x7f :: Int
x7f     = 127
x80 :: Int
x80     = 128
xc0 :: Int
xc0     = 192
xe0 :: Int
xe0     = 224

isChar1Range :: Int -> Boolean
isChar1Range c = c > 0 && c <= x7f

isChar2Range :: Int -> Boolean
isChar2Range c = c >= x0080 && c <= x07ff

isChar3Range :: Int -> Boolean
isChar3Range c = c >= x0800 && c <= xffff

charSize :: Char -> Either String ByteLength
charSize ch =
  case toCharCode ch of
    0                  -> Right 2
    c | isChar1Range c -> Right 1
    c | isChar2Range c -> Right 2
    c | isChar3Range c -> Right 3
    _ -> Left $ "Invalidad char " <> (show ch)

charBytes :: forall m. MonadEffect m => Char -> m (Array Int)
charBytes ch =
  case toCharCode ch of
    0 ->
      pure [ 0, 0]
    c | isChar1Range c ->
      pure [c]
    c | isChar2Range c ->
      pure [xc0 .|. (shr c 6),
            x80 .|. (x3f .&. c) ]
    c | isChar3Range c ->
      pure [  xe0 .|. (x0f .&. (shr c 12)),
              x80 .|. (x3f .&. (shr c 6)),
              x80 .|. (x3f .&. c) ]
    _ ->
      liftEffect $ throw $ "Invalid char " <> (show ch)

mkEmptyBuffer :: forall m. MonadEffect m => Put -> m ArrayBuffer
mkEmptyBuffer put = do
  len <- case byteLengthAt put 0 of
          (Left err) -> liftEffect $ throw err
          (Right len) -> pure len
  liftEffect $ AB.create len

writePut :: forall m. MonadEffect m => Put -> DataView -> ByteOffset -> m Int
writePut (PutFail err) _ ofs = liftEffect $ throw $ err ofs
writePut put @ (PutInt8 i) dv ofs = do
  _ <- liftEffect $ setInt8 dv i ofs
  pure (ofs + 1)
writePut put @ (PutUInt8 i) dv ofs = do
  _ <- liftEffect $ setUint8 dv i ofs
  pure (ofs + 1)
writePut put @ (PutUInt16 i) dv ofs = do
  _ <- liftEffect $ setUint16be dv i ofs
  pure (ofs + 2)
writePut put @ (PutUInt32 i) dv ofs = do
  _ <- liftEffect $ setUint32be dv i ofs
  pure (ofs + 4)
writePut put @ (PutFloat32 i) dv ofs = do
  _ <- liftEffect $ setFloat32be dv i ofs
  pure (ofs + 4)
writePut put @ (PutFloat64 i) dv ofs = do
  _ <- liftEffect $ setFloat64be dv i ofs
  pure (ofs + 8)
writePut put @ (PutChar ch) dv ofs = do
  chs <- charBytes ch
  writePut (PutArray (int8Put <$> chs)) dv ofs
writePut (PutWithOffset f) dv ofs =
  writePut (f ofs) dv ofs
writePut (PutArray arr) dv ofs =
  A.foldl accum (pure ofs) arr
  where
    accum ::  m Int -> Put -> m Int
    accum posEff put = do
      pos <- posEff
      writePut put dv pos

runPut :: forall m. MonadEffect m => Put -> m ArrayBuffer
runPut put = do
  buf <- mkEmptyBuffer put
  let dv = DV.whole buf
  _ <- writePut put dv 0
  pure buf

putToDataView :: Put -> DataView
putToDataView put = unsafePerformEffect $ DV.whole <$> runPut put

putToString :: Put -> Maybe String
putToString =  putToDataView >>> asInt8Array >>> toIntArray >>> toString
  where
    toString arr = fromCharArray <$> (traverse fromCharCode arr)

putToInt8Array :: Put -> Array Int
putToInt8Array =  putToDataView >>> asUint8Array >>> toIntArray