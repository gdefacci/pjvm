module Data.Binary.Decoder where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, ByteOffset)
import Data.Char (fromCharCode)
import Data.Char as CH
import Data.Int.Bits (shl, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), snd)
import Data.UInt (UInt, toInt)
import Effect (Effect)
import Effect.Exception (throw)

newtype Decoder a = Decoder (DataView -> ByteOffset -> Effect (Tuple Int a))

decodeBuffer :: forall a. Decoder a -> ArrayBuffer -> Effect (Tuple Int a)
decodeBuffer (Decoder getter) buf =
  getter (DV.whole buf) 0

decodeFull :: forall a. Decoder a -> ArrayBuffer -> Effect a
decodeFull decoder arr = snd <$> (decodeBuffer (consumeAllInput decoder) arr)

lookAhead :: forall a. Decoder a -> Decoder a
lookAhead (Decoder decoder) =
  Decoder $ \dv -> \ofs -> do
    (Tuple _ r) <- decoder dv ofs
    pure $ Tuple ofs r

runDecoder :: forall a. Decoder a -> DataView -> ByteOffset -> Effect (Tuple Int a)
runDecoder (Decoder dec) dv ofs = dec dv ofs

derive instance decoderFunctor :: Functor Decoder

instance decoderApply :: Apply Decoder where
  apply ff fa = do
    f <- ff
    a <- fa
    pure $ f a

instance decoderApplicative :: Applicative Decoder where
  pure a = Decoder $ \_ -> \ofs -> pure $ (Tuple ofs a)

instance decoderBind :: Bind Decoder where
  bind (Decoder fa) f =
    Decoder $ \dv -> \ofs -> do
      (Tuple ofst1 a) <- fa dv ofs
      let (Decoder getter') = f a
      getter' dv ofst1

instance decoderMonad :: Monad Decoder

getByteOffset :: Decoder Int
getByteOffset = Decoder $ \_ -> \ofs -> pure $ Tuple ofs ofs

getDataViewByteLenght :: Decoder Int
getDataViewByteLenght = Decoder $ \dv -> \ofs -> pure $ Tuple ofs (DV.byteLength dv)

getRemainingBytesLength :: Decoder Int
getRemainingBytesLength = do
  ofs <- getByteOffset
  tot <- getDataViewByteLenght
  pure $ tot - ofs

hasMoreBytes :: Decoder Boolean
hasMoreBytes = do
  rb <- getRemainingBytesLength
  pure $ rb > 0

withSlice :: forall a. Int -> Decoder a -> Decoder a
withSlice sliceLength (Decoder decoder) =
  Decoder $ \dv -> \ofs ->
    case DV.slice ofs sliceLength (DV.buffer dv) of
      Nothing -> throw $  "Not enough bytes, avaiable " <> show ((DV.byteLength dv) - ofs) <>
                          "required " <> (show sliceLength)
      (Just sliceDv) -> do
        (Tuple len r) <- decoder sliceDv 0
        pure $ Tuple (ofs + len) r

sized :: forall a. Int -> DV.Getter a -> Decoder a
sized n f = Decoder $ \dv -> \ofs ->
  (toResult ofs) =<< f dv ofs
  where
    toResult _ Nothing = throw "EOF"
    toResult ofs (Just v) = pure $ Tuple (ofs+n) v

getUInt8 :: Decoder UInt
getUInt8 = sized 1 $ DV.getUint8

getUInt16 :: Decoder UInt
getUInt16 = sized 2 $ DV.getUint16be

getUInt32 :: Decoder UInt
getUInt32 = sized 4 $ DV.getUint32be

getFloat32 :: Decoder Number
getFloat32 = sized 4 $ DV.getFloat32be

getFloat64 :: Decoder Number
getFloat64 = sized 8 $ DV.getFloat64be

fail :: forall a. String -> Decoder a
fail msg = Decoder $ \dv -> \ofst -> throw $ "Error at " <> (show ofst) <> ". " <> msg

getRest :: forall a. Decoder a -> Decoder (Array a)
getRest decoder = do
  more <- hasMoreBytes
  if more
    then do
      hd <- decoder
      rest <- getRest decoder
      pure $ A.cons hd rest
    else
      pure []

consumeAllInput :: forall a. Decoder a -> Decoder a
consumeAllInput decoder = do
  res <- decoder
  more <- hasMoreBytes
  if more
    then fail "Expecting to consume the input completelly"
    else pure res

getString :: Decoder String
getString = (\(ByteLengthString _ txt) -> txt) <$> getByteLengthString

data ByteLengthString = ByteLengthString Int String

getByteLengthString :: Decoder ByteLengthString
getByteLengthString = do
  len <- toInt <$> getUInt16
  (ByteLengthString len) <$> fromCharArray <$> (withSlice len $ getRest getChar)

getChar8 :: Decoder Char
getChar8 = do
  x <- getUInt8
  case CH.fromCharCode (toInt x) of
    Nothing -> fail $ "Invalid char code" <> (show x)
    (Just ch) -> pure ch

getRep :: forall a. Int -> Decoder a -> Decoder (Array a)
getRep 0 _ = pure []
getRep n decoder = do
  r1 <- decoder
  rest <- getRep (n - 1) decoder
  pure $ A.cons r1 rest

skip :: Int -> Decoder Unit
skip n = void $ getRep n getUInt8

getChar :: Decoder Char
getChar =
  let x0f     = 15
      x1f     = 31
      x3f     = 63
      bit :: Int -> Int
      bit i = 1 `shl` i
      testBit :: Boolean -> Int -> Int -> Boolean
      testBit expectedValue x i =
        let bt = x .&. bit i
        in if expectedValue then bt /= 0 else bt /= 1

      is0xxxxxxx b = testBit false b 7

      is110xxxxx b = testBit true b 7 &&
                      testBit true b 6 &&
                      testBit false b 5

      is1110xxxx b = testBit true b 7 &&
                      testBit true b 6 &&
                      testBit true b 5 &&
                      testBit false b 4

  in do
    uchr8 <- getUInt8
    resCharCode <- case toInt uchr8 of
      a | is0xxxxxxx a -> pure $ Just a
      a | is110xxxxx a -> do
        uchrb <- getUInt8
        let b = toInt uchrb
        pure $ Just $ (shl 6 (a .&. x1f)) .|. (b .&. x3f)
      a | is1110xxxx a -> do
        uchrb <- getUInt8
        uchrc <- getUInt8
        let b = toInt uchrb
            c = toInt uchrc
        pure $ Just $ (shl 12 (a .&. x0f)) .|. (shl 6 (b .&. x3f)) .|. (c .&. x3f)
      _ -> pure Nothing
    case fromCharCode =<< resCharCode of
      Nothing -> fail $ "cant convert code " <> (show uchr8) <> "to char "
      (Just ch) -> pure ch
