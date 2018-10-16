module Data.Binary.Decoder where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, ByteOffset)
import Data.Char (fromCharCode)
import Data.Char as CH
import Data.Either (Either(..))
import Data.Int.Bits (shl, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt, toInt)
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Exception.Unsafe (unsafeThrow)

newtype Decoder a = Decoder (DV.Getter (Tuple Int a))

decodeBuffer :: forall a. Decoder a -> ArrayBuffer -> Effect (Maybe (Tuple Int a))
decodeBuffer (Decoder getter) buf =
  getter (DV.whole buf) 0

decodeFull :: forall a. Decoder a -> ArrayBuffer -> Effect (Maybe a)
decodeFull dec buf =
  let len = AB.byteLength buf
  in (checkConsume len) =<< decodeBuffer dec buf
  where
    checkConsume _ Nothing                         = pure Nothing
    checkConsume len (Just (Tuple n r)) | n == len = pure $ Just r
    checkConsume len (Just (Tuple n _))            = throw $  "Expecting " <> (show len) <>
                                                              " bytes, got " <> (show n)

runDecoder :: forall a. Decoder a -> DataView -> ByteOffset -> Effect (Maybe (Tuple Int a))
runDecoder (Decoder dec) dv ofs = dec dv ofs

derive instance decoderFunctor :: Functor Decoder

instance decoderApply :: Apply Decoder where
  apply ff fa = do
    f <- ff
    a <- fa
    pure $ f a

instance decoderApplicative :: Applicative Decoder where
  pure a = Decoder $ \_ -> \ofs -> pure $ pure $ (Tuple ofs a)

instance decoderBind :: Bind Decoder where
  bind (Decoder fa) f =
    Decoder $ \dv -> \ofs -> do
      optTup1 <- fa dv ofs
      case optTup1 of
        Nothing -> pure $ Nothing
        (Just (Tuple ofst1 a)) ->
          let (Decoder getter') = f a
          in getter' dv ofst1

sized :: forall a. Int -> DV.Getter a -> Decoder a
sized n f = Decoder (\dv -> \ofs -> ((Tuple (ofs+n)) <$> _) <$> (f dv ofs) )

getUInt8 :: Decoder UInt
getUInt8 = sized 1 $ DV.getUint8

getUInt16 :: Decoder UInt
getUInt16 = sized 2 $ DV.getUint16be

getUInt32 :: Decoder UInt
getUInt32 = sized 4 $ DV.getUint32be


{- getW8 :: Decoder Word8
getW8 = sized 1 $ \dv -> \bo -> (Word8 <$> _) <$> DV.getUint8 dv bo

getW16 :: Decoder Word16
getW16 = sized 2 $ \dv -> \bo -> (Word16 <$> _) <$> DV.getUint16be dv bo

getW32 :: Decoder Word32
getW32 = sized 4 $ \dv -> \bo -> (Word32 <$> _) <$> DV.getUint32be dv bo 

getW64 :: Decoder Word64
getW64 = sized 8 $ \dv -> \bo -> do
  ow1 <- DV.getUint32be dv bo
  ow2 <- DV.getUint32be dv (bo+4)
  pure $ do
    w1 <- ow1
    w2 <- ow2
    pure $ Word64 w1 w2
    -}

getFloat32 :: Decoder Number
getFloat32 = sized 4 $ DV.getFloat32be

getFloat64 :: Decoder Number
getFloat64 = sized 8 $ DV.getFloat64be

fail :: forall a. String -> Decoder a
fail msg = Decoder $ \dv -> \ofst -> throw $ "Error at " <> (show ofst) <> ". " <> msg

getN :: forall a. Int -> Decoder a -> Decoder (Array a)
getN 0 _ = pure []
getN 1 decoder = A.singleton <$> decoder
getN n decoder = do
  hd <- decoder
  rest <- getN (n - 1) decoder
  pure $ A.cons hd rest

getString :: Decoder String
getString = do
  len <- getUInt16
  res <- getN (toInt len) getChar
  pure $ fromCharArray res


{- getAll :: forall a. Decoder a -> Decoder (Array a)
getAll (Decoder decoder) = 
  Decoder go
  where
    go dv ofs =
      getRemaining =<< (decoder dv ofs)
      where
        getRemaining Nothing = pure $ Just $ (Tuple ofs mempty)
        getRemaining (Just (Tuple ofst1 a)) = do
          rest <- go dv ofst1
          pure $ Just $ case rest of
            Nothing -> Tuple ofst1 $ singleton a
            (Just (Tuple ofst2 rst)) -> Tuple ofst2 $ cons a rst

consumeAll :: forall a. Decoder a -> Decoder a
consumeAll (Decoder decoder) = 
  Decoder $ \dv -> \ofs -> 
    (checkConsumeAll dv ofs) =<< decoder dv ofs 
    where
      checkConsumeAll dv ofs r @ (Just (Tuple finOfst _)) | finOfst == (ofs + (DV.byteLength dv)) = pure r
      checkConsumeAll _ _ _ = pure Nothing -}

{- 
getString :: Decoder String
getString = Decoder $ \dv -> \ofst -> do
    optTup <- runDecoder getUInt16 dv ofst
    case optTup of
      Nothing -> throw "Not enough bytes"
      (Just (Tuple ofst1 len)) ->
        replicateM len
        
        case DV.slice ofst1 (toInt len) (DV.buffer dv) of
          Nothing -> throw $ "Not enough bytes, required " <> (show (toInt len)) <> " bytes"
          (Just ndv) -> 
            adaptResult <$> (runDecoder (consumeAll $ getAll getChar) ndv 0)
            where
              adaptResult Nothing -> throw "Not enough bytes"
              adaptResult (Just (Tuple ofst res)) -> pure $ (Tuple ofst)
 -}


    -- case (\(Tuple ofst1 len) -> DV.slice ofst1 (toInt len) (DV.buffer dv)) =<< optTup of
    --   Nothing -> fail "Not enough bytes"
    --   (Just ndv) -> consumeAll $ getAll getChar

{-   Decoder $ \dv -> \bo -> (getStringOpt dv bo) =<< (DV.getUint16be dv bo)
  where
    getStringOpt _ _ Nothing = pure $ Nothing
    getStringOpt dv ofst1 (Just ulen) = getStringValue dv ofst1 ulen

    getStringValue dv ofst1 ulen = do
      bufSlice <- AB.slice startOfst endOfst (DV.buffer dv)
      toResult $ AB.decodeToString bufSlice
      where
        startOfst = ofst1 + 2
        endOfst = startOfst + (toInt ulen)
        toResult (Right r) = pure $ Just $ Tuple endOfst r
        toResult (Left err) = throw $ show err
 -}
getChar8 :: Decoder Char
getChar8 = do
  x <- getUInt8
  case CH.fromCharCode (toInt x) of
    Nothing -> fail $ "Invalid char code" <> (show x)
    (Just ch) -> pure ch

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
