module Data.Binary.Decoder where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Char as CH
import Effect (Effect)

import Effect.Exception (throw)

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.ArrayBuffer as AB

import Data.Binary.Types

import Data.UInt (toInt)

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

getW8 :: Decoder Word8
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

getFloat32 :: Decoder Float32
getFloat32 = sized 4 $ \dv -> \bo -> (Float32 <$> _) <$> DV.getFloat32be dv bo

getFloat64 :: Decoder Float64
getFloat64 = sized 4 $ \dv -> \bo -> (Float64 <$> _) <$> DV.getFloat64be dv bo

fail :: forall a. String -> Decoder a
fail msg = Decoder $ \dv -> \ofst -> throw $ "Error at " <> (show ofst) <> ". " <> msg

getString :: Decoder String
getString =
  Decoder $ \dv -> \bo -> (getStringOpt dv bo) =<< (DV.getUint16be dv bo)
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

getChar8 :: Decoder Char
getChar8 = do
  (Word8 x) <- getW8
  case CH.fromCharCode (toInt x) of
    Nothing -> fail $ "Invalid char code" <> (show x)
    (Just ch) -> pure ch