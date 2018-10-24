module Data.Binary.Decoder where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (State, runState)
import Control.Monad.State as ST
import Data.Array as A
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, DataView)
import Data.Char (fromCharCode)
import Data.Char as CH
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int.Bits (shl, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), snd)
import Data.UInt (UInt, toInt)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

data ParserError =  NotEnoughBytes { offset::Int, required::Int, avaiable::Int}
                    | UnrecognizedChar { offset::Int, charCode :: UInt }
                    | InputPartiallyParsed { offset::Int }
                    | GenericParserError { offset::Int, message::String }

type ParserState =
  { dataview :: DataView
  , offset :: ByteOffset
  }

derive instance genericParserError :: Generic ParserError _
instance showParserError :: Show ParserError where
  show = genericShow

type Decoder a = ExceptT ParserError (State ParserState) a

decode :: forall err a. ExceptT err (State ParserState) a -> ArrayBuffer -> Either err (Tuple ParserState a)
decode decoder buf =
  toReult $ runDecoder decoder 0 (DV.whole buf)
  where
    toReult (Tuple (Left err) s) = Left err
    toReult (Tuple (Right v) s) = Right $ Tuple s v

decodeBuffer :: forall a m. MonadEffect m => ExceptT ParserError (State ParserState) a -> ArrayBuffer -> m (Tuple ParserState a)
decodeBuffer decoder arr =
  case decode (consumeAllInput decoder) arr of
    (Left err) -> liftEffect $ throw (show err)
    (Right v) -> liftEffect $ pure v

decodeFull :: forall a m. MonadEffect m => ExceptT ParserError (State ParserState) a -> ArrayBuffer -> m a
decodeFull decoder arr = snd <$> (decodeBuffer decoder arr)

runDecoder :: forall a err. ExceptT err (State ParserState) a
                            -> ByteOffset
                            -> DataView
                            -> Tuple (Either err a) ParserState
runDecoder dec offset dataview = runState (runExceptT dec) {offset, dataview}

getOffset :: forall m. ST.MonadState ParserState m  => m ByteOffset
getOffset = do
  {offset} <- ST.get
  pure offset

getDataView :: forall m. ST.MonadState ParserState m  => m DataView
getDataView = do
  {dataview} <- ST.get
  pure dataview

fail :: forall a m. MonadThrow ParserError m
                    => ST.MonadState ParserState m
                    => (ByteOffset -> ParserError)
                    -> m a
fail err = do
  ofs <- getOffset
  throwError $ err ofs

setOffset :: forall m. ST.MonadState ParserState m => ByteOffset -> m Unit
setOffset offset = do
  dataview <- getDataView
  ST.put {offset, dataview}

setDataView :: forall m. ST.MonadState ParserState m => DataView -> m Unit
setDataView dataview = do
  offset <- getOffset
  ST.put {dataview, offset}

getDataViewByteLenght :: forall m. ST.MonadState ParserState m => m Int
getDataViewByteLenght = DV.byteLength <$> getDataView

hasMoreBytes :: forall m. ST.MonadState ParserState m => m Boolean
hasMoreBytes = do
  ofs <- getOffset
  tot <- getDataViewByteLenght
  pure $ (tot - ofs) > 0

lookAhead :: forall a m. ST.MonadState ParserState m =>  m a -> m a
lookAhead decoder = do
  ofs <- getOffset
  r <- decoder
  setOffset ofs
  pure $ r

withSlice :: forall a m. ST.MonadState ParserState m => MonadThrow ParserError m => Int -> m a -> m a
withSlice sliceLength decoder = do
  {offset:ofs, dataview:dv} <- ST.get
  case DV.slice ofs sliceLength (DV.buffer dv) of
    Nothing -> throwError $ NotEnoughBytes { offset: ofs, avaiable: ((DV.byteLength dv) - ofs), required: sliceLength }
    (Just sliceDV) -> do
      setDataView sliceDV
      setOffset 0
      r <- decoder
      ST.put {offset : ofs + sliceLength, dataview : dv}
      pure $ r

sized :: forall a m. ST.MonadState ParserState m
                      => MonadThrow ParserError m
                      => Int
                      -> DV.Getter a
                      -> m a
sized n f = do
  {offset:ofs, dataview:dv} <- ST.get
  case unsafePerformEffect $ f dv ofs of
    Nothing -> throwError $ NotEnoughBytes { offset:ofs, avaiable: ((DV.byteLength dv) - ofs), required:n }
    (Just v) -> do
      setOffset $ ofs + n
      pure v

getUInt8 :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m UInt
getUInt8 = sized 1 $ DV.getUint8

getUInt16 :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m UInt
getUInt16 = sized 2 $ DV.getUint16be

getUInt32 :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m UInt
getUInt32 = sized 4 $ DV.getUint32be

getFloat32 :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m Number
getFloat32 = sized 4 $ DV.getFloat32be

getFloat64 :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m Number
getFloat64 = sized 8 $ DV.getFloat64be

getRest :: forall a m. ST.MonadState ParserState m => MonadThrow ParserError m => m a -> m (Array a)
getRest decoder = do
  more <- hasMoreBytes
  if more
    then do
      hd <- decoder
      rest <- getRest decoder
      pure $ A.cons hd rest
    else
      pure []

consumeAllInput :: forall a m. ST.MonadState ParserState m => MonadThrow ParserError m => m a -> m a
consumeAllInput decoder = do
  res <- decoder
  more <- hasMoreBytes
  if more
    then fail $ \offset -> InputPartiallyParsed {offset}
    else pure res

getString :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m String
getString = (\(ByteLengthString _ txt) -> txt) <$> getByteLengthString

data ByteLengthString = ByteLengthString Int String

getByteLengthString :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m ByteLengthString
getByteLengthString = do
  len <- toInt <$> getUInt16
  (ByteLengthString len) <$> fromCharArray <$> (withSlice len $ getRest getChar)

getChar8 :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m Char
getChar8 = do
  x <- getUInt8
  case CH.fromCharCode (toInt x) of
    Nothing -> fail $ \offset -> UnrecognizedChar { offset, charCode: x }
    (Just ch) -> pure ch

getRep :: forall a m. ST.MonadState ParserState m => MonadThrow ParserError m => Int -> m a -> m (Array a)
getRep 0 _ = pure []
getRep n decoder = do
  r1 <- decoder
  rest <- getRep (n - 1) decoder
  pure $ A.cons r1 rest

skip ::  forall m. ST.MonadState ParserState m => MonadThrow ParserError m => Int -> m Unit
skip n = do
  ofs <- getOffset
  setOffset $ ofs + n

getChar :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m Char
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
      Nothing -> fail $ \offset -> UnrecognizedChar { offset, charCode: uchr8 }
      (Just ch) -> pure ch
