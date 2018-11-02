module Data.Binary.Decoder where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State as ST
import Data.Array as A
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, DataView)
import Data.Char (fromCharCode)
import Data.Char as CH
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), snd)
import Data.UInt (UInt, fromInt, shl, toInt, (.&.), (.|.))
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

type Decoder m a = ExceptT ParserError (StateT ParserState m) a

decode :: forall err m a. Functor m => ExceptT err (StateT ParserState m) a -> ArrayBuffer -> m (Either err (Tuple ParserState a))
decode decoder buf =
  toReult <$> (runDecoder decoder 0 (DV.whole buf))
  where
    toReult (Tuple (Left err) s) = Left err
    toReult (Tuple (Right v) s)  = Right $ Tuple s v
--
decodeBuffer :: forall a m. MonadEffect m => ExceptT ParserError (StateT ParserState m) a -> ArrayBuffer -> m (Tuple ParserState a)
decodeBuffer decoder arr =
  toResult =<< (decode (consumeAllInput decoder) arr)
  where
    toResult ::  Either ParserError (Tuple ParserState a) -> m (Tuple ParserState a)
    toResult (Left err) = liftEffect $ throw (show err)
    toResult (Right v)  = liftEffect $ pure v


--decodeFull :: forall a m. MonadEffect m => ExceptT ParserError (State ParserState) a -> ArrayBuffer -> m a
decodeFull :: forall m a. Functor m => MonadEffect m => ExceptT ParserError (StateT ParserState m) a -> ArrayBuffer -> m a
decodeFull decoder arr = snd <$> (decodeBuffer decoder arr)

-- runDecoder :: forall err m a. ExceptT err (StateT ParserState m) a
--                             -> ByteOffset
--                             -> DataView
--                             -> Tuple (Either err a) ParserState
runDecoder :: forall m a err. ExceptT err (StateT ParserState m) a -> ByteOffset -> DataView -> m (Tuple (Either err a) ParserState)
runDecoder dec offset dataview = runStateT (runExceptT dec) {offset, dataview}

-- runDecoder1 :: forall a err. ExceptT err (State ParserState) a
--                             -> ByteOffset
--                             -> DataView
--                             -> Tuple (Either err a) ParserState
-- runDecoder1 dec offset dataview = runTrampoline $ runDecoder dec offset dataview


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

getInt8 :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m Int
getInt8 = sized 1 $ DV.getInt8

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

x00 :: UInt
x00     = fromInt 0
x0f :: UInt
x0f     = fromInt 15
x1f :: UInt
x1f     = fromInt 31
x3f :: UInt
x3f     = fromInt 63
x80 :: UInt
x80     = fromInt 128
xc0 :: UInt
xc0     = fromInt 192
xe0 :: UInt
xe0     = fromInt 224
xf0 :: UInt
xf0     = fromInt 240

getChar :: forall m. ST.MonadState ParserState m => MonadThrow ParserError m => m Char
getChar = do
  uchr8 <- getUInt8
  let unrecognizedChar offset = UnrecognizedChar { offset, charCode: uchr8 }
  resCharCode <- case uchr8 of
    a | a .&. x80 == x00 -> pure a
    a | a .&. xe0 == xc0 -> do
      b <- getUInt8
      pure $ (shl (a .&. x1f) $ fromInt 6) .|. (b .&. x3f)
    a | a .&. xf0 == xe0 -> do
      b <- getUInt8
      c <- getUInt8
      pure $ (shl (a .&. x0f) $ fromInt 12 ) .|. (shl (b .&. x3f) $ fromInt 6) .|. (c .&. x3f)
    _ -> fail unrecognizedChar
  case fromCharCode $ toInt resCharCode of
    (Just ch) -> pure ch
    _ -> fail unrecognizedChar


