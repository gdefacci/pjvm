module Data.Binary.DecoderTest (spec) where

import Prelude

import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Binary.Binary (get, put, class Binary)
import Data.Binary.Decoder (Decoder, decode, decodeBuffer, decodeFull, getString)
import Data.Binary.Put (runPut)
import Data.Binary.PutTest (bin)
import Data.Binary.Types (Word16(..), Word32(..), Word64(..), Word8(..))
import Data.Either (fromRight, isRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Unit.Assert as Assert

spec :: Aff Unit
spec = do
  testGetString
  testData
  testData1
  testRec
  testReadWrite "a simple string"
  testReadWrite 'c'
  testReadWrite '%'
  testReadWrite $ Word8  $ fromInt 42
  testReadWrite $ Word16 $ fromInt 42
  testReadWrite $ Word32 $ fromInt 42
  testReadWrite $ Word64 (fromInt $ 42) (fromInt $ 442)

  testGetEquals "ù"  [bin "0", bin "10", bin "11000011", bin "10111001"] "ù"
  testGetEquals "à"  [bin "0", bin "10", bin "11000011", bin "10100000"] "à"
  testGetEquals "è"  [bin "0", bin "10", bin "11000011", bin "10101000"] "è"
  testGetEquals "é"  [bin "0", bin "10", bin "11000011", bin "10101001"] "é"
  testGetEquals "д"  [bin "0", bin "10", bin "11010000", bin "10110100"] "д"
  testGetEquals "и"  [bin "0", bin "10", bin "11010000", bin "10111000"] "и"
  testGetEquals "й"  [bin "0", bin "10", bin "11010000", bin "10111001"] "й"
  testGetEquals "电" [bin "0", bin "11", bin "11100111", bin "10010100", bin "10110101"] "电"
  testGetEquals "脑" [bin "0", bin "11", bin "11101000", bin "10000100", bin "10010001"] "脑"

  testReadWrite "ù"
  testReadWrite "a i18 string èàù"

testGetString :: Aff Unit
testGetString = do
  let ab = AB.fromIntArray [0, 5, 100, 101, 102, 103, 104]
  (Tuple {offset:ofst} str) <- liftEffect $ decodeBuffer getString ab
  Assert.assert "string must be correct" $ str == "defgh"
  Assert.assert "must parse all the string" $ ofst == 7

arrayBuffer1 :: ArrayBuffer
arrayBuffer1 = AB.fromIntArray [  0, 0, 0, 80,
                                  0, 3, 98, 99, 97,
                                  0, 0, 0, 0, 0, 0, 0, 100,
                                  0, 200]

expectedMyType1 :: MyType
expectedMyType1 = MyType (Word32 $ fromInt 80) "bca" (Word64 (fromInt 0) (fromInt 100)) (Word16 $ fromInt 200)

testData :: Aff Unit
testData = do
  res <- liftEffect $ decodeFull myTypeDecoderAp arrayBuffer1
  Assert.assert "can parse data (applicative)" $ res == expectedMyType1

testData1 :: Aff Unit
testData1 = do
  res <- liftEffect $ decodeFull myTypeDecoder arrayBuffer1
  Assert.assert "can parse data" $ res == expectedMyType1

testReadWrite :: forall a. Binary a => Show a => Eq a => a -> Aff Unit
testReadWrite a = do
  buff <- runPut $ put a
  eithr <- decode get buff
  Assert.assert ("can read written data " <> show a) $ isRight eithr
  let (Tuple {offset} res) = unsafePartial $ fromRight eithr
  when (a /= res) do
    log $ ""
    log $ "input  :" <> (show a)
    log $ ""
    log $ "output :" <> (show res)
  Assert.assert ("putting and getting gives back the original input " <> show a) $ a == res
  Assert.assert ("get consume all ther buffer " <> show a) $ offset == AB.byteLength buff

testRec :: Aff Unit
testRec = do
  let expectedMyRec = MyRec
        { a : Word32 $ fromInt 80
        , b : "bca"
        , c : Word64 (fromInt 0) (fromInt 100)
        , d : Word16 $ fromInt 200
        }
  res <- liftEffect $ decodeFull myRecDecoder arrayBuffer1
  Assert.assert "can parse record" $ res == expectedMyRec

type Description = String

testGetEquals :: forall a. Eq a => Show a => Binary a => Description -> (Array Int) -> a  -> Aff Unit
testGetEquals desc arr expected = do
  let buf = AB.fromIntArray arr
  (Tuple {offset} re) <- decodeBuffer get buf
  Assert.assert (" didnt parse all input while decoding " <> show arr) $ offset == (AB.byteLength buf)
  Assert.assert (desc <> ", but the result is " <> (show re)) $ expected == re

-------------------------------

data MyType = MyType Word32 String Word64 Word16

newtype MyRec = MyRec
  { a :: Word32
  , b :: String
  , c :: Word64
  , d :: Word16
  }

myTypeDecoderAp :: forall m. Monad m => Decoder m MyType
myTypeDecoderAp = MyType <$> get <*> get <*> get <*> get

myTypeDecoder :: forall m. Monad m => Decoder m MyType
myTypeDecoder = do
  a <- get
  b <- get
  c <- get
  d <- get
  pure $ MyType a b c d

derive instance eqMyType :: Eq MyType

instance myTypeShow :: Show MyType where
  show (MyType a b c d) = "(MyType " <> (show a) <> " " <> (show b) <> " " <> (show c) <> " " <> (show d) <> ")"

derive instance genericMyRec :: Generic MyRec _

instance eqMyRec :: Eq MyRec where
  eq = genericEq

instance showMyRec :: Show MyRec where
  show = genericShow

myRecDecoder :: forall m. Monad m => Decoder m MyRec
myRecDecoder = MyRec <$> ({a : _, b: _,  c:_, d:_ } <$> get <*> get <*> get <*> get)