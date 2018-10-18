module Data.Binary.DecoderTest (spec) where

import Prelude

import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer, DataView)
import Data.Binary.Binary (get)
import Data.Binary.Decoder (Decoder, decodeBuffer, decodeFull, getString, runDecoder)
import Data.Binary.Types (Word16(..), Word32(..), Word64(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.Unit.Assert as Assert


spec :: Aff Unit
spec = do
  testGetString
  testData
  testData1
  testRec

testGetString :: Aff Unit
testGetString = do
  let ab = AB.fromIntArray [0, 5, 100, 101, 102, 103, 104]
  (Tuple ofst str) <- liftEffect $ decodeBuffer getString ab
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

data MyType = MyType Word32 String Word64 Word16

newtype MyRec = MyRec
  { a :: Word32
  , b :: String
  , c :: Word64
  , d :: Word16
  }

myTypeDecoderAp ::   Decoder MyType
myTypeDecoderAp = MyType <$> get <*> get <*> get <*> get

myTypeDecoder :: Decoder MyType
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

myRecDecoder :: Decoder MyRec
myRecDecoder = MyRec <$> ({a : _, b: _,  c:_, d:_ } <$> get <*> get <*> get <*> get)