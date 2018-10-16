module Main where

import Data.Binary.Decoder
import Data.Binary.Types
import Data.Maybe
import Prelude

import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Binary.Binary (get)
import Data.Tuple (snd)
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

data MyType = MyType Word32 String Word64 Word16

type MyRec = {
  a :: Word32,
  b :: String,
  c :: Word64
}

myTypeDecoder ::   Decoder MyType
myTypeDecoder = MyType <$> get <*> get <*> get <*> get

myTypeDecoder1 :: Decoder MyType
myTypeDecoder1 = do
  a <- get
  b <- get
  c <- get
  d <- get
  pure $ MyType a b c d

derive instance eqMyType :: Eq MyType

instance myTypeShow :: Show MyType where
  show (MyType a b c d) = "(MyType " <> (show a) <> " " <> (show b) <> " " <> (show c) <> " " <> (show d) <> ")"

combr :: Decoder MyRec
combr = {a : _, b: _,  c:_ } <$> get <*> get <*> get

main :: Effect Unit
main = do
  let buf = AB.fromIntArray [ 0, 0, 0, 80,
                              0, 3, 98, 99, 97,
                              0, 0, 0, 0, 0, 0, 0, 100,
                              0, 200]
  res <- decodeFull myTypeDecoder buf
  let expected = Just $ MyType (Word32 $ fromInt 80) "bca" (Word64  (fromInt 0) (fromInt 100)) (Word16 $ fromInt 200)
  _ <- if res == expected then pure unit else throw $ "Expecting\n" <> (show expected) <> "     but got\n" <> (show res)
  log (show res)
