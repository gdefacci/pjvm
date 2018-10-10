module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

import Data.Binary.Types
import Data.Binary.Decoder
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.Maybe
import Data.Tuple (snd)
import Data.UInt (fromInt)

data MyType = MyType Word32 String Word64 Word16

type MyRec = {
  a :: Word32,
  b :: String,
  c :: Word64
}

myTypeDecoder = MyType <$> w32 <*> wString <*> w64 <*> w16

myTypeDecoder1 = do
  a <- w32
  b <- wString
  c <- w64
  d <- w16
  pure $ MyType a b c d

derive instance eqMyType :: Eq MyType

instance myTypeShow :: Show MyType where
  show (MyType a b c d) = "(MyType " <> (show a) <> " " <> (show b) <> " " <> (show c) <> " " <> (show d) <> ")"

combr :: Decoder MyRec
combr = {a : _, b: _,  c:_ } <$> w32 <*> wString <*> w64

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
