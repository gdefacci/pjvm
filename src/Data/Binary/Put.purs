module Data.Binary.Put (Put, charPut, fromSetter, putN, putFail, runPut, uint8Put, putToString, putToDataView, putToInt8Array, putChar8) where

import Prelude

import Data.Array (fold, length)
import Data.ArrayBuffer.ArrayBuffer (create)
import Data.ArrayBuffer.DataView (Setter, setInt8, setUint8, whole)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asInt8Array, asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, DataView)
import Data.Char (fromCharCode, toCharCode)
import Data.Char as CH
import Data.Function (applyN)
import Data.Int.Bits (shl, shr, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt, fromInt)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

newtype Put = Put ( ByteOffset -> { newOffset::Int,  effect::DataView -> Effect Unit } )

instance semigroupPut :: Semigroup Put where
  append (Put f1) (Put f2) = Put $ \ofs ->
    let { newOffset : ofst1, effect : efct1 } = f1 ofs
        { newOffset, effect : efct2 } = f2 ofst1
        effect dv = (efct1 dv) *> (efct2 dv)
    in {newOffset, effect}

instance monoidPut :: Monoid Put where
  mempty = Put $ \ofst -> { newOffset : ofst, effect : \_ -> pure unit }

fromSetter :: forall a. Int -> Setter a -> a -> Put
fromSetter sz setter a = Put $ \ofst -> { newOffset : ofst + sz, effect : \dv -> setter dv a ofst }

putFail :: (ByteOffset -> String) -> Put
putFail msg = Put $ \ofst -> { newOffset : ofst + 0, effect : \_ -> throw $ msg ofst }

runPut :: forall m. MonadEffect m => Put -> m ArrayBuffer
runPut (Put f) = do
  let {newOffset : totLen, effect } = f 0
  buff <- liftEffect $ create totLen
  let dv = whole buff
  wlen <- liftEffect $ effect dv
  pure $ buff

putN :: forall a. (a -> Put) -> (ByteOffset -> ByteOffset) -> a -> Put
putN enc f w =
  Put $ \ofs ->
    let count = f ofs
        accum { newOffset : ofst1, effect: eff1 } =
          let (Put fa) = enc w
              { newOffset : ofst2, effect: eff2 } = fa ofst1
          in { newOffset : ofst2, effect: \dv -> (eff1 dv) *> (eff2 dv) }
    in applyN accum count { newOffset : ofs, effect: \_ -> pure unit }

charBytes :: Char -> Maybe (Array Int)
charBytes ch =
  let x0080   = 128
      x07ff   = 2047
      x0800   = 2048
      xffff   = 65535
      x0f     = 15
      x1f     = 31
      x3f     = 63
      x7f     = 127
      x80     = 128
      xc0     = 192
      xe0     = 224
  in case toCharCode ch of
    0 ->
      Just [ 0, 0]
    c | c > 0 && c <= x7f ->
      Just [c]
    c | c >= x0080 && c <= x07ff ->
      Just [xc0 .|. (shr c 6),
            x80 .|. (x3f .&. c) ]
      -- Just [  xc0 .|. (x1f .&. (shr c 6)),
      --         x80 .|. (x3f .&. c) ]
    c | c >= x0800 && c <= xffff ->
      Just [  xe0 .|. (x0f .&. (shr c 12)),
              x80 .|. (x3f .&. (shr c 6)),
              x80 .|. (x3f .&. c) ]
    _ -> Nothing

uint8Put :: UInt -> Put
uint8Put = fromSetter 1 setUint8

int8Put :: Int -> Put
int8Put = fromSetter 1 setInt8

putChar8 :: Char -> Put
putChar8 = uint8Put <<< fromInt <<< CH.toCharCode

charPut :: Char -> (Tuple Int Put)
charPut ch =
  case charBytes ch of
    Nothing -> (Tuple 0 $ putFail $ \ofst -> "Cant convert char " <> (show ch) <> " to modified UTF-8, offset" <> (show ofst))
    (Just cbs) -> Tuple (length cbs) (fold $ int8Put <$> cbs)

putToDataView :: Put -> DataView
putToDataView put = unsafePerformEffect $ DV.whole <$> runPut put

putToString :: Put -> Maybe String
putToString =  putToDataView >>> asInt8Array >>> toIntArray >>> toString
  where
    toString arr = fromCharArray <$> (traverse fromCharCode arr)

putToInt8Array :: Put -> Array Int
putToInt8Array =  putToDataView >>> asUint8Array >>> toIntArray