module Data.Binary.Put (Put, charPut, fromSetter, putN, putFail, runPut, uint8Put) where

import Prelude

import Data.Array (fold)
import Data.ArrayBuffer.ArrayBuffer (create)
import Data.ArrayBuffer.DataView (Setter, setUint8, whole)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, DataView)
import Data.Char (toCharCode)
import Data.Function (applyN)
import Data.Int.Bits (shr, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.UInt (UInt, fromInt)
import Effect (Effect)
import Effect.Exception (throw)

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

runPut :: Put -> Effect ArrayBuffer
runPut (Put f) = do
  let {newOffset : totLen, effect } = f 0
  buff <- create totLen
  let dv = whole buff
  wlen <- effect dv
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
      Just [  xc0 .|. (x1f .&. (shr c 6)),
              x80 .|. (x3f .&. c) ]
    c | c >= x0800 && c <= xffff ->
      Just [  xe0 .|. (x0f .&. (shr c 12)),
              x80 .|. (x3f .&. (shr c 6)),
              x80 .|. (x3f .&. c) ]
    _ -> Nothing

uint8Put :: UInt -> Put
uint8Put = fromSetter 1 setUint8

charPut :: Char -> Put
charPut ch =
  case charBytes ch of
    Nothing -> putFail $ \ofst -> "Cant convert char " <> (show ch) <> " to modified UTF-8, offset" <> (show ofst)
    (Just cbs) -> fold $ (fromInt >>> uint8Put) <$> cbs


