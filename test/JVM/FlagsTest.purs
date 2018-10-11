module JVM.FlagsTest where

import Prelude
import Effect (Effect)
import Data.Set as S
import JVM.Flags
import Data.BitMask

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

setAndGet :: forall a. BitMask a => Ord a => (Array a) -> Boolean
setAndGet flsArr =
  let fls = (S.fromFoldable flsArr)
      mv = maskValue fls
      msk = toMask mv
  in msk == fls

spec :: Effect Unit
spec = do
  quickCheck (setAndGet :: Array AccessFlag -> Boolean)
  quickCheck (setAndGet :: Array MethodAccessFlag -> Boolean)
  quickCheck (setAndGet :: Array FieldAccessFlag -> Boolean)