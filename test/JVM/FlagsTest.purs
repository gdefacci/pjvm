module JVM.FlagsTest where

import Prelude
import Data.Set as S
import Effect.Aff (Aff)

import Data.BitMask (class BitMask, maskValue, toMask)
import JVM.Flags (AccessFlag, FieldAccessFlag, MethodAccessFlag)

import Test.Unit.QuickCheck (quickCheck)

setAndGet :: forall a. BitMask a => Ord a => (Array a) -> Boolean
setAndGet flsArr =
  let fls = (S.fromFoldable flsArr)
      mv = maskValue fls
      msk = toMask mv
  in msk == fls

spec :: Aff Unit
spec = do
  quickCheck (setAndGet :: Array AccessFlag -> Boolean)
  quickCheck (setAndGet :: Array MethodAccessFlag -> Boolean)
  quickCheck (setAndGet :: Array FieldAccessFlag -> Boolean)