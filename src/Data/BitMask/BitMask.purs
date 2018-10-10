module Data.BitMask where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Foldable (indexl)
import Data.Int (pow)
import Data.Int.Bits ((.|.), (.&.), shl)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NE
import Data.Set as S
import Data.Tuple (Tuple(..), snd, fst)
import Test.QuickCheck.Gen (Gen, elements)

class BitMask a where
  all :: NonEmpty Array a
  maskBit :: a -> Int

toNonEmptyArray :: forall a. NonEmpty Array a -> NEA.NonEmptyArray a
toNonEmptyArray naa = NEA.cons' (NE.head naa) (NE.tail naa)

toNonEmpty :: forall a. NEA.NonEmptyArray a -> NE.NonEmpty Array a
toNonEmpty nea = NonEmpty (NEA.head nea) (NEA.tail nea)

reverseTupleArray :: forall a. BitMask a => NEA.NonEmptyArray (Tuple Int a)
reverseTupleArray =
  toPair <$> (toNonEmptyArray all)
  where
    toPair a = Tuple (maskBit a) a

reverseMap :: forall a. BitMask a => M.Map Int a
reverseMap =
  M.fromFoldable $ reverseTupleArray

toMaskBit :: forall a. BitMask a => Int -> Maybe a
toMaskBit i = M.lookup i reverseMap

compareBitMask :: forall a. Eq a => BitMask a => a -> a -> Ordering
compareBitMask a b = compare (maskBit a) (maskBit b)

sortedReverseTupleArray :: forall a. BitMask a => NE.NonEmpty Array (Tuple Int a)
sortedReverseTupleArray = toNonEmpty $ NEA.sortBy (comparing fst) reverseTupleArray

minBitMask :: forall a. BitMask a => a
minBitMask = snd $ NE.head sortedReverseTupleArray

last :: forall a. NE.NonEmpty Array a -> a
last (NE.NonEmpty h t) = fromMaybe h $ A.last t

maxBitMask :: forall a. BitMask a => a
maxBitMask = snd $ last sortedReverseTupleArray

succRelativeBitMask :: forall a. BitMask a => Int -> a -> Maybe a
succRelativeBitMask relativePosition a =
  let ordA = maskBit a
      arr :: Array (Tuple Int a)
      arr  = A.fromFoldable sortedReverseTupleArray
      succElem idx = indexl (idx + relativePosition) arr
      is v (Tuple v' _) = v == v'
  in snd <$> (succElem =<< (A.findIndex (is ordA) arr))

succBitMask :: forall a. BitMask a => a -> Maybe a
succBitMask = succRelativeBitMask 1

predBitMask :: forall a. BitMask a => a -> Maybe a
predBitMask = succRelativeBitMask (-1)

maskFlagValue :: forall a. BitMask a => a -> Int
maskFlagValue f = (pow 2 (maskBit f))

maskValue :: forall a. BitMask a => S.Set a -> Int
maskValue s = A.foldl (\v -> maskFlagValue >>> (v .|. _)) 0 (S.toUnfoldable s :: Array a)

toMask :: forall a. Ord a => BitMask a => Int -> S.Set a
toMask n =
    S.fromFoldable $ filterFalse =<< (mkPair <$> (A.fromFoldable all))
    where
      filterFalse (Tuple _ false) = []
      filterFalse (Tuple x _) = [x]
      mkPair f = Tuple f $ testBit n (maskBit f)
      bit :: Int -> Int
      bit i = 1 `shl` i
      testBit :: Int -> Int -> Boolean
      testBit x i = (x .&. bit i) /= 0

arbitraryBitMask :: forall a. BitMask a => Gen a
arbitraryBitMask = elements all
