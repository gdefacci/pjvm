module Data.Binary.Types where

import Prelude (class Show, class Eq, (<>), show)

import Data.Newtype (class Newtype)

import Data.UInt

newtype Word8 = Word8 UInt
newtype Word16 = Word16 UInt
newtype Word32 = Word32 UInt
data Word64 = Word64 UInt UInt

newtype Float32 = Float32 Number
newtype Float64 = Float64 Number

derive instance eqWord8 :: Eq Word8
derive instance eqWord16 :: Eq Word16
derive instance eqWord32 :: Eq Word32
derive instance eqWord64 :: Eq Word64

derive instance newtypeWord8 ::  Newtype Word8 _
derive instance newtypeWord16 :: Newtype Word16 _
derive instance newtypeWord32 :: Newtype Word32 _

derive instance eqFloat32 :: Eq Float32
derive instance eqFloat64 :: Eq Float64

derive instance newtypeFloat32 :: Newtype Float32 _
derive instance newtypeFloat64 :: Newtype Float64 _

instance w8Show :: Show Word8 where
  show (Word8 i) = "Word8 " <> (show i)

instance w16Show :: Show Word16 where
  show (Word16 i) = "Word16 " <> (show i)

instance w32Show :: Show Word32 where
  show (Word32 i) = "Word32 " <> (show i)

instance w64Show :: Show Word64 where
  show (Word64 l r) = "Word64 " <> (show l) <> " " <> (show r)

instance float32Show :: Show Float32 where
  show (Float32 i) = "Float32 " <> (show i)

instance float64Show :: Show Float64 where
  show (Float64 i) = "Float64 " <> (show i)