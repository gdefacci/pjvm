module JVM.ClassFile where

import Prelude
import Data.Binary.Types
import JVM.ConstantPool

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Set as S
import JVM.Flags (AccessFlag)
import JVM.Members (AttributesDirect, AttributesFile, FieldDirect, FieldFile, MethodDirect)

-- | Generic .class file format
data Class pool accessFlag b fld mthd attr = Class {
  magic :: Word32,                -- ^ Magic value: 0xCAFEBABE
  minorVersion :: Word16,
  majorVersion :: Word16,
  constsPoolSize :: Word16,       -- ^ Number of items in constants pool
  constsPool :: pool,             -- ^ Constants pool itself
  accessFlags :: accessFlag,      -- ^ See @JVM.Types.AccessFlag@
  thisClass :: b,                 -- ^ Constants pool item index for this class
  superClass :: b,                -- ^ --/-- for super class, zero for java.lang.Object
  interfacesCount :: Word16,      -- ^ Number of implemented interfaces
  interfaces :: Array b,          -- ^ Constants pool item indexes for implemented interfaces
  classFieldsCount :: Word16,     -- ^ Number of class fileds
  classFields :: Array fld,       -- ^ Class fields
  classMethodsCount :: Word16,    -- ^ Number of class methods
  classMethods :: Array mthd,     -- ^ Class methods
  classAttributesCount :: Word16, -- ^ Number of class attributes
  classAttributes :: Array attr   -- ^ Class attributes
  }

derive instance repGenericClass :: Generic (Class pool accessFlag b fld mthd attr) _
derive instance eqClass :: (Eq pool, Eq accessFlag, Eq b, Eq fld, Eq mthd, Eq attr) => Eq (Class pool accessFlag b fld mthd attr)
instance showClass :: (Show pool, Show accessFlag, Show b, Show fld, Show mthd, Show attr) => Show (Class pool accessFlag b fld mthd attr) where
  show = genericShow

type ClassDirect = Class PoolDirect (S.Set AccessFlag) String FieldDirect MethodDirect AttributesDirect

type ClassFile = Class PoolFile Word16 Word16 FieldFile Word16 AttributesFile
