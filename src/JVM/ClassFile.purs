module JVM.ClassFile where

import Prelude

import Data.Array as A
import Data.Binary.Binary (class Binary, foldablePut, put, get)
import Data.Binary.Decoder (ParserError(..), fail)
import Data.Binary.Types (Word16(..), Word32(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as S
import Data.UInt (fromNumber, toInt)
import JVM.Attributes (AttributesDirect, AttributesFile(..), attributesList)
import JVM.ConstantPool (PoolDirect, PoolFile, getPool, putPool)
import JVM.Flags (AccessFlag)
import JVM.Members (Field(..), FieldDirect(..), FieldFile, MethodDirect)

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
  classAttributes :: attr   -- ^ Class attributes
  }

derive instance repGenericClass :: Generic (Class pool accessFlag b fld mthd attr) _
derive instance eqClass :: (Eq pool, Eq accessFlag, Eq b, Eq fld, Eq mthd, Eq attr) => Eq (Class pool accessFlag b fld mthd attr)
instance showClass :: (Show pool, Show accessFlag, Show b, Show fld, Show mthd, Show attr) => Show (Class pool accessFlag b fld mthd attr) where
  show = genericShow

newtype ClassDirect = ClassDirect (Class PoolDirect (S.Set AccessFlag) String FieldDirect MethodDirect AttributesDirect)
newtype ClassFile = ClassFile (Class PoolFile Word16 Word16 FieldFile Word16 AttributesFile)

lookupField :: String -> ClassDirect -> Maybe (FieldDirect)
lookupField name (ClassDirect(Class {classFields})) = look classFields
  where
    look arr = lookEntry =<< A.uncons arr

    lookEntry ({head : field @ (FieldDirect (Field {fieldName})), tail : fs})
      | fieldName == name = Just field
      | otherwise         = look fs

instance binaryClassFile :: Binary ClassFile where
  put (ClassFile (Class {  magic,minorVersion,majorVersion,constsPoolSize,constsPool,accessFlags,
                thisClass,superClass,interfacesCount,interfaces,classFieldsCount,
                classFields,classMethodsCount,classMethods,classAttributesCount,classAttributes
              })) =
    put magic <>
    put minorVersion <>
    put majorVersion <>
    putPool constsPool <>
    put accessFlags <>
    put thisClass <>
    put superClass <>
    put interfacesCount <>
    foldablePut interfaces <>
    put classFieldsCount <>
    foldablePut classFields <>
    put classMethodsCount <>
    foldablePut classMethods <>
    put classAttributesCount <>
    foldablePut (attributesList classAttributes)

  get = do
    let xCAFEBABE = Word32 $ fromNumber 3405691582.0
    magic <- get
    when (magic /= xCAFEBABE) $ 
      fail (\offset -> GenericParserError { offset, message: "Invalid .class file MAGIC value: " <> show magic })
    minorVersion <- get
    majorVersion <- get
    when ((toInt $ unwrap majorVersion) > 50) $ 
      fail (\offset -> GenericParserError { offset, message: "Too new .class file format: " <> show majorVersion })
    constsPoolSize <- get
    constsPool <- getPool ((toInt $ unwrap $ constsPoolSize) - 1)
    accessFlags <-  get
    thisClass <- get
    superClass <- get
    interfacesCount <- get
    interfaces <- A.fromFoldable <$> replicateM (toInt $ unwrap interfacesCount) get
    classFieldsCount <- get
    classFields <- A.fromFoldable <$> replicateM (toInt $ unwrap classFieldsCount) get
    classMethodsCount <- get
    classMethods <- A.fromFoldable <$> replicateM (toInt $ unwrap classMethodsCount) get
    classAttributesCount <- get
    classAttributes <- (A.fromFoldable >>> AttributesFile) <$> replicateM (toInt $ unwrap $ classAttributesCount) get
    pure $ ClassFile $ Class
              { magic,minorVersion,majorVersion,constsPoolSize,constsPool,accessFlags,
                thisClass,superClass,interfacesCount,interfaces,classFieldsCount,
                classFields,classMethodsCount,classMethods,classAttributesCount,classAttributes
              }


