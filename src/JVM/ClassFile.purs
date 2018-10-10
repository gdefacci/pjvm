module JVM.ClassFile where

import Data.Binary.Types
import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import JVM.Flags (AccessFlag, FieldAccessFlag(..), MethodAccessFlag(..))

data File = File
data Direct = Direct

-- | Field signature format
data FieldType =
    SignedByte -- ^ B
  | CharByte   -- ^ C
  | DoubleType -- ^ D
  | FloatType  -- ^ F
  | IntType    -- ^ I
  | LongInt    -- ^ J
  | ShortInt   -- ^ S
  | BoolType   -- ^ Z
  | ObjectType String -- ^ L @{class name}@
  | Array (Maybe Int) FieldType -- ^ @[{type}@

derive instance eqFieldType :: Eq FieldType

instance fieldTypeShow :: Show FieldType where
  show SignedByte = "byte"
  show CharByte = "char"
  show DoubleType = "double"
  show FloatType = "float"
  show IntType = "int"
  show LongInt = "long"
  show ShortInt = "short"
  show BoolType = "bool"
  show (ObjectType s) = "Object " <> s
  show (Array Nothing t) = show t <> "[]"
  show (Array (Just n) t) = show t <> "[" <> show n <> "]"

-- | Return value signature
data ReturnSignature =
    Returns FieldType
  | ReturnsVoid

derive instance eqReturnSignature :: Eq ReturnSignature

instance showReturnSignature :: Show ReturnSignature where
  show (Returns t) = show t
  show ReturnsVoid = "Void"

-- | Method argument signature
type ArgumentSignature = FieldType

-- | Class method argument signature
data MethodSignature =
    MethodSignature (Array ArgumentSignature) ReturnSignature
  --deriving (Eq, Ord)

derive instance eqMethodSignature :: Eq MethodSignature

instance showMethodSignature :: Show MethodSignature where
  show (MethodSignature args ret) = "(" <> intercalate ", " (show <$> args) <> ") returns " <> show ret

data FieldNameType = FieldNameType {
  ntName :: String,
  ntSignature :: FieldType
}

data MethodNameType = MethodNameType {
  ntName :: String,
  ntSignature :: MethodSignature
}

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data Attribute = Attribute {
  attributeName :: Word16,
  attributeLength :: Word32,
  attributeValue :: String
}

derive instance repGenericAttribute :: Generic Attribute _
derive instance eqAttribute :: Eq Attribute
instance showAttribute :: Show Attribute where
  show = genericShow

data AttributesDirect = AP (Array (Tuple String String))

derive instance repGenericAttributesDirect :: Generic AttributesDirect _
derive instance eqAttributesDirect :: Eq AttributesDirect
instance showAttributesDirect :: Show AttributesDirect where
  show = genericShow

data AttributesFile = AR (Array Attribute)

derive instance repGenericAttributesFile :: Generic AttributesFile _
derive instance eqAttributesFile :: Eq AttributesFile
instance showAttributesFile :: Show AttributesFile where
  show = genericShow

data Constant b f m =
    CClass b
  | CField b f
  | CMethod b m
  | CIfaceMethod b m
  | CString b
  | CInteger Word32
  | CFloat Float32
  | CLong Word64
  | CDouble Float64
  | CNameType b b
  | CUTF8 {getString :: String}
  | CUnicode {getString :: String}

instance showConstant :: (Show b, Show f, Show m) => Show (Constant b f m) where
  show (CClass name) = "class " <> show name
  show (CField cls nt) = "field " <> show cls <> "." <> show nt
  show (CMethod cls nt) = "method " <> show cls <> "." <> show nt
  show (CIfaceMethod cls nt) = "interface method " <> show cls <> "." <> show nt
  show (CString s) = "String \"" <> show s <> "\""
  show (CInteger x) = show x
  show (CFloat x) = show x
  show (CLong x) = show x
  show (CDouble x) = show x
  show (CNameType name tp) = show name <> ": " <> show tp
  show (CUTF8 s) = "UTF8 \"" <> show s <> "\""
  show (CUnicode s) = "Unicode \"" <> show s <> "\""

type ConstantDirect = Constant String FieldNameType MethodNameType

type ConstantFile = Constant Word16 Word16 Word16

type PoolDirect = M.Map Word16 ConstantDirect

type PoolFile = M.Map Word16 ConstantFile

data Field flgs b fld attrs = Field {
  fieldAccessFlags :: flgs,
  fieldName :: b,
  fieldSignature :: fld,
  fieldAttributesCount :: Word16,
  fieldAttributes :: attrs
  }

type FieldDirect = Field (S.Set FieldAccessFlag) String FieldType AttributesDirect

type FieldFile = Field Word16 Word16 Word16 AttributesFile

data Method flgs b mthd attrs = Method {
  methodAccessFlags :: flgs,
  methodName :: b,
  methodSignature :: mthd,
  methodAttributesCount :: Word16,
  methodAttributes :: attrs
}

type MethodDirect = Method (S.Set MethodAccessFlag) String MethodSignature AttributesDirect

type MethodFile = Method Word16 Word16 Word16 AttributesFile

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

type ClassDirect = Class PoolDirect (S.Set AccessFlag) String FieldDirect MethodDirect AttributesDirect

type ClassFile = Class PoolFile Word16 Word16 FieldFile Word16 AttributesFile