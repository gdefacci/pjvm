module JVM.ClassFile where

import Prelude

import Data.Map as M
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Data.Binary.Types

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

type ConstantDirect = Constant String FieldNameType MethodNameType

type ConstantFile = Constant Word16 Word16 Word16

type PoolDirect = M.Map Word16 ConstantDirect

type PoolFile = M.Map Word16 ConstantFile