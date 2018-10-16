module JVM.Members where

import Prelude

import Data.Array as A
import Data.Binary.Binary (class Binary, Put, get, put)
import Data.Binary.Decoder (Decoder(..), fail, getChar8)
import Data.Binary.Types (Word16(..), Word32(..))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect.Exception.Unsafe (unsafeThrow)
import JVM.Attributes (AttributesDirect(..), AttributesFile(..))
import JVM.Flags (FieldAccessFlag, MethodAccessFlag)

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

instance showFieldType :: Show FieldType where
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

data Field flgs b fld attrs = Field {
  fieldAccessFlags :: flgs,
  fieldName :: b,
  fieldSignature :: fld,
  fieldAttributesCount :: Word16,
  fieldAttributes :: attrs
  }

derive instance repGenericField :: Generic (Field flgs b fld attrs) _
derive instance eqField :: (Eq flgs, Eq b, Eq fld, Eq attrs) => Eq (Field flgs b fld attrs)
instance showField :: (Show flgs, Show b, Show fld, Show attrs) => Show (Field flgs b fld attrs) where
  show = genericShow

type FieldDirect = Field (S.Set FieldAccessFlag) String FieldType AttributesDirect

type FieldFile = Field Word16 Word16 Word16 AttributesFile

data Method flgs b mthd attrs = Method {
  methodAccessFlags :: flgs,
  methodName :: b,
  methodSignature :: mthd,
  methodAttributesCount :: Word16,
  methodAttributes :: attrs
}

derive instance repGenericMethod :: Generic (Method flgs b fld attrs) _
derive instance eqMethod :: (Eq flgs, Eq b, Eq fld, Eq attrs) => Eq (Method flgs b fld attrs)
instance showMethod :: (Show flgs, Show b, Show fld, Show attrs) => Show (Method flgs b fld attrs) where
  show = genericShow

type MethodDirect = Method (S.Set MethodAccessFlag) String MethodSignature AttributesDirect

type MethodFile = Method Word16 Word16 Word16 AttributesFile

-- | Size of attributes set at Direct stage
arsize :: AttributesDirect -> Int
arsize (AR m) = A.length m

-- | Associative list of attributes at Direct stage
arlist :: AttributesDirect -> (Array (Tuple String String))
arlist (AR m) = m

-- | Map of attributes at Direct stage
arlookup :: String -> AttributesDirect -> Maybe String
arlookup nm (AR m) = M.lookup nm (M.fromFoldable m)

-- | Size of attributes set at File stage
apsize :: AttributesFile -> Int
apsize (AP list) = A.length list

putMethodSignature :: MethodSignature -> Put
putMethodSignature (MethodSignature args ret) =
  put "("
    <> put ")"

instance binaryFieldType :: Binary FieldType where
  put SignedByte = put "B"
  put CharByte   = put "C"
  put DoubleType = put "D"
  put FloatType  = put "F"
  put IntType    = put "I"
  put LongInt    = put "J"
  put ShortInt   = put "S"
  put BoolType   = put "Z"
  put (ObjectType name) = put "L" <> put name <> put ";"
  put (Array Nothing sig) = put "[" <> put sig
  put (Array (Just n) sig) = put "[" <> put (show n) <> put sig

  get = do
    b <- getChar8
    case b of
      'B' -> pure SignedByte
      'C' -> pure CharByte
      'D' -> pure DoubleType
      'F' -> pure FloatType
      'I' -> pure IntType
      'J' -> pure LongInt
      'S' -> pure ShortInt
      'Z' -> pure BoolType
      'L' -> do
             name <- fromCharArray <$> getToSemicolon
             pure (ObjectType name)
      '[' -> do
             mbSize <- getInt
             sig <- get
             pure (Array (Just mbSize) sig)
      _   -> fail "Unknown signature"

{- instance Binary ReturnSignature where
  put (Returns sig) = put sig
  put ReturnsVoid   = put 'V'

  get = do
    x <- lookAhead getChar8
    case x of
      'V' -> skip 1 >> return ReturnsVoid
      _   -> Returns <$> get -}

getInt :: Decoder Int
getInt = unsafeThrow "todo"

getToSemicolon :: Decoder (Array Char)
getToSemicolon = do
  x <- getChar8
  if x == ';'
    then pure []
    else do
         next <- getToSemicolon
         pure $ A.cons x next



{--
instance Binary MethodSignature where
  put (MethodSignature args ret) = do
    put '('
    forM_ args put
    put ')'
    put ret

  get =  do
    x <- getChar8
    when (x /= '(') $
      fail "Cannot parse method signature: no starting `(' !"
    args <- getArgs
    y <- getChar8
    when (y /= ')') $
      fail "Internal error: method signature without `)' !?"
    ret <- get
    return (MethodSignature args ret)
--}
