module JVM.Members where

import Prelude

import Data.Array as A
import Data.Binary.Binary (class Binary, putFoldable, get, put)
import Data.Binary.Decoder (Decoder, ParserError(..), fail, getChar8, getUInt8, lookAhead, skip)
import Data.Binary.Types (Word16(..))
import Data.Char.Unicode (isDigit)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.UInt (toInt)
import JVM.Attributes (AttributesDirect, AttributesFile(..), attributesList)
import JVM.Flags (FieldAccessFlag, MethodAccessFlag)

-- | Field signature format
data JVMType =
    SignedByte -- ^ B
  | CharByte   -- ^ C
  | DoubleType -- ^ D
  | FloatType  -- ^ F
  | IntType    -- ^ I
  | LongInt    -- ^ J
  | ShortInt   -- ^ S
  | BoolType   -- ^ Z
  | ObjectType String -- ^ L @{class name}@
  | ArrayType (Maybe Int) JVMType -- ^ @[{type}@

derive instance eqFieldType :: Eq JVMType

instance showFieldType :: Show JVMType where
  show SignedByte = "byte"
  show CharByte = "char"
  show DoubleType = "double"
  show FloatType = "float"
  show IntType = "int"
  show LongInt = "long"
  show ShortInt = "short"
  show BoolType = "bool"
  show (ObjectType s) = "Object " <> s
  show (ArrayType Nothing t) = show t <> "[]"
  show (ArrayType (Just n) t) = show t <> "[" <> show n <> "]"

-- | Return value signature
data ReturnSignature =
    Returns JVMType
  | ReturnsVoid

derive instance eqReturnSignature :: Eq ReturnSignature

instance showReturnSignature :: Show ReturnSignature where
  show (Returns t) = show t
  show ReturnsVoid = "Void"

-- | Class method argument signature
data MethodSignature =
    MethodSignature (Array JVMType) ReturnSignature

derive instance eqMethodSignature :: Eq MethodSignature

instance showMethodSignature :: Show MethodSignature where
  show (MethodSignature args ret) = "(" <> intercalate ", " (show <$> args) <> ") returns " <> show ret

data FieldNameType = FieldNameType {
  ntName :: String,
  ntSignature :: JVMType
}

derive instance repGenericFieldNameType :: Generic FieldNameType _
derive instance eqFieldNameType :: Eq FieldNameType
instance showFieldNameType :: Show FieldNameType where
  show = genericShow

data MethodNameType = MethodNameType {
  ntName :: String,
  ntSignature :: MethodSignature
}

derive instance repGenericMethodNameType :: Generic MethodNameType _
derive instance eqMethodNameType :: Eq MethodNameType
instance showMethodNameType :: Show MethodNameType where
  show = genericShow

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

newtype FieldDirect = FieldDirect (Field (S.Set FieldAccessFlag) String JVMType AttributesDirect)
newtype FieldFile = FieldFile (Field Word16 Word16 Word16 AttributesFile)

derive instance repGenericFieldDirect :: Generic FieldDirect _
derive instance eqFieldDirect :: Eq FieldDirect
instance showFieldDirect :: Show FieldDirect where
  show = genericShow

derive instance repGenericFieldFile :: Generic FieldFile _
derive instance eqFieldFile :: Eq FieldFile
instance showFieldFile :: Show FieldFile where
  show = genericShow

derive instance newTypeFieldDirect :: Newtype FieldDirect _
derive instance newTypeFieldFile :: Newtype FieldFile _

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

newtype MethodDirect = MethodDirect (Method (S.Set MethodAccessFlag) String MethodSignature AttributesDirect)
newtype MethodFile = MethodFile (Method Word16 Word16 Word16 AttributesFile)

derive instance repGenericMethodDirect :: Generic MethodDirect _
derive instance eqMethodDirect :: Eq MethodDirect
instance showMethodDirect :: Show MethodDirect where
  show = genericShow

derive instance repGenericMethodFile :: Generic MethodFile _
derive instance eqMethodFile :: Eq MethodFile
instance showMethodFile :: Show MethodFile where
  show = genericShow

fieldNameType :: FieldDirect -> FieldNameType
fieldNameType (FieldDirect (Field {fieldName, fieldSignature})) = FieldNameType
  { ntName : fieldName
  , ntSignature : fieldSignature
  }

instance binaryFieldType :: Binary JVMType where
  put SignedByte = put 'B'
  put CharByte   = put 'C'
  put DoubleType = put 'D'
  put FloatType  = put 'F'
  put IntType    = put 'I'
  put LongInt    = put 'J'
  put ShortInt   = put 'S'
  put BoolType   = put 'Z'
  put (ObjectType name)    = put 'L' <> putFoldable (toCharArray name) <> put ';'
  put (ArrayType Nothing sig)  = put '[' <> put sig
  put (ArrayType (Just n) sig) = put '[' <> put (show n) <> put sig

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
             pure (ArrayType mbSize sig)
      _   -> fail $ \offset -> GenericParserError { offset, message: "Unknown signature"}

instance binaryReturnSignature :: Binary ReturnSignature where
  put (Returns sig) = put sig
  put ReturnsVoid   = put 'V'

  get = do
    x <- lookAhead getChar8
    case x of
      'V' -> do
        _ <- getUInt8
        pure $ ReturnsVoid
      _   ->
        Returns <$> get

getInt :: forall m. Monad m => Decoder m (Maybe Int)
getInt = do
    sds <- getDigits
    pure $ fromString $ fromCharArray sds

  where
    getDigits :: Decoder m (Array Char)
    getDigits = do
      c <- lookAhead getChar8
      if isDigit c
        then do
             skip 1
             next <- getDigits
             pure $ A.cons c next
        else pure []

getToSemicolon :: forall m. Monad m => Decoder m (Array Char)
getToSemicolon = do
  x <- getChar8
  if x == ';'
    then pure []
    else do
         next <- getToSemicolon
         pure $ A.cons x next

instance binaryMethodSignature :: Binary MethodSignature where
  put (MethodSignature args ret) =
    put '(' <>
    putFoldable args <>
    put ')' <>
    put ret

  get =  do
    x <- getChar8
    when (x /= '(') $ fail $ \offset -> GenericParserError { offset, message: "Cannot parse method signature: no starting `(' !" }
    args <- getArgs
    y <- getChar8
    when (y /= ')') $ fail \offset -> GenericParserError { offset, message: "Internal error: method signature without `)' !?" }
    ret <- get
    pure $ MethodSignature args ret

-- | Read arguments signatures (up to `)')
getArgs :: forall m. Monad m => Decoder m (Array JVMType)
getArgs = whileJust getArg
  where
    getArg :: Decoder m (Maybe JVMType)
    getArg = do
      x <- lookAhead $ getChar8
      if x == ')'
        then pure Nothing
        else do
          v <- get
          pure $ Just v

whileJust :: forall m a. Monad m => m (Maybe a) -> m (Array a)
whileJust m = do
  r <- m
  case r of
    Just x ->
      do
        next <- whileJust m
        pure $ A.cons x next
    Nothing ->
        pure []

instance fieldFileBinary :: Binary FieldFile where
  put (FieldFile (Field {fieldAccessFlags, fieldName, fieldSignature, fieldAttributesCount, fieldAttributes })) =
    put fieldAccessFlags <>
    put fieldName <>
    put fieldSignature <>
    put fieldAttributesCount <>
    putFoldable (attributesList fieldAttributes)

  get = do
    fieldAccessFlags <- get
    fieldName <- get
    fieldSignature <- get
    fieldAttributesCount @ (Word16 n) <- get
    fieldAttributes <- (A.fromFoldable >>> AttributesFile) <$> replicateM (toInt n) get
    pure $ FieldFile $
      Field {fieldAccessFlags, fieldName, fieldSignature, fieldAttributesCount, fieldAttributes}

instance binaryMethodFile :: Binary MethodFile where
  put (MethodFile (Method {methodAccessFlags, methodName, methodSignature, methodAttributesCount, methodAttributes})) =
    put methodAccessFlags <>
    put methodName <>
    put methodSignature <>
    put methodAttributesCount <>
    putFoldable (attributesList methodAttributes)

  get = do
    methodAccessFlags <- get
    methodName <- get
    methodSignature <- get
    methodAttributesCount <- get
    methodAttributes <- (A.fromFoldable >>> AttributesFile) <$> replicateM (toInt $ unwrap methodAttributesCount) get
    pure $ MethodFile $ Method {
               methodAccessFlags,
               methodName,
               methodSignature,
               methodAttributesCount,
               methodAttributes }
