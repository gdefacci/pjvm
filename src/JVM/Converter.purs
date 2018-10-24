module JVM.Convereter where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (State)
import Data.Array (find)
import Data.Array as A
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.Binary.Binary as Binary
import Data.Binary.Decoder (ParserError, ParserState, decode)
import Data.Binary.Types (Word16(..), Word8)
import Data.BitMask (toMask)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt, toInt)
import JVM.Attributes (Attribute(..), AttributesDirect(..), AttributesFile(..), attributesDirectLookup)
import JVM.ClassFile (Class(..), ClassDirect(..), ClassFile(..))
import JVM.ConstantPool (Constant(..), PoolDirect, PoolFile)
import JVM.Members (Field(..), FieldDirect(..), FieldFile(..), FieldNameType(..), Method(..), MethodDirect(..), MethodFile(..), MethodNameType(..))

-- | Size of attributes set at Direct stage
arsize :: AttributesDirect -> Int
arsize (AttributesDirect m) = A.length m

-- | Associative list of attributes at Direct stage
arlist :: AttributesDirect -> (Array (Tuple String (Array Word8)))
arlist (AttributesDirect m) = m

-- | Size of attributes set at File stage
apsize :: AttributesFile -> Int
apsize (AttributesFile list) = A.length list

data File2DirectError = 
  MissingMethod String
  | MissingField String
  | MissingAttribute String
  | ParseError ParserError
  | MissingEntry String
  | NoStringConst String
  | NoClassConst String
  | NoNameType String

derive instance genricFile2DirectError :: Generic File2DirectError _

instance showFile2DirectError :: Show File2DirectError where
  show = genericShow

-- | Try to get class method by name
methodByName :: forall m. MonadThrow File2DirectError m => ClassDirect -> String -> m MethodDirect
methodByName (ClassDirect (Class {classMethods})) name =
  case find (\(MethodDirect (Method {methodName})) -> methodName == name) classMethods of
    Nothing -> throwError $ MissingMethod name
    (Just v) -> pure v

-- FIXME : remove
-- | Try to get object attribute by name
attrByName :: forall m. MonadThrow File2DirectError m => AttributesDirect -> String -> m (Array Word8)
attrByName a n = 
  case attributesDirectLookup n a of
    Nothing -> throwError $ MissingAttribute n
    (Just v) -> pure v

getString :: forall b fld mthd m. MonadThrow File2DirectError m => Show b => Show fld => Show mthd => Constant b fld mthd -> m String
getString (CUTF8 {getString: txt})    = pure txt
getString (CUnicode {getString: txt}) = pure txt
getString x = throwError $ NoStringConst $ show x

attributesFile2Direct :: forall m. MonadThrow File2DirectError m => PoolDirect -> AttributesFile -> m AttributesDirect
attributesFile2Direct pool (AttributesFile attrs) = 
  let attrs' = traverse go attrs
  in AttributesDirect <$> attrs'
  where
    go :: Attribute -> m (Tuple String (Array Word8))
    go (Attribute {attributeName, attributeValue}) = do
      cnst <- lookup attributeName pool
      nm <- getString cnst
      pure $ Tuple nm attributeValue

decodeOpt :: forall a m. MonadThrow File2DirectError m => ExceptT ParserError (State ParserState) a -> String -> m a
decodeOpt dec str = 
  case decode dec (AB.fromString str) of
    (Left err) -> throwError $ ParseError err
    (Right (Tuple _ v)) -> pure v

lookup :: forall k v m. MonadThrow File2DirectError m => Show k => Ord k => k -> M.Map k v -> m v
lookup k mp = 
  case M.lookup k mp of
    (Just v) -> pure v
    Nothing -> throwError $ MissingEntry $ show k

methodFile2Direct :: forall m. MonadThrow File2DirectError m => PoolDirect -> MethodFile -> m MethodDirect
methodFile2Direct pool (MethodFile (Method {methodAccessFlags, methodName, methodSignature, methodAttributes})) =
  do
    let methodAttributesCount = Word16 $ fromInt $ apsize methodAttributes
    mattrs <- attributesFile2Direct pool methodAttributes
    mthdNm <- getString =<< (lookup methodName pool)
    msig <- (decodeOpt Binary.get) =<< getString =<< (lookup methodSignature pool)
    pure $ MethodDirect $ Method
      { methodAccessFlags : toMask $ toInt $ unwrap methodAccessFlags
      , methodName : mthdNm
      , methodSignature : msig
      , methodAttributesCount
      , methodAttributes : mattrs }

fieldFile2Direct :: forall m. MonadThrow File2DirectError m => PoolDirect -> FieldFile -> m FieldDirect
fieldFile2Direct pool (FieldFile (Field {fieldAccessFlags, fieldName, fieldSignature, fieldAttributes})) =
  do
    let fieldAttributesCount = Word16 $ fromInt $ apsize fieldAttributes
    fattrs <- attributesFile2Direct pool fieldAttributes
    fnm <- getString =<< (lookup fieldName pool)
    fsig <- (decodeOpt Binary.get) =<< getString =<< (lookup fieldSignature pool)
    pure $ FieldDirect $ Field
      { fieldAccessFlags : toMask $ toInt $ unwrap fieldAccessFlags
      , fieldName : fnm
      , fieldSignature : fsig
      , fieldAttributesCount
      , fieldAttributes : fattrs }

getAsUTF8String :: forall m. MonadThrow File2DirectError m => PoolFile -> Word16 -> m String
getAsUTF8String pool idx = do
  asUTF8 =<< (lookup idx pool)
  where
    asUTF8 (CUTF8 {getString : s}) = pure s
    asUTF8 x = throwError $ NoStringConst $ show x

getAsString :: forall m. MonadThrow File2DirectError m => PoolFile -> Word16 -> m String
getAsString pool idx =
  asString =<< lookup idx pool
  where
    asString (CUTF8 {getString : s})    = pure s
    asString (CUnicode {getString : s}) = pure s
    asString x = throwError $ NoStringConst $ show x

convertClassName :: forall m. MonadThrow File2DirectError m => PoolFile -> Word16 -> m String
convertClassName pool i = do
  cnst <- lookup i pool
  idx <- case cnst of
    (CClass nmIdx) -> pure nmIdx
    x -> throwError $ NoClassConst $ show x
  nm <- getAsString pool idx
  pure nm

convertNameTypeTuple :: forall t m. MonadThrow File2DirectError m => Binary.Binary t => PoolFile -> Word16 -> m (Tuple String t)
convertNameTypeTuple pool i = do
  cnst <- lookup i pool
  case cnst of
    CNameType nmidx sidx -> do
      ntName <- getAsString pool nmidx
      ntSignature <- (decodeOpt Binary.get) =<< (getAsString pool sidx)
      pure $ (Tuple ntName ntSignature)
    x -> throwError $ NoNameType $ show x

convertFieldNameType :: forall m. MonadThrow File2DirectError m => PoolFile -> Word16 -> m FieldNameType
convertFieldNameType pool i = 
  (\(Tuple ntName ntSignature) -> FieldNameType {ntName, ntSignature} ) <$> (convertNameTypeTuple pool i)

convertMethodNameType :: forall m. MonadThrow File2DirectError m => PoolFile -> Word16 -> m MethodNameType
convertMethodNameType pool i = 
  (\(Tuple ntName ntSignature) -> MethodNameType {ntName, ntSignature} ) <$> (convertNameTypeTuple pool i)
  
poolFile2Direct :: forall m. MonadThrow File2DirectError m => PoolFile -> m PoolDirect
poolFile2Direct ps =
  traverse convert ps
  where
    convert (CClass i) = CClass <$> (getAsString ps i)
    convert (CField i j) = CField <$> (convertClassName ps i) <*> (convertFieldNameType ps j)
    convert (CMethod i j) = CMethod <$> (convertClassName ps i) <*> (convertMethodNameType ps j)
    convert (CIfaceMethod i j) = CIfaceMethod <$> (convertClassName ps i) <*> (convertMethodNameType ps j)
    convert (CString i) = CString <$> (getAsString ps i)
    convert (CFloat x)   = pure $ CFloat x
    convert (CInteger x) = pure $ CInteger x
    convert (CLong x)    = pure $ CLong x
    convert (CDouble x)  = pure $ CDouble x
    convert (CNameType i j) = CNameType <$> (getAsString ps i) <*> (getAsString ps j)
    convert (CUTF8 bs)    = pure $ CUTF8 bs
    convert (CUnicode bs) = pure $ CUnicode bs

classFile2Direct :: forall m. MonadThrow File2DirectError m => ClassFile -> m ClassDirect
classFile2Direct (ClassFile (Class {magic,minorVersion,majorVersion,constsPoolSize,constsPool,accessFlags,
                    thisClass,superClass,interfacesCount,interfaces,classFieldsCount,
                    classFields,classMethodsCount,classMethods,classAttributesCount,classAttributes
                  })) =
  do
    pool <- poolFile2Direct constsPool
    superName <- if (toInt $ unwrap superClass) == 0 
                    then pure "" 
                    else convertClassName constsPool superClass
    thisCls <- convertClassName constsPool thisClass
    attrs <- attributesFile2Direct pool classAttributes
    ifcs <- traverse (convertClassName constsPool) interfaces
    cflds <- traverse (fieldFile2Direct pool) classFields
    cmthds <- traverse (methodFile2Direct pool) classMethods
    pure $ ClassDirect $ Class $ 
      { magic 
      , minorVersion
      , majorVersion
      , constsPoolSize : Word16 $ fromInt $ M.size pool
      , constsPool : pool
      , accessFlags : toMask $ toInt $ unwrap accessFlags
      , thisClass : thisCls
      , superClass : superName
      , interfacesCount
      , interfaces : ifcs
      , classFieldsCount
      , classFields : cflds
      , classMethodsCount
      , classMethods : cmthds
      , classAttributesCount
      , classAttributes : attrs }