module JVM.Converter.ToFile where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as A
import Data.Binary.Binary (class Binary, put)
import Data.Binary.Put (putToInt8Array)
import Data.Binary.Types (Word16(..), Word32(..), Word8(..))
import Data.BitMask (maskValue)
import Data.Char (fromCharCode)
import Data.FoldableWithIndex (findWithIndex, foldlWithIndex)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt)
import JVM.Attributes (Attribute(..), AttributesDirect(..), AttributesFile(..))
import JVM.ClassFile (Class(..), ClassDirect(..), ClassFile(..))
import JVM.ConstantPool (Constant(..), PoolDirect, PoolFile)
import JVM.Members (Field(..), FieldDirect(..), FieldFile(..), FieldNameType(..), FieldType, Method(..), MethodDirect(..), MethodFile(..), MethodNameType(..), MethodSignature(..))

data Direct2FileError = NoItemInPool String
                        | InvalidFieldSignature FieldType
                        | InvalidMethodSignature MethodSignature

attrInfo :: forall m. MonadThrow Direct2FileError m
                      => PoolDirect
                      -> Tuple String (Array Word8)
                      -> m Attribute
attrInfo pool (Tuple name attributeValue) = do
  pidx <- poolIndex pool name
  let len = Word32 $ fromInt $ A.length attributeValue
  pure $ Attribute
    { attributeName : pidx
    , attributeLength : len
    , attributeValue}

toResult :: forall m a r k. MonadThrow Direct2FileError m => Show k => k -> Maybe { index :: a | r} -> m a
toResult name Nothing = throwError $ NoItemInPool (show name)
toResult _ (Just {index}) = pure index

-- | Find index of given string in the list of constants
poolIndex :: forall m. MonadThrow Direct2FileError m => PoolDirect -> String -> m Word16
poolIndex pool name =
  (toResult name) $ findWithIndex withName pool
  where
    withName _ (CUTF8 {getString : s})    | s == name = true
    withName _ (CUnicode {getString : s}) | s == name = true
    withName _ _ = false

sigToString :: forall m a. MonadThrow Direct2FileError m => Binary a => (a -> Direct2FileError) -> a -> m String
sigToString errFun sig =
  case traverse fromCharCode $ putToInt8Array (put sig) of
    Nothing -> throwError $ errFun sig
    (Just arr) -> pure $ fromCharArray arr

poolClassIndex :: forall m. MonadThrow Direct2FileError m => PoolDirect -> String -> m Word16
poolClassIndex pool name =
  (toResult name) $ findWithIndex withClassName pool
  where
    withClassName _ (CClass nm) | nm == name = true
    withClassName _ _ = false

poolCNameTypeIndex :: forall m s. MonadThrow Direct2FileError m
                            => Show s
                            => Binary s
                            => (s -> Direct2FileError)
                            -> PoolDirect
                            -> {ntName::String, ntSignature::s}
                            -> m Word16
poolCNameTypeIndex errFun pool fldNT @ {ntName, ntSignature} = do
  sigTxt <- sigToString errFun ntSignature
  (toResult $ "{ ntName " <> ntName <> " signature " <> (show ntSignature) <> "}") $ findWithIndex (withCNameType sigTxt) pool
  where
    withCNameType sigTxt _ (CNameType nm sig) | nm == ntName && sig == sigTxt = true
    withCNameType _ _ _ = false

poolFieldIndex :: forall m. MonadThrow Direct2FileError m => PoolDirect -> FieldNameType -> m Word16
poolFieldIndex pool (FieldNameType r) = poolCNameTypeIndex InvalidFieldSignature pool r

poolMethodIndex :: forall m. MonadThrow Direct2FileError m => PoolDirect -> MethodNameType -> m Word16
poolMethodIndex pool (MethodNameType r) = poolCNameTypeIndex InvalidMethodSignature pool r

arsize :: AttributesDirect -> Int
arsize (AttributesDirect m) = A.length m

arlist :: AttributesDirect -> (Array (Tuple String (Array Word8)))
arlist (AttributesDirect m) = m

toAttributesFile :: forall m. MonadThrow Direct2FileError m => PoolDirect -> (Array (Tuple String (Array Word8))) -> m AttributesFile
toAttributesFile pool pairs = AttributesFile <$> (traverse (attrInfo pool) pairs)

methodDirect2File :: forall m. MonadThrow Direct2FileError m => PoolDirect -> MethodDirect -> m MethodFile
methodDirect2File pool (MethodDirect (Method {methodAccessFlags, methodName, methodSignature, methodAttributes})) =
  do
    nameIndex <- poolIndex pool methodName
    msig <- sigToString InvalidMethodSignature methodSignature
    sigIndex <- poolIndex pool msig
    mattrs <- toAttributesFile pool (arlist methodAttributes)
    pure $ MethodFile $ Method
      { methodAccessFlags : Word16 $ fromInt $ maskValue methodAccessFlags
      , methodName : nameIndex
      , methodSignature : sigIndex
      , methodAttributesCount : Word16 $ fromInt $ arsize methodAttributes
      , methodAttributes : mattrs }

fieldDirect2File :: forall m. MonadThrow Direct2FileError m => PoolDirect -> FieldDirect -> m FieldFile
fieldDirect2File pool (FieldDirect (Field {fieldAccessFlags, fieldName, fieldSignature, fieldAttributes})) =
  do
    nameIndex <- poolIndex pool fieldName
    msig <- sigToString InvalidFieldSignature fieldSignature
    sigIndex <- poolIndex pool msig
    fattrs <- toAttributesFile pool (arlist fieldAttributes)
    pure $ FieldFile $ Field
      { fieldAccessFlags : Word16 $ fromInt $ maskValue fieldAccessFlags
      , fieldName : nameIndex
      , fieldSignature : sigIndex
      , fieldAttributesCount : Word16 $ fromInt $ arsize fieldAttributes
      , fieldAttributes : fattrs }

poolDirect2File :: forall m. MonadThrow Direct2FileError m => PoolDirect -> m PoolFile
poolDirect2File pool = traverse cpInfo pool
  where
    cpInfo (CClass name) = do
      idx <- poolIndex pool name
      pure $ CClass idx
    cpInfo (CField cls name) = do
      clsIdx <- poolClassIndex pool cls
      fldIdx <- poolFieldIndex pool name
      pure $ CField clsIdx fldIdx
    cpInfo (CMethod cls name) = do
      clsIdx <- poolClassIndex pool cls
      mthdIdx <- poolMethodIndex pool name
      pure $ CMethod clsIdx mthdIdx
    cpInfo (CIfaceMethod cls name) = do
      clsIdx <- poolClassIndex pool cls
      mthdIdx <- poolMethodIndex pool name
      pure $ CIfaceMethod clsIdx mthdIdx
    cpInfo (CString s) = do
      idx <- poolIndex pool s
      pure $ CString idx
    cpInfo (CInteger x) = pure $ CInteger x
    cpInfo (CFloat x)   = pure $ CFloat x
    cpInfo (CLong x)    = pure $ CLong x
    cpInfo (CDouble x)  = pure $ CDouble x
    cpInfo (CNameType n t) =  do
      nidx <- poolIndex pool n
      tidx <- poolIndex pool t
      pure $ CNameType nidx tidx
    cpInfo (CUTF8 s)    = pure $ CUTF8 s
    cpInfo (CUnicode s) = pure $ CUnicode s

classDirect2File :: forall m. MonadThrow Direct2FileError m => ClassDirect -> m ClassFile
classDirect2File (ClassDirect (Class {magic,minorVersion,majorVersion,constsPoolSize,constsPool,accessFlags,
                    thisClass,superClass,interfacesCount,interfaces,classFieldsCount,
                    classFields,classMethodsCount,classMethods,classAttributesCount,classAttributes
                  })) =
  do
    poolInfo <- poolDirect2File constsPool
    thsClss <- poolClassIndex constsPool thisClass
    supClss <-  poolClassIndex constsPool superClass
    ifcs <- traverse (poolClassIndex constsPool) interfaces
    attrs <- toAttributesFile constsPool (arlist classAttributes)
    clsFlds <- traverse (fieldDirect2File constsPool) classFields
    clsMthds <- traverse (methodDirect2File constsPool) classMethods
    pure $ ClassFile $ Class $
      { magic
      , minorVersion
      , majorVersion
      , constsPoolSize : Word16 $ fromInt (M.size poolInfo + 1)
      , constsPool : poolInfo
      , accessFlags : Word16 $ fromInt $ maskValue accessFlags
      , thisClass : thsClss
      , superClass : supClss
      , interfacesCount : Word16 $ fromInt $ A.length interfaces
      , interfaces : ifcs
      , classFieldsCount : Word16 $ fromInt $ A.length classFields
      , classFields : clsFlds
      , classMethodsCount : Word16 $ fromInt $ A.length classMethods
      , classMethods : clsMthds
      , classAttributesCount : Word16 $ fromInt $ arsize classAttributes
      , classAttributes : attrs }

{-- --}