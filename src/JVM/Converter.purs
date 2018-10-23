module JVM.Convereter where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (State)
import Data.Array (find)
import Data.Array as A
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.Binary.Binary as Binary
import Data.Binary.Decoder (ParserState, decode)
import Data.Binary.Types (Word16(..), Word8(..))
import Data.BitMask (toMask)
import Data.Either (hush)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), snd)
import Data.UInt (fromInt, toInt)
import JVM.Attributes (Attribute(..), AttributesDirect(..), AttributesFile(..), attributesDirectLookup)
import JVM.ClassFile (Class(..), ClassDirect(..))
import JVM.ConstantPool (Constant(..), ConstantDirect, ConstantFile, PoolDirect, PoolFile)
import JVM.Members (Field(..), FieldDirect(..), FieldFile(..), FieldNameType(..), Method(..), MethodDirect(..), MethodFile(..))

-- | Size of attributes set at Direct stage
arsize :: AttributesDirect -> Int
arsize (AttributesDirect m) = A.length m

-- | Associative list of attributes at Direct stage
arlist :: AttributesDirect -> (Array (Tuple String (Array Word8)))
arlist (AttributesDirect m) = m

-- | Size of attributes set at File stage
apsize :: AttributesFile -> Int
apsize (AttributesFile list) = A.length list

-- | Try to get class method by name
methodByName :: ClassDirect -> String -> Maybe (MethodDirect)
methodByName (ClassDirect (Class {classMethods})) name =
  find (\(MethodDirect (Method {methodName})) -> methodName == name) classMethods

-- FIXME : remove
-- | Try to get object attribute by name
attrByName :: AttributesDirect -> String -> Maybe (Array Word8)
attrByName a n = attributesDirectLookup n a

getString :: forall b f m. Constant b f m -> Maybe String
getString (CUTF8 {getString: txt}) = Just txt
getString (CUnicode {getString: txt}) = Just txt
getString _ = Nothing

attributesFile2Direct :: PoolDirect -> AttributesFile -> AttributesDirect
attributesFile2Direct pool (AttributesFile attrs) = AttributesDirect (go =<< attrs)
  where
    go :: Attribute -> Array (Tuple String (Array Word8))
    go (Attribute {attributeName, attributeValue}) = A.fromFoldable $ do
      cnst <- M.lookup attributeName pool
      nm <- getString cnst
      pure $ Tuple nm attributeValue

decodeOpt :: forall err a. ExceptT err (State ParserState) a -> String -> Maybe a
decodeOpt dec str = hush $ snd <$> decode dec (AB.fromString str)

methodFile2Direct :: PoolDirect -> MethodFile -> Maybe MethodDirect
methodFile2Direct pool (MethodFile (Method {methodAccessFlags, methodName, methodSignature, methodAttributes})) =
  do
    let methodAttributesCount = Word16 $ fromInt $ apsize methodAttributes
        mattrs = attributesFile2Direct pool methodAttributes
    mthdNm <- getString =<< (M.lookup methodName pool)
    msig <- (decodeOpt Binary.get) =<< getString =<< (M.lookup methodSignature pool)
    pure $ MethodDirect $ Method
      { methodAccessFlags : toMask $ toInt $ unwrap methodAccessFlags
      , methodName : mthdNm
      , methodSignature : msig
      , methodAttributesCount
      , methodAttributes : mattrs }

fieldFile2Direct :: PoolDirect -> FieldFile -> Maybe FieldDirect
fieldFile2Direct pool (FieldFile (Field {fieldAccessFlags, fieldName, fieldSignature, fieldAttributes})) =
  do
    let fieldAttributesCount = Word16 $ fromInt $ apsize fieldAttributes
        fattrs = attributesFile2Direct pool fieldAttributes
    fnm <- getString =<< (M.lookup fieldName pool)
    fsig <- (decodeOpt Binary.get) =<< getString =<< (M.lookup fieldSignature pool)
    pure $ FieldDirect $ Field
      { fieldAccessFlags : toMask $ toInt $ unwrap fieldAccessFlags
      , fieldName : fnm
      , fieldSignature : fsig
      , fieldAttributesCount
      , fieldAttributes : fattrs }

getAsUTF8String :: PoolFile -> Word16 -> Maybe String
getAsUTF8String pool idx =
  asUTF8 =<< M.lookup idx pool
  where
    asUTF8 (CUTF8 {getString : s}) = Just s
    asUTF8 _ = Nothing

getAsString :: PoolFile -> Word16 -> Maybe String
getAsString pool idx =
  asString =<< M.lookup idx pool
  where
    asString (CUTF8 {getString : s}) = Just s
    asString (CUnicode {getString : s}) = Just s
    asString _ = Nothing

convertClassName :: PoolFile -> Word16 -> Maybe String
convertClassName pool i = do
  cnst <- M.lookup i pool
  idx <- case cnst of
    (CClass nmIdx) -> pure nmIdx
    _ -> Nothing
  nm <- getAsString pool idx
  pure nm

convertFieldNameType :: PoolFile -> Word16 -> Maybe FieldNameType
convertFieldNameType pool i = do
  cnst <- M.lookup i pool
  case cnst of
    CNameType nmidx sidx -> do
      ntName <- getAsString pool nmidx
      s <- getAsString pool sidx
      ntSignature <- decodeOpt Binary.get s
      pure $ FieldNameType {ntName, ntSignature}
    _ -> Nothing


{--
poolFile2Direct :: PoolFile -> PoolDirect
poolFile2Direct ps =
  pool poolIndex
  where
    pool :: Pool Direct
    pool = convert <$> ps

    n = fromIntegral $ M.size ps

    convert _ (CClass i) = do
      nm <- getAsUTF8String ps i
      pure $ CClass name
    convert (CField i j) = do
      cls <- convertClassName ps i
      fld <- convertFieldNameType ps j
      CField cls fld

    convert (CMethod i j) = CMethod (className $ pool ! i) (convertNameType j)
    convert (CIfaceMethod i j) = CIfaceMethod (className $ pool ! i) (convertNameType j)
    convert (CString i) = CString $ getString $ pool ! i
    convert (CInteger x) = CInteger x
    convert (CFloat x)   = CFloat x
    convert (CLong x)    = CLong (fromIntegral x)
    convert (CDouble x)  = CDouble x
    convert (CNameType i j) = CNameType (getString $ pool ! i) (getString $ pool ! j)
    convert (CUTF8 bs) = CUTF8 bs
    convert (CUnicode bs) = CUnicode bs

--}