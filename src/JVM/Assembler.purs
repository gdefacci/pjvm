module JVM.Assembler where

import Prelude

import Data.Array as A
import Data.Binary.Binary (class Binary, get, put, putFoldable)
import Data.Binary.Decoder (getRest, withSlice)
import Data.Binary.Put (putToInt8Array)
import Data.Binary.Types (Word16, Word32, Word8(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (replicateM)
import Data.Newtype (unwrap)
import Data.UInt (fromInt, toInt)
import JVM.Attributes (AttributesFile(..), attributesList)
import JVM.Instruction (Instruction)

data Code = Code
    { codeStackSize :: Word16
    , codeMaxLocals :: Word16
    , codeLength :: Word32
    , codeInstructions :: Array Instruction
    , codeExceptionsN :: Word16
    , codeExceptions :: Array CodeException
    , codeAttrsN :: Word16
    , codeAttributes :: AttributesFile }

derive instance eqCode :: Eq Code

derive instance genericCode :: Generic Code _

instance showCode :: Show Code where
  show = genericShow

-- | Exception descriptor
data CodeException = CodeException
    { eStartPC :: Word16
    , eEndPC :: Word16
    , eHandlerPC :: Word16
    , eCatchType :: Word16 }

derive instance eqCodeException :: Eq CodeException

derive instance genericCodeException :: Generic CodeException _

instance showCodeException :: Show CodeException where
  show = genericShow

instance binaryCodeException :: Binary CodeException where
  put (CodeException {eStartPC, eEndPC, eHandlerPC, eCatchType}) =
    put eStartPC <>
    put eEndPC <>
    put eHandlerPC <>
    put eCatchType

  get = CodeException <$> ({ eStartPC : _  , eEndPC : _ , eHandlerPC : _ , eCatchType : _ } <$> get <*> get <*> get <*> get)

instance  binaryCode :: Binary Code where
  put (Code {codeStackSize,codeMaxLocals,codeLength,codeInstructions,
             codeExceptionsN,codeExceptions,codeAttrsN,codeAttributes}) =
    put codeStackSize <>
    put codeMaxLocals <>
    put codeLength <>
    putFoldable codeInstructions <>
    put codeExceptionsN <>
    putFoldable codeExceptions <>
    put codeAttrsN <>
    putFoldable (attributesList codeAttributes)

  get = do
    codeStackSize <- get
    codeMaxLocals <- get
    codeLength <- get
    codeInstructions <- withSlice (toInt $ unwrap $ codeLength) (getRest get)
    codeExceptionsN <- get
    codeExceptions <- A.fromFoldable <$> replicateM (toInt $ unwrap codeExceptionsN) get
    codeAttrsN <- get
    codeAttributes <- (A.fromFoldable >>> AttributesFile) <$> (replicateM (toInt $ unwrap codeAttrsN) get)
    pure $ Code { codeStackSize,codeMaxLocals,codeLength,codeInstructions,
                  codeExceptionsN,codeExceptions,codeAttrsN,codeAttributes}

encodeInstructions :: (Array Instruction) -> Array Word8
encodeInstructions intructions = (fromInt >>> Word8) <$> putToInt8Array (putFoldable intructions)

-- | Encode Java method
encodeMethod :: Code -> Array Word8
encodeMethod code = (fromInt >>> Word8) <$> putToInt8Array (put code)
