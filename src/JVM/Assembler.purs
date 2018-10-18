module JVM.Assembler where

import Prelude

import Data.Binary.Types (Word16, Word32)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import JVM.Attributes (AttributesFile)
import JVM.Instruction (Instruction)

data Code = Code {
    codeStackSize :: Word16,
    codeMaxLocals :: Word16,
    codeLength :: Word32,
    codeInstructions :: Array Instruction,
    codeExceptionsN :: Word16,
    codeExceptions :: Array CodeException,
    codeAttrsN :: Word16,
    codeAttributes :: AttributesFile }
--  deriving (Eq, Show)

derive instance eqCode :: Eq Code

derive instance genericCode :: Generic Code _

instance showCode :: Show Code where
  show = genericShow

-- | Exception descriptor
data CodeException = CodeException {
    eStartPC :: Word16,
    eEndPC :: Word16,
    eHandlerPC :: Word16,
    eCatchType :: Word16 }
--  deriving (Eq, Show)

derive instance eqCodeException :: Eq CodeException

derive instance genericCodeException :: Generic CodeException _

instance showCodeException :: Show CodeException where
  show = genericShow

