module JVM.Assembler where

import Prelude

import Data.Binary.Types (Word16(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import JVM.ClassFile (AttributesFile)

data IMM =
    I0     -- ^ 0
  | I1     -- ^ 1
  | I2     -- ^ 2
  | I3     -- ^ 3

derive instance eqIMM :: Eq IMM
derive instance ordIMM :: Ord IMM

derive instance genericIMM :: Generic IMM _

instance showIMM :: Show IMM where
  show = genericShow

-- | Comparation operation type. Not all CMP instructions support all operations.
data CMP =
    C_EQ
  | C_NE
  | C_LT
  | C_GE
  | C_GT
  | C_LE

derive instance eqCMP :: Eq CMP
derive instance ordCMP :: Ord CMP

derive instance genericCMP :: Generic CMP _

instance showCMP :: Show CMP where
  show = genericShow

data Code = Code {
    codeStackSize :: Word16,
    codeMaxLocals :: Word16,
    codeLength :: Word32,
    codeInstructions :: [Instruction],
    codeExceptionsN :: Word16,
    codeExceptions :: [CodeException],
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

-- | JVM array type (primitive types)
data ArrayType =
    T_BOOLEAN  -- ^ 4
  | T_CHAR     -- ^ 5
  | T_FLOAT    -- ^ 6
  | T_DOUBLE   -- ^ 7
  | T_BYTE     -- ^ 8
  | T_SHORT    -- ^ 9
  | T_INT      -- ^ 10
  | T_LONG     -- ^ 11
  -- deriving (Eq, Show, Enum)

derive instance eqArrayType :: Eq ArrayType

derive instance genericArrayType :: Generic ArrayType _

instance showArrayType :: Show ArrayType where
  show = genericShow
