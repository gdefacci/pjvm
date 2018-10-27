module JVM.Instruction where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (class MonadState)
import Data.Array as A
import Data.Binary.Binary (class Binary, putFoldable, get, put, putPad)
import Data.Binary.Decoder (Decoder, ParserError(..), ParserState, fail, getOffset, skip)
import Data.Binary.Put (Put, putFail)
import Data.Binary.Types (Word16, Word32, Word8(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality, cardinality, defaultFromEnum, defaultToEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
import Data.UInt (fromInt, toInt)

data IMM =
    I0     -- ^ 0
  | I1     -- ^ 1
  | I2     -- ^ 2
  | I3     -- ^ 3

  -- | Comparation operation type. Not all CMP instructions support all operations.
data CMP =
    C_EQ
  | C_NE
  | C_LT
  | C_GE
  | C_GT
  | C_LE

data ILFD = ILFD_I | ILFD_L | ILFD_F | ILFD_D
data ILFDA = ILFDA_I | ILFDA_L | ILFDA_F | ILFDA_D | ILFDA_A
data T32 = T32_I | T32_F
data JTA = JTA_I | JTA_L | JTA_F | JTA_D | JTA_A | JTA_B | JTA_C | JTA_S
data JT  = JT_I | JT_L | JT_F | JT_D | JT_B | JT_C | JT_S
data IL = IL_I | IL_L

data StackOperation = Pop | Pop2 | Dup | Dup_x1 | Dup_x2 | Dup2 | Dup2_x1 | Dup2_x2 | Swap
data NumOperation = Add | Sub | Mul | Div | Rem | Neg
data BitOperation = Shl | Shr | Ushr | And | Or | Xor

data Instruction =
    NOP
  | ACONST_NULL
  | ICONST_M1
  | CONST_0 ILFD
  | CONST_1 ILFD
  | CONST_2 T32
  | ICONST_3
  | ICONST_4
  | ICONST_5
  | BIPUSH Word8
  | SIPUSH Word16
  | LDC1 Word8
  | LDC2 Word16
  | LDC2W Word16
  | LOAD ILFDA Word8
  | LOAD_ ILFDA IMM
  | ALOAD JTA
  | STORE ILFDA Word8
  | STORE_ ILFDA IMM
  | ASTORE JTA
  | STACK_OP StackOperation
  | NUM_OP NumOperation ILFD
  | BIT_OP BitOperation IL
  | IINC Word8 Word8
  | CONVERT ILFD JT
  | LCMP
  | FCMP CMP
  | DCMP CMP
  | IF CMP Word16
  | IF_ICMP CMP Word16
  | IF_ACMP CMP Word16
  | GOTO Word16
  | JSR Word16
  | RET
  | TABLESWITCH Word8 Word32 Word32 Word32 (Array Word32)
  | LOOKUPSWITCH Word8 Word32 Word32 (Array (Tuple Word32 Word32))
  | RET_ ILFDA
  | RETURN
  | GETSTATIC Word16
  | PUTSTATIC Word16
  | GETFIELD Word16
  | PUTFIELD Word16
  | INVOKEVIRTUAL Word16
  | INVOKESPECIAL Word16
  | INVOKESTATIC Word16
  | INVOKEINTERFACE Word16 Word8
  | NEW Word16
  | NEWARRAY Word8
  | ANEWARRAY Word16
  | ARRAYLENGTH
  | ATHROW
  | CHECKCAST Word16
  | INSTANCEOF Word16
  | MONITORENTER
  | MONITOREXIT
  | WIDE Word8 Instruction
  | MULTINANEWARRAY Word16 Word8
  | IFNULL Word16
  | IFNONNULL Word16
  | GOTO_W Word32
  | JSR_W Word32

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

decodeEnum :: forall m a. Bounded a
                      => Enum a
                      => MonadThrow ParserError m
                      => MonadState ParserState m
                      => String
                      -> Int
                      -> m a
decodeEnum desc v =
  case defaultToEnum v of
    Nothing -> fail $ \offset -> GenericParserError { offset, message: "Cant convert to " <> desc <> " byte: " <> show v}
    (Just r) -> pure r

-- | Parse opcode with immediate constant
imm :: forall a. Int                   -- ^ Base opcode
    -> (IMM -> a)    -- ^ Instruction constructor
    -> Int                   -- ^ Opcode to parse
    -> Decoder a
imm base constr x =
  constr <$> decodeEnum "IMM" (x - base)

-- | Put opcode with immediate constant
putImm :: forall t. Bounded t
        => Enum t
        => Int                  -- ^ Base opcode
        -> t
        -> IMM                  -- ^ Constant to add to opcode
        -> Put
putImm base t i =
  let sizeOfIMM = unwrap (cardinality :: Cardinality IMM)
      tord = defaultFromEnum t
      tdelta =  tord * sizeOfIMM
  in putByte $ base + tdelta + (defaultFromEnum i)

atype2byte :: ArrayType -> Word8
atype2byte T_BOOLEAN  = Word8 $ fromInt 4
atype2byte T_CHAR     = Word8 $ fromInt 5
atype2byte T_FLOAT    = Word8 $ fromInt 6
atype2byte T_DOUBLE   = Word8 $ fromInt 7
atype2byte T_BYTE     = Word8 $ fromInt 8
atype2byte T_SHORT    = Word8 $ fromInt 9
atype2byte T_INT      = Word8 $ fromInt 10
atype2byte T_LONG     = Word8 $ fromInt 11

byte2atype :: Word8 -> Decoder ArrayType
byte2atype (Word8 n) =
  case toInt n of
    4  -> pure T_BOOLEAN
    5  -> pure T_CHAR
    6  -> pure T_FLOAT
    7  -> pure T_DOUBLE
    8  -> pure T_BYTE
    9  -> pure T_SHORT
    10 -> pure T_INT
    11 -> pure T_LONG
    x -> fail $ \offset -> GenericParserError { offset, message: "Unknown array type byte: " <> show x}

inRange :: forall a. Ord a => a -> a -> a -> Boolean
inRange l h v = (v >= l) && (v <= h)

instance binaryArrayType :: Binary ArrayType where
  get = do
    x <- get
    byte2atype x

  put t = put (atype2byte t)

-- | Put opcode with one argument
put1t :: forall a t. (Binary a)
        => Bounded t
        => Enum t
        => Int                    -- ^ Opcode
        -> t
        -> a                      -- ^ First argument
        -> Put
put1t code t x =
  put (Word8 $ fromInt (code + (defaultFromEnum t))) <>
  put x

put1 :: forall a. (Binary a)
      => Int                    -- ^ Opcode
      -> a                      -- ^ First argument
      -> Put
put1 code x =
  put (Word8 $ fromInt code) <>
  put x

put2 :: forall a b. Binary a
      => Binary b
      => Int                      -- ^ Opcode
      -> a                        -- ^ First argument
      -> b                        -- ^ Second argument
      -> Put
put2 code x y =
  put (Word8 $ fromInt code) <>
  put x <>
  put y

putByte :: Int -> Put
putByte i = put $ Word8 $ fromInt i

--NumOperation ILFD
putOp :: forall op t. Enum op => Enum t => BoundedEnum t => Int -> op -> t -> Put
putOp base op t =
  let ifldSize = unwrap $ (cardinality :: Cardinality t)
  in putByte (base + ((defaultFromEnum op) * ifldSize) + (defaultFromEnum t))


instance binaryInstruction :: Binary Instruction where
  put  NOP         = putByte 0
  put  ACONST_NULL = putByte 1
  put  ICONST_M1   = putByte 2

  put  (CONST_0 ILFD_I)   = putByte 3
  put  (CONST_1 ILFD_I)   = putByte 4
  put  (CONST_2 T32_I)    = putByte 5
  put  ICONST_3           = putByte 6
  put  ICONST_4           = putByte 7
  put  ICONST_5           = putByte 8
  put  (CONST_2 T32_F)    = putByte 9
  put  (CONST_0 ILFD_L)   = putByte 10
  put  (CONST_1 ILFD_L)   = putByte 11
  put  (CONST_0 ILFD_F)   = putByte 12
  put  (CONST_1 ILFD_F)   = putByte 13
  put  (CONST_0 ILFD_D)   = putByte 14
  put  (CONST_1 ILFD_D)   = putByte 15

  put (BIPUSH x)   = put1 16 x
  put (SIPUSH x)   = put1 17 x
  put (LDC1 x)     = put1 18 x
  put (LDC2 x)     = put1 19 x
  put (LDC2W x)    = put1 20 x

  put (LOAD t x)    = put1t 21 t x

  put (LOAD_ t i)   = putImm 26 t i

  put  (ALOAD t)     = putByte $ 46 + (defaultFromEnum t)

  put (STORE t x)   = put1t 54 t x

  put (STORE_ t i)   = putImm 59 t i

  put (ASTORE t)   = putByte $ 79 + (defaultFromEnum t)

  put  (STACK_OP Pop     ) = putByte 87
  put  (STACK_OP Pop2    ) = putByte 88
  put  (STACK_OP Dup     ) = putByte 89
  put  (STACK_OP Dup_x1  ) = putByte 90
  put  (STACK_OP Dup_x2  ) = putByte 91
  put  (STACK_OP Dup2    ) = putByte 92
  put  (STACK_OP Dup2_x1 ) = putByte 93
  put  (STACK_OP Dup2_x2 ) = putByte 94
  put  (STACK_OP Swap    ) = putByte 95

  put  (NUM_OP op t) = putOp 96 op t

  put  (BIT_OP op t) = putOp 120 op t

  put (IINC x y)      = put2 132 x y

  -- CONVERT ILFD JT
  put  (CONVERT ILFD_I JT_L) = putByte 133
  put  (CONVERT ILFD_I JT_F) = putByte 134
  put  (CONVERT ILFD_I JT_D) = putByte 135
  put  (CONVERT ILFD_L JT_I) = putByte 136
  put  (CONVERT ILFD_L JT_F) = putByte 137
  put  (CONVERT ILFD_L JT_D) = putByte 138
  put  (CONVERT ILFD_F JT_I) = putByte 139
  put  (CONVERT ILFD_F JT_L) = putByte 140
  put  (CONVERT ILFD_F JT_D) = putByte 141
  put  (CONVERT ILFD_D JT_I) = putByte 142
  put  (CONVERT ILFD_D JT_L) = putByte 143
  put  (CONVERT ILFD_D JT_F) = putByte 144
  put  (CONVERT ILFD_I JT_B) = putByte 145
  put  (CONVERT ILFD_I JT_C) = putByte 146
  put  (CONVERT ILFD_I JT_S) = putByte 147
  put  c @ (CONVERT _ _) = putFail $ \_ -> "No such instruction: Conversion " <> show c

  put  LCMP           = putByte 148
  put (FCMP C_LT)     = putByte 149
  put (FCMP C_GT)     = putByte 150
  put (FCMP c)        = putFail $ \_ -> "No such instruction: FCMP " <> show c
  put (DCMP C_LT)     = putByte 151
  put (DCMP C_GT)     = putByte 152
  put (DCMP c)        = putFail $ \_ -> "No such instruction: DCMP " <> show c
  put (IF c x)        = putByte (153 + (defaultFromEnum c)) <> put x
  put (IF_ACMP C_EQ x) = put1 165 x
  put (IF_ACMP C_NE x) = put1 166 x
  put (IF_ACMP c _)   = putFail $ \_ -> "No such instruction: IF_ACMP " <> show c
  put (IF_ICMP c x)   = putByte (159 + (defaultFromEnum c)) <> put x
  put (GOTO x)        = put1 167 x
  put (JSR x)         = put1 168 x
  put  RET            = putByte 169
  put (TABLESWITCH _ def low high offs) =
    put (Word8 $ fromInt 170) <>
    putPad padding (Word8 $ fromInt 0) <>
    put low <>
    put high <>
    putFoldable offs
  put (LOOKUPSWITCH _ def n pairs) =
    put (Word8 $ fromInt 171) <>
    putPad padding (Word8 $ fromInt 0) <>
    put def <>
    put n <>
    putFoldable pairs

  put (RET_ t)        = putByte $ 172 + (defaultFromEnum t)

  put  RETURN         = putByte 177
  put (GETSTATIC x)   = put1 178 x
  put (PUTSTATIC x)   = put1 179 x
  put (GETFIELD x)    = put1 180 x
  put (PUTFIELD x)    = put1 181 x
  put (INVOKEVIRTUAL x)     = put1 182 x
  put (INVOKESPECIAL x)     = put1 183 x
  put (INVOKESTATIC x)      = put1 184 x
  put (INVOKEINTERFACE x c) = put2 185 x c <> putByte 0
  put (NEW x)         = put1 187 x
  put (NEWARRAY x)    = put1 188 x
  put (ANEWARRAY x)   = put1 189 x
  put  ARRAYLENGTH    = putByte 190
  put  ATHROW         = putByte 191
  put (CHECKCAST x)   = put1 192 x
  put (INSTANCEOF x)  = put1 193 x
  put  MONITORENTER   = putByte 194
  put  MONITOREXIT    = putByte 195
  put (WIDE x inst)   = put2 196 x inst
  put (MULTINANEWARRAY x y) = put2 197 x y
  put (IFNULL x)      = put1 198 x
  put (IFNONNULL x)   = put1 199 x
  put (GOTO_W x)      = put1 200 x
  put (JSR_W x)       = put1 201 x

  get = do
    (Word8 ch) <- get
    let c = toInt ch
    case c of
      0 -> pure NOP
      1 -> pure ACONST_NULL
      2 -> pure ICONST_M1

      3  -> pure (CONST_0 ILFD_I)
      4  -> pure (CONST_1 ILFD_I)
      5  -> pure (CONST_2 T32_I)
      6  -> pure ICONST_3
      7  -> pure ICONST_4
      8  -> pure ICONST_5
      9  -> pure (CONST_2 T32_F)
      10 -> pure (CONST_0 ILFD_L)
      11 -> pure (CONST_1 ILFD_L)
      12 -> pure (CONST_0 ILFD_F)
      13 -> pure (CONST_1 ILFD_F)
      14 -> pure (CONST_0 ILFD_D)
      15 -> pure (CONST_1 ILFD_D)

      16 -> BIPUSH <$> get
      17 -> SIPUSH <$> get
      18 -> LDC1 <$> get
      19 -> LDC2 <$> get
      20 -> LDC2W <$> get

      21 -> (LOAD ILFDA_I) <$> get
      22 -> (LOAD ILFDA_L) <$> get
      23 -> (LOAD ILFDA_F) <$> get
      24 -> (LOAD ILFDA_D) <$> get
      25 -> (LOAD ILFDA_A) <$> get

      46 -> pure $ ALOAD JTA_I
      47 -> pure $ ALOAD JTA_L
      48 -> pure $ ALOAD JTA_F
      49 -> pure $ ALOAD JTA_D
      50 -> pure $ ALOAD JTA_A
      51 -> pure $ ALOAD JTA_B
      52 -> pure $ ALOAD JTA_C
      53 -> pure $ ALOAD JTA_S

      54 -> (STORE ILFDA_I) <$> get
      55 -> (STORE ILFDA_L) <$> get
      56 -> (STORE ILFDA_F) <$> get
      57 -> (STORE ILFDA_D) <$> get
      58 -> (STORE ILFDA_A) <$> get

      79 -> pure $ ASTORE JTA_I
      80 -> pure $ ASTORE JTA_L
      81 -> pure $ ASTORE JTA_F
      82 -> pure $ ASTORE JTA_D
      83 -> pure $ ASTORE JTA_A
      84 -> pure $ ASTORE JTA_B
      85 -> pure $ ASTORE JTA_C
      86 -> pure $ ASTORE JTA_S

      87 -> pure $ STACK_OP Pop
      88 -> pure $ STACK_OP Pop2
      89 -> pure $ STACK_OP Dup
      90 -> pure $ STACK_OP Dup_x1
      91 -> pure $ STACK_OP Dup_x2
      92 -> pure $ STACK_OP Dup2
      93 -> pure $ STACK_OP Dup2_x1
      94 -> pure $ STACK_OP Dup2_x2
      95 -> pure $ STACK_OP Swap

      96  -> pure $ NUM_OP Add ILFD_I
      97  -> pure $ NUM_OP Add ILFD_L
      98  -> pure $ NUM_OP Add ILFD_F
      99  -> pure $ NUM_OP Add ILFD_D
      100 -> pure $ NUM_OP Sub ILFD_I
      101 -> pure $ NUM_OP Sub ILFD_L
      102 -> pure $ NUM_OP Sub ILFD_F
      103 -> pure $ NUM_OP Sub ILFD_D
      104 -> pure $ NUM_OP Mul ILFD_I
      105 -> pure $ NUM_OP Mul ILFD_L
      106 -> pure $ NUM_OP Mul ILFD_F
      107 -> pure $ NUM_OP Mul ILFD_D
      108 -> pure $ NUM_OP Div ILFD_I
      109 -> pure $ NUM_OP Div ILFD_L
      110 -> pure $ NUM_OP Div ILFD_F
      111 -> pure $ NUM_OP Div ILFD_D
      112 -> pure $ NUM_OP Rem ILFD_I
      113 -> pure $ NUM_OP Rem ILFD_L
      114 -> pure $ NUM_OP Rem ILFD_F
      115 -> pure $ NUM_OP Rem ILFD_D
      116 -> pure $ NUM_OP Neg ILFD_I
      117 -> pure $ NUM_OP Neg ILFD_L
      118 -> pure $ NUM_OP Neg ILFD_F
      119 -> pure $ NUM_OP Neg ILFD_D

      120 -> pure $ BIT_OP Shl  IL_I
      121 -> pure $ BIT_OP Shl  IL_L
      122 -> pure $ BIT_OP Shr  IL_I
      123 -> pure $ BIT_OP Shr  IL_L
      124 -> pure $ BIT_OP Ushr IL_I
      125 -> pure $ BIT_OP Ushr IL_L
      126 -> pure $ BIT_OP And  IL_I
      127 -> pure $ BIT_OP And  IL_L
      128 -> pure $ BIT_OP Or   IL_I
      129 -> pure $ BIT_OP Or   IL_L
      130 -> pure $ BIT_OP Xor  IL_I
      131 -> pure $ BIT_OP Xor  IL_L

      132 -> IINC <$> get <*> get

      133 -> pure $ CONVERT ILFD_I JT_L
      134 -> pure $ CONVERT ILFD_I JT_F
      135 -> pure $ CONVERT ILFD_I JT_D
      136 -> pure $ CONVERT ILFD_L JT_I
      137 -> pure $ CONVERT ILFD_L JT_F
      138 -> pure $ CONVERT ILFD_L JT_D
      139 -> pure $ CONVERT ILFD_F JT_I
      140 -> pure $ CONVERT ILFD_F JT_L
      141 -> pure $ CONVERT ILFD_F JT_D
      142 -> pure $ CONVERT ILFD_D JT_I
      143 -> pure $ CONVERT ILFD_D JT_L
      144 -> pure $ CONVERT ILFD_D JT_F
      145 -> pure $ CONVERT ILFD_I JT_B
      146 -> pure $ CONVERT ILFD_I JT_C
      147 -> pure $ CONVERT ILFD_I JT_S

      148 -> pure LCMP

      149 -> pure $ FCMP C_LT
      150 -> pure $ FCMP C_GT
      151 -> pure $ DCMP C_LT
      152 -> pure $ DCMP C_GT
      165 -> IF_ACMP C_EQ <$> get
      166 -> IF_ACMP C_NE <$> get
      167 -> GOTO <$> get
      168 -> JSR <$> get
      169 -> pure RET
      170 -> do
             offset <- getOffset
             let pads = padding offset
             skip pads
             def <- get
             low <- get
             high <- get
             offs <- A.fromFoldable <$> replicateM ((toInt $ unwrap high) - (toInt $ unwrap low) + 1) get
             pure $ TABLESWITCH (Word8 $ fromInt pads) def low high offs
      171 -> do
             offset <- getOffset
             let pads = padding offset
             skip pads
             def <- get
             n <- get
             pairs <- A.fromFoldable <$> replicateM (toInt $ unwrap n) get
             pure $ LOOKUPSWITCH (Word8 $ fromInt pads) def n pairs

      172 -> pure $ RET_ ILFDA_I
      173 -> pure $ RET_ ILFDA_L
      174 -> pure $ RET_ ILFDA_F
      175 -> pure $ RET_ ILFDA_D
      176 -> pure $ RET_ ILFDA_A

      177 -> pure RETURN
      178 -> GETSTATIC <$> get
      179 -> PUTSTATIC <$> get
      180 -> GETFIELD <$> get
      181 -> PUTFIELD <$> get
      182 -> INVOKEVIRTUAL <$> get
      183 -> INVOKESPECIAL <$> get
      184 -> INVOKESTATIC <$> get
      185 -> (INVOKEINTERFACE <$> get <*> get) <* skip 1
      187 -> NEW <$> get
      188 -> NEWARRAY <$> get
      189 -> ANEWARRAY <$> get
      190 -> pure ARRAYLENGTH
      191 -> pure ATHROW
      192 -> CHECKCAST <$> get
      193 -> INSTANCEOF <$> get
      194 -> pure MONITORENTER
      195 -> pure MONITOREXIT
      196 -> WIDE <$> get <*> get
      197 -> MULTINANEWARRAY <$> get <*> get
      198 -> IFNULL <$> get
      199 -> IFNONNULL <$> get
      200 -> GOTO_W <$> get
      201 -> JSR_W <$> get
      _ | inRange 59 62 c  -> imm 59 (STORE_ ILFDA_I) c
        | inRange 63 66 c  -> imm 63 (STORE_ ILFDA_L) c
        | inRange 67 70 c  -> imm 67 (STORE_ ILFDA_F) c
        | inRange 71 74 c  -> imm 71 (STORE_ ILFDA_D) c
        | inRange 75 78 c  -> imm 75 (STORE_ ILFDA_A) c
        | inRange 26 29 c  -> imm 26 (LOAD_ ILFDA_I)  c
        | inRange 30 33 c  -> imm 30 (LOAD_ ILFDA_L)  c
        | inRange 34 37 c  -> imm 34 (LOAD_ ILFDA_F)  c
        | inRange 38 41 c  -> imm 38 (LOAD_ ILFDA_D)  c
        | inRange 42 45 c  -> imm 42 (LOAD_ ILFDA_A)  c
        | inRange 15 158 c -> IF <$> (decodeEnum "CMP" $ c- 153) <*> get
        | inRange 15 164 c -> IF_ICMP <$> (decodeEnum "CMP" $ c- 159) <*> get
        | otherwise -> fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}

-- | Calculate padding for current bytecode offset (cf. TABLESWITCH and LOOKUPSWITCH)
-- padding :: (Integral a, Integral b) => a -> b
padding :: Int -> Int
padding offset = (4 - offset) `mod` 4


derive instance eqIMM :: Eq IMM
derive instance ordIMM :: Ord IMM

derive instance genericIMM :: Generic IMM _

instance showIMM :: Show IMM where
  show = genericShow

instance boundedIMM :: Bounded IMM where
  top = genericTop
  bottom = genericBottom

instance enumIMM :: Enum IMM where
  succ = genericSucc
  pred = genericPred

instance boundedEnumIMM :: BoundedEnum IMM where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance eqCMP :: Eq CMP
derive instance ordCMP :: Ord CMP
derive instance genericCMP :: Generic CMP _

instance showCMP :: Show CMP where
  show = genericShow

instance boundedCMP :: Bounded CMP where
  top = genericTop
  bottom = genericBottom

instance enumCMP :: Enum CMP where
  succ = genericSucc
  pred = genericPred

derive instance eqILFD :: Eq ILFD
derive instance ordILFD :: Ord ILFD
derive instance genericILFD :: Generic ILFD _

instance showILFD :: Show ILFD where
  show = genericShow

instance boundedILFD :: Bounded ILFD where
  top = genericTop
  bottom = genericBottom

instance enumILFD :: Enum ILFD where
  succ = genericSucc
  pred = genericPred

instance boundedEnumILFD :: BoundedEnum ILFD where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance eqILFDA :: Eq ILFDA
derive instance ordILFDA :: Ord ILFDA
derive instance genericILFDA :: Generic ILFDA _

instance showILFDA :: Show ILFDA where
  show = genericShow

instance boundedILFDA :: Bounded ILFDA where
  top = genericTop
  bottom = genericBottom

instance enumILFDA :: Enum ILFDA where
  succ = genericSucc
  pred = genericPred

derive instance eqT32 :: Eq T32
derive instance ordT32 :: Ord T32
derive instance genericT32 :: Generic T32 _

instance showT32 :: Show T32 where
  show = genericShow

instance enumT32 :: Enum T32 where
  succ = genericSucc
  pred = genericPred

derive instance eqJTA :: Eq JTA
derive instance ordJTA :: Ord JTA
derive instance genericJTA :: Generic JTA _

instance showJTA :: Show JTA where
  show = genericShow

instance enumJTA :: Enum JTA where
  succ = genericSucc
  pred = genericPred

derive instance eqJT :: Eq JT
derive instance ordJT :: Ord JT
derive instance genericJT :: Generic JT _

instance showJT :: Show JT where
  show = genericShow

instance enumJT :: Enum JT where
  succ = genericSucc
  pred = genericPred

derive instance eqIL :: Eq IL
derive instance ordIL :: Ord IL
derive instance genericIL :: Generic IL _

instance showIL :: Show IL where
  show = genericShow

instance boundedIL :: Bounded IL where
  top = genericTop
  bottom = genericBottom

instance enumIL :: Enum IL where
  succ = genericSucc
  pred = genericPred

instance boundedEnumIL :: BoundedEnum IL where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

derive instance eqNumOperation :: Eq NumOperation
derive instance ordNumOperation :: Ord NumOperation
derive instance genericNumOperation :: Generic NumOperation _

instance showNumOperation :: Show NumOperation where
  show = genericShow

instance enumNumOperation :: Enum NumOperation where
  succ = genericSucc
  pred = genericPred

derive instance eqStackOperation :: Eq StackOperation
derive instance ordStackOperation :: Ord StackOperation
derive instance genericStackOperation :: Generic StackOperation _

instance showStackOperation :: Show StackOperation where
  show = genericShow

instance enumStackOperation :: Enum StackOperation where
  succ = genericSucc
  pred = genericPred

derive instance eqBitOperation :: Eq BitOperation
derive instance ordBitOperation :: Ord BitOperation
derive instance genericBitOperation :: Generic BitOperation _

instance showBitOperation :: Show BitOperation where
  show = genericShow

instance enumBitOperation :: Enum BitOperation where
  succ = genericSucc
  pred = genericPred

derive instance eqInstruction :: Eq Instruction

derive instance genericInstruction :: Generic Instruction _

instance showIstr :: Show Instruction where
  show instr = genericShow instr

derive instance eqArrayType :: Eq ArrayType

derive instance genericArrayType :: Generic ArrayType _

instance showArrayType :: Show ArrayType where
  show = genericShow
