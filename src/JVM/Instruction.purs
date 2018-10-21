module JVM.Instruction where

import Data.Binary.Types
import Prelude

import Data.Binary.Binary (class Binary, putFoldable, get, put, putPad)
import Data.Binary.Put (Put(..), putFail)
import Data.Binary.Decoder (Decoder(..), ParserError(..), fail, getOffset, skip)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt, toInt)

import Data.Array as A

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

immOrdValue :: IMM -> Int
immOrdValue I0 = 0
immOrdValue I1 = 1
immOrdValue I2 = 2
immOrdValue I3 = 3

toIMMEnum :: Int -> Maybe IMM
toIMMEnum 0 = Just I0
toIMMEnum 1 = Just I1
toIMMEnum 2 = Just I2
toIMMEnum 3 = Just I3
toIMMEnum _ = Nothing

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

cmpOrd :: CMP -> Int
cmpOrd C_EQ = 0
cmpOrd C_NE = 1
cmpOrd C_LT = 2
cmpOrd C_GE = 3
cmpOrd C_GT = 4
cmpOrd C_LE = 5

toCmp :: Int -> Maybe CMP
toCmp 0 = Just C_EQ
toCmp 1 = Just C_NE
toCmp 2 = Just C_LT
toCmp 3 = Just C_GE
toCmp 4 = Just C_GT
toCmp 5 = Just C_LE
toCmp _ = Nothing


-- | JVM instruction set. For comments, see JVM specification.
data Instruction =
    NOP            -- ^ 0
  | ACONST_NULL    -- ^ 1
  | ICONST_M1      -- ^ 2
  | ICONST_0       -- ^ 3
  | ICONST_1       -- ^ 4
  | ICONST_2       -- ^ 5
  | ICONST_3       -- ^ 6
  | ICONST_4       -- ^ 7
  | ICONST_5       -- ^ 8
  | LCONST_0       -- ^ 9
  | LCONST_1       -- ^ 10
  | FCONST_0       -- ^ 11
  | FCONST_1       -- ^ 12
  | FCONST_2       -- ^ 13
  | DCONST_0       -- ^ 14
  | DCONST_1       -- ^ 15
  | BIPUSH Word8   -- ^ 16
  | SIPUSH Word16  -- ^ 17
  | LDC1 Word8     -- ^ 18
  | LDC2 Word16    -- ^ 19
  | LDC2W Word16   -- ^ 20
  | ILOAD Word8    -- ^ 21
  | LLOAD Word8    -- ^ 22
  | FLOAD Word8    -- ^ 23
  | DLOAD Word8    -- ^ 24
  | ALOAD Word8    -- ^ 25
  | ILOAD_ IMM     -- ^ 26, 27, 28, 29
  | LLOAD_ IMM     -- ^ 30, 31, 32, 33
  | FLOAD_ IMM     -- ^ 34, 35, 36, 37
  | DLOAD_ IMM     -- ^ 38, 39, 40, 41
  | ALOAD_ IMM     -- ^ 42, 43, 44, 45
  | IALOAD         -- ^ 46
  | LALOAD         -- ^ 47
  | FALOAD         -- ^ 48
  | DALOAD         -- ^ 49
  | AALOAD         -- ^ 50
  | BALOAD         -- ^ 51
  | CALOAD         -- ^ 52
  | SALOAD         -- ^ 53
  | ISTORE Word8   -- ^ 54
  | LSTORE Word8   -- ^ 55
  | FSTORE Word8   -- ^ 56
  | DSTORE Word8   -- ^ 57
  | ASTORE Word8   -- ^ 58
  | ISTORE_ IMM    -- ^ 59, 60, 61, 62
  | LSTORE_ IMM    -- ^ 63, 64, 65, 66
  | FSTORE_ IMM    -- ^ 67, 68, 69, 70
  | DSTORE_ IMM    -- ^ 71, 72, 73, 74
  | ASTORE_ IMM    -- ^ 75, 76, 77, 78
  | IASTORE        -- ^ 79
  | LASTORE        -- ^ 80
  | FASTORE        -- ^ 81
  | DASTORE        -- ^ 82
  | AASTORE        -- ^ 83
  | BASTORE        -- ^ 84
  | CASTORE        -- ^ 85
  | SASTORE        -- ^ 86
  | POP            -- ^ 87
  | POP2           -- ^ 88
  | DUP            -- ^ 89
  | DUP_X1         -- ^ 90
  | DUP_X2         -- ^ 91
  | DUP2           -- ^ 92
  | DUP2_X1        -- ^ 93
  | DUP2_X2        -- ^ 94
  | SWAP           -- ^ 95
  | IADD           -- ^ 96
  | LADD           -- ^ 97
  | FADD           -- ^ 98
  | DADD           -- ^ 99
  | ISUB           -- ^ 100
  | LSUB           -- ^ 101
  | FSUB           -- ^ 102
  | DSUB           -- ^ 103
  | IMUL           -- ^ 104
  | LMUL           -- ^ 105
  | FMUL           -- ^ 106
  | DMUL           -- ^ 107
  | IDIV           -- ^ 108
  | LDIV           -- ^ 109
  | FDIV           -- ^ 110
  | DDIV           -- ^ 111
  | IREM           -- ^ 112
  | LREM           -- ^ 113
  | FREM           -- ^ 114
  | DREM           -- ^ 115
  | INEG           -- ^ 116
  | LNEG           -- ^ 117
  | FNEG           -- ^ 118
  | DNEG           -- ^ 119
  | ISHL           -- ^ 120
  | LSHL           -- ^ 121
  | ISHR           -- ^ 122
  | LSHR           -- ^ 123
  | IUSHR          -- ^ 124
  | LUSHR          -- ^ 125
  | IAND           -- ^ 126
  | LAND           -- ^ 127
  | IOR            -- ^ 128
  | LOR            -- ^ 129
  | IXOR           -- ^ 130
  | LXOR           -- ^ 131
  | IINC Word8 Word8       -- ^ 132
  | I2L                    -- ^ 133
  | I2F                    -- ^ 134
  | I2D                    -- ^ 135
  | L2I                    -- ^ 136
  | L2F                    -- ^ 137
  | L2D                    -- ^ 138
  | F2I                    -- ^ 139
  | F2L                    -- ^ 140
  | F2D                    -- ^ 141
  | D2I                    -- ^ 142
  | D2L                    -- ^ 143
  | D2F                    -- ^ 144
  | I2B                    -- ^ 145
  | I2C                    -- ^ 146
  | I2S                    -- ^ 147
  | LCMP                   -- ^ 148
  | FCMP CMP               -- ^ 149, 150
  | DCMP CMP               -- ^ 151, 152
  | IF CMP Word16          -- ^ 153, 154, 155, 156, 157, 158
  | IF_ICMP CMP Word16     -- ^ 159, 160, 161, 162, 163, 164
  | IF_ACMP CMP Word16     -- ^ 165, 166
  | GOTO Word16            -- ^ 167
  | JSR Word16             -- ^ 168
  | RET                    -- ^ 169
  | TABLESWITCH Word8 Word32 Word32 Word32 (Array Word32)     -- ^ 170
  | LOOKUPSWITCH Word8 Word32 Word32 (Array (Tuple Word32 Word32)) -- ^ 171
  | IRETURN                -- ^ 172
  | LRETURN                -- ^ 173
  | FRETURN                -- ^ 174
  | DRETURN                -- ^ 175
  | ARETURN                -- ^ 176
  | RETURN                 -- ^ 177
  | GETSTATIC Word16       -- ^ 178
  | PUTSTATIC Word16       -- ^ 179
  | GETFIELD Word16        -- ^ 180
  | PUTFIELD Word16        -- ^ 181
  | INVOKEVIRTUAL Word16   -- ^ 182
  | INVOKESPECIAL Word16   -- ^ 183
  | INVOKESTATIC Word16    -- ^ 184
  | INVOKEINTERFACE Word16 Word8 -- ^ 185
  | NEW Word16             -- ^ 187
  | NEWARRAY Word8         -- ^ 188, see @ArrayType@
  | ANEWARRAY Word16       -- ^ 189
  | ARRAYLENGTH            -- ^ 190
  | ATHROW                 -- ^ 191
  | CHECKCAST Word16       -- ^ 192
  | INSTANCEOF Word16      -- ^ 193
  | MONITORENTER           -- ^ 194
  | MONITOREXIT            -- ^ 195
  | WIDE Word8 Instruction -- ^ 196
  | MULTINANEWARRAY Word16 Word8 -- ^ 197
  | IFNULL Word16          -- ^ 198
  | IFNONNULL Word16       -- ^ 199
  | GOTO_W Word32          -- ^ 200
  | JSR_W Word32           -- ^ 201

derive instance eqInstruction :: Eq Instruction

derive instance genericInstruction :: Generic Instruction _

instance showIstr :: Show Instruction where
  show instr = genericShow instr

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

-- | Parse opcode with immediate constant
imm :: Int                   -- ^ Base opcode
    -> (IMM -> Instruction)    -- ^ Instruction constructor
    -> Int                   -- ^ Opcode to parse
    -> Decoder Instruction
imm base constr x =
  let immValue = x - base
  in case toIMMEnum immValue of
    Nothing -> fail $ \offset -> GenericParserError { offset, message: "Unknown IMM byte: " <> show immValue}
    (Just r) -> pure $ constr r
  -- pure $ constr $ toEnum $ fromIntegral (x-base)

-- | Put opcode with immediate constant
putImm :: Int                  -- ^ Base opcode
       -> IMM                  -- ^ Constant to add to opcode
       -> Put
putImm base i = put $ Word8 $ fromInt $ base + (immOrdValue i)

atype2byte :: ArrayType -> Word8
atype2byte T_BOOLEAN  = Word8 $ fromInt 4
atype2byte T_CHAR     = Word8 $ fromInt 5
atype2byte T_FLOAT    = Word8 $ fromInt 6
atype2byte T_DOUBLE   = Word8 $ fromInt 7
atype2byte T_BYTE     = Word8 $ fromInt 8
atype2byte T_SHORT    = Word8 $ fromInt 9
atype2byte T_INT      = Word8 $ fromInt 10
atype2byte T_LONG     = Word8 $ fromInt 11

byte2atype :: Word8 -> Maybe ArrayType
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
    _ -> Nothing

inRange :: forall a. Ord a => a -> a -> a -> Boolean
inRange l h v = (v >= l) && (v <= h)

instance binaryArrayType :: Binary ArrayType where
  get = do
    x <- get
    case byte2atype x of
      Nothing -> fail $ \offset -> GenericParserError { offset, message: "Unknown array type byte: " <> show x}
      (Just r) -> pure r

  put t = put (atype2byte t)

-- | Put opcode with one argument
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

instance binaryInstruction :: Binary Instruction where
  put  NOP         = put $ Word8 $ fromInt 0
  put  ACONST_NULL = put $ Word8 $ fromInt 1
  put  ICONST_M1   = put $ Word8 $ fromInt 2
  put  ICONST_0    = put $ Word8 $ fromInt 3
  put  ICONST_1    = put $ Word8 $ fromInt 4
  put  ICONST_2    = put $ Word8 $ fromInt 5
  put  ICONST_3    = put $ Word8 $ fromInt 6
  put  ICONST_4    = put $ Word8 $ fromInt 7
  put  ICONST_5    = put $ Word8 $ fromInt 8
  put  LCONST_0    = put $ Word8 $ fromInt 9
  put  LCONST_1    = put $ Word8 $ fromInt 10
  put  FCONST_0    = put $ Word8 $ fromInt 11
  put  FCONST_1    = put $ Word8 $ fromInt 12
  put  FCONST_2    = put $ Word8 $ fromInt 13
  put  DCONST_0    = put $ Word8 $ fromInt 14
  put  DCONST_1    = put $ Word8 $ fromInt 15
  put (BIPUSH x)   = put1 16 x
  put (SIPUSH x)   = put1 17 x
  put (LDC1 x)     = put1 18 x
  put (LDC2 x)     = put1 19 x
  put (LDC2W x)    = put1 20 x
  put (ILOAD x)    = put1 21 x
  put (LLOAD x)    = put1 22 x
  put (FLOAD x)    = put1 23 x
  put (DLOAD x)    = put1 24 x
  put (ALOAD x)    = put1 25 x
  put (ILOAD_ i)   = putImm 26 i
  put (LLOAD_ i)   = putImm 30 i
  put (FLOAD_ i)   = putImm 34 i
  put (DLOAD_ i)   = putImm 38 i
  put (ALOAD_ i)   = putImm 42 i
  put  IALOAD      = put $ Word8 $ fromInt 46
  put  LALOAD      = put $ Word8 $ fromInt 47
  put  FALOAD      = put $ Word8 $ fromInt 48
  put  DALOAD      = put $ Word8 $ fromInt 49
  put  AALOAD      = put $ Word8 $ fromInt 50
  put  BALOAD      = put $ Word8 $ fromInt 51
  put  CALOAD      = put $ Word8 $ fromInt 52
  put  SALOAD      = put $ Word8 $ fromInt 53
  put (ISTORE x)   = put1  54 x
  put (LSTORE x)   = put1  55 x
  put (FSTORE x)   = put1  56 x
  put (DSTORE x)   = put1  57 x
  put (ASTORE x)   = put1  58 x
  put (ISTORE_ i)  = putImm 59 i
  put (LSTORE_ i)  = putImm 63 i
  put (FSTORE_ i)  = putImm 67 i
  put (DSTORE_ i)  = putImm 71 i
  put (ASTORE_ i)  = putImm 75 i
  put  IASTORE     = put $ Word8 $ fromInt 79
  put  LASTORE     = put $ Word8 $ fromInt 80
  put  FASTORE     = put $ Word8 $ fromInt 81
  put  DASTORE     = put $ Word8 $ fromInt 82
  put  AASTORE     = put $ Word8 $ fromInt 83
  put  BASTORE     = put $ Word8 $ fromInt 84
  put  CASTORE     = put $ Word8 $ fromInt 85
  put  SASTORE     = put $ Word8 $ fromInt 86
  put  POP         = put $ Word8 $ fromInt 87
  put  POP2        = put $ Word8 $ fromInt 88
  put  DUP         = put $ Word8 $ fromInt 89
  put  DUP_X1      = put $ Word8 $ fromInt 90
  put  DUP_X2      = put $ Word8 $ fromInt 91
  put  DUP2        = put $ Word8 $ fromInt 92
  put  DUP2_X1     = put $ Word8 $ fromInt 93
  put  DUP2_X2     = put $ Word8 $ fromInt 94
  put  SWAP        = put $ Word8 $ fromInt 95
  put  IADD        = put $ Word8 $ fromInt 96
  put  LADD        = put $ Word8 $ fromInt 97
  put  FADD        = put $ Word8 $ fromInt 98
  put  DADD        = put $ Word8 $ fromInt 99
  put  ISUB        = put $ Word8 $ fromInt 100
  put  LSUB        = put $ Word8 $ fromInt 101
  put  FSUB        = put $ Word8 $ fromInt 102
  put  DSUB        = put $ Word8 $ fromInt 103
  put  IMUL        = put $ Word8 $ fromInt 104
  put  LMUL        = put $ Word8 $ fromInt 105
  put  FMUL        = put $ Word8 $ fromInt 106
  put  DMUL        = put $ Word8 $ fromInt 107
  put  IDIV        = put $ Word8 $ fromInt 108
  put  LDIV        = put $ Word8 $ fromInt 109
  put  FDIV        = put $ Word8 $ fromInt 110
  put  DDIV        = put $ Word8 $ fromInt 111
  put  IREM        = put $ Word8 $ fromInt 112
  put  LREM        = put $ Word8 $ fromInt 113
  put  FREM        = put $ Word8 $ fromInt 114
  put  DREM        = put $ Word8 $ fromInt 115
  put  INEG        = put $ Word8 $ fromInt 116
  put  LNEG        = put $ Word8 $ fromInt 117
  put  FNEG        = put $ Word8 $ fromInt 118
  put  DNEG        = put $ Word8 $ fromInt 119
  put  ISHL        = put $ Word8 $ fromInt 120
  put  LSHL        = put $ Word8 $ fromInt 121
  put  ISHR        = put $ Word8 $ fromInt 122
  put  LSHR        = put $ Word8 $ fromInt 123
  put  IUSHR       = put $ Word8 $ fromInt 124
  put  LUSHR       = put $ Word8 $ fromInt 125
  put  IAND        = put $ Word8 $ fromInt 126
  put  LAND        = put $ Word8 $ fromInt 127
  put  IOR         = put $ Word8 $ fromInt 128
  put  LOR         = put $ Word8 $ fromInt 129
  put  IXOR        = put $ Word8 $ fromInt 130
  put  LXOR        = put $ Word8 $ fromInt 131
  put (IINC x y)      = put2 132 x y
  put  I2L            = put $ Word8 $ fromInt 133
  put  I2F            = put $ Word8 $ fromInt 134
  put  I2D            = put $ Word8 $ fromInt 135
  put  L2I            = put $ Word8 $ fromInt 136
  put  L2F            = put $ Word8 $ fromInt 137
  put  L2D            = put $ Word8 $ fromInt 138
  put  F2I            = put $ Word8 $ fromInt 139
  put  F2L            = put $ Word8 $ fromInt 140
  put  F2D            = put $ Word8 $ fromInt 141
  put  D2I            = put $ Word8 $ fromInt 142
  put  D2L            = put $ Word8 $ fromInt 143
  put  D2F            = put $ Word8 $ fromInt 144
  put  I2B            = put $ Word8 $ fromInt 145
  put  I2C            = put $ Word8 $ fromInt 146
  put  I2S            = put $ Word8 $ fromInt 147
  put  LCMP           = put $ Word8 $ fromInt 148
  put (FCMP C_LT)     = put $ Word8 $ fromInt 149
  put (FCMP C_GT)     = put $ Word8 $ fromInt 150
  put (FCMP c)        = putFail $ \_ -> "No such instruction: FCMP " <> show c
  put (DCMP C_LT)     = put $ Word8 $ fromInt 151
  put (DCMP C_GT)     = put $ Word8 $ fromInt 152
  put (DCMP c)        = putFail $ \_ -> "No such instruction: DCMP " <> show c
  put (IF c x)        = put (Word8 $ fromInt $ 153 + (cmpOrd c)) <>
                        put x
  put (IF_ACMP C_EQ x) = put1 165 x
  put (IF_ACMP C_NE x) = put1 166 x
  put (IF_ACMP c _)   = putFail $ \_ -> "No such instruction: IF_ACMP " <> show c
  put (IF_ICMP c x)   = put (Word8 $ fromInt $ 159 + (cmpOrd c)) <>
                        put x
  put (GOTO x)        = put1 167 x
  put (JSR x)         = put1 168 x
  put  RET            = put $ Word8 $ fromInt 169
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

  put  IRETURN        = put $ Word8 $ fromInt 172
  put  LRETURN        = put $ Word8 $ fromInt 173
  put  FRETURN        = put $ Word8 $ fromInt 174
  put  DRETURN        = put $ Word8 $ fromInt 175
  put  ARETURN        = put $ Word8 $ fromInt 176
  put  RETURN         = put $ Word8 $ fromInt 177
  put (GETSTATIC x)   = put1 178 x
  put (PUTSTATIC x)   = put1 179 x
  put (GETFIELD x)    = put1 180 x
  put (PUTFIELD x)    = put1 181 x
  put (INVOKEVIRTUAL x)     = put1 182 x
  put (INVOKESPECIAL x)     = put1 183 x
  put (INVOKESTATIC x)      = put1 184 x
  put (INVOKEINTERFACE x c) = put2 185 x c <>
                              put (Word8 $ fromInt 0)

  put (NEW x)         = put1 187 x
  put (NEWARRAY x)    = put1 188 x
  put (ANEWARRAY x)   = put1 189 x
  put  ARRAYLENGTH    = put $ Word8 $ fromInt 190
  put  ATHROW         = put $ Word8 $ fromInt 191
  put (CHECKCAST x)   = put1 192 x
  put (INSTANCEOF x)  = put1 193 x
  put  MONITORENTER   = put $ Word8 $ fromInt 194
  put  MONITOREXIT    = put $ Word8 $ fromInt 195
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
      3 -> pure ICONST_0
      4 -> pure ICONST_1
      5 -> pure ICONST_2
      6 -> pure ICONST_3
      7 -> pure ICONST_4
      8 -> pure ICONST_5
      9 -> pure LCONST_0
      10 -> pure LCONST_1
      11 -> pure FCONST_0
      12 -> pure FCONST_1
      13 -> pure FCONST_2
      14 -> pure DCONST_0
      15 -> pure DCONST_1
      16 -> BIPUSH <$> get
      17 -> SIPUSH <$> get
      18 -> LDC1 <$> get
      19 -> LDC2 <$> get
      20 -> LDC2W <$> get
      21 -> ILOAD <$> get
      22 -> LLOAD <$> get
      23 -> FLOAD <$> get
      24 -> DLOAD <$> get
      25 -> ALOAD <$> get
      46 -> pure IALOAD
      47 -> pure LALOAD
      48 -> pure FALOAD
      49 -> pure DALOAD
      50 -> pure AALOAD
      51 -> pure BALOAD
      52 -> pure CALOAD
      53 -> pure SALOAD
      54 -> ISTORE <$> get
      55 -> LSTORE <$> get
      56 -> FSTORE <$> get
      57 -> DSTORE <$> get
      58 -> ASTORE <$> get
      79 -> pure IASTORE
      80 -> pure LASTORE
      81 -> pure FASTORE
      82 -> pure DASTORE
      83 -> pure AASTORE
      84 -> pure BASTORE
      85 -> pure CASTORE
      86 -> pure SASTORE
      87 -> pure POP
      88 -> pure POP2
      89 -> pure DUP
      90 -> pure DUP_X1
      91 -> pure DUP_X2
      92 -> pure DUP2
      93 -> pure DUP2_X1
      94 -> pure DUP2_X2
      95 -> pure SWAP
      96 -> pure IADD
      97 -> pure LADD
      98 -> pure FADD
      99 -> pure DADD
      100 -> pure ISUB
      101 -> pure LSUB
      102 -> pure FSUB
      103 -> pure DSUB
      104 -> pure IMUL
      105 -> pure LMUL
      106 -> pure FMUL
      107 -> pure DMUL
      108 -> pure IDIV
      109 -> pure LDIV
      110 -> pure FDIV
      111 -> pure DDIV
      112 -> pure IREM
      113 -> pure LREM
      114 -> pure FREM
      115 -> pure DREM
      116 -> pure INEG
      117 -> pure LNEG
      118 -> pure FNEG
      119 -> pure DNEG
      120 -> pure ISHL
      121 -> pure LSHL
      122 -> pure ISHR
      123 -> pure LSHR
      124 -> pure IUSHR
      125 -> pure LUSHR
      126 -> pure IAND
      127 -> pure LAND
      128 -> pure IOR
      129 -> pure LOR
      130 -> pure IXOR
      131 -> pure LXOR
      132 -> IINC <$> get <*> get
      133 -> pure I2L
      134 -> pure I2F
      135 -> pure I2D
      136 -> pure L2I
      137 -> pure L2F
      138 -> pure L2D
      139 -> pure F2I
      140 -> pure F2L
      141 -> pure F2D
      142 -> pure D2I
      143 -> pure D2L
      144 -> pure D2F
      145 -> pure I2B
      146 -> pure I2C
      147 -> pure I2S
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
      172 -> pure IRETURN
      173 -> pure LRETURN
      174 -> pure FRETURN
      175 -> pure DRETURN
      176 -> pure ARETURN
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
      _ | inRange 59 62 c -> imm 59 ISTORE_ c
        | inRange 63 66 c -> imm 63 LSTORE_ c
        | inRange 67 70 c -> imm 67 FSTORE_ c
        | inRange 71 74 c -> imm 71 DSTORE_ c
        | inRange 75 78 c -> imm 75 ASTORE_ c
        | inRange 26 29 c -> imm 26 ILOAD_ c
        | inRange 30 33 c -> imm 30 LLOAD_ c
        | inRange 34 37 c -> imm 34 FLOAD_ c
        | inRange 38 41 c -> imm 38 DLOAD_ c
        | inRange 42 45 c -> imm 42 ALOAD_ c
        | inRange 15 15 c ->
          case toCmp (c-153) of
            Nothing -> fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}
            (Just cmp) -> IF cmp <$> get
        | inRange 15 16 c ->
          case toCmp (c-159) of
            Nothing -> fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}
            (Just cmp) -> IF_ICMP cmp <$> get
        | otherwise -> fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}

-- | Calculate padding for current bytecode offset (cf. TABLESWITCH and LOOKUPSWITCH)
-- padding :: (Integral a, Integral b) => a -> b
padding :: Int -> Int
padding offset = (4 - offset) `mod` 4
