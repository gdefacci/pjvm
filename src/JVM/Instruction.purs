module JVM.Instruction where

import Prelude

import Data.Binary.Types (Word16, Word32, Word8(..))

import Data.Array as A
import Data.Binary.Binary (class Binary, putFoldable, get, put, putPad)
import Data.Binary.Decoder (Decoder, ParserError(..), fail, getOffset, skip)
import Data.Binary.Put (Put, putFail)
import Data.Generic.Rep (class Generic)
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

toCmp :: Int -> Decoder CMP
toCmp 1 = pure C_NE
toCmp 0 = pure C_EQ
toCmp 2 = pure C_LT
toCmp 3 = pure C_GE
toCmp 4 = pure C_GT
toCmp 5 = pure C_LE
toCmp c = fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}

data MathOp = ADD | SUB | MUL | DIV | REM | NEG
data JNumType = MT_I | MT_L | MT_F | MT_D

mathTypeOrd :: JNumType -> Int
mathTypeOrd MT_I = 0
mathTypeOrd MT_L = 1
mathTypeOrd MT_F = 2
mathTypeOrd MT_D = 3

mathOpOrd :: MathOp -> Int
mathOpOrd ADD = 96
mathOpOrd SUB = 100
mathOpOrd MUL = 104
mathOpOrd DIV = 108
mathOpOrd REM = 112
mathOpOrd NEG = 116

mathInstrOrd :: MathOp -> JNumType -> Int
mathInstrOrd op typ = (mathOpOrd op) + (mathTypeOrd typ)

mathInstruction :: Int -> Decoder Instruction
mathInstruction cd = 
  let code = cd - (mathOpOrd ADD)
      tp = code `mod` 4
  in do
      op <- mathOpFromOrd cd code
      typ <- mathTypeFromOrd cd tp
      pure $ MATH op typ
  where
    mathOpFromOrd :: Int -> Int -> Decoder MathOp
    mathOpFromOrd _ x | x >= 96  && x < 100 = pure ADD  
    mathOpFromOrd _ x | x >= 100 && x < 104 = pure SUB 
    mathOpFromOrd _ x | x >= 104 && x < 108 = pure MUL 
    mathOpFromOrd _ x | x >= 108 && x < 112 = pure DIV 
    mathOpFromOrd _ x | x >= 112 && x < 116 = pure REM 
    mathOpFromOrd _ x | x >= 116 && x < 120 = pure NEG 
    mathOpFromOrd ch _ = fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show ch}

    mathTypeFromOrd :: Int -> Int -> Decoder JNumType 
    mathTypeFromOrd _ 0 = pure MT_I 
    mathTypeFromOrd _ 1 = pure MT_L 
    mathTypeFromOrd _ 2 = pure MT_F 
    mathTypeFromOrd _ 3 = pure MT_D
    mathTypeFromOrd ch _ = fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show ch}

derive instance eqMathOp :: Eq MathOp
derive instance ordMathOp :: Ord MathOp

derive instance genericMathOp :: Generic MathOp _

instance showMathOp :: Show MathOp where
  show = genericShow

derive instance eqJNumType :: Eq JNumType
derive instance ordJNumType :: Ord JNumType

derive instance genericJNumType :: Generic JNumType _

instance showJNumType :: Show JNumType where
  show = genericShow

data MemOpType = LT_I | LT_L | LT_F | LT_D | LT_A

data MemOp =  LOAD MemOpType Word8
                | LOAD_IMM MemOpType IMM
                | ALOAD MemOpType
                | BALOAD
                | CALOAD
                | SALOAD
                | STORE MemOpType Word8
                | STORE_IMM MemOpType IMM
                | ASTORE MemOpType 
                | BASTORE        
                | CASTORE        
                | SASTORE        

derive instance eqMemOpType :: Eq MemOpType
derive instance ordMemOpType :: Ord MemOpType

derive instance genericMemOpType :: Generic MemOpType _

instance showMemOpType :: Show MemOpType where
  show = genericShow

derive instance eqMemOp :: Eq MemOp
derive instance ordMemOp :: Ord MemOp

derive instance genericMemOp :: Generic MemOp _

instance showMemOp :: Show MemOp where
  show = genericShow

memTypeOrd :: MemOpType -> Int
memTypeOrd LT_I = 0
memTypeOrd LT_L = 1
memTypeOrd LT_F = 2
memTypeOrd LT_D = 3
memTypeOrd LT_A = 4

putMemOp :: MemOp -> Put
putMemOp (LOAD lt w8        ) = put1 (21 + (memTypeOrd lt)) w8
putMemOp (LOAD_IMM lt i   ) = putImm (26 + ((memTypeOrd lt) * 4)) i
putMemOp (ALOAD lt          ) = putWord8 (46 + (memTypeOrd lt))
putMemOp (BALOAD            ) = putWord8 51
putMemOp (CALOAD            ) = putWord8 52
putMemOp (SALOAD            ) = putWord8 53
putMemOp (STORE lt w8       ) = put1 (54 + (memTypeOrd lt)) w8
putMemOp (STORE_IMM lt i  ) = putImm (59 + ((memTypeOrd lt) * 4)) i
putMemOp (ASTORE lt         ) = putWord8 (79 + (memTypeOrd lt))
putMemOp (BASTORE           ) = putWord8 84
putMemOp (CASTORE           ) = putWord8 85
putMemOp (SASTORE           ) = putWord8 86

getMemOp :: Int -> Decoder MemOp
getMemOp 21 = (LOAD LT_I) <$> get
getMemOp 22 = (LOAD LT_L) <$> get
getMemOp 23 = (LOAD LT_F) <$> get
getMemOp 24 = (LOAD LT_D) <$> get
getMemOp 25 = (LOAD LT_A) <$> get
getMemOp 47 = pure $ ALOAD $ LT_L
getMemOp 46 = pure $ ALOAD $ LT_I
getMemOp 48 = pure $ ALOAD $ LT_F
getMemOp 49 = pure $ ALOAD $ LT_D
getMemOp 50 = pure $ ALOAD $ LT_A
getMemOp 51 = pure $ BALOAD
getMemOp 52 = pure $ CALOAD
getMemOp 53 = pure $ SALOAD
getMemOp 54 = (STORE LT_I) <$> get
getMemOp 55 = (STORE LT_L) <$> get
getMemOp 56 = (STORE LT_F) <$> get
getMemOp 57 = (STORE LT_D) <$> get
getMemOp 58 = (STORE LT_A) <$> get
getMemOp 79 = pure $ ASTORE LT_I
getMemOp 80 = pure $ ASTORE LT_L
getMemOp 81 = pure $ ASTORE LT_F
getMemOp 82 = pure $ ASTORE LT_D
getMemOp 83 = pure $ ASTORE LT_A
getMemOp 84 = pure BASTORE
getMemOp 85 = pure CASTORE
getMemOp 86 = pure SASTORE
getMemOp c  | inRange 59 62 c = imm 59 (STORE_IMM LT_I) c
              | inRange 63 66 c = imm 63 (STORE_IMM LT_L) c
              | inRange 67 70 c = imm 67 (STORE_IMM LT_F) c
              | inRange 71 74 c = imm 71 (STORE_IMM LT_D) c
              | inRange 75 78 c = imm 75 (STORE_IMM LT_A) c
              | inRange 26 29 c = imm 26 (LOAD_IMM LT_I) c
              | inRange 30 33 c = imm 30 (LOAD_IMM LT_L) c
              | inRange 34 37 c = imm 34 (LOAD_IMM LT_F) c
              | inRange 38 41 c = imm 38 (LOAD_IMM LT_D) c
              | inRange 42 45 c = imm 42 (LOAD_IMM LT_A) c
              | otherwise       = fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}

data ConstOp = 
    ACONST_NULL             
  | CONST_M1                
  | CONST_0 JNumType        
  | CONST_1 JNumType        
  | ICONST_2                
  | ICONST_3                
  | ICONST_4                
  | ICONST_5                
  | FCONST_2                
  
constOpOrd :: ConstOp -> Int
constOpOrd ACONST_NULL   = 1
constOpOrd CONST_M1      = 2
constOpOrd (CONST_0 MT_I)  = 3
constOpOrd (CONST_1 MT_I)  = 4
constOpOrd ICONST_2      = 5
constOpOrd ICONST_3      = 6
constOpOrd ICONST_4      = 7
constOpOrd ICONST_5      = 8
constOpOrd FCONST_2      = 13
constOpOrd (CONST_0 MT_L)  = 9
constOpOrd (CONST_1 MT_L)  = 10
constOpOrd (CONST_0 MT_F)  = 11
constOpOrd (CONST_1 MT_F)  = 12
constOpOrd (CONST_0 MT_D)  = 14
constOpOrd (CONST_1 MT_D)  = 15

constOpFromOrd :: Int -> Decoder ConstOp
constOpFromOrd 1  = pure $ ACONST_NULL  
constOpFromOrd 2  = pure $ CONST_M1     
constOpFromOrd 3  = pure $ CONST_0 MT_I 
constOpFromOrd 4  = pure $ CONST_1 MT_I 
constOpFromOrd 5  = pure $ ICONST_2     
constOpFromOrd 6  = pure $ ICONST_3     
constOpFromOrd 7  = pure $ ICONST_4     
constOpFromOrd 8  = pure $ ICONST_5     
constOpFromOrd 13 = pure $ FCONST_2     
constOpFromOrd 9  = pure $ CONST_0 MT_L 
constOpFromOrd 10 = pure $ CONST_1 MT_L 
constOpFromOrd 11 = pure $ CONST_0 MT_F 
constOpFromOrd 12 = pure $ CONST_1 MT_F 
constOpFromOrd 14 = pure $ CONST_0 MT_D 
constOpFromOrd 15 = pure $ CONST_1 MT_D 
constOpFromOrd c  = fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}

derive instance eqConstOp :: Eq ConstOp
derive instance ordConstOp :: Ord ConstOp

derive instance genericConstOp :: Generic ConstOp _

instance showConstOp :: Show ConstOp where
  show = genericShow

-- | JVM instruction set. For comments, see JVM specification.
data Instruction =
    NOP            -- ^ 0
  | CONST ConstOp
  | BIPUSH Word8   -- ^ 16
  | SIPUSH Word16  -- ^ 17
  | LDC1 Word8     -- ^ 18
  | LDC2 Word16    -- ^ 19
  | LDC2W Word16   -- ^ 20
  | MEM MemOp
  | POP            -- ^ 87
  | POP2           -- ^ 88
  | DUP            -- ^ 89
  | DUP_X1         -- ^ 90
  | DUP_X2         -- ^ 91
  | DUP2           -- ^ 92
  | DUP2_X1        -- ^ 93
  | DUP2_X2        -- ^ 94
  | SWAP           -- ^ 95
  | MATH MathOp JNumType -- (96..119)
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
imm :: forall a. Int                   -- ^ Base opcode
    -> (IMM -> a)    -- ^ Instruction constructor
    -> Int                   -- ^ Opcode to parse
    -> Decoder a
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
putImm base i = putWord8 $ base + (immOrdValue i)

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

putWord8 :: Int -> Put
putWord8 i = put $ Word8 $ fromInt i

instance binaryInstruction :: Binary Instruction where
  put  NOP         = putWord8 0
  put (CONST cp)  = putWord8 $ constOpOrd cp
  put (BIPUSH x)   = put1 16 x
  put (SIPUSH x)   = put1 17 x
  put (LDC1 x)     = put1 18 x
  put (LDC2 x)     = put1 19 x
  put (LDC2W x)    = put1 20 x
  put (MEM lop)  = putMemOp lop
  put  POP         = putWord8 87
  put  POP2        = putWord8 88
  put  DUP         = putWord8 89
  put  DUP_X1      = putWord8 90
  put  DUP_X2      = putWord8 91
  put  DUP2        = putWord8 92
  put  DUP2_X1     = putWord8 93
  put  DUP2_X2     = putWord8 94
  put  SWAP        = putWord8 95
  put  (MATH op t) = putWord8 (mathInstrOrd op t)
  put  ISHL        = putWord8 120
  put  LSHL        = putWord8 121
  put  ISHR        = putWord8 122
  put  LSHR        = putWord8 123
  put  IUSHR       = putWord8 124
  put  LUSHR       = putWord8 125
  put  IAND        = putWord8 126
  put  LAND        = putWord8 127
  put  IOR         = putWord8 128
  put  LOR         = putWord8 129
  put  IXOR        = putWord8 130
  put  LXOR        = putWord8 131
  put (IINC x y)      = put2 132 x y
  put  I2L            = putWord8 133
  put  I2F            = putWord8 134
  put  I2D            = putWord8 135
  put  L2I            = putWord8 136
  put  L2F            = putWord8 137
  put  L2D            = putWord8 138
  put  F2I            = putWord8 139
  put  F2L            = putWord8 140
  put  F2D            = putWord8 141
  put  D2I            = putWord8 142
  put  D2L            = putWord8 143
  put  D2F            = putWord8 144
  put  I2B            = putWord8 145
  put  I2C            = putWord8 146
  put  I2S            = putWord8 147
  put  LCMP           = putWord8 148
  put (FCMP C_LT)     = putWord8 149
  put (FCMP C_GT)     = putWord8 150
  put (FCMP c)        = putFail $ \_ -> "No such instruction: FCMP " <> show c
  put (DCMP C_LT)     = putWord8 151
  put (DCMP C_GT)     = putWord8 152
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
  put  RET            = putWord8 169
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

  put  IRETURN        = putWord8 172
  put  LRETURN        = putWord8 173
  put  FRETURN        = putWord8 174
  put  DRETURN        = putWord8 175
  put  ARETURN        = putWord8 176
  put  RETURN         = putWord8 177
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
  put  ARRAYLENGTH    = putWord8 190
  put  ATHROW         = putWord8 191
  put (CHECKCAST x)   = put1 192 x
  put (INSTANCEOF x)  = put1 193 x
  put  MONITORENTER   = putWord8 194
  put  MONITOREXIT    = putWord8 195
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
      16 -> BIPUSH <$> get
      17 -> SIPUSH <$> get
      18 -> LDC1 <$> get
      19 -> LDC2 <$> get
      20 -> LDC2W <$> get
      87 -> pure POP
      88 -> pure POP2
      89 -> pure DUP
      90 -> pure DUP_X1
      91 -> pure DUP_X2
      92 -> pure DUP2
      93 -> pure DUP2_X1
      94 -> pure DUP2_X2
      95 -> pure SWAP
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
      _ | inRange 1 15 c -> CONST <$> (constOpFromOrd c)
        | inRange 21 86 c -> MEM <$> (getMemOp c)
        | inRange 96 119 c -> mathInstruction c
        | inRange 153 158 c -> (\cmp -> IF cmp <$> get) =<< (toCmp $ c-153)
        | inRange 159 164 c -> (\cmp -> IF_ICMP cmp <$> get) =<< (toCmp $ c-159)
        | otherwise -> fail $ \offset -> GenericParserError { offset, message: "Unknown instruction byte code: " <> show c}

-- | Calculate padding for current bytecode offset (cf. TABLESWITCH and LOOKUPSWITCH)
-- padding :: (Integral a, Integral b) => a -> b
padding :: Int -> Int
padding offset = (4 - offset) `mod` 4
