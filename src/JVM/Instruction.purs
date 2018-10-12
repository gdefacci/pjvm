module JVM.Instruction where

import Prelude

import Data.Binary.Types 
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple(..))

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
