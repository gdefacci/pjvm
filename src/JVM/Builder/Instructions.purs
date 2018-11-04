module JVM.Builder.Instructions where

import JVM.Instruction
import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (class MonadState)
import Data.Binary.Types (Word16(..), Word8(..))
import Data.UInt (fromInt, toInt)
import JVM.Builder.Monad (GState(..), GenError, addToPool, i0, i1, i8)
import JVM.ConstantPool (Constant(..), ConstantDirect)
import JVM.Instruction (Instruction(..))
import JVM.Members (FieldNameType(..), MethodNameType(..))

nop :: forall m. MonadState GState m => m Unit
nop = i0 NOP
aconst_null :: forall m. MonadState GState m => m Unit
aconst_null = i0 $ ACONST_NULL
iconst_m1 :: forall m. MonadState GState m => m Unit
iconst_m1 = i0 ICONST_M1
iconst_0 :: forall m. MonadState GState m => m Unit
iconst_0 = i0 $ CONST_0 ILFD_I
iconst_1 :: forall m. MonadState GState m => m Unit
iconst_1 = i0 $ CONST_1 ILFD_I
iconst_2 :: forall m. MonadState GState m => m Unit
iconst_2 = i0 $ CONST_2 T32_I
iconst_3 :: forall m. MonadState GState m => m Unit
iconst_3 = i0 ICONST_3
iconst_4 :: forall m. MonadState GState m => m Unit
iconst_4 = i0 ICONST_4
iconst_5 :: forall m. MonadState GState m => m Unit
iconst_5 = i0 ICONST_5
lconst_0 :: forall m. MonadState GState m => m Unit
lconst_0 = i0 $ CONST_0 ILFD_L
lconst_1 :: forall m. MonadState GState m => m Unit
lconst_1 = i0 $ CONST_1 ILFD_L
fconst_0 :: forall m. MonadState GState m => m Unit
fconst_0 = i0 $ CONST_0 ILFD_F
fconst_1 :: forall m. MonadState GState m => m Unit
fconst_1 = i0 $ CONST_1 ILFD_F
fconst_2 :: forall m. MonadState GState m => m Unit
fconst_2 = i0 $ CONST_2 T32_F
dconst_0 :: forall m. MonadState GState m => m Unit
dconst_0 = i0 $ CONST_0 ILFD_D
dconst_1 :: forall m. MonadState GState m => m Unit
dconst_1 = i0 $ CONST_1 ILFD_D

bipush :: forall m. MonadState GState m => Word8 -> m Unit
bipush x = i0 (BIPUSH x)
sipush :: forall m. MonadState GState m => Word16 -> m Unit
sipush x = i0 (SIPUSH x)

ldc1 :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
ldc1 x = i8 LDC1 x
ldc2 :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
ldc2 x = i1 LDC2 x
ldc2w :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
ldc2w x = i1 LDC2W x

loadString :: forall m. MonadThrow GenError m => MonadState GState m => String -> m Unit
loadString str = ldc1 (CString str)

-- Broken, iload works on the local stack variable, not on the constant pool
--
-- iload :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- iload x = i8 (LOAD ILFDA_I) x
-- lload :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- lload x = i8 (LOAD ILFDA_L) x
-- fload :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- fload x = i8 (LOAD ILFDA_F) x
-- dload :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- dload x = i8 (LOAD ILFDA_D) x
-- aload :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- aload x = i8 (LOAD ILFDA_A) x

iload_ :: forall m. MonadState GState m => IMM -> m Unit
iload_ x = i0 (LOAD_ ILFDA_I x)
lload_ :: forall m. MonadState GState m => IMM -> m Unit
lload_ x = i0 (LOAD_ ILFDA_L x)
fload_ :: forall m. MonadState GState m => IMM -> m Unit
fload_ x = i0 (LOAD_ ILFDA_F x)
dload_ :: forall m. MonadState GState m => IMM -> m Unit
dload_ x = i0 (LOAD_ ILFDA_D x)
aload_ :: forall m. MonadState GState m => IMM -> m Unit
aload_ x = i0 (LOAD_ ILFDA_I x)

iaload :: forall m. MonadState GState m => m Unit
iaload = i0 $ ALOAD JTA_I
laload :: forall m. MonadState GState m => m Unit
laload = i0 $ ALOAD JTA_I
faload :: forall m. MonadState GState m => m Unit
faload = i0 $ ALOAD JTA_F
daload :: forall m. MonadState GState m => m Unit
daload = i0 $ ALOAD JTA_D
aaload :: forall m. MonadState GState m => m Unit
aaload = i0 $ ALOAD JTA_A
caload :: forall m. MonadState GState m => m Unit
caload = i0 $ ALOAD JTA_C
saload :: forall m. MonadState GState m => m Unit
saload = i0 $ ALOAD JTA_S

-- Broken, istore works on the local stack variable, not on the constant pool
--
-- istore :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- istore x = i8 (STORE ILFDA_I) x
-- lstore :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- lstore x = i8 (STORE ILFDA_L) x
-- fstore :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- fstore x = i8 (STORE ILFDA_F) x
-- dstore :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- dstore x = i8 (STORE ILFDA_D) x
-- astore :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Unit
-- astore x = i8 (STORE ILFDA_A) x

istore_ :: forall m. MonadState GState m => IMM -> m Unit
istore_ x = i0 (STORE_ ILFDA_I x)
lstore_ :: forall m. MonadState GState m => IMM -> m Unit
lstore_ x = i0 (STORE_ ILFDA_L x)
fstore_ :: forall m. MonadState GState m => IMM -> m Unit
fstore_ x = i0 (STORE_ ILFDA_F x)
dstore_ :: forall m. MonadState GState m => IMM -> m Unit
dstore_ x = i0 (STORE_ ILFDA_D x)
astore_ :: forall m. MonadState GState m => IMM -> m Unit
astore_ x = i0 (STORE_ ILFDA_I x)

iastore :: forall m. MonadState GState m => m Unit
iastore = i0 $ ASTORE JTA_I
lastore :: forall m. MonadState GState m => m Unit
lastore = i0 $ ASTORE JTA_I
fastore :: forall m. MonadState GState m => m Unit
fastore = i0 $ ASTORE JTA_F
dastore :: forall m. MonadState GState m => m Unit
dastore = i0 $ ASTORE JTA_D
aastore :: forall m. MonadState GState m => m Unit
aastore = i0 $ ASTORE JTA_A
castore :: forall m. MonadState GState m => m Unit
castore = i0 $ ASTORE JTA_C
sastore :: forall m. MonadState GState m => m Unit
sastore = i0 $ ASTORE JTA_S

pop :: forall m. MonadState GState m => m Unit
pop     = i0 $ STACK_OP Pop
pop2 :: forall m. MonadState GState m => m Unit
pop2    = i0 $ STACK_OP Pop2
dup :: forall m. MonadState GState m => m Unit
dup     = i0 $ STACK_OP Dup
dup_x1 :: forall m. MonadState GState m => m Unit
dup_x1  = i0 $ STACK_OP Dup_x1
dup_x2 :: forall m. MonadState GState m => m Unit
dup_x2  = i0 $ STACK_OP Dup_x2
dup2 :: forall m. MonadState GState m => m Unit
dup2    = i0 $ STACK_OP Dup2
dup2_x1 :: forall m. MonadState GState m => m Unit
dup2_x1 = i0 $ STACK_OP Dup2_x1
dup2_x2 :: forall m. MonadState GState m => m Unit
dup2_x2 = i0 $ STACK_OP Dup2_x2
swap :: forall m. MonadState GState m => m Unit
swap    = i0 $ STACK_OP Swap

iadd :: forall m. MonadState GState m => m Unit
iadd    = i0 $ NUM_OP Add ILFD_I
ladd :: forall m. MonadState GState m => m Unit
ladd    = i0 $ NUM_OP Add ILFD_L
fadd :: forall m. MonadState GState m => m Unit
fadd    = i0 $ NUM_OP Add ILFD_F
dadd :: forall m. MonadState GState m => m Unit
dadd    = i0 $ NUM_OP Add ILFD_D
isub :: forall m. MonadState GState m => m Unit
isub    = i0 $ NUM_OP Sub ILFD_I
lsub :: forall m. MonadState GState m => m Unit
lsub    = i0 $ NUM_OP Sub ILFD_L
fsub :: forall m. MonadState GState m => m Unit
fsub    = i0 $ NUM_OP Sub ILFD_F
dsub :: forall m. MonadState GState m => m Unit
dsub    = i0 $ NUM_OP Sub ILFD_D
imul :: forall m. MonadState GState m => m Unit
imul    = i0 $ NUM_OP Mul ILFD_I
lmul :: forall m. MonadState GState m => m Unit
lmul    = i0 $ NUM_OP Mul ILFD_L
fmul :: forall m. MonadState GState m => m Unit
fmul    = i0 $ NUM_OP Mul ILFD_F
dmul :: forall m. MonadState GState m => m Unit
dmul    = i0 $ NUM_OP Mul ILFD_D
idiv :: forall m. MonadState GState m => m Unit
idiv    = i0 $ NUM_OP Div ILFD_I
ldiv :: forall m. MonadState GState m => m Unit
ldiv    = i0 $ NUM_OP Div ILFD_L
fdiv :: forall m. MonadState GState m => m Unit
fdiv    = i0 $ NUM_OP Div ILFD_F
ddiv :: forall m. MonadState GState m => m Unit
ddiv    = i0 $ NUM_OP Div ILFD_D
irem :: forall m. MonadState GState m => m Unit
irem    = i0 $ NUM_OP Rem ILFD_I
lrem :: forall m. MonadState GState m => m Unit
lrem    = i0 $ NUM_OP Rem ILFD_L
frem :: forall m. MonadState GState m => m Unit
frem    = i0 $ NUM_OP Rem ILFD_F
drem :: forall m. MonadState GState m => m Unit
drem    = i0 $ NUM_OP Rem ILFD_D
ineg :: forall m. MonadState GState m => m Unit
ineg    = i0 $ NUM_OP Neg ILFD_I
lneg :: forall m. MonadState GState m => m Unit
lneg    = i0 $ NUM_OP Neg ILFD_L
fneg :: forall m. MonadState GState m => m Unit
fneg    = i0 $ NUM_OP Neg ILFD_F
dneg :: forall m. MonadState GState m => m Unit
dneg    = i0 $ NUM_OP Neg ILFD_D

ishl :: forall m. MonadState GState m => m Unit
ishl    = i0 $ BIT_OP Shl IL_I
lshl :: forall m. MonadState GState m => m Unit
lshl    = i0 $ BIT_OP Shl IL_L
ishr :: forall m. MonadState GState m => m Unit
ishr    = i0 $ BIT_OP Shr IL_I
lshr :: forall m. MonadState GState m => m Unit
lshr    = i0 $ BIT_OP Shr IL_L
iushr :: forall m. MonadState GState m => m Unit
iushr   = i0 $ BIT_OP Ushr IL_I
lushr :: forall m. MonadState GState m => m Unit
lushr   = i0 $ BIT_OP Ushr IL_L
iand :: forall m. MonadState GState m => m Unit
iand    = i0 $ BIT_OP And IL_I
land :: forall m. MonadState GState m => m Unit
land    = i0 $ BIT_OP And IL_L
ior :: forall m. MonadState GState m => m Unit
ior     = i0 $ BIT_OP Or IL_I
lor :: forall m. MonadState GState m => m Unit
lor     = i0 $ BIT_OP Or IL_L
ixor :: forall m. MonadState GState m => m Unit
ixor    = i0 $ BIT_OP Xor IL_I
lxor :: forall m. MonadState GState m => m Unit
lxor    = i0 $ BIT_OP Xor IL_L

iinc :: forall m. MonadState GState m => Word8 -> Word8 -> m Unit
iinc x y = i0 $ IINC x y

i2l :: forall m. MonadState GState m => m Unit
i2l  = i0 $ CONVERT ILFD_I JT_L
i2f :: forall m. MonadState GState m => m Unit
i2f  = i0 $ CONVERT ILFD_I JT_F
i2d :: forall m. MonadState GState m => m Unit
i2d  = i0 $ CONVERT ILFD_I JT_D
l2i :: forall m. MonadState GState m => m Unit
l2i  = i0 $ CONVERT ILFD_L JT_I
l2f :: forall m. MonadState GState m => m Unit
l2f  = i0 $ CONVERT ILFD_L JT_F
l2d :: forall m. MonadState GState m => m Unit
l2d  = i0 $ CONVERT ILFD_L JT_D
f2i :: forall m. MonadState GState m => m Unit
f2i  = i0 $ CONVERT ILFD_F JT_I
f2l :: forall m. MonadState GState m => m Unit
f2l  = i0 $ CONVERT ILFD_F JT_L
f2d :: forall m. MonadState GState m => m Unit
f2d  = i0 $ CONVERT ILFD_F JT_D
d2i :: forall m. MonadState GState m => m Unit
d2i  = i0 $ CONVERT ILFD_D JT_I
d2l :: forall m. MonadState GState m => m Unit
d2l  = i0 $ CONVERT ILFD_D JT_L
d2f :: forall m. MonadState GState m => m Unit
d2f  = i0 $ CONVERT ILFD_D JT_F
i2b :: forall m. MonadState GState m => m Unit
i2b  = i0 $ CONVERT ILFD_I JT_B
i2c :: forall m. MonadState GState m => m Unit
i2c  = i0 $ CONVERT ILFD_I JT_C
i2s :: forall m. MonadState GState m => m Unit
i2s  = i0 $ CONVERT ILFD_I JT_S

lcmp :: forall m. MonadState GState m => m Unit
lcmp = i0 LCMP
fcmp :: forall m. MonadState GState m => CMP -> m Unit
fcmp cmp = i0 $ FCMP cmp
dcmp :: forall m. MonadState GState m => CMP -> m Unit
dcmp cmp = i0 $ DCMP cmp

-- | Wide instruction
-- wide :: forall m. MonadThrow GenError m => MonadState GState m => (Word8 -> Instruction) -> ConstantDirect -> m Unit
-- wide fn c = do
--   (Word16 ixu) <- addToPool c
--   let ix = toInt ixu
--       ix0 = Word8 $ fromInt $ ix `div` 256
--       ix1 = Word8 $ fromInt $ ix `mod` 256
--   i0 $ WIDE ix0 $ fn ix1

new :: forall m. MonadThrow GenError m => MonadState GState m => String -> m Unit
new cls =
  i1 NEW (CClass cls)

newArray :: forall m. MonadState GState m => ArrayType -> m Unit
newArray t =
  i0 (NEWARRAY $ atype2byte t)

allocNewArray :: forall m. MonadThrow GenError m => MonadState GState m => String -> m Unit
allocNewArray cls =
  i1 ANEWARRAY (CClass cls)

invokeVirtual :: forall m. MonadThrow GenError m => MonadState GState m => String -> MethodNameType -> m Unit
invokeVirtual cls sig =
  i1 INVOKEVIRTUAL (CMethod cls sig)

invokeStatic :: forall m. MonadThrow GenError m => MonadState GState m => String -> MethodNameType -> m Unit
invokeStatic cls sig =
  i1 INVOKESTATIC (CMethod cls sig)

invokeSpecial :: forall m. MonadThrow GenError m => MonadState GState m => String -> MethodNameType -> m Unit
invokeSpecial cls sig =
  i1 INVOKESPECIAL (CMethod cls sig)

getStaticField :: forall m. MonadThrow GenError m => MonadState GState m => String -> FieldNameType -> m Unit
getStaticField cls sig =
  i1 GETSTATIC (CField cls sig)

allocArray :: forall m. MonadThrow GenError m => MonadState GState m => String -> m Unit
allocArray cls =
  i1 ANEWARRAY (CClass cls)