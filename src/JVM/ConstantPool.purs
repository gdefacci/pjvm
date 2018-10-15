module JVM.ConstantPool where

import Prelude

import Data.Binary.Types (Float32(..), Float64(..), Word16(..), Word32(..), Word64(..))
import Data.Map as M
import JVM.Members (FieldNameType(..), MethodNameType(..))

data Constant b f m =
    CClass b
  | CField b f
  | CMethod b m
  | CIfaceMethod b m
  | CString b
  | CInteger Word32
  | CFloat Float32
  | CLong Word64
  | CDouble Float64
  | CNameType b b
  | CUTF8 {getString :: String}
  | CUnicode {getString :: String}

instance showConstant :: (Show b, Show f, Show m) => Show (Constant b f m) where
  show (CClass name) = "class " <> show name
  show (CField cls nt) = "field " <> show cls <> "." <> show nt
  show (CMethod cls nt) = "method " <> show cls <> "." <> show nt
  show (CIfaceMethod cls nt) = "interface method " <> show cls <> "." <> show nt
  show (CString s) = "String \"" <> show s <> "\""
  show (CInteger x) = show x
  show (CFloat x) = show x
  show (CLong x) = show x
  show (CDouble x) = show x
  show (CNameType name tp) = show name <> ": " <> show tp
  show (CUTF8 s) = "UTF8 \"" <> show s <> "\""
  show (CUnicode s) = "Unicode \"" <> show s <> "\""

type ConstantDirect = Constant String FieldNameType MethodNameType

type ConstantFile = Constant Word16 Word16 Word16

type PoolDirect = M.Map Word16 ConstantDirect

type PoolFile = M.Map Word16 ConstantFile
