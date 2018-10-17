module JVM.ConstantPool where

import Prelude

import Control.Monad.State as ST
import Data.Array as A
import Data.Binary.Binary (Put(..), get, put)
import Data.Binary.Decoder (Decoder(..), fail)
import Data.Binary.Types (Float32(..), Float64(..), Word16(..), Word32(..), Word64(..), Word8(..))
import Data.Foldable (foldMap)
import Data.Map as M
import Data.Tuple (Tuple(..), snd)
import Data.UInt (fromInt, toInt)
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

long :: forall b f m. Constant b f m -> Boolean
long (CLong _)   = true
long (CDouble _) = true
long _           = false

putPool :: PoolFile -> Put
putPool pool = 
    let list :: Array ConstantFile
        list = A.fromFoldable (M.values pool)
        d = A.length $ A.filter long list
    in put (Word16  (fromInt $ (M.size pool) + d + 1)) <>
       foldMap putC list
  where
    putC (CClass i) = put (Word8 $ fromInt 7) <> put i
    putC (CField i j) = put (Word8 $ fromInt 9) <> put i <> put j
    putC (CMethod i j) = put (Word8 $ fromInt 10) <> put i <> put j
    putC (CIfaceMethod i j) = put (Word8 $ fromInt 11) <> put i <> put j
    putC (CString i) = put (Word8 $ fromInt 8) <> put i
    putC (CInteger x) = put (Word8 $ fromInt 3) <> put x
    putC (CFloat x)   = put (Word8 $ fromInt 4) <> put x
    putC (CLong x)    = put (Word8 $ fromInt 5) <> put x
    putC (CDouble x)  = put (Word8 $ fromInt 6) <> put x
    putC (CNameType i j) = put (Word8 $ fromInt 12) <> put i <> put j
    putC (CUTF8 {getString}) = 
                     put (Word8 $ fromInt 1) <>
                     put getString
    putC (CUnicode {getString}) = 
                     put (Word8 $ fromInt 2) <>
                     put getString

getPool :: Word16 -> Decoder (PoolFile)
getPool (Word16 n) = do
    items <- ST.evalStateT go (Word16 $ fromInt 1)
    pure $ M.fromFoldable items
  where
    go :: ST.StateT Word16 Decoder (Array (Tuple Word16 ConstantFile))
    go = do
      (Word16 i) <- ST.get
      if i > n
        then pure []
        else do
          c <- ST.lift getC
          let i' = if long c
                      then Word16 $ fromInt $ (toInt i) + 2
                      else Word16 $ fromInt $ (toInt i) + 1
          ST.put i'
          next <- go
          pure $ A.cons (Tuple i' c) next

    getC = do
      -- !offset <- bytesRead
      (Word8 utag) <- get
      let tag = toInt utag
      case tag of
        1 -> do
          bs <- get
          pure $ CUTF8 { getString : bs }
        2 -> do
          bs <- get
          pure $ CUnicode { getString : bs }
        3  -> CInteger      <$> get
        4  -> CFloat        <$> get
        5  -> CLong         <$> get
        6  -> CDouble       <$> get
        7  -> CClass        <$> get
        8  -> CString       <$> get
        9  -> CField        <$> get <*> get
        10 -> CMethod       <$> get <*> get
        11 -> CIfaceMethod  <$> get <*> get
        12 -> CNameType     <$> get <*> get
        _  -> fail $ "Unknown constants pool entry tag: " <> show tag
