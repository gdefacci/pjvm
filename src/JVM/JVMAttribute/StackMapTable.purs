module JVM.JVMAttribute.StackMapTable where

import Prelude

import Data.Array as A
import Data.Binary.Binary (class Binary, get, put, putFoldable)
import Data.Binary.Decoder (ParserError(..), fail)
import Data.Binary.Types (Word16(..), Word32(..), Word8(..))
import Data.List.Lazy (replicateM)
import Data.UInt (fromInt, toInt)

data StackMapTable = StackMapTable 
  { attributeNameIndex :: Word16
  , attributeLength :: Word32
  , numberOfEntries :: Word16
  , entries :: Array StackMapFrame
  } 

data VerificationTypeInfo = TopVariableInfo 
  | IntegerVariableInfo 
  | FloatVariableInfo 
  | LongVariableInfo
  | DoubleVariableInfo  
  | NullVariableInfo
  | UninitializedThisVariableInfo
  | ObjectVariableInfo Word16
  | UninitializedVariableInfo Word16 

data StackMapFrame = SameFrame { frameType::Word8 }
  | SameLocals1StackItemFrame { frameType::Word8, verificationTypeInfo::VerificationTypeInfo }
  | SameLocals1StackItemFrameExtended {frameType::Word8, offsetDelta::Word16, verificationTypeInfo::VerificationTypeInfo }   
  | ChopFrame {frameType::Word8, offsetDelta::Word16 }
  | SameFrameExtended {frameType::Word8, offsetDelta::Word16}
  | AppendFrame {frameType::Word8, offsetDelta::Word16, verificationTypeInfo::Array VerificationTypeInfo }
  | FullFrame {frameType::Word8, offsetDelta::Word16, numberOfLocals::Word16, locals::Array VerificationTypeInfo,
                numberOfStackItems :: Word16, stack:: Array VerificationTypeInfo }


frameOffsetDelta :: StackMapFrame -> Int
frameOffsetDelta (SameFrame {frameType:(Word8 w8)}) = toInt w8
frameOffsetDelta (SameLocals1StackItemFrame {frameType:(Word8 w8)})  = (toInt w8) - 64
frameOffsetDelta (SameLocals1StackItemFrameExtended { offsetDelta:(Word16 w16) }) = (toInt w16)
frameOffsetDelta (ChopFrame { offsetDelta:(Word16 w16) }) = (toInt w16)
frameOffsetDelta (SameFrameExtended { offsetDelta:(Word16 w16) }) = (toInt w16)
frameOffsetDelta (AppendFrame { offsetDelta:(Word16 w16) }) = (toInt w16)
frameOffsetDelta (FullFrame { offsetDelta:(Word16 w16) }) = (toInt w16)

instance verificationTypeInfoBinary :: Binary VerificationTypeInfo where
  put TopVariableInfo     = put $ Word8 $ fromInt 0
  put IntegerVariableInfo = put $ Word8 $ fromInt 1
  put FloatVariableInfo   = put $ Word8 $ fromInt 2
  put DoubleVariableInfo  = put $ Word8 $ fromInt 3
  put LongVariableInfo    = put $ Word8 $ fromInt 4
  put NullVariableInfo    = put $ Word8 $ fromInt 5
  put UninitializedThisVariableInfo   = (put $ Word8 $ fromInt 6)
  put (ObjectVariableInfo w16)        = (put $ Word8 $ fromInt 7) <> put w16
  put (UninitializedVariableInfo w16) = (put $ Word8 $ fromInt 8) <> put w16

  get = do
    (Word8 w8) <- get
    case (toInt w8) of
      0 -> pure $ TopVariableInfo
      1 -> pure $ IntegerVariableInfo
      2 -> pure $ FloatVariableInfo
      3 -> pure $ DoubleVariableInfo
      4 -> pure $ LongVariableInfo
      5 -> pure $ NullVariableInfo
      6 -> pure $ UninitializedThisVariableInfo
      7 -> ObjectVariableInfo <$> get 
      8 -> UninitializedVariableInfo <$> get 
      x -> fail $ \offset -> GenericParserError {offset, message:"Invalid VerificationTypeInfo id "<> show x}

instance stackMapFrameBinary :: Binary StackMapFrame where
  put (SameFrame { frameType }) = put frameType
  put (SameLocals1StackItemFrame { frameType, verificationTypeInfo }) = 
    put frameType <> put verificationTypeInfo
  put (SameLocals1StackItemFrameExtended {frameType, offsetDelta, verificationTypeInfo }) =
    put frameType <>
    put offsetDelta <>
    put verificationTypeInfo
  put (ChopFrame {frameType, offsetDelta }) =
    put frameType <>
    put offsetDelta 
  put (SameFrameExtended {frameType, offsetDelta}) =
    put frameType <>
    put offsetDelta 
  put (AppendFrame {frameType, offsetDelta, verificationTypeInfo }) =
    put frameType <>
    put offsetDelta <>
    putFoldable verificationTypeInfo
  put (FullFrame {frameType, offsetDelta, numberOfLocals, locals, numberOfStackItems, stack }) =
    put frameType <>
    put offsetDelta <> 
    put numberOfLocals <>
    putFoldable locals <>
    put numberOfStackItems <>
    putFoldable stack

  get = do
    (Word8 w8) <- get
    case toInt w8 of
      x | x <= 63 -> pure $ SameFrame { frameType:Word8 $ fromInt x }
      x | x >= 64 && x <= 127 -> do
        vti <- get
        pure $ SameLocals1StackItemFrame { frameType:Word8 $ fromInt x, verificationTypeInfo: vti }
      247 -> do
        offsetDelta <- get
        verificationTypeInfo <- get
        pure $ SameLocals1StackItemFrameExtended 
          { frameType: Word8 $ fromInt 247
          , offsetDelta
          , verificationTypeInfo }
      x | x >= 248 && x <= 250 -> do
        offsetDelta <- get
        pure $ ChopFrame { frameType: Word8 $ fromInt x, offsetDelta}
      251 -> do
        offsetDelta <- get
        pure $ SameFrameExtended { frameType:Word8 $ fromInt 251, offsetDelta }
      x | x >= 252 && x <= 254 -> do
        offsetDelta @ (Word16 w16) <- get
        let k = (toInt w16) - 251
        verificationTypeInfo <- A.fromFoldable <$> replicateM k get 
        pure $ AppendFrame { frameType: Word8 $ fromInt x, offsetDelta, verificationTypeInfo}
      255 -> do
        offsetDelta <- get
        numberOfLocals @ (Word16 nloc) <- get
        let k = toInt nloc
        locals <- A.fromFoldable <$> replicateM k get
        numberOfStackItems @ (Word16 nsi) <- get
        let k1 = toInt nsi
        stack <- A.fromFoldable <$> replicateM k1 get
        pure $ FullFrame 
                { frameType: Word8 $ fromInt 255
                , offsetDelta
                , numberOfLocals
                , locals
                , numberOfStackItems
                , stack
                }
      x -> fail $ \offset -> GenericParserError {offset, message:"Invalid StackMapFrame id "<> show x}

instance binaryStackMapTable :: Binary StackMapTable where
  put (StackMapTable {attributeNameIndex, attributeLength, numberOfEntries, entries}) =
    put attributeNameIndex <>
    put attributeLength <>
    put numberOfEntries <>
    putFoldable entries

  get = do
    attributeNameIndex <- get 
    attributeLength <- get 
    numberOfEntries @ (Word16 ne) <- get 
    entries <- A.fromFoldable <$> replicateM (toInt ne) get
    pure $ StackMapTable {attributeNameIndex, attributeLength, numberOfEntries, entries}



