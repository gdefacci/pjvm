module TestHelper where

import Prelude

import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Binary.Binary (get, put)
import Data.Binary.Decoder (decodeFull)
import Data.Binary.Put (runPut)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import JVM.ClassFile (ClassFile)
import Node.Buffer (fromArrayBuffer)
import Node.Buffer (toArrayBuffer) as ND
import Node.FS.Aff (readFile, writeFile) as ND
import Node.Path (FilePath)

bufferToArray :: ArrayBuffer -> Array Int
bufferToArray bf1 =
  let dv1 = DV.whole bf1
      toArray = asUint8Array >>> toIntArray
  in toArray dv1

bufferEquals :: ArrayBuffer -> ArrayBuffer -> Boolean
bufferEquals bf1 bf2 = (bufferToArray bf1) == (bufferToArray bf2)

readClassFile :: FilePath -> Aff (Tuple ClassFile ArrayBuffer)
readClassFile path =
  do
    buff <- ND.readFile path
    ab <- liftEffect $ ND.toArrayBuffer buff
    (clss :: ClassFile) <- decodeFull get ab
    pure (Tuple clss ab)

writeClassFile :: FilePath -> ClassFile -> Aff Unit
writeClassFile path content = do
  arrBuff <- runPut $ put content
  buf <- liftEffect $ fromArrayBuffer arrBuff
  ND.writeFile path buf