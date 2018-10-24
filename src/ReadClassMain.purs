module Test.ReadClassMain where

import Prelude

import Data.Array (find)
import Data.Array as A
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset)
import Data.Binary.Binary (get, put)
import Data.Binary.Decoder (decodeFull)
import Data.Binary.Put (runPut)
import Data.Either (Either, fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, runAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import JVM.ClassFile (Class(..), ClassDirect(..), ClassFile(..))
import JVM.ConstantPool (PoolDirect)
import JVM.Converter.ToDirect (File2DirectError, classFile2Direct)
import Node.Buffer as ND
import Node.Encoding (Encoding(..))
import Node.FS.Aff as ND
import Node.Path (concat)
import Partial.Unsafe (unsafePartial)

data Diff = Diff (Array Int) (Array Int)
            | DifferentSize Int Int

derive instance genericDiff :: Generic Diff _

instance showDiff :: Show Diff where
  show = genericShow

bufferEqual :: ArrayBuffer -> ArrayBuffer -> Boolean
bufferEqual bf1 bf2 =
  let dv1 = DV.whole bf1
      dv2 = DV.whole bf2
      toArray = asUint8Array >>> toIntArray
      arr1 = toArray dv1
      arr2 = toArray dv2
  in arr1 == arr2

main :: Effect Unit
main = launchAff_ $ do
  buff <- ND.readFile $ concat [".", "test", "resources", "Class1.class"]
  ab <- liftEffect $ ND.toArrayBuffer buff
  (clss :: ClassFile) <- liftEffect $ decodeFull get ab
  let clssDirRes :: Either File2DirectError ClassDirect
      clssDirRes = classFile2Direct clss
  let (ClassFile (Class {constsPool})) = clss
  let clssDir = (unsafePartial $ fromRight) clssDirRes
  let writer = put clss
  resArr <- liftEffect $ runPut writer
  log $ "Buffer equals : " <> (show $ bufferEqual ab resArr)
  log $ "Class File"
  log $ show clss
  log "============================="
  log $ "Class Direct"
  log $ show clssDir
  log "============================="