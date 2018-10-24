module Test.ReadClassMain where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
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
import JVM.Convereter (File2DirectError, classFile2Direct, poolFile2Direct)
import Node.Buffer as ND
import Node.Encoding (Encoding(..))
import Node.FS.Aff as ND
import Node.Path (concat)
import Partial.Unsafe (unsafePartial)

data Diff = Diff ByteOffset Int Int
            | DifferentSize Int Int

derive instance genericDiff :: Generic Diff _

instance showDiff :: Show Diff where
  show = genericShow

bufferEqual :: ArrayBuffer -> ArrayBuffer -> (Maybe Diff)
bufferEqual bf1 bf2 =
  let len1 = AB.byteLength bf1
      len2 = AB.byteLength bf2
  in if len1 /= len2
        then Just $ DifferentSize len1 len2
        else checkSameElements (DV.whole bf1) (DV.whole bf2) len1
  where
    checkSameElements dv1 dv2 len =
      A.foldl accum Nothing $ A.range 0 (len - 1)
      where
        accum j @ (Just _) _ = j
        accum _ idx =
          let eff = do
                      a1 <- (unsafePartial $ fromJust) <$> DV.getInt8 dv1 idx
                      a2 <- (unsafePartial $ fromJust) <$> DV.getInt8 dv1 idx
                      if a1 /= a2
                        then pure $ Just $ Diff idx a1 a2
                        else pure $ Nothing
          in unsafePerformEffect eff

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