module Test.ReadClassMain where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.State (State)
import Data.Array (find, intercalate)
import Data.Array as A
import Data.Array.Partial (tail)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset)
import Data.Binary.Binary (get, put)
import Data.Binary.Decoder (ParserError, ParserState, decode, decodeFull, getRest)
import Data.Binary.Put (runPut)
import Data.Binary.Types (Word8(..))
import Data.Either (Either(..), fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.UInt (toInt)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, runAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import JVM.Assembler (Code(..))
import JVM.Attributes (AttributesDirect(..))
import JVM.ClassFile (Class(..), ClassDirect(..), ClassFile(..))
import JVM.ConstantPool (PoolDirect)
import JVM.Converter.ToDirect (File2DirectError, classFile2Direct)
import JVM.Converter.ToFile (classDirect2File)
import JVM.Instruction (Instruction)
import JVM.Members (Method(..), MethodDirect(..))
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

getClassMethodsCode :: ClassDirect -> Effect (Array (Tuple String Code))
getClassMethodsCode (ClassDirect (Class {classMethods})) =
  traverse showClassMethod classMethods
  where
    showClassMethod (MethodDirect (Method {methodName, methodAttributes : AttributesDirect attrsArr })) =
      case M.lookup "Code" (M.fromFoldable attrsArr) of
        Nothing -> throw "cant find code attribute"
        (Just code) -> (Tuple methodName) <$> decodeWord8Array get code

decodeWord8Array :: forall a. ExceptT ParserError (State ParserState) a -> Array Word8 -> Effect a
decodeWord8Array decoder arr =
  let buff = AB.fromIntArray $ (unwrap >>> toInt) <$> arr
  in case decode decoder buff of
    (Left err) -> throw $ show err
    (Right (Tuple _ a)) -> pure a

showMethod :: (Tuple String Code) -> Effect Unit
showMethod (Tuple nm (Code {codeInstructions})) = do
  log $ "method name " <> nm
  log $ ""
  log $ intercalate "\n" (show <$> codeInstructions)

main :: Effect Unit
main = launchAff_ $ do
  buff <- ND.readFile $ concat [".", "test", "resources", "Rectangle$Point.class"]
  ab <- liftEffect $ ND.toArrayBuffer buff
  (clss :: ClassFile) <- liftEffect $ decodeFull get ab
  let clssDirRes = classFile2Direct clss
      clssDir = (unsafePartial $ fromRight) clssDirRes
      classFilePut = put clss
      clssFile = unsafePartial $ fromRight $ classDirect2File clssDir
  mthds <- liftEffect $ getClassMethodsCode clssDir
  resArr <- runPut classFilePut
  log $ "class files equals " <> (show $ clssFile == clss)
  log $ "Buffer equals : " <> (show $ bufferEqual ab resArr)
  log $ "Class File"
  log $ show clss
  log "============================="
  log $ "Class Direct"
  log $ show clssDir
  _ <- liftEffect $ traverse showMethod mthds
  log "============================="