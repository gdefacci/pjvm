module JVM.ConverterTest where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (asUint8Array, toIntArray)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Binary.Binary (get, put)
import Data.Binary.Decoder (decodeFull)
import Data.Binary.Put (runPut)
import Data.Either (fromRight, isLeft, isRight)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import JVM.ClassFile (Class(..), ClassFile(..))
import JVM.Converter.ToDirect (classFile2Direct)
import JVM.Converter.ToFile (classDirect2File)
import Node.Buffer (toArrayBuffer) as ND
import Node.FS.Aff (readFile, readdir) as ND
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert

bufferEquals :: ArrayBuffer -> ArrayBuffer -> Boolean
bufferEquals bf1 bf2 =
  let dv1 = DV.whole bf1
      dv2 = DV.whole bf2
      toArray = asUint8Array >>> toIntArray
      arr1 = toArray dv1
      arr2 = toArray dv2
  in arr1 == arr2

readClassFile :: String -> Aff (Tuple ClassFile ArrayBuffer)
readClassFile path =
  do
    buff <- ND.readFile path
    ab <- liftEffect $ ND.toArrayBuffer buff
    (clss :: ClassFile) <- liftEffect $ decodeFull get ab
    pure (Tuple clss ab)

testToFileReadWrite :: String -> Aff Unit
testToFileReadWrite path =
  do
    (Tuple clss ab) <- readClassFile path
    let classFilePut = put clss
    resArr <- runPut classFilePut
    Assert.assert (show path) $ resArr `bufferEquals` ab

testToDirectReadWrite :: String -> Aff Unit
testToDirectReadWrite path =
  do
    clss <- fst <$> readClassFile path
    let clssDirEither = classFile2Direct clss
    when (isLeft clssDirEither) $ log $ (show clssDirEither)
    Assert.assert ("can convert"<> (show path) <> "to direct") $ isRight clssDirEither
    let clssDir = (unsafePartial $ fromRight clssDirEither)
    let clssFileEither = classDirect2File clssDir
    when (isLeft clssFileEither) $ log $ (show clssFileEither)
    Assert.assert ("toDirect can convert back to file " <> path) $ isRight clssFileEither
    let clssFile = (unsafePartial $ fromRight clssFileEither)
    when (clssFile /= clss) $ log $ "\n" <> (show clss) <> "\n\n\n" <> (show clssFile)
    Assert.assert ("toDirect read and write " <> path) $ clssFile == clss

base :: Array String
base = [".", "test", "resources", "testclasses"]

testClasses :: Aff (Array Path.FilePath)
testClasses = do
    files <- ND.readdir $ Path.concat base
    pure $ (\cl -> Path.concat $ A.snoc base cl) <$> (A.filter (\pth -> (Path.extname pth) == ".class") files)

spec :: TestSuite
spec = do
  test "testToFileReadWrite" $ do
    void $ testClasses >>= traverse testToFileReadWrite
  test "testToDirectReadWrite" $ do
    void $ testClasses >>= (traverse testToDirectReadWrite)

