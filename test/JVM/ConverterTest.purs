module JVM.ConverterTest where

import Prelude

import TestHelper (bufferEquals, bufferToArray)
import Data.Array as A
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
import JVM.ClassFile (ClassFile)
import JVM.Converter.ToDirect (classFile2Direct)
import JVM.Converter.ToFile (classDirect2File)
import Node.Buffer (toArrayBuffer) as ND
import Node.FS.Aff (readFile, readdir) as ND
import Node.FS.Stats (isDirectory) as ND
import Node.FS.Sync (stat) as ND
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert

readClassFile :: String -> Aff (Tuple ClassFile ArrayBuffer)
readClassFile path =
  do
    log $ "Reading class " <> path
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
    when (not $ resArr `bufferEquals` ab) do
      let arr1 = bufferToArray resArr
          arr2 = bufferToArray ab
      log $ "len 1 " <> (show $ A.length arr1)
      log $ "len 2 " <> (show $ A.length arr2)
      log $ show $ arr1
      log "============="
      log $ show $ arr2
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
    when (clssFile /= clss) $ do
      let clssDirEither1 = classFile2Direct clss
      when (isLeft clssDirEither1) $ log $ (show clssDirEither1)
      Assert.assert ("can convert back "<> (show path) <> " to direct") $ isRight clssDirEither1
      let clssDir1 = (unsafePartial $ fromRight clssDirEither)
      Assert.assert ("the same class direct is generated " <> path) $ clssDir1 == clssDir

base :: Array String
base = [".", "test", "resources", "testclasses"]

-- allClasses :: (Array String) -> Aff FilePath
allClasses :: Array FilePath -> Aff (Array FilePath)
allClasses folder = do
  content <- ND.readdir $ Path.concat folder
  join <$> traverse getContent content
  where
    isClass pth = (Path.extname pth) == ".class"

    getContent :: FilePath -> Aff (Array FilePath)
    getContent f = do
      let fullpathSegments = A.snoc folder f
          fullpath = Path.concat fullpathSegments
      fileInfo <- liftEffect $ ND.stat fullpath
      if ND.isDirectory fileInfo
        then allClasses fullpathSegments
        else if isClass fullpath
          then liftEffect $ pure $ A.singleton fullpath
          else liftEffect $ pure mempty


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
