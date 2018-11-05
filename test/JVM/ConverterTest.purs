module JVM.ConverterTest where

import Prelude

import Data.Array as A
import Data.Binary.Binary (put)
import Data.Binary.Put (runPut)
import Data.Binary.Types (Float32(..), Float64(..))
import Data.Either (fromRight, isLeft, isRight)
import Data.Map as M
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Global (isNaN)
import JVM.ClassFile (Class(..), ClassDirect(..))
import JVM.ConstantPool (Constant(..), PoolDirect)
import JVM.Converter.ToDirect (classFile2Direct)
import JVM.Converter.ToFile (classDirect2File)
import Node.FS.Aff (readdir) as ND
import Node.FS.Stats (isDirectory) as ND
import Node.FS.Sync (stat) as ND
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import Test.Unit (TestSuite, test)
import Test.Unit.Assert as Assert
import TestHelper (bufferEquals, readClassFile)

testToFileReadWrite :: String -> Aff Unit
testToFileReadWrite path =
  do
    (Tuple clss ab) <- readClassFile path
    let classFilePut = put clss
    resArr <- runPut classFilePut
    Assert.assert (show path) $ resArr `bufferEquals` ab

constPoolWithoutNaNs :: PoolDirect -> PoolDirect
constPoolWithoutNaNs =
  M.filter notIsNaN
  where
    notIsNaN (CFloat (Float32 n)) = not $ isNaN n
    notIsNaN (CDouble(Float64 n)) = not $ isNaN n
    notIsNaN _ = true

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
      when (clssDir1 /= clssDir) $ do
        let (ClassDirect (Class cl1 @ {constsPool:cp1})) = clssDir
            (ClassDirect (Class cl2 @ {constsPool:cp2})) = clssDir1
            cls1Nan = ClassDirect $ Class $ cl1 { constsPool = constPoolWithoutNaNs cp1 }
            cls2Nan = ClassDirect $ Class $ cl2 { constsPool = constPoolWithoutNaNs cp1 }
        Assert.assert "Generated class with ConstantPool without NaNs match " $ cls1Nan == cls2Nan

base :: Array String
base = [".", "test", "resources", "testclasses"]

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
