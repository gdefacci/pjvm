module JVM.TestGen where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (class MonadState)
import Data.Binary.Types (Word16(..))
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import JVM.Builder.Instructions (aastore, allocArray, aload_, dup, getStaticField, iconst_0, iconst_1, iconst_5, iload_, invokeSpecial, invokeStatic, invokeVirtual, loadString, pop)
import JVM.Builder.Monad (GState, GenError, generate, i0, newMethod, setStackSize)
import JVM.Converter.ToFile (classDirect2File)
import JVM.Flags (MethodAccessFlag(..))
import JVM.Instruction (IMM(..), Instruction(..))
import JVM.Members (FieldNameType(..), FieldType(..), MethodNameType(..), MethodSignature(..), ReturnSignature(..))
import Partial.Unsafe (unsafePartial)
import TestHelper (writeClassFile)

methodNameType :: String -> MethodSignature -> MethodNameType
methodNameType ntName ntSignature = MethodNameType {ntName, ntSignature}

fieldNameType :: String -> FieldType -> FieldNameType
fieldNameType ntName ntSignature = FieldNameType {ntName, ntSignature}

defaultStack :: { stackSize :: Int , maxLocals :: Int}
defaultStack = { stackSize : 32, maxLocals : 32 }

objectInit :: MethodNameType
objectInit = methodNameType "<init>" $ MethodSignature [] ReturnsVoid

stringClass :: FieldType
stringClass = ObjectType "java/lang/String"

printStreamClass :: FieldType
printStreamClass = ObjectType "java/io/PrintStream"

printf :: MethodNameType
printf = methodNameType "printf" $  MethodSignature
             [stringClass, ArrayType Nothing $ ObjectType "java/lang/Object"] (Returns $ printStreamClass)

test :: forall m. MonadThrow GenError m => MonadState GState m => m Unit
test = do

  -- Initializer method. Just calls java.lang.Object.<init>
  _ <- newMethod defaultStack [M_PUBLIC] "<init>" [] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 1

      aload_ I0
      invokeSpecial "java/lang/Object" objectInit
      i0 RETURN

  -- Declare hello() method and bind it's signature to hello.
  hello <- newMethod defaultStack [M_PUBLIC, M_STATIC] "hello" [IntType] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 8

      let out = fieldNameType "out" $ printStreamClass
      getStaticField "java/lang/System" out
      loadString "Здравствуй, мир!"
      let println = methodNameType "println" $ MethodSignature [stringClass] ReturnsVoid
      invokeVirtual "java/io/PrintStream" println
      getStaticField "java/lang/System" out
      loadString "Argument: %d\n"
      iconst_1
      allocArray "java/lang/Object"
      dup
      iconst_0
      iload_ I0
      let valueOf = methodNameType "valueOf" $ MethodSignature [IntType] (Returns $ ObjectType $ "java/lang/Integer")
      invokeStatic "java/lang/Integer" valueOf
      aastore
      invokeVirtual "java/io/PrintStream" printf
      -- Call Hello.hello()
      -- invokeStatic "Hello" helloJava
      pop
      i0 RETURN
  -- Main class method.
  _ <- newMethod defaultStack [M_PUBLIC, M_STATIC] "main" [ArrayType Nothing $ stringClass] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 1

      iconst_5
      -- Call previously declared method
      invokeStatic "Test" hello
      i0 RETURN
  pure unit

main :: Effect Unit
main = launchAff_ $ do
  log "========================= 1"
  let testClassDirectEither = generate "Test" test
      testClassDirect = unsafePartial $ fromRight testClassDirectEither
  log "========================= 2"
  let testClassEither = classDirect2File testClassDirect
      testClass = unsafePartial $ fromRight testClassEither
  log "========================= 3"
  writeClassFile "Test.class" testClass
