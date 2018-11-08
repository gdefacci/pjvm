module JVM.TestGen where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (class MonadState)
import Data.Binary.Types (Word16(..))
import Data.Either (Either, fromRight)
import Data.Maybe (Maybe(..))
import Data.UInt (fromInt)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import JVM.Builder.Instructions (aastore, allocArray, aload_, dup, getStaticField, goto, iconst_0, iconst_1, iconst_5, if_icmp, iload_, invokeSpecial, invokeStatic, invokeVirtual, ldc1, loadString, pop)
import JVM.Builder.Monad (GState, GenError, Generate(..), generate, i0, newBlock, newLabel, newMethod, setMaxLocals, setStackSize)
import JVM.ClassFile (ClassDirect)
import JVM.ConstantPool (Constant(..))
import JVM.Converter.ToFile (classDirect2File)
import JVM.Flags (MethodAccessFlag(..))
import JVM.Instruction (CMP(..), IMM(..), Instruction(..))
import JVM.Members (FieldNameType(..), JVMType(..), MethodNameType(..), MethodSignature(..), ReturnSignature(..))
import Partial.Unsafe (unsafePartial)
import TestHelper (writeClassFile)

methodNameType :: String -> MethodSignature -> MethodNameType
methodNameType ntName ntSignature = MethodNameType {ntName, ntSignature}

fieldNameType :: String -> JVMType -> FieldNameType
fieldNameType ntName ntSignature = FieldNameType {ntName, ntSignature}

objectInit :: MethodNameType
objectInit = methodNameType "<init>" $ MethodSignature [] ReturnsVoid

stringClass :: JVMType
stringClass = ObjectType "java/lang/String"

printStreamClass :: JVMType
printStreamClass = ObjectType "java/io/PrintStream"

objectClass :: JVMType
objectClass = ObjectType "java/lang/Object"

printf :: MethodNameType
printf = methodNameType "printf" $  MethodSignature
             [stringClass, ArrayType Nothing $ objectClass] (Returns $ printStreamClass)

println = methodNameType "println" $ MethodSignature [stringClass] ReturnsVoid

out = fieldNameType "out" $ printStreamClass

testGen :: forall m. MonadThrow GenError m => MonadState GState m => m Unit
testGen = do
  -- Initializer method. Just calls java.lang.Object.<init>
  _ <- newMethod [M_PUBLIC] "<init>" [] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 1

      aload_ I0
      invokeSpecial "java/lang/Object" objectInit
      i0 RETURN

  -- Declare hello() method and bind it's signature to hello.
  hello <- newMethod [M_PUBLIC, M_STATIC] "hello" [IntType] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 8

      getStaticField "java/lang/System" out
      loadString "Здравствуй, мир!"
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
  _ <- newMethod [M_PUBLIC, M_STATIC] "main" [ArrayType Nothing $ stringClass] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 1

      iconst_5
      -- Call previously declared method
      invokeStatic "Test" hello
      i0 RETURN
  pure unit

testCond :: forall m. MonadThrow GenError m => MonadState GState m => m Unit
testCond = do
    _ <- newMethod [M_PUBLIC] "<init>" [] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 1
      setMaxLocals $ Word16 $ fromInt 1

      aload_ I0
      invokeSpecial "java/lang/Object" objectInit
      i0 RETURN
    _ <- newMethod [] "myCond" [IntType] ReturnsVoid $ do
      setStackSize $ Word16 $ fromInt 2
      setMaxLocals $ Word16 $ fromInt 2

      iload_ I1
      iconst_5
      labelElse <- newLabel
      labelReturn <- newLabel
      if_icmp C_LE labelElse
      getStaticField "java/lang/System" out
      ldc1 $ CString "Less than 5"
      invokeVirtual "java/io/PrintStream" println
      goto labelReturn
      newBlock labelElse $ do
        getStaticField "java/lang/System" out
        ldc1 $ CString "Greater than 5"
        invokeVirtual "java/io/PrintStream" println
      newBlock labelReturn $
        i0 RETURN
    pure unit

saveClass :: String -> Generate GenError (Either GenError) Unit -> Aff Unit
saveClass name genClass = do
  let testClassDirectEither :: Either GenError ClassDirect
      testClassDirectEither = generate name genClass
  log $ show testClassDirectEither
  let testClassDirect = unsafePartial $ fromRight testClassDirectEither
  let testClassEither = classDirect2File testClassDirect
      testClass = unsafePartial $ fromRight testClassEither
  writeClassFile (name <> ".class") testClass

main :: Effect Unit
main = launchAff_ $ saveClass "Cond" testCond
