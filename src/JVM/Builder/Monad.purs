module JVM.Builder.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State (class MonadState, StateT(..), execStateT)
import Control.Monad.State as ST
import Data.Array as A
import Data.Binary.Binary (class Binary, put)
import Data.Binary.Put (putToString)
import Data.Binary.Types (Word16(..), Word32(..), Word8(..))
import Data.FoldableWithIndex (findWithIndex)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Data.UInt (fromInt, toInt)
import Effect.Class (class MonadEffect)
import JVM.Assembler (Code(..), encodeInstructions, encodeMethod)
import JVM.Attributes (AttributesDirect(..), AttributesFile(..))
import JVM.ClassFile (Class(..), ClassDirect(..), xCAFEBABE)
import JVM.ConstantPool (Constant(..), ConstantDirect, PoolDirect, long)
import JVM.Flags (AccessFlag(..), MethodAccessFlag)
import JVM.Instruction (Instruction)
import JVM.Members (FieldNameType(..), FieldType, Method(..), MethodDirect(..), MethodNameType(..), MethodSignature(..), ReturnSignature)

data GState = GState {
  generated :: Array Instruction,          -- ^ Already generated code (in current method)
  currentPool :: PoolDirect,               -- ^ Already generated constants pool
  nextPoolIndex :: Word16,                 -- ^ Next index to be used in constants pool
  doneMethods :: Array (MethodDirect),     -- ^ Already generated class methods
  currentMethod :: Maybe (MethodDirect),   -- ^ Current method
  stackSize :: Word16,                     -- ^ Maximum stack size for current method
  locals :: Word16                         -- ^ Maximum number of local variables for current method
  }

newtype Generate e m a = Generate ( ExceptT e (StateT GState m) a  )

data GenError = EncodeError String | UnexpectedEndMethod

derive newtype instance functorGenerate :: Functor m => Functor (Generate e m)
derive newtype instance applyGenerate :: Monad m => Apply (Generate e m)
derive newtype instance applicativeGenerate :: Monad m => Applicative (Generate e m)
derive newtype instance bindGenerate :: Monad m => Bind (Generate e m)
derive newtype instance monadGenerate :: Monad m => Monad (Generate e m)
derive newtype instance monadEffGenerate :: MonadEffect m => MonadEffect (Generate e m)
derive newtype instance monadStateGenerate :: Monad m => MonadState GState (Generate e m)
derive newtype instance monadThrowGenerate :: Monad m => MonadThrow GenError (Generate GenError m)
derive newtype instance monadErrorGenerate :: Monad m => MonadError GenError (Generate GenError m)

execGenerate :: forall e m a. Functor m => Generate e m a -> m GState
execGenerate (Generate m) =
  execStateT (runExceptT m) emptyGState
  where
    emptyGState = GState {
      generated : [],
      currentPool : M.empty,
      nextPoolIndex : Word16 $ fromInt 1,
      doneMethods : [],
      currentMethod : Nothing,
      stackSize : Word16 $ fromInt 496,
      locals : Word16 $ fromInt 0 }

-- | Lookup in the pool
lookupPool :: ConstantDirect -> PoolDirect -> Maybe Word16
lookupPool c pool =
  (\{index} -> index ) <$> findWithIndex (\_ -> \ce -> c == ce) pool

-- | Add a constant to pool
addItem :: forall m. MonadState GState m => ConstantDirect -> m Word16
addItem c = do
  (GState cuurentState @ {currentPool : pool, nextPoolIndex: wi @(Word16 i)}) <- ST.get
  case lookupPool c pool of
    (Just r) -> pure r
    Nothing -> do
      let constSize = if long c then 2 else 1
          pool' = M.insert wi c pool
          i' = Word16 $ fromInt $ (toInt i) + constSize
      ST.put $ GState $ cuurentState {currentPool = pool', nextPoolIndex = i'}
      pure wi

encode :: forall m a. MonadThrow GenError m => Binary a => Show a => a -> m String
encode a =
  case putToString (put a) of
    Nothing -> throwError $ EncodeError (show a)
    (Just r) -> pure r

addNT :: forall m sig.  MonadThrow GenError m
                        => MonadState GState m
                        => Binary sig
                        => Show sig
                        => {ntName:: String, ntSignature :: sig}
                        -> m Word16
addNT { ntName:name,  ntSignature:sig } = do
  bsig <- encode sig
  x <- addItem (CNameType name bsig)
  _ <- addItem (CUTF8 { getString : name })
  _ <- addItem (CUTF8 { getString : bsig })
  pure x

addSig :: forall m. MonadThrow GenError m
                    => MonadState GState m
                    => MethodSignature
                    -> m Word16
addSig c@(MethodSignature args ret) = do
  bsig <- encode c
  addItem (CUTF8 { getString : bsig })

-- | Add a constant into pool
addToPool :: forall m. MonadThrow GenError m => MonadState GState m => ConstantDirect -> m Word16
addToPool c@(CClass str) = do
  _ <- addItem (CUTF8 { getString : str })
  addItem c
addToPool c@(CField cls (FieldNameType nameRec)) = do
  _ <- addToPool (CClass cls)
  _ <- addNT nameRec
  addItem c
addToPool c@(CMethod cls (MethodNameType nameRec)) = do
  _ <- addToPool (CClass cls)
  _ <- addNT nameRec
  addItem c
addToPool c@(CIfaceMethod cls (MethodNameType nameRec)) = do
  _ <- addToPool (CClass cls)
  _ <- addNT nameRec
  addItem c
addToPool c@(CString str) = do
  _ <- addToPool (CUTF8 { getString : str })
  addItem c
addToPool c@(CNameType name sig) = do
  _ <- addItem (CUTF8 { getString : name })
  _ <- addItem (CUTF8 { getString : sig })
  addItem c
addToPool c = addItem c

putInstruction :: forall m. MonadState GState m => Instruction -> m Unit
putInstruction instr =
  void $ ST.modify $ \(GState st @ {generated}) ->
    GState $ st { generated = A.snoc generated instr }

-- | Generate one (zero-arguments) instruction
i0 :: forall m. MonadState GState m => Instruction -> m Unit
i0 = putInstruction

-- | Generate one one-argument instruction
i1 :: forall m. MonadThrow GenError m
                => MonadState GState m
                => (Word16 -> Instruction)
                -> ConstantDirect
                -> m Unit
i1 fn c = do
  ix <- addToPool c
  i0 (fn ix)

-- | Generate one one-argument instruction
i8 :: forall m. MonadThrow GenError m
                => MonadState GState m
                => (Word8 -> Instruction)
                -> ConstantDirect
                -> m Unit
i8 fn c = do
  (Word16 ix) <- addToPool c
  i0 (fn $ Word8 ix)

-- | Set maximum stack size for current method
setStackSize :: forall m. MonadState GState m => Word16 -> m Unit
setStackSize n =
  ST.modify_ $ \(GState st) -> GState $ st {stackSize = n}

-- | Set maximum number of local variables for current method
setMaxLocals :: forall m. MonadState GState m => Word16 -> m Unit
setMaxLocals n =
  ST.modify_ $ \(GState st) -> GState $ st {locals = n}

-- | Start generating new method
startMethod :: forall m. MonadThrow GenError m
                          => MonadState GState m
                          => { stackSize :: Int, maxLocals :: Int }
                          -> Array MethodAccessFlag
                          -> String
                          -> MethodSignature
                          -> m Unit
startMethod {stackSize, maxLocals} flags methodName methodSignature = do
  _ <- addToPool (CString methodName)
  _ <- addSig methodSignature
  _ <- setStackSize $ Word16 $ fromInt stackSize
  _ <- setMaxLocals $ Word16 $ fromInt maxLocals
  (GState st) <- ST.get
  let methodAttributesCount = Word16 $ fromInt 0
      methodAttributes = AttributesDirect []
      method = MethodDirect $ Method
        { methodAccessFlags : S.fromFoldable flags
        , methodName
        , methodSignature
        , methodAttributesCount
        , methodAttributes
        }
  ST.put $ GState $ st {generated = [], currentMethod = Just method }

-- | End of method generation
endMethod :: forall m. MonadState GState m => MonadThrow GenError m => m Unit
endMethod = do
  (st @ GState { currentMethod:m }) <- ST.get
  case m of
    Nothing -> throwError UnexpectedEndMethod
    (Just (MethodDirect (Method method @ {}))) -> do
      let code = genCode st
          method' = MethodDirect $ Method $ method
            { methodAttributes = AttributesDirect $ [(Tuple "Code" $ encodeMethod code)]
            , methodAttributesCount = Word16 $ fromInt 1 }
      ST.modify_ $ \(GState st1 @ {doneMethods}) -> GState $
        st1 { generated = []
        , currentMethod = Nothing
        , doneMethods = A.snoc doneMethods method'}

encodedCodeLength :: GState -> Word32
encodedCodeLength (GState {generated}) = Word32 <<< fromInt <<< A.length <<< encodeInstructions $ generated

genCode :: GState -> Code
genCode st @ (GState {stackSize, locals, generated}) =
    Code {
    codeStackSize : stackSize,
    codeMaxLocals : locals,
    codeLength : encodedCodeLength st,
    codeInstructions : generated,
    codeExceptionsN : Word16 $ fromInt 0,
    codeExceptions : [],
    codeAttrsN : Word16 $ fromInt 0,
    codeAttributes : AttributesFile [] }

-- | Generate new method
newMethod :: forall m. MonadThrow GenError m
          => MonadState GState m
          => { stackSize :: Int, maxLocals :: Int }
          -> Array MethodAccessFlag   -- ^ Access flags for method (public, static etc)
          -> String                   -- ^ Method name
          -> Array FieldType          -- ^ Signatures of method arguments
          -> ReturnSignature          -- ^ Method return signature
          -> m Unit                   -- ^ Generator for method code
          -> m MethodNameType
newMethod opts flags ntName args ret gen = do
  let ntSignature = MethodSignature args ret
  startMethod opts flags ntName ntSignature
  gen
  endMethod
  pure $ MethodNameType { ntName, ntSignature }

-- | Start class generation.
initClass :: forall m. MonadThrow GenError m => MonadState GState m => String -> m Unit
initClass name = do
  _ <- addToPool (CClass "java/lang/Object")
  _ <- addToPool (CClass name)
  void $ addToPool (CString "Code")

defaultClass :: String -> ClassDirect
defaultClass name =
  ClassDirect $ Class $ {
        magic : xCAFEBABE,
        minorVersion : Word16 $ fromInt 0,
        majorVersion : Word16 $ fromInt 50,
        constsPoolSize : Word16 $ fromInt 0,
        constsPool : M.empty,
        accessFlags : S.fromFoldable [],
        thisClass : name,
        superClass : "java/lang/Object",
        interfacesCount : Word16 $ fromInt 0,
        interfaces : [],
        classFieldsCount : Word16 $ fromInt 0,
        classFields : [],
        classMethodsCount : Word16 $ fromInt 0,
        classMethods : [],
        classAttributesCount : Word16 $ fromInt 0,
        classAttributes : AttributesDirect []}

-- | Generate a class
generate ::forall m a. Monad m
              => String
              -> Generate GenError m a
              -> m ClassDirect
generate name gen = do
  let generator = do
        initClass name
        gen
  res @ (GState {currentPool, doneMethods}) <- execGenerate generator
  let code = genCode res
      (ClassDirect (Class classDefault)) = defaultClass name
  pure $ ClassDirect $ Class $ classDefault
    { constsPoolSize = Word16 $ fromInt $ M.size currentPool
    , constsPool = currentPool
    , accessFlags = S.fromFoldable [ACC_PUBLIC, ACC_SUPER]
    , superClass = "java/lang/Object"
    , classMethodsCount = Word16 $ fromInt $ A.length doneMethods
    , classMethods = doneMethods }