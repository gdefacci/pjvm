module JVM.Flags where

import Data.BitMask
import Prelude

import Data.Bounded (class Bounded)
import Data.Enum (class Enum)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.NonEmpty (NonEmpty(..))
import Data.Ord (class Ord)
import Test.QuickCheck.Arbitrary (class Arbitrary)

data AccessFlag =
  ACC_PUBLIC         -- 0x0001 Declared public; may be accessed from outside its package.
  | ACC_FINAL        -- 0x0010 Declared final; no subclasses allowed.
  | ACC_SUPER        -- 0x0020 Treat superclass methods specially when invoked by the invokespecial instruction.
  | ACC_INTERFACE    -- 0x0200 Is an interface, not a class.
  | ACC_ABSTRACT     -- 0x0400 Declared abstract; must not be instantiated.
  | ACC_SYNTHETIC    -- 0x1000 Declared synthetic; not present in the source code.
  | ACC_ANNOTATION   -- 0x2000 Declared as an annotation type.
  | ACC_ENUM         -- 0x4000 Declared as an enum ty

derive instance eqAccessFlag :: Eq AccessFlag

instance ordAccessFlag :: Ord AccessFlag where
  compare = compareBitMask

instance bitMaskAccessFlag :: BitMask AccessFlag where
  all = NonEmpty ACC_PUBLIC [ACC_FINAL, ACC_SUPER, ACC_INTERFACE, ACC_ABSTRACT,
                             ACC_SYNTHETIC, ACC_ANNOTATION, ACC_ENUM]
  maskBit x = case x of
    ACC_PUBLIC       -> 0        -- 0x0001
    ACC_FINAL        -> 4        -- 0x0010
    ACC_SUPER        -> 5        -- 0x0020
    ACC_INTERFACE    -> 9        -- 0x0200
    ACC_ABSTRACT     -> 10       -- 0x0400
    ACC_SYNTHETIC    -> 12       -- 0x1000
    ACC_ANNOTATION   -> 13       -- 0x2000
    ACC_ENUM         -> 14       -- 0x4000

instance boundedAccessFlag :: Bounded AccessFlag where
  top = maxBitMask
  bottom = minBitMask

instance enumAccessFlag :: Enum AccessFlag where
  succ = succBitMask
  pred = predBitMask

instance arbAccessFlag :: Arbitrary AccessFlag where
  arbitrary = arbitraryBitMask

derive instance repGenericAccessFlag :: Generic AccessFlag _
instance showAccessFlag :: Show AccessFlag where
  show = genericShow

data MethodAccessFlag =
  M_PUBLIC                       -- 0x0001 Declared public; may be accessed from outside its package.
  | M_PRIVATE                    -- 0x0002 Declared private; accessible only within the defining class.
  | M_PROTECTED                  -- 0x0004 Declared protected; may be accessed within subclasses.
  | M_STATIC                     -- 0x0008 Declared static.
  | M_FINAL                      -- 0x0010 Declared final; must not be overridden (§5.4.5).
  | M_SYNCHRONIZED               -- 0x0020 Declared synchronized; invocation is wrapped by a monitor use.
  | M_BRIDGE                     -- 0x0040 A bridge method, generated by the compiler.
  | M_VARARGS                    -- 0x0080 Declared with variable number of arguments.
  | M_NATIVE                     -- 0x0100 Declared native; implemented in a language other than Java.
  | M_ABSTRACT                   -- 0x0400 Declared abstract; no implementation is provided.
  | M_STRICT                     -- 0x0800 Declared strictfp; floating-point mode is FP-strict.
  | M_SYNTHETIC                  -- 0x1000

derive instance eqMethodAccessFlag :: Eq MethodAccessFlag

instance ordMethodAccessFlag :: Ord MethodAccessFlag where
  compare = compareBitMask

instance bitMaskMethodAccessFlag :: BitMask MethodAccessFlag where
  all = NonEmpty M_PUBLIC [ M_PRIVATE, M_PROTECTED, M_STATIC, M_FINAL, M_SYNCHRONIZED, M_BRIDGE, 
                            M_VARARGS, M_NATIVE, M_ABSTRACT, M_STRICT, M_SYNTHETIC ]
  maskBit x = case x of
    M_PUBLIC           ->  0        -- 0x0001
    M_PRIVATE          ->  1        -- 0x0002
    M_PROTECTED        ->  2        -- 0x0004
    M_STATIC           ->  3        -- 0x0008
    M_FINAL            ->  4        -- 0x0010
    M_SYNCHRONIZED     ->  5        -- 0x0020
    M_BRIDGE           ->  6        -- 0x0040
    M_VARARGS          ->  7        -- 0x0080
    M_NATIVE           ->  8        -- 0x0100
    M_ABSTRACT         ->  10       -- 0x0400
    M_STRICT           ->  11       -- 0x0800
    M_SYNTHETIC        ->  12       -- 0x1000

instance boundedMethodAccessFlag :: Bounded MethodAccessFlag where
  top = maxBitMask
  bottom = minBitMask

instance enumMethodAccessFlag :: Enum MethodAccessFlag where
  succ = succBitMask
  pred = predBitMask

instance arbMethodAccessFlag :: Arbitrary MethodAccessFlag where
  arbitrary = arbitraryBitMask

derive instance repGenericMethodAccessFlag :: Generic MethodAccessFlag _
instance showMethodAccessFlag :: Show MethodAccessFlag where
  show = genericShow

data FieldAccessFlag =
  F_PUBLIC                    -- 0x0001 Declared public; may be accessed from outside its package.
  | F_PRIVATE                 -- 0x0002 Declared private; usable only within the defining class.
  | F_PROTECTED               -- 0x0004 Declared protected; may be accessed within subclasses.
  | F_STATIC                  -- 0x0008 Declared static.
  | F_FINAL                   -- 0x0010 Declared final; never directly assigned to after object construction (JLS §17.5).
  | F_VOLATILE                -- 0x0040 Declared volatile; cannot be cached.
  | F_TRANSIENT               -- 0x0080 Declared transient; not written or read by a persistent object manager.
  | F_SYNTHETIC               -- 0x1000 Declared synthetic; not present in the source code.
  | F_ENUM                    -- 0x4000 Declared as an element of an enum.

derive instance eqFieldAccessFlag :: Eq FieldAccessFlag

instance fieldAccessFlagBitMask :: BitMask FieldAccessFlag where
  all =  NonEmpty F_PUBLIC [F_PRIVATE, F_PROTECTED, F_STATIC, F_FINAL, F_VOLATILE, F_TRANSIENT, F_SYNTHETIC, F_ENUM]
  maskBit x = case x of
    F_PUBLIC       -> 0             -- 0x0001
    F_PRIVATE      -> 1             -- 0x0002
    F_PROTECTED    -> 2             -- 0x0004
    F_STATIC       -> 3             -- 0x0008
    F_FINAL        -> 4             -- 0x0010
    F_VOLATILE     -> 6             -- 0x0040
    F_TRANSIENT    -> 7             -- 0x0080
    F_SYNTHETIC    -> 12            -- 0x1000
    F_ENUM         -> 14            -- 0x4000 -

instance ordFieldAccessFlag :: Ord FieldAccessFlag where
  compare = compareBitMask

instance boundedFieldAccessFlag :: Bounded FieldAccessFlag where
  top = maxBitMask
  bottom = minBitMask

instance enumFieldAccessFlag :: Enum FieldAccessFlag where
  succ = succBitMask
  pred = predBitMask

instance arbFieldAccessFlag :: Arbitrary FieldAccessFlag where
  arbitrary = arbitraryBitMask

derive instance repGenericFieldAccessFlag :: Generic FieldAccessFlag _
instance showFieldAccessFlag :: Show FieldAccessFlag where
  show = genericShow