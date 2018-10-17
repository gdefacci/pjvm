module JVM.Attributes where

import Prelude

import Data.Binary.Binary (class Binary, put, get)
import Data.Binary.Decoder (ByteLengthString(..))
import Data.Binary.Types (Word16(..), Word32(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple)
import Data.UInt (fromInt)

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data Attribute = Attribute {
  attributeName :: Word16,
  attributeLength :: Word32,
  attributeValue :: String
}

derive instance repGenericAttribute :: Generic Attribute _
derive instance eqAttribute :: Eq Attribute
instance showAttribute :: Show Attribute where
  show = genericShow

newtype AttributesDirect = AttributesDirect (Array (Tuple String String))

derive instance repGenericAttributesDirect :: Generic AttributesDirect _
derive instance eqAttributesDirect :: Eq AttributesDirect
instance showAttributesDirect :: Show AttributesDirect where
  show = genericShow

newtype AttributesFile = AttributesFile (Array Attribute)

derive instance repGenericAttributesFile :: Generic AttributesFile _
derive instance eqAttributesFile :: Eq AttributesFile
instance showAttributesFile :: Show AttributesFile where
  show = genericShow

instance attributeBinary :: Binary Attribute where
  put (Attribute {attributeName, attributeLength, attributeValue}) =
    put attributeName <>
    put attributeLength <>
    put attributeValue

  get = do
    let getLen (ByteLengthString l _) = (Word32 $ fromInt l)
        getStr (ByteLengthString _ s) = s
    attributeName <- get
    (ByteLengthString len attributeValue) <- get
    pure $
      Attribute { attributeName, attributeValue, attributeLength : Word32 $ fromInt len }

