module JVM.Attributes where

import Prelude

import Data.Map as M
import Data.Binary.Binary (class Binary, putFoldable, get, put)
import Data.Binary.Decoder (getRep)
import Data.Binary.Types (Word16, Word32, Word8)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
import Data.UInt (toInt)

-- | Any (class/ field/ method/ ...) attribute format.
-- Some formats specify special formats for @attributeValue@.
data Attribute = Attribute {
  attributeName :: Word16,
  attributeLength :: Word32,
  attributeValue :: Array Word8
}

derive instance repGenericAttribute :: Generic Attribute _
derive instance eqAttribute :: Eq Attribute
instance showAttribute :: Show Attribute where
  show = genericShow

newtype AttributesDirect = AttributesDirect (Array (Tuple String (Array Word8)))

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
    putFoldable attributeValue

  get = do
    attributeName <- get
    attributeLength <- get
    attributeValue <- getRep (toInt $ unwrap attributeLength) get
    pure $
      Attribute { attributeName, attributeValue, attributeLength }

attributesList :: AttributesFile -> Array Attribute
attributesList (AttributesFile attrs) = attrs

-- | Map of attributes at Direct stage
attributesDirectLookup :: String -> AttributesDirect -> Maybe (Array Word8)
attributesDirectLookup nm (AttributesDirect m) = M.lookup nm (M.fromFoldable m)
