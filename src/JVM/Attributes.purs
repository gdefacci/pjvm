module JVM.Attributes where

import Prelude

import Data.Binary.Types (Word16, Word32)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (Tuple)

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

data AttributesDirect = AttributesDirect (Array (Tuple String String))

derive instance repGenericAttributesDirect :: Generic AttributesDirect _
derive instance eqAttributesDirect :: Eq AttributesDirect
instance showAttributesDirect :: Show AttributesDirect where
  show = genericShow

data AttributesFile = AttributesFile (Array Attribute)

derive instance repGenericAttributesFile :: Generic AttributesFile _
derive instance eqAttributesFile :: Eq AttributesFile
instance showAttributesFile :: Show AttributesFile where
  show = genericShow
