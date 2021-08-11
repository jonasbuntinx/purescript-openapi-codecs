module Data.JSONSchema where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, jsonNull)
import Data.Codec (basicCodec, (>~>))
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, decode, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Lens (_Just)
import Data.Lens as L
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..))
import Data.BooleanInt (BooleanInt, booleanIntCodec)
import Data.OAIMap (OAIMap, _oaiMap, oaiMapCodec)
import Data.Reference (Reference, referenceCodec)
import Data.ReferenceOr (ReferenceOr, referenceOrCodec)

newtype JSONSchema
  = JSONSchema
  { title :: Maybe String
  , multipleOf :: Maybe Number
  , maximum :: Maybe Number
  , exclusiveMaximum :: Maybe BooleanInt
  , minimum :: Maybe Number
  , exclusiveMinimum :: Maybe BooleanInt
  , maxLength :: Maybe Int
  , minLength :: Maybe Int
  , pattern :: Maybe String
  , maxItems :: Maybe Int
  , minItems :: Maybe Int
  , uniqueItems :: Maybe Boolean
  , maxProperties :: Maybe Int
  , minProperties :: Maybe Int
  , required :: Maybe (Array String)
  -- , enum :: Maybe (Array JSON)
  , allOf :: Maybe (Array (ReferenceOr JSONSchema))
  , oneOf :: Maybe (Array (ReferenceOr JSONSchema))
  , anyOf :: Maybe (Array (ReferenceOr JSONSchema))
  , items :: Maybe Items
  , properties :: Maybe (OAIMap (ReferenceOr JSONSchema))
  -- , additionalProperties :: Maybe Additionals
  , description :: Maybe String
  -- , default :: Maybe JSON
  , nullable :: Maybe Boolean
  -- , discriminator :: Maybe Discriminator
  , readOnly :: Maybe Boolean
  , writeOnly :: Maybe Boolean
  -- , example :: Maybe JSON
  -- , externalDocs :: Maybe ExternalDocumentation
  , deprecated :: Maybe Boolean
  -- , xml :: Maybe XML
  , format :: Maybe String
  , type :: Maybe String
  , not :: Maybe (ReferenceOr JSONSchema)
  }

derive instance newtypeJSONSchema :: Newtype JSONSchema _

derive instance genericJSONSchema :: Generic JSONSchema _

instance eqJSONSchema :: Eq JSONSchema where
  eq s = genericEq s

instance showJSONSchema :: Show JSONSchema where
  show s = genericShow s

data Items
  = ItemsAsTuple (Array (ReferenceOr JSONSchema))
  | SingleItem JSONSchema
  | SingleItemReference Reference

derive instance genericItems :: Generic Items _

instance eqItems :: Eq Items where
  eq b = genericEq b

instance showItems :: Show Items where
  show b = genericShow b

-- Codecs
jsonSchemaCodec :: JsonCodec JSONSchema
jsonSchemaCodec =
  CA.fix \_codec ->
    CAM.addDefaultField "title" jsonNull
      >~> CAM.addDefaultField "multipleOf" jsonNull
      >~> CAM.addDefaultField "maximum" jsonNull
      >~> CAM.addDefaultField "exclusiveMaximum" jsonNull
      >~> CAM.addDefaultField "minimum" jsonNull
      >~> CAM.addDefaultField "exclusiveMinimum" jsonNull
      >~> CAM.addDefaultField "maxLength" jsonNull
      >~> CAM.addDefaultField "minLength" jsonNull
      >~> CAM.addDefaultField "pattern" jsonNull
      >~> CAM.addDefaultField "maxItems" jsonNull
      >~> CAM.addDefaultField "minItems" jsonNull
      >~> CAM.addDefaultField "uniqueItems" jsonNull
      >~> CAM.addDefaultField "maxProperties" jsonNull
      >~> CAM.addDefaultField "minProperties" jsonNull
      >~> CAM.addDefaultField "required" jsonNull
      >~> CAM.addDefaultField "allOf" jsonNull
      >~> CAM.addDefaultField "oneOf" jsonNull
      >~> CAM.addDefaultField "anyOf" jsonNull
      >~> CAM.addDefaultField "items" jsonNull
      >~> CAM.addDefaultField "properties" jsonNull
      >~> CAM.addDefaultField "description" jsonNull
      >~> CAM.addDefaultField "nullable" jsonNull
      >~> CAM.addDefaultField "readOnly" jsonNull
      >~> CAM.addDefaultField "writeOnly" jsonNull
      >~> CAM.addDefaultField "deprecated" jsonNull
      >~> CAM.addDefaultField "format" jsonNull
      >~> CAM.addDefaultField "type" jsonNull
      >~> CAM.addDefaultField "not" jsonNull
      >~> codec _codec
  where
  codec _codec =
    wrapIso JSONSchema
      ( CAR.object "JSONSchema"
          { title: CAC.maybe CA.string
          , multipleOf: CAC.maybe CA.number
          , maximum: CAC.maybe CA.number
          , exclusiveMaximum: CAC.maybe booleanIntCodec
          , minimum: CAC.maybe CA.number
          , exclusiveMinimum: CAC.maybe booleanIntCodec
          , maxLength: CAC.maybe CA.int
          , minLength: CAC.maybe CA.int
          , pattern: CAC.maybe CA.string
          , maxItems: CAC.maybe CA.int
          , minItems: CAC.maybe CA.int
          , uniqueItems: CAC.maybe CA.boolean
          , maxProperties: CAC.maybe CA.int
          , minProperties: CAC.maybe CA.int
          , required: CAC.maybe (CAC.array CA.string)
          , allOf: CAC.maybe (CAC.array (referenceOrCodec _codec))
          , oneOf: CAC.maybe (CAC.array (referenceOrCodec _codec))
          , anyOf: CAC.maybe (CAC.array (referenceOrCodec _codec))
          , items: CAC.maybe itemsCodec
          , properties: CAC.maybe (oaiMapCodec (referenceOrCodec _codec))
          , description: CAC.maybe CA.string
          , nullable: CAC.maybe CA.boolean
          , readOnly: CAC.maybe CA.boolean
          , writeOnly: CAC.maybe CA.boolean
          , deprecated: CAC.maybe CA.boolean
          , format: CAC.maybe CA.string
          , type: CAC.maybe CA.string
          , not: CAC.maybe (referenceOrCodec _codec)
          }
      )

itemsCodec :: JsonCodec Items
itemsCodec = basicCodec dec enc
  where
  dec :: Json -> Either JsonDecodeError Items
  dec j =
    (ItemsAsTuple <$> decode (CA.array (referenceOrCodec jsonSchemaCodec)) j)
      <|> (SingleItem <$> decode jsonSchemaCodec j)
      <|> (SingleItemReference <$> decode referenceCodec j)

  enc :: Items -> Json
  enc = case _ of
    ItemsAsTuple a -> encode (CA.array (referenceOrCodec jsonSchemaCodec)) a
    SingleItem a -> encode jsonSchemaCodec a
    SingleItemReference a -> encode referenceCodec a

-- Optics
_properties :: L.Traversal' JSONSchema (M.Map String (ReferenceOr JSONSchema))
_properties = _Newtype <<< prop (SProxy :: SProxy "properties") <<< _Just <<< _oaiMap

_property :: String -> L.Traversal' JSONSchema (ReferenceOr JSONSchema)
_property key = _properties <<< at key <<< _Just

_required :: L.Traversal' JSONSchema (Array String)
_required = _Newtype <<< prop (SProxy :: SProxy "required") <<< _Just

_items :: L.Traversal' JSONSchema Items
_items = _Newtype <<< prop (SProxy :: SProxy "items") <<< _Just
