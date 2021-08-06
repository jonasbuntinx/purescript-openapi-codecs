module Data.OpenApi3.MediaType where

import Prelude
import Data.Argonaut.Core (jsonNull)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Lens (_Just)
import Data.Lens as L
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Symbol (SProxy(..))
import Data.ReferenceOr (ReferenceOr, referenceOrCodec)
import Data.JSONSchema (JSONSchema, jsonSchemaCodec)

newtype MediaType
  = MediaType
  { schema :: Maybe (ReferenceOr JSONSchema)
  -- , example :: Maybe JSON
  -- , examples :: Maybe (OAIMap (ReferenceOr Example))
  -- , encoding :: Maybe (OAIMap Encoding)
  }

derive instance newtypeMediaType :: Newtype MediaType _

derive newtype instance eqMediaType :: Eq MediaType

derive newtype instance showMediaType :: Show MediaType

-- Codecs
mediaTypeCodec :: JsonCodec MediaType
mediaTypeCodec = CAM.addDefaultField "schema" jsonNull >~> codec
  where
  codec =
    wrapIso MediaType
      ( CAR.object "MediaType"
          { schema: CAC.maybe (referenceOrCodec jsonSchemaCodec)
          }
      )

-- Optics
_schema :: L.Traversal' MediaType (ReferenceOr JSONSchema)
_schema = _Newtype <<< prop (SProxy :: SProxy "schema") <<< _Just
