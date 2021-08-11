module Data.OpenApi3.Components where

import Prelude
import Data.Argonaut.Core (jsonNull)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.OAIMap (OAIMap, oaiMapCodec)
import Data.OpenApi3.Parameter (Parameter, parameterCodec)
import Data.OpenApi3.PathItem (PathItem, pathItemCodec)
import Data.ReferenceOr (ReferenceOr, referenceOrCodec)
import Data.OpenApi3.RequestBody (RequestBody, requestBodyCodec)
import Data.OpenApi3.Response (Response, responseCodec)
import Data.JSONSchema (JSONSchema, jsonSchemaCodec)

newtype Components
  = Components
  { schemas :: Maybe (OAIMap (ReferenceOr JSONSchema))
  , responses :: Maybe (OAIMap (ReferenceOr Response))
  , parameters :: Maybe (OAIMap (ReferenceOr Parameter))
  -- , examples :: Maybe (OAIMap (ReferenceOr Example))
  , requestBodies :: Maybe (OAIMap (ReferenceOr RequestBody))
  -- , headers :: Maybe (OAIMap (ReferenceOr Header))
  -- , securitySchemes :: Maybe (OAIMap SecuritySchema)
  -- , links :: Maybe (OAIMap (ReferenceOr Link))
  , callbacks :: Maybe (OAIMap (ReferenceOr (OAIMap PathItem)))
  }

derive instance newtypeComponents :: Newtype Components _

derive newtype instance eqComponents :: Eq Components

derive newtype instance showComponents :: Show Components

-- Codecs
componentsCodec :: JsonCodec Components
componentsCodec =
  CAM.addDefaultField "schemas" jsonNull
    >~> CAM.addDefaultField "responses" jsonNull
    >~> CAM.addDefaultField "parameters" jsonNull
    >~> CAM.addDefaultField "requestBodies" jsonNull
    >~> CAM.addDefaultField "callbacks" jsonNull
    >~> codec
  where
  codec =
    wrapIso Components
      ( CAR.object "Components"
          { schemas: CAC.maybe (oaiMapCodec (referenceOrCodec jsonSchemaCodec))
          , responses: CAC.maybe (oaiMapCodec (referenceOrCodec responseCodec))
          , parameters: CAC.maybe (oaiMapCodec (referenceOrCodec parameterCodec))
          , requestBodies: CAC.maybe (oaiMapCodec (referenceOrCodec requestBodyCodec))
          , callbacks: CAC.maybe (oaiMapCodec (referenceOrCodec (oaiMapCodec pathItemCodec)))
          }
      )
