module Data.OpenApi3.Operation where

import Prelude
import Data.Argonaut.Core (jsonNull)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Lens (_Just)
import Data.Lens as L
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Symbol (SProxy(..))
import Data.OpenApi3.OAIMap (OAIMap, _oaiMap, oaiMapCodec)
import Data.OpenApi3.Parameter (Parameter, parameterCodec)
import Data.ReferenceOr (ReferenceOr, referenceOrCodec)
import Data.OpenApi3.RequestBody (RequestBody, requestBodyCodec)
import Data.OpenApi3.Response (Response, responseCodec)
import Data.OpenApi3.Server (Server, serverCodec)

newtype Operation
  = Operation
  { responses :: OAIMap (ReferenceOr Response)
  , tags :: Maybe (Array String)
  , summary :: Maybe String
  , description :: Maybe String
  -- , externalDocs :: Maybe ExternalDocumentation
  , operationId :: Maybe String
  , parameters :: Maybe (Array (ReferenceOr Parameter))
  , requestBody :: Maybe (ReferenceOr RequestBody)
  -- , callbacks :: Maybe (OAIMap (ReferenceOr (OAIMap PathItem)))
  , deprecated :: Maybe Boolean
  , security :: Maybe (Array (OAIMap (Array String)))
  , servers :: Maybe (Array Server)
  }

derive instance newtypeOperation :: Newtype Operation _

derive newtype instance eqOperation :: Eq Operation

derive newtype instance showOperation :: Show Operation

-- Codecs
operationCodec :: JsonCodec Operation
operationCodec =
  CAM.addDefaultField "tags" jsonNull
    >~> CAM.addDefaultField "summary" jsonNull
    >~> CAM.addDefaultField "description" jsonNull
    >~> CAM.addDefaultField "operationId" jsonNull
    >~> CAM.addDefaultField "parameters" jsonNull
    >~> CAM.addDefaultField "requestBody" jsonNull
    >~> CAM.addDefaultField "deprecated" jsonNull
    >~> CAM.addDefaultField "security" jsonNull
    >~> CAM.addDefaultField "servers" jsonNull
    >~> codec
  where
  codec =
    wrapIso Operation
      ( CAR.object "Operation"
          { responses: oaiMapCodec (referenceOrCodec responseCodec)
          , tags: CAC.maybe (CAC.array CA.string)
          , summary: CAC.maybe CA.string
          , description: CAC.maybe CA.string
          , operationId: CAC.maybe CA.string
          , parameters: CAC.maybe (CAC.array (referenceOrCodec parameterCodec))
          , requestBody: CAC.maybe (referenceOrCodec requestBodyCodec)
          , deprecated: CAC.maybe CA.boolean
          , security: CAC.maybe (CAC.array (oaiMapCodec (CAC.array CA.string)))
          , servers: CAC.maybe (CAC.array serverCodec)
          }
      )

-- Optics
_responses :: L.Lens' Operation (M.Map String (ReferenceOr Response))
_responses = _Newtype <<< prop (SProxy :: SProxy "responses") <<< _oaiMap

_response :: String -> L.Traversal' Operation (ReferenceOr Response)
_response key = _responses <<< at key <<< _Just

_parameters :: L.Traversal' Operation (Array (ReferenceOr Parameter))
_parameters = _Newtype <<< prop (SProxy :: SProxy "parameters") <<< _Just

_requestBody :: L.Traversal' Operation (ReferenceOr RequestBody)
_requestBody = _Newtype <<< prop (SProxy :: SProxy "requestBody") <<< _Just
