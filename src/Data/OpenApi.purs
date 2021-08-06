module Data.OpenApi where

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
import Data.OpenApi3.Components (Components, componentsCodec)
import Data.OpenApi3.OAIMap (OAIMap, _oaiMap, oaiMapCodec)
import Data.OpenApi3.PathItem (PathItem, pathItemCodec)
import Data.OpenApi3.Server (Server, serverCodec)

newtype OpenApi
  = OpenApi
  { openapi :: String
  -- , info :: Info
  , paths :: OAIMap PathItem
  -- , externalDocs :: Maybe ExternalDocumentation
  , servers :: Maybe (Array Server)
  -- , security :: Maybe (Array (OAIMap (Array String)))
  -- , tags :: Maybe (Array Tag)
  , components :: Maybe Components
  }

derive instance newtypeOpenApi :: Newtype OpenApi _

derive newtype instance eqOpenApi :: Eq OpenApi

derive newtype instance showOpenApi :: Show OpenApi

-- Codecs
openApiCodec :: JsonCodec OpenApi
openApiCodec =
  CAM.addDefaultField "servers" jsonNull
    >~> CAM.addDefaultField "components" jsonNull
    >~> codec
  where
  codec =
    wrapIso OpenApi
      ( CAR.object "OpenApi"
          { openapi: CA.string
          , paths: oaiMapCodec pathItemCodec
          , servers: CAC.maybe (CAC.array serverCodec)
          , components: CAC.maybe componentsCodec
          }
      )

-- Optics
_paths :: L.Lens' OpenApi (M.Map String PathItem)
_paths = _Newtype <<< prop (SProxy :: SProxy "paths") <<< _oaiMap

_pathItem :: String -> L.Traversal' OpenApi PathItem
_pathItem key = _paths <<< at key <<< _Just

_servers :: L.Traversal' OpenApi (Array Server)
_servers = _Newtype <<< prop (SProxy :: SProxy "servers") <<< _Just

_components :: L.Traversal' OpenApi Components
_components = _Newtype <<< prop (SProxy :: SProxy "components") <<< _Just
