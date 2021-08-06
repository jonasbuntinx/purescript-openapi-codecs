module Data.OpenApi3.PathItem where

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
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Data.Symbol (SProxy(..))
import Data.OpenApi3.Operation (Operation, operationCodec)
import Data.OpenApi3.Parameter (Parameter, parameterCodec)
import Data.ReferenceOr (ReferenceOr, referenceOrCodec)
import Data.OpenApi3.Server (Server, serverCodec)

newtype PathItem
  = PathItem
  { summary :: Maybe String
  , description :: Maybe String
  , servers :: Maybe (Array Server)
  , parameters :: Maybe (Array (ReferenceOr Parameter))
  , get :: Maybe Operation
  , put :: Maybe Operation
  , post :: Maybe Operation
  , delete :: Maybe Operation
  , options :: Maybe Operation
  , head :: Maybe Operation
  , patch :: Maybe Operation
  , trace :: Maybe Operation
  , _ref :: Maybe String
  }

derive instance newtypePathItem :: Newtype PathItem _

derive newtype instance eqPathItem :: Eq PathItem

derive newtype instance showPathItem :: Show PathItem

-- Codecs
pathItemCodec :: JsonCodec PathItem
pathItemCodec =
  CAM.addDefaultField "summary" jsonNull
    >~> CAM.addDefaultField "description" jsonNull
    >~> CAM.addDefaultField "servers" jsonNull
    >~> CAM.addDefaultField "parameters" jsonNull
    >~> CAM.addDefaultField "get" jsonNull
    >~> CAM.addDefaultField "put" jsonNull
    >~> CAM.addDefaultField "post" jsonNull
    >~> CAM.addDefaultField "delete" jsonNull
    >~> CAM.addDefaultField "options" jsonNull
    >~> CAM.addDefaultField "head" jsonNull
    >~> CAM.addDefaultField "patch" jsonNull
    >~> CAM.addDefaultField "trace" jsonNull
    >~> CAM.addDefaultField "$ref" jsonNull
    >~> CAM.renameField "$ref" "_ref"
    >~> codec
  where
  codec =
    wrapIso PathItem
      ( CAR.object "PathItem"
          { summary: CAC.maybe CA.string
          , description: CAC.maybe CA.string
          , servers: CAC.maybe (CAC.array serverCodec)
          , parameters: CAC.maybe (CAC.array (referenceOrCodec parameterCodec))
          , get: CAC.maybe operationCodec
          , put: CAC.maybe operationCodec
          , post: CAC.maybe operationCodec
          , delete: CAC.maybe operationCodec
          , options: CAC.maybe operationCodec
          , head: CAC.maybe operationCodec
          , patch: CAC.maybe operationCodec
          , trace: CAC.maybe operationCodec
          , _ref: CAC.maybe CA.string
          }
      )

-- Optics
_get :: L.Traversal' PathItem Operation
_get = _Newtype <<< prop (SProxy :: SProxy "get") <<< _Just

_put :: L.Traversal' PathItem Operation
_put = _Newtype <<< prop (SProxy :: SProxy "put") <<< _Just

_post :: L.Traversal' PathItem Operation
_post = _Newtype <<< prop (SProxy :: SProxy "post") <<< _Just

_delete :: L.Traversal' PathItem Operation
_delete = _Newtype <<< prop (SProxy :: SProxy "delete") <<< _Just

_options :: L.Traversal' PathItem Operation
_options = _Newtype <<< prop (SProxy :: SProxy "options") <<< _Just

_head :: L.Traversal' PathItem Operation
_head = _Newtype <<< prop (SProxy :: SProxy "head") <<< _Just

_patch :: L.Traversal' PathItem Operation
_patch = _Newtype <<< prop (SProxy :: SProxy "patch") <<< _Just
