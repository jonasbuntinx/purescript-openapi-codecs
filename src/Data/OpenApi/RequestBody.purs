module Data.OpenApi3.RequestBody where

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
import Data.OpenApi3.MediaType (MediaType, mediaTypeCodec)
import Data.OpenApi3.OAIMap (OAIMap, _oaiMap, oaiMapCodec)

newtype RequestBody
  = RequestBody
  { content :: OAIMap MediaType
  , description :: Maybe String
  , required :: Maybe Boolean
  }

derive instance newtypeRequestBody :: Newtype RequestBody _

derive newtype instance eqRequestBody :: Eq RequestBody

derive newtype instance showRequestBody :: Show RequestBody

-- Codecs
requestBodyCodec :: JsonCodec RequestBody
requestBodyCodec =
  CAM.addDefaultField "description" jsonNull
    >~> CAM.addDefaultField "required" jsonNull
    >~> codec
  where
  codec =
    wrapIso RequestBody
      ( CAR.object "RequestBody"
          { content: oaiMapCodec mediaTypeCodec
          , description: CAC.maybe CA.string
          , required: CAC.maybe CA.boolean
          }
      )

-- Optics
_content :: L.Lens' RequestBody (M.Map String MediaType)
_content = _Newtype <<< prop (SProxy :: SProxy "content") <<< _oaiMap

_mediaType :: String -> L.Traversal' RequestBody MediaType
_mediaType key = _content <<< at key <<< _Just
