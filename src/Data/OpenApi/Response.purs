module Data.OpenApi3.Response where

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
import Data.OAIMap (OAIMap, _oaiMap, oaiMapCodec)

newtype Response
  = Response
  { description :: String
  -- , headers :: (Maybe ((OAIMap ((ReferenceOr Header)))))
  , content :: Maybe (OAIMap MediaType)
  -- , links :: (Maybe ((OAIMap ((ReferenceOr Link)))))
  }

derive instance newtypeResposne :: Newtype Response _

derive newtype instance eqResponse :: Eq Response

derive newtype instance showResponse :: Show Response

-- Codecs
responseCodec :: JsonCodec Response
responseCodec = CAM.addDefaultField "content" jsonNull >~> codec
  where
  codec =
    wrapIso Response
      ( CAR.object "Response"
          { description: CA.string
          , content: CAC.maybe (oaiMapCodec mediaTypeCodec)
          }
      )

-- Optics
_content :: L.Traversal' Response (M.Map String MediaType)
_content = _Newtype <<< prop (SProxy :: SProxy "content") <<< _Just <<< _oaiMap

_mediaType :: String -> L.Traversal' Response MediaType
_mediaType key = _content <<< at key <<< _Just
