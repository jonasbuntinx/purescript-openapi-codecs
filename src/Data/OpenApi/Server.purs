module Data.OpenApi3.Server where

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

newtype Server
  = Server
  { url :: String
  , description :: Maybe String
  --  , variables ::Maybe (OAIMap ServerVariable)
  }

derive instance newtypeServer :: Newtype Server _

derive newtype instance eqServer :: Eq Server

derive newtype instance showServer :: Show Server

-- Codecs
serverCodec :: JsonCodec Server
serverCodec = CAM.addDefaultField "description" jsonNull >~> codec
  where
  codec =
    wrapIso Server
      ( CAR.object "Server"
          { url: CA.string
          , description: CAC.maybe CA.string
          }
      )

-- Optics
_url :: L.Traversal' Server String
_url = _Newtype <<< prop (SProxy :: SProxy "url")

_description :: L.Traversal' Server String
_description = _Newtype <<< prop (SProxy :: SProxy "description") <<< _Just
