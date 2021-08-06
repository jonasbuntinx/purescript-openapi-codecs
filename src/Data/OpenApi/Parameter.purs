module Data.OpenApi3.Parameter where

import Prelude
import Data.Argonaut.Core (jsonNull)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype Parameter
  = Parameter
  { name :: String
  , in :: String
  , required :: Maybe Boolean
  }

derive instance newtypeParameter :: Newtype Parameter _

derive newtype instance eqParameter :: Eq Parameter

derive newtype instance showParameter :: Show Parameter

-- Codecs
parameterCodec :: JsonCodec Parameter
parameterCodec = CAM.addDefaultField "required" jsonNull >~> codec
  where
  codec =
    wrapIso Parameter
      ( CAR.object "Parameter"
          { name: CA.string
          , in: CA.string
          , required: CAC.maybe CA.boolean
          }
      )
