module Data.Reference where

import Prelude
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype Reference
  = Reference { _ref :: String }

derive instance newtypeReference :: Newtype Reference _

derive newtype instance eqReference :: Eq Reference

derive newtype instance showReference :: Show Reference

-- Codecs
referenceCodec :: JsonCodec Reference
referenceCodec = CAM.renameField "$ref" "_ref" >~> codec
  where
  codec = wrapIso Reference (CAR.object "Reference" { _ref: CA.string })
