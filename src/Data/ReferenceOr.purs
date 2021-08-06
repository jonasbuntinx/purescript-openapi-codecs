module Data.ReferenceOr where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Codec (basicCodec)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, decode, encode)
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Lens as L
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Reference (Reference, referenceCodec)

data ReferenceOr a
  = Ref Reference
  | RealDeal a

derive instance genericReferenceOr :: Generic (ReferenceOr a) _

instance eqReferenceOr :: (Eq a) => Eq (ReferenceOr a) where
  eq b = genericEq b

instance showReferenceOr :: (Show a) => Show (ReferenceOr a) where
  show b = genericShow b

-- Codecs
referenceOrCodec :: forall a. JsonCodec a -> JsonCodec (ReferenceOr a)
referenceOrCodec codec = basicCodec dec enc
  where
  dec :: Json -> Either JsonDecodeError (ReferenceOr a)
  dec j =
    (Ref <$> decode referenceCodec j)
      <|> (RealDeal <$> decode codec j)

  enc :: ReferenceOr a -> Json
  enc = case _ of
    Ref ref -> encode referenceCodec ref
    RealDeal a -> encode codec a

-- Optics
_Ref :: forall a. L.Prism' (ReferenceOr a) Reference
_Ref =
  L.prism' Ref case _ of
    Ref ref -> Just ref
    _ -> Nothing

_RealDeal :: forall a. L.Prism' (ReferenceOr a) a
_RealDeal =
  L.prism' RealDeal case _ of
    RealDeal a -> Just a
    _ -> Nothing
