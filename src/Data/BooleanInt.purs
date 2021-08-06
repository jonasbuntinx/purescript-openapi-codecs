module Data.BooleanInt where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Codec (basicCodec)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, decode, encode)
import Data.Codec.Argonaut as CA
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data BooleanInt
  = ABoolean Boolean
  | AnInt Int

derive instance genericBooleanInt :: Generic BooleanInt _

instance eqBooleanInt :: Eq BooleanInt where
  eq b = genericEq b

instance showBooleanInt :: Show BooleanInt where
  show b = genericShow b

-- Codecs
booleanIntCodec :: JsonCodec BooleanInt
booleanIntCodec = basicCodec dec enc
  where
  dec :: Json -> Either JsonDecodeError BooleanInt
  dec j =
    (ABoolean <$> decode CA.boolean j)
      <|> (AnInt <$> decode CA.int j)

  enc :: BooleanInt -> Json
  enc = case _ of
    ABoolean b -> encode CA.boolean b
    AnInt i -> encode CA.int i
