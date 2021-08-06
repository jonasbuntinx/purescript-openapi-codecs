module Data.OpenApi3.OAIMap where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Compat as CAC
import Data.Lens as L
import Data.Lens.At (at)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (dimap, wrapIso)
import Data.Tuple (Tuple)
import Foreign.Object as FO

newtype OAIMap a
  = OAIMap (M.Map String a)

derive instance newtypeOAIMap :: Newtype (OAIMap a) _

derive newtype instance eqOAIMap :: (Eq a) => Eq (OAIMap a)

derive newtype instance showOAIMap :: (Show a) => Show (OAIMap a)

-- Codecs
oaiMapCodec :: forall a. JsonCodec a -> JsonCodec (OAIMap a)
oaiMapCodec = wrapIso OAIMap <<< dimap mapToObject objectToMap <<< CAC.foreignObject
  where
  mapToObject :: M.Map String a -> FO.Object a
  mapToObject f = FO.fromFoldable ((M.toUnfoldable f) :: (Array (Tuple String a)))

  objectToMap :: FO.Object a -> M.Map String a
  objectToMap f = M.fromFoldable ((FO.toUnfoldable f) :: (Array (Tuple String a)))

-- Optics
_oaiMap :: forall a. L.Lens' (OAIMap a) (M.Map String a)
_oaiMap = _Newtype

_oaiMapValue :: forall a. String -> L.Lens' (OAIMap a) (Maybe a)
_oaiMapValue key = _Newtype <<< at key
