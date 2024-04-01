module Test.Event where

import Prelude

import Data.Either (Either(..))
import Data.Profunctor (lcmap)
import Data.Tuple.Nested (type (/\), uncurry2, (/\))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Node.EventEmitter (EventHandle(..))
import Node.EventEmitter as EE

class Curried a b | a -> b where
  curried :: (b -> Effect Unit) -> a

instance Curried (Effect Unit) Unit where
  curried f = f unit

instance Curried (a -> Effect Unit) a where
  curried = identity

instance Curried (a -> b -> Effect Unit) (a /\ b) where
  curried f = \a b -> f $ a /\ b

instance Curried (a -> b -> c -> Effect Unit) (a /\ b /\ c) where
  curried f = \a b c -> f $ a /\ b /\ c

onceAff :: forall psCb jsCb emitter r. Curried psCb r => EventHandle emitter psCb jsCb -> emitter -> Aff r
onceAff ev em = makeAff \res -> mempty <* EE.once ev (curried (res <<< Right)) em
