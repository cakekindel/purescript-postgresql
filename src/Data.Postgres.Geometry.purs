module Data.Postgres.Geometry where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype Point = Point { x :: Number, y :: Number }

derive instance Newtype Point _
derive instance Generic Point _
derive instance Eq Point
instance Show Point where
  show = genericShow

newtype Circle = Circle { center :: Point, radius :: Number }

derive instance Newtype Circle _
derive instance Generic Circle _
derive instance Eq Circle
instance Show Circle where
  show = genericShow
