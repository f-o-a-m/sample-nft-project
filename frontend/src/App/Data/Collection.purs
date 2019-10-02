module App.Data.Collections where

import Prelude

import Data.Maybe (Maybe(..))

type LinkedCollection a =
  { items :: Array a
  , next :: Maybe Cursor
  , loading :: Boolean
  }

insertCollection :: forall a. LinkedCollection a -> Collection a -> LinkedCollection a
insertCollection l {items, next} = {items: l.items <> items, next, loading:false}

initialLinkedCollection :: forall a. LinkedCollection a
initialLinkedCollection = {items: [], next: Nothing, loading: true}

initialCursor :: Cursor
initialCursor = {limit: 100, offset: 0}

type Cursor = {limit :: Int, offset :: Int}

type Collection a =
  { items :: Array a
  , next :: Maybe Cursor
  }
