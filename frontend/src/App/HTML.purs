module App.HTML where

import Prelude

import Data.Maybe (Maybe, maybe)
import React.Basic (JSX)

maybeHtml :: forall a. Maybe a -> (a -> JSX) -> JSX
maybeHtml m f = maybe mempty f m

whenHtml :: Boolean -> (Unit -> JSX) -> JSX
whenHtml test html = if test then html unit else mempty

classy
  :: ({ className :: String, children :: Array JSX } -> JSX)
  -> String
  -> (Array JSX -> JSX)
classy element className children = element { className, children }
