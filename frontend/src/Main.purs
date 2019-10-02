module Main where

import Prelude

import App.Components.Root (root)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  mbRootNode <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case mbRootNode of
    Nothing -> throw "Root element not found."
    Just rootNode -> render root rootNode
