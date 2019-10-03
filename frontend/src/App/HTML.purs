module App.HTML where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Aff (Canceler)
import React.Basic (JSX)
import React.Basic as React

maybeHtml :: forall a. Maybe a -> (a -> JSX) -> JSX
maybeHtml m f = maybe mempty f m

whenHtml :: Boolean -> (Unit -> JSX) -> JSX
whenHtml test html = if test then html unit else mempty

 
pushCanceler
  :: forall props state
   . React.Self props { canceler :: Canceler | state }
  -> Canceler
  -> Effect Unit
pushCanceler self c = self.setState \s -> s {canceler = s.canceler <> c}